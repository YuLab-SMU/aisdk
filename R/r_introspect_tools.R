#' @title R Introspection Tools for Autonomous Debugging
#' @description
#' General-purpose primitives that let a console Agent enrich diagnostic
#' context on its own when R's `geterrmessage()` / `last.warning` /
#' `traceback()` snapshots are incomplete.
#'
#' These tools are intentionally **broad** (not problem-specific). Domain
#' tactics -- e.g. where to look for an install log, how to interpret an
#' Rcpp compilation error -- live in the `r-debug` skill (`inst/skills/r-debug/`).
#' The tools provide eyes and hands; the skill provides the playbook.
#'
#' Provided tools:
#'  - `r_eval`: run R code in an isolated subprocess, capture stdout,
#'    stderr (including from grandchild processes like compilers and
#'    `install.packages` subprocesses), messages, warnings, value, error.
#'  - `r_session_state`: structured snapshot of the live R session
#'    (`.libPaths()`, repos, search path, key env vars, options, sessionInfo).
#' @name r_introspect_tools
NULL

# ---------------------------------------------------------------------------
# r_eval safety: pattern detection + credential scrubbing + output capping
# (issue #26 layer A/C/D)
# ---------------------------------------------------------------------------

# Functions whose names alone indicate a long-running REPL or server that will
# block until timeout inside a stdin-closed subprocess. The match is on the
# call's function symbol -- both bare (`console_chat`) and namespace-qualified
# (`aisdk::console_chat`) -- so dynamic dispatch (`do.call`, `get`) is the
# main bypass left, which the layer-B subprocess marker handles.
r_eval_repl_launcher_targets <- c(
  "console_chat",
  "create_chat_session",  # session itself is fine; users actually want $send()
  "runApp", "shinyApp",
  "startServer", "startDaemonizedServer", "runServer",
  "ask_ai_interactive"
)

# Functions whose interactive return value is meaningless without a TTY. They
# do not block the subprocess (stdin is closed so they return "" / NA / 0)
# but the answer is misleading -- the agent might think the user said "no"
# when in reality the prompt never reached them.
r_eval_blind_prompt_targets <- c(
  "readline", "readLines",  # readLines is fine on files but flagged when called on stdin()
  "menu", "select.list",
  "ask_user", "console_input", "console_confirm", "console_menu"
)

#' @keywords internal
r_eval_call_name <- function(expr) {
  if (!is.call(expr)) return(NULL)
  fn <- expr[[1]]
  if (is.name(fn)) return(as.character(fn))
  if (is.call(fn) && length(fn) == 3 &&
      (identical(fn[[1]], as.name("::")) || identical(fn[[1]], as.name(":::"))) &&
      is.name(fn[[3]])) {
    return(as.character(fn[[3]]))
  }
  NULL
}

#' @keywords internal
r_eval_walk_calls <- function(expr, visit) {
  if (is.call(expr)) {
    visit(expr)
    for (arg in as.list(expr)[-1]) {
      r_eval_walk_calls(arg, visit)
    }
  } else if (is.pairlist(expr) || is.expression(expr)) {
    for (arg in as.list(expr)) {
      r_eval_walk_calls(arg, visit)
    }
  }
}

# Returns NULL if the code is fine, otherwise a list describing why we are
# refusing to execute. The body is later rendered into the result envelope so
# the LLM can read it like any other tool output and pivot.
#' @keywords internal
detect_r_eval_unsafe_pattern <- function(code) {
  parsed <- tryCatch(parse(text = code), error = function(e) NULL)
  if (is.null(parsed)) return(NULL)  # parse errors handled by inner()

  hit <- NULL
  for (top in as.list(parsed)) {
    r_eval_walk_calls(top, function(expr) {
      if (!is.null(hit)) return()
      name <- r_eval_call_name(expr)
      if (is.null(name)) return()
      if (name %in% r_eval_repl_launcher_targets) {
        hit <<- list(kind = "repl_launcher", target = name)
      } else if (name %in% r_eval_blind_prompt_targets) {
        # readLines only counts when called on stdin()
        if (name == "readLines") {
          args <- as.list(expr)[-1]
          if (length(args) == 0) return()
          first <- args[[1]]
          if (!(is.call(first) && identical(first[[1]], as.name("stdin")))) return()
        }
        hit <<- list(kind = "blind_prompt", target = name)
      }
    })
    if (!is.null(hit)) break
  }
  hit
}

# Sensitive env-var patterns (issue #26 layer C). We use NAME matching only --
# we never look at values, since false negatives on values are inevitable.
r_eval_sensitive_envvar_pattern <- "(?i)(api[_-]?key|token|secret|password|credential|access[_-]?key|private[_-]?key|pat$)"

r_eval_sensitive_envvar_explicit <- c(
  "OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GOOGLE_API_KEY", "GEMINI_API_KEY",
  "DEEPSEEK_API_KEY", "MOONSHOT_API_KEY", "GROQ_API_KEY", "XAI_API_KEY",
  "AZURE_OPENAI_KEY", "AZURE_OPENAI_API_KEY",
  "GITHUB_PAT", "GITHUB_TOKEN",
  "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN",
  "HUGGINGFACE_TOKEN", "HF_TOKEN",
  "FEISHU_APP_SECRET", "FEISHU_APP_ID"
)

#' @keywords internal
r_eval_is_sensitive_envvar <- function(name) {
  if (!nzchar(name)) return(FALSE)
  if (name %in% r_eval_sensitive_envvar_explicit) return(TRUE)
  grepl(r_eval_sensitive_envvar_pattern, name, perl = TRUE)
}

# Build the env overrides to hand to callr. callr's `env` argument is ADDITIVE
# (it does not replace the inherited parent env), so to scrub a sensitive var
# we must explicitly set it to "" -- which together with `--no-environ` on the
# subprocess command line guarantees the var is empty in the child.
#' @keywords internal
r_eval_build_env <- function(share_credentials = FALSE, extra = NULL) {
  base <- c(callr::rcmd_safe_env(), AISDK_INSIDE_R_EVAL = "1")

  if (!isTRUE(share_credentials)) {
    parent_env <- Sys.getenv()
    sensitive_names <- names(parent_env)[
      vapply(names(parent_env), r_eval_is_sensitive_envvar, logical(1))
    ]
    if (length(sensitive_names)) {
      scrub <- setNames(rep("", length(sensitive_names)), sensitive_names)
      base <- c(base[!names(base) %in% names(scrub)], scrub)
    }
  }

  if (length(extra)) {
    base <- c(base[!names(base) %in% names(extra)], extra)
  }
  base
}

# Output truncation (layer D). 256KB per stream is generous for diagnostics
# but small enough to keep the LLM's context healthy.
r_eval_output_cap_bytes <- 256L * 1024L

#' @keywords internal
r_eval_truncate_output <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(list(text = text %||% "", truncated = FALSE, original_bytes = 0L))
  }
  raw <- charToRaw(text)
  if (length(raw) <= r_eval_output_cap_bytes) {
    return(list(text = text, truncated = FALSE, original_bytes = length(raw)))
  }
  head_n <- as.integer(r_eval_output_cap_bytes * 0.7)
  tail_n <- r_eval_output_cap_bytes - head_n
  head_txt <- tryCatch(
    rawToChar(raw[seq_len(head_n)]),
    error = function(e) substr(text, 1L, head_n)
  )
  tail_txt <- tryCatch(
    rawToChar(raw[(length(raw) - tail_n + 1L):length(raw)]),
    error = function(e) substr(text, nchar(text) - tail_n + 1L, nchar(text))
  )
  list(
    text = paste0(
      head_txt,
      sprintf(
        "\n\n[... %s of output truncated -- only first %s and last %s shown ...]\n\n",
        format(length(raw) - r_eval_output_cap_bytes, big.mark = ","),
        format(head_n, big.mark = ","),
        format(tail_n, big.mark = ",")
      ),
      tail_txt
    ),
    truncated = TRUE,
    original_bytes = length(raw)
  )
}

# ---------------------------------------------------------------------------
# r_eval: isolated subprocess R execution with full output capture
# ---------------------------------------------------------------------------

#' Run R code in an isolated subprocess with safety layers
#'
#' Uses `callr::r_bg()` plus a polling loop so the parent session can react to
#' `Ctrl-C` while a long-running or persistent subprocess (e.g. `system("vim")`,
#' a hung network call) is executing. On timeout OR interrupt the subprocess
#' and its whole process tree are killed via `process$kill_tree()`.
#'
#' @keywords internal
r_eval_subprocess <- function(code,
                              timeout_secs = 30,
                              working_dir = NULL,
                              libpaths = NULL,
                              envvars = NULL,
                              share_credentials = FALSE) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    rlang::abort("Package 'callr' is required for r_eval(). Install with: install.packages('callr')")
  }

  # Layer A: refuse obvious REPL/server launchers before spending 120s timing
  # out. The agent gets a teaching message it can read like any other result.
  unsafe <- detect_r_eval_unsafe_pattern(code)
  if (!is.null(unsafe)) {
    return(list(
      stdout = "",
      stderr = "",
      result = list(
        .callr_failure = TRUE,
        .timeout = FALSE,
        .rejected = TRUE,
        .rejection = unsafe,
        .message = sprintf("r_eval refused to launch '%s' in an isolated subprocess.", unsafe$target),
        .class = "r_eval_rejected"
      )
    ))
  }

  stdout_file <- tempfile("aisdk_r_eval_stdout_", fileext = ".log")
  stderr_file <- tempfile("aisdk_r_eval_stderr_", fileext = ".log")
  # Files are unlinked only if they end up small enough to fit inline (see end
  # of this function). Truncated logs are kept so the agent can grep/read_file
  # them; they live in tempdir() and are cleaned when the R session ends.

  inner <- function(code_str, wd, libpaths) {
    if (!is.null(libpaths) && length(libpaths) > 0) {
      .libPaths(libpaths)
    }
    if (!is.null(wd) && nzchar(wd) && dir.exists(wd)) {
      setwd(wd)
    }

    state <- new.env(parent = emptyenv())
    state$messages <- character(0)
    state$warnings <- character(0)
    state$error <- NULL
    state$value <- NULL
    state$value_repr <- NULL
    state$value_class <- NULL
    state$visible <- FALSE

    expr <- tryCatch(
      parse(text = code_str),
      error = function(e) {
        state$error <- list(
          phase = "parse",
          message = conditionMessage(e),
          call = NULL
        )
        NULL
      }
    )

    if (!is.null(expr)) {
      tryCatch(
        withCallingHandlers(
          {
            evaluated <- withVisible(eval(expr, envir = globalenv()))
            state$value <- evaluated$value
            state$visible <- isTRUE(evaluated$visible)
            if (isTRUE(evaluated$visible)) {
              tryCatch(print(evaluated$value), error = function(e) NULL)
            }
          },
          message = function(m) {
            state$messages <- c(state$messages, trimws(conditionMessage(m)))
            invokeRestart("muffleMessage")
          },
          warning = function(w) {
            state$warnings <- c(state$warnings, trimws(conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          call_text <- tryCatch(
            {
              cc <- conditionCall(e)
              if (is.null(cc)) NULL else paste(deparse(cc, width.cutoff = 200L), collapse = " ")
            },
            error = function(e2) NULL
          )
          state$error <- list(
            phase = "eval",
            message = conditionMessage(e),
            call = call_text
          )
        }
      )

      if (!is.null(state$value)) {
        state$value_class <- tryCatch(class(state$value), error = function(e) NA_character_)
        state$value_repr <- tryCatch(
          {
            lines <- utils::capture.output(print(state$value))
            paste(utils::head(lines, 80L), collapse = "\n")
          },
          error = function(e) sprintf("<unprintable value: %s>", conditionMessage(e))
        )
      }
    }

    list(
      messages = state$messages,
      warnings = state$warnings,
      error = state$error,
      value_repr = state$value_repr,
      value_class = state$value_class,
      visible = state$visible
    )
  }

  callr_envvars <- r_eval_build_env(
    share_credentials = share_credentials,
    extra = if (!is.null(envvars)) envvars else NULL
  )

  # `--no-environ`: prevent the subprocess from re-reading .Renviron, which
  # would silently put any scrubbed API keys back into the subprocess env and
  # defeat layer C of issue #26.
  subprocess_cmdargs <- if (isTRUE(share_credentials)) {
    c("--slave", "--no-save", "--no-restore")
  } else {
    c("--slave", "--no-save", "--no-restore", "--no-environ")
  }

  process <- tryCatch(
    callr::r_bg(
      func = inner,
      args = list(
        code_str = code,
        wd = working_dir,
        libpaths = libpaths
      ),
      stdout = stdout_file,
      stderr = stderr_file,
      env = callr_envvars,
      cmdargs = subprocess_cmdargs,
      supervise = TRUE,
      poll_connection = FALSE
    ),
    error = function(e) e
  )

  if (inherits(process, "error")) {
    return(list(
      stdout = "",
      stderr = "",
      result = list(
        .callr_failure = TRUE,
        .timeout = FALSE,
        .message = conditionMessage(process),
        .class = class(process)
      )
    ))
  }

  deadline <- Sys.time() + as.numeric(timeout_secs)
  poll_interval <- 0.2
  timed_out <- FALSE
  user_interrupt <- FALSE

  tryCatch(
    {
      while (process$is_alive()) {
        remaining <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
        if (remaining <= 0) {
          timed_out <- TRUE
          break
        }
        Sys.sleep(min(poll_interval, remaining))
      }
    },
    interrupt = function(e) {
      user_interrupt <<- TRUE
    }
  )

  if (timed_out || user_interrupt) {
    tryCatch(process$kill_tree(), error = function(e) NULL)
    tryCatch(process$wait(timeout = 2000), error = function(e) NULL)
  }

  call_result <- if (timed_out) {
    list(
      .callr_failure = TRUE,
      .timeout = TRUE,
      .message = sprintf("subprocess exceeded %s seconds", timeout_secs),
      .class = "callr_timeout_error"
    )
  } else if (user_interrupt) {
    list(
      .callr_failure = TRUE,
      .timeout = FALSE,
      .interrupted = TRUE,
      .message = "Evaluation was interrupted by the user (Ctrl-C); subprocess killed.",
      .class = "user_interrupt"
    )
  } else {
    tryCatch(
      process$get_result(),
      error = function(e) {
        cls <- class(e)
        list(
          .callr_failure = TRUE,
          .timeout = FALSE,
          .message = conditionMessage(e),
          .class = cls
        )
      }
    )
  }

  read_file_safely <- function(path) {
    if (!file.exists(path)) return("")
    tryCatch(
      paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
      error = function(e) {
        tryCatch(
          paste(readLines(path, warn = FALSE), collapse = "\n"),
          error = function(e2) ""
        )
      }
    )
  }

  stdout_capped <- r_eval_truncate_output(read_file_safely(stdout_file))
  stderr_capped <- r_eval_truncate_output(read_file_safely(stderr_file))

  # When the captured output was small enough to fit inline, the log files add
  # no information -- delete them. When truncated, KEEP the file (in tempdir,
  # cleaned on session exit) so the agent can grep/read_file the full content.
  stdout_log_path <- NULL
  if (isTRUE(stdout_capped$truncated)) {
    stdout_log_path <- normalizePath(stdout_file, winslash = "/", mustWork = FALSE)
  } else {
    unlink(stdout_file, force = TRUE)
  }
  stderr_log_path <- NULL
  if (isTRUE(stderr_capped$truncated)) {
    stderr_log_path <- normalizePath(stderr_file, winslash = "/", mustWork = FALSE)
  } else {
    unlink(stderr_file, force = TRUE)
  }

  list(
    stdout = stdout_capped$text,
    stderr = stderr_capped$text,
    stdout_truncated = stdout_capped$truncated,
    stderr_truncated = stderr_capped$truncated,
    stdout_original_bytes = stdout_capped$original_bytes,
    stderr_original_bytes = stderr_capped$original_bytes,
    stdout_log_path = stdout_log_path,
    stderr_log_path = stderr_log_path,
    result = call_result
  )
}

#' @keywords internal
format_r_eval_result <- function(captured, code, timeout_secs) {
  res <- captured$result
  is_callr_failure <- isTRUE(res$.callr_failure)
  is_timeout <- isTRUE(res$.timeout)

  lines <- c(
    "[r_eval_begin]",
    sprintf("timeout_secs: %s", timeout_secs)
  )

  is_interrupted <- isTRUE(res$.interrupted)
  is_rejected <- isTRUE(res$.rejected)

  if (is_rejected) {
    rj <- res$.rejection %||% list()
    lines <- c(lines,
      "status: REJECTED",
      sprintf("rejection_kind: %s", rj$kind %||% "unknown"),
      sprintf("rejected_call: %s", rj$target %||% "unknown")
    )
    if (identical(rj$kind, "repl_launcher")) {
      lines <- c(lines,
        "why_blocked: this function would launch an interactive REPL or a long-running server inside a callr subprocess that has no stdin and no TTY. It would block until the 120s timeout and waste the user's time.",
        "what_to_do_instead:",
        sprintf("  - If you want to test an agent's reasoning, call generate_text(model = ..., prompt = ...) or build a session with create_chat_session() and call session$send() once -- do not enter the REPL."),
        "  - If you want to test the UI itself, ask the user to run it; you cannot drive a TTY from here.",
        "  - If you want to test a Shiny app or local server, ask the user to run it -- inspect its source instead."
      )
    } else if (identical(rj$kind, "blind_prompt")) {
      lines <- c(lines,
        "why_blocked: this call would prompt for interactive input, but the subprocess has no stdin/TTY. Its return value would be a silent default (empty string, 0, NA) that misleads you into thinking the user answered.",
        "what_to_do_instead:",
        "  - Use the parent agent's ask_user tool (NOT inside r_eval) to ask the user a question.",
        "  - If you only need to compute on a value, pass it as a literal in the code argument instead of prompting for it."
      )
    }
  } else if (is_timeout) {
    lines <- c(lines,
      "status: TIMEOUT",
      sprintf("note: subprocess exceeded %s seconds and was killed (whole process tree). Partial output below.", timeout_secs),
      "hint: if the code launched an interactive program (vim, ssh, python -i, etc.) or blocks on stdin, replace it with a non-interactive variant."
    )
  } else if (is_interrupted) {
    lines <- c(lines,
      "status: INTERRUPTED",
      "note: the user pressed Ctrl-C during evaluation. The subprocess and its child processes were killed. Partial output below.",
      "hint: do not retry the same command without a different approach -- the user wanted you to stop."
    )
  } else if (is_callr_failure) {
    lines <- c(lines,
      "status: SUBPROCESS_ERROR",
      sprintf("subprocess_error: %s", res$.message %||% "")
    )
  } else if (!is.null(res$error)) {
    lines <- c(lines,
      "status: R_ERROR",
      sprintf("error_phase: %s", res$error$phase %||% "eval"),
      "[error_message_begin]",
      res$error$message %||% "",
      "[error_message_end]"
    )
    if (!is.null(res$error$call) && nzchar(res$error$call)) {
      lines <- c(lines,
        "[error_call_begin]",
        res$error$call,
        "[error_call_end]"
      )
    }
  } else {
    lines <- c(lines, "status: OK")
  }

  append_block <- function(lines, label, content) {
    if (is.null(content)) return(lines)
    text <- if (is.character(content)) paste(content, collapse = "\n") else as.character(content)
    if (!nzchar(trimws(text))) return(lines)
    c(lines, sprintf("[%s_begin]", label), text, sprintf("[%s_end]", label))
  }

  if (!is_callr_failure && !is.null(res$value_repr)) {
    lines <- c(lines,
      sprintf("value_class: %s", paste(res$value_class %||% "NULL", collapse = ",")),
      sprintf("value_visible: %s", isTRUE(res$visible))
    )
    lines <- append_block(lines, "value_repr", res$value_repr)
  }

  truncation_hint <- function(stream_label, original_bytes, log_path) {
    path_hint <- if (!is.null(log_path) && nzchar(log_path)) {
      sprintf(
        "Full original output is saved at: %s -- query it directly with `bash` (e.g. `grep -n 'pattern' '%s'`, `tail -n 100 '%s'`, `wc -l '%s'`) or `read_file` for a specific line range. Do NOT retry r_eval just to see more output.",
        log_path, log_path, log_path, log_path
      )
    } else {
      "Original output file is unavailable (could not be retained)."
    }
    sprintf(
      "%s_truncated: original_bytes=%s retained_bytes=%s. %s",
      stream_label,
      format(original_bytes %||% 0L, big.mark = ","),
      format(r_eval_output_cap_bytes, big.mark = ","),
      path_hint
    )
  }

  lines <- append_block(lines, "stdout", captured$stdout)
  if (isTRUE(captured$stdout_truncated)) {
    lines <- c(lines, truncation_hint(
      "stdout", captured$stdout_original_bytes, captured$stdout_log_path
    ))
  }
  lines <- append_block(lines, "stderr", captured$stderr)
  if (isTRUE(captured$stderr_truncated)) {
    lines <- c(lines, truncation_hint(
      "stderr", captured$stderr_original_bytes, captured$stderr_log_path
    ))
  }

  if (!is_callr_failure) {
    if (length(res$messages %||% character(0)) > 0) {
      lines <- append_block(lines, "messages",
                            paste(sprintf("- %s", res$messages), collapse = "\n"))
    }
    if (length(res$warnings %||% character(0)) > 0) {
      lines <- append_block(lines, "warnings",
                            paste(sprintf("- %s", res$warnings), collapse = "\n"))
    }
  }

  lines <- c(lines, "[r_eval_end]")
  paste(lines, collapse = "\n")
}

# ---------------------------------------------------------------------------
# r_session_state: snapshot of the live R session
# ---------------------------------------------------------------------------

#' @keywords internal
r_session_state_default_envvars <- c(
  "R_HOME", "R_LIBS_USER", "R_LIBS_SITE", "R_USER", "R_PROFILE_USER",
  "LANG", "LC_ALL", "LC_CTYPE", "TZ",
  "PATH", "TMPDIR", "TEMP", "TMP",
  "http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY", "no_proxy", "NO_PROXY",
  "CURL_CA_BUNDLE", "SSL_CERT_FILE",
  "MAKEFLAGS",
  "RETICULATE_PYTHON",
  "GITHUB_PAT", "GITHUB_TOKEN"
)

#' @keywords internal
mask_envvar_value <- function(name, value) {
  if (is.null(value) || !nzchar(value)) return(value)
  sensitive_pat <- "(?i)token|secret|password|api[_-]?key|pat$"
  if (grepl(sensitive_pat, name, perl = TRUE)) {
    width <- nchar(value)
    if (width <= 4) return("***")
    return(paste0("***", substr(value, width - 3L, width)))
  }
  value
}

#' @keywords internal
collect_r_session_state <- function(include = c("libpaths", "repos", "envvars",
                                                 "search", "options", "session_info",
                                                 "platform", "writable_check")) {
  include <- intersect(include,
                       c("libpaths", "repos", "envvars", "search",
                         "options", "session_info", "platform", "writable_check"))

  out <- list()

  if ("platform" %in% include) {
    sysname <- tryCatch(Sys.info()[["sysname"]], error = function(e) NA_character_)
    out$platform <- list(
      r_version = R.Version()$version.string,
      r_version_short = paste(R.Version()$major, R.Version()$minor, sep = "."),
      platform = R.Version()$platform,
      os = sysname,
      working_dir = tryCatch(normalizePath(getwd(), winslash = "/", mustWork = FALSE),
                             error = function(e) ""),
      tempdir = tryCatch(normalizePath(tempdir(), winslash = "/", mustWork = FALSE),
                         error = function(e) "")
    )
  }

  if ("libpaths" %in% include) {
    paths <- tryCatch(.libPaths(), error = function(e) character(0))
    out$libpaths <- lapply(paths, function(p) {
      list(
        path = p,
        exists = dir.exists(p),
        writable = tryCatch(file.access(p, mode = 2L)[[1]] == 0L,
                            error = function(e) NA)
      )
    })
  }

  if ("repos" %in% include) {
    out$repos <- tryCatch(as.list(getOption("repos", list())), error = function(e) list())
  }

  if ("envvars" %in% include) {
    envvars <- list()
    for (name in r_session_state_default_envvars) {
      value <- Sys.getenv(name, unset = NA_character_)
      if (!is.na(value)) {
        envvars[[name]] <- mask_envvar_value(name, value)
      }
    }
    out$envvars <- envvars
  }

  if ("search" %in% include) {
    out$search <- tryCatch(search(), error = function(e) character(0))
  }

  if ("options" %in% include) {
    keys <- c("repos", "pkgType", "install.packages.check.source",
              "install.packages.compile.from.source",
              "download.file.method", "download.file.extra",
              "timeout", "encoding", "OutDec",
              "stringsAsFactors", "warn", "deparse.max.lines",
              "buildtools.check")
    opts <- list()
    for (k in keys) {
      v <- tryCatch(getOption(k), error = function(e) NULL)
      if (!is.null(v)) {
        opts[[k]] <- tryCatch(
          paste(utils::capture.output(print(v)), collapse = " "),
          error = function(e) as.character(v)
        )
      }
    }
    out$options <- opts
  }

  if ("session_info" %in% include) {
    out$session_info <- tryCatch(
      paste(utils::capture.output(utils::sessionInfo()), collapse = "\n"),
      error = function(e) NA_character_
    )
  }

  if ("writable_check" %in% include) {
    paths <- c(
      tryCatch(.libPaths(), error = function(e) character(0)),
      tryCatch(tempdir(), error = function(e) NULL),
      tryCatch(Sys.getenv("R_LIBS_USER"), error = function(e) NULL)
    )
    paths <- unique(paths[nzchar(paths)])
    out$writable_check <- lapply(paths, function(p) {
      list(
        path = p,
        exists = dir.exists(p),
        writable = tryCatch(file.access(p, mode = 2L)[[1]] == 0L,
                            error = function(e) NA)
      )
    })
  }

  out
}

#' @keywords internal
format_r_session_state <- function(state) {
  lines <- c("[r_session_state_begin]")

  if (!is.null(state$platform)) {
    lines <- c(lines, "[platform]")
    for (k in names(state$platform)) {
      lines <- c(lines, sprintf("  %s: %s", k, state$platform[[k]]))
    }
  }

  if (!is.null(state$libpaths)) {
    lines <- c(lines, "[libpaths]")
    for (lp in state$libpaths) {
      lines <- c(lines,
                 sprintf("  - %s (exists=%s, writable=%s)",
                         lp$path, lp$exists, lp$writable))
    }
  }

  if (!is.null(state$repos)) {
    lines <- c(lines, "[repos]")
    for (name in names(state$repos)) {
      lines <- c(lines, sprintf("  %s: %s", name, state$repos[[name]]))
    }
  }

  if (!is.null(state$envvars)) {
    lines <- c(lines, "[envvars]")
    if (length(state$envvars) == 0) {
      lines <- c(lines, "  (none of the tracked env vars are set)")
    } else {
      for (name in names(state$envvars)) {
        lines <- c(lines, sprintf("  %s=%s", name, state$envvars[[name]]))
      }
    }
  }

  if (!is.null(state$options)) {
    lines <- c(lines, "[options]")
    if (length(state$options) == 0) {
      lines <- c(lines, "  (none of the tracked options are set)")
    } else {
      for (name in names(state$options)) {
        lines <- c(lines, sprintf("  %s: %s", name, state$options[[name]]))
      }
    }
  }

  if (!is.null(state$search)) {
    lines <- c(lines, "[search_path]",
               paste0("  ", paste(state$search, collapse = " > ")))
  }

  if (!is.null(state$writable_check)) {
    lines <- c(lines, "[writable_check]")
    for (wc in state$writable_check) {
      lines <- c(lines,
                 sprintf("  - %s (exists=%s, writable=%s)",
                         wc$path, wc$exists, wc$writable))
    }
  }

  if (!is.null(state$session_info) && nzchar(state$session_info %||% "")) {
    lines <- c(lines, "[session_info_begin]", state$session_info, "[session_info_end]")
  }

  lines <- c(lines, "[r_session_state_end]")
  paste(lines, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Factory
# ---------------------------------------------------------------------------

#' Create R Introspection Tools
#'
#' Build Tool objects that let an Agent enrich diagnostic context on its own.
#'
#' @return A list of two Tool objects: `r_eval` and `r_session_state`.
#' @export
#' @examples
#' \dontrun{
#' tools <- create_r_introspect_tools()
#' agent <- create_agent(
#'   name = "Diagnostician",
#'   tools = tools,
#'   model = "openai:gpt-4o"
#' )
#' }
create_r_introspect_tools <- function() {
  list(
    tool(
      name = "r_eval",
      description = paste(
        "Run R code in an isolated subprocess (callr) and capture stdout, stderr,",
        "messages, warnings, the expression's value (if any), and any error.",
        "Stderr capture includes output from grandchild processes -- useful for",
        "re-running install.packages(), system(), compilation, or any command",
        "whose real error message was lost from the parent session.",
        "The subprocess does NOT inherit the user's loaded packages; it starts",
        "clean. Use `library(...)` inside `code` if you need a package loaded.",
        "Default timeout 30s; the maximum is 120s -- split long work into chunks.",
        "The user can press Ctrl-C to abort: the whole subprocess tree is killed",
        "and the tool result will report status: INTERRUPTED.",
        "",
        "Refused on sight (status: REJECTED -- see the result envelope for an",
        "alternative path):",
        "  - REPL / server entrypoints: console_chat, runApp, shinyApp, startServer, etc.",
        "  - blind interactive prompts: readline, menu, select.list (subprocess has no TTY).",
        "",
        "Credentials are NOT shared with the subprocess by default: env vars",
        "matching API keys / tokens / secrets are scrubbed. Set",
        "share_credentials = TRUE only when the user explicitly asks you to test",
        "a real API call (and tell them you are about to do so).",
        "",
        "stdout/stderr inline preview is capped at ~256KB each (head + tail with",
        "a truncation marker in the middle). When truncation kicks in, the",
        "**full original output is saved to a temp file** whose path is included",
        "in the result envelope -- use `bash` (grep/tail/wc) or `read_file` to",
        "query specific parts. Do NOT re-run r_eval just to see more output.",
        "The subprocess cannot modify the user's session."
      ),
      parameters = z_object(
        code = z_string("R code to evaluate (one or more expressions)."),
        timeout_secs = z_integer(
          "Maximum seconds to wait before killing the subprocess (default 30, hard cap 120).",
          nullable = TRUE
        ),
        working_dir = z_string(
          "Optional working directory for the subprocess. Defaults to the current R session's getwd().",
          nullable = TRUE
        ),
        inherit_libpaths = z_boolean(
          "If TRUE (default), pass the parent's .libPaths() into the subprocess so installed packages are visible.",
          nullable = TRUE
        ),
        share_credentials = z_boolean(
          paste(
            "If TRUE, pass API keys / tokens / secrets from the parent env into the subprocess.",
            "Default FALSE: credentials are scrubbed so a buggy or compromised eval cannot",
            "burn API tokens or exfiltrate keys. Only set TRUE when the user explicitly asks",
            "you to test a real API call."
          ),
          nullable = TRUE
        )
      ),
      execute = function(args) {
        code <- args$code
        if (!is.character(code) || length(code) != 1 || !nzchar(code)) {
          return("Error: `code` must be a non-empty single string.")
        }
        timeout_secs <- args$timeout_secs %||% 30L
        if (!is.numeric(timeout_secs) || timeout_secs <= 0) {
          timeout_secs <- 30L
        }
        # Cap timeout aggressively: persistent/interactive commands should never
        # be allowed to block the console for minutes. Users can split long work
        # into chunks instead. (Issue #26.)
        timeout_secs <- as.integer(min(timeout_secs, 120L))

        working_dir <- args$working_dir
        if (is.null(working_dir) || !nzchar(working_dir)) {
          working_dir <- tryCatch(getwd(), error = function(e) NULL)
        }

        inherit_libpaths <- args$inherit_libpaths %||% TRUE
        libpaths <- if (isTRUE(inherit_libpaths)) {
          tryCatch(.libPaths(), error = function(e) NULL)
        } else {
          NULL
        }

        share_credentials <- isTRUE(args$share_credentials)

        captured <- tryCatch(
          r_eval_subprocess(
            code = code,
            timeout_secs = timeout_secs,
            working_dir = working_dir,
            libpaths = libpaths,
            share_credentials = share_credentials
          ),
          error = function(e) {
            list(
              stdout = "",
              stderr = "",
              result = list(
                .callr_failure = TRUE,
                .timeout = FALSE,
                .message = conditionMessage(e),
                .class = class(e)
              )
            )
          }
        )

        format_r_eval_result(captured, code = code, timeout_secs = timeout_secs)
      }
    ),
    tool(
      name = "r_session_state",
      description = paste(
        "Return a structured snapshot of the live R session relevant for",
        "diagnosis: .libPaths() and whether each is writable, getOption('repos'),",
        "important Sys.getenv() values (locale, proxy, R_LIBS_USER, etc.),",
        "the search path, key install/download options, and sessionInfo().",
        "Sensitive env vars (tokens, secrets) are masked.",
        "Use this when an error mentions paths, packages, locale, proxies,",
        "or before suggesting any install/library/repo fix."
      ),
      parameters = z_object(
        include = z_array(
          items = z_string(),
          description = paste(
            "Which sections to include. Subset of:",
            "'platform','libpaths','repos','envvars','search','options',",
            "'session_info','writable_check'.",
            "Default: all of them except 'session_info' (which is verbose)."
          ),
          nullable = TRUE
        )
      ),
      execute = function(args) {
        default_include <- c("platform", "libpaths", "repos", "envvars",
                             "search", "options", "writable_check")
        include <- args$include
        if (is.null(include) || length(include) == 0) {
          include <- default_include
        }
        state <- collect_r_session_state(include = include)
        format_r_session_state(state)
      }
    )
  )
}
