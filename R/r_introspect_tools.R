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
# r_eval: isolated subprocess R execution with full output capture
# ---------------------------------------------------------------------------

#' @keywords internal
r_eval_subprocess <- function(code,
                              timeout_secs = 30,
                              working_dir = NULL,
                              libpaths = NULL,
                              envvars = NULL) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    rlang::abort("Package 'callr' is required for r_eval(). Install with: install.packages('callr')")
  }

  stdout_file <- tempfile("aisdk_r_eval_stdout_", fileext = ".log")
  stderr_file <- tempfile("aisdk_r_eval_stderr_", fileext = ".log")
  on.exit(unlink(c(stdout_file, stderr_file), force = TRUE), add = TRUE)

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

  callr_envvars <- c(
    callr::rcmd_safe_env(),
    if (!is.null(envvars)) envvars else character(0)
  )

  call_result <- tryCatch(
    callr::r(
      func = inner,
      args = list(
        code_str = code,
        wd = working_dir,
        libpaths = libpaths
      ),
      stdout = stdout_file,
      stderr = stderr_file,
      timeout = timeout_secs,
      env = callr_envvars,
      show = FALSE
    ),
    error = function(e) {
      cls <- class(e)
      is_timeout <- any(c("system_command_timeout_error", "callr_timeout_error") %in% cls) ||
        grepl("timeout|timed out", conditionMessage(e), ignore.case = TRUE)
      list(
        .callr_failure = TRUE,
        .timeout = is_timeout,
        .message = conditionMessage(e),
        .class = cls
      )
    }
  )

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

  list(
    stdout = read_file_safely(stdout_file),
    stderr = read_file_safely(stderr_file),
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

  if (is_timeout) {
    lines <- c(lines,
      "status: TIMEOUT",
      sprintf("note: subprocess exceeded %s seconds. The R code was killed; partial output below.", timeout_secs)
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

  lines <- append_block(lines, "stdout", captured$stdout)
  lines <- append_block(lines, "stderr", captured$stderr)

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
        "Default timeout 30s. The subprocess cannot modify the user's session."
      ),
      parameters = z_object(
        code = z_string("R code to evaluate (one or more expressions)."),
        timeout_secs = z_integer(
          "Maximum seconds to wait before killing the subprocess (default 30).",
          nullable = TRUE
        ),
        working_dir = z_string(
          "Optional working directory for the subprocess. Defaults to the current R session's getwd().",
          nullable = TRUE
        ),
        inherit_libpaths = z_boolean(
          "If TRUE (default), pass the parent's .libPaths() into the subprocess so installed packages are visible.",
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
        timeout_secs <- as.integer(min(timeout_secs, 600L))

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

        captured <- tryCatch(
          r_eval_subprocess(
            code = code,
            timeout_secs = timeout_secs,
            working_dir = working_dir,
            libpaths = libpaths
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
