#' @title Ask aisdk About Recent R Context
#' @description
#' Collect recent R error context, active script context, session information,
#' and workspace object summaries, then open `console_chat()` with that context
#' as the initial prompt.
#' @name ask_ai
NULL

#' @keywords internal
ai_safe_text <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }
  x <- as.character(x)
  x <- x[!is.na(x)]
  x
}

#' @keywords internal
ai_collapse_text <- function(x) {
  x <- ai_safe_text(x)
  if (length(x) == 0) {
    return("")
  }
  paste(x, collapse = "\n")
}

#' @keywords internal
ai_truncate_text <- function(text, max_chars = Inf) {
  text <- text %||% ""
  if (!is.finite(max_chars) || is.na(max_chars) || max_chars <= 0) {
    return(text)
  }
  if (nchar(text, type = "chars") <= max_chars) {
    return(text)
  }
  paste0(substr(text, 1L, max_chars), "\n... [truncated]")
}

#' @keywords internal
ai_error_tracker_env <- new.env(parent = emptyenv())

#' @keywords internal
ai_track_error <- function(error_msg) {
  if (!is.null(error_msg) && nzchar(error_msg)) {
    ai_error_tracker_env$last_error <- error_msg
    ai_error_tracker_env$last_error_time <- Sys.time()
  }
}

#' @keywords internal
ai_read_last_error <- function(max_age_secs = 300) {
  text <- tryCatch(geterrmessage(), error = function(e) "")
  text <- trimws(text %||% "")
  if (!nzchar(text)) {
    return(NULL)
  }
  if (identical(text, ai_error_tracker_env$ignored_error %||% NULL)) {
    return(NULL)
  }

  # Check if this error was recently tracked
  tracked_error <- ai_error_tracker_env$last_error
  tracked_time <- ai_error_tracker_env$last_error_time

  # If we have a tracked error and it matches, check its age
  if (!is.null(tracked_error) && !is.null(tracked_time) && identical(text, tracked_error)) {
    age_secs <- as.numeric(difftime(Sys.time(), tracked_time, units = "secs"))
    if (is.finite(max_age_secs) && age_secs > max_age_secs) {
      return(NULL)  # Error is too old
    }
  } else {
    # New error detected, track it
    ai_track_error(text)
  }

  text
}

#' @keywords internal
ai_read_traceback <- function() {
  lines <- tryCatch(utils::capture.output(traceback()), error = function(e) character(0))
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) {
    return(NULL)
  }
  text <- paste(lines, collapse = "\n")
  if (grepl("No traceback available", text, fixed = TRUE)) {
    return(NULL)
  }
  text
}

#' @keywords internal
ai_format_warning_object <- function(warnings) {
  if (is.null(warnings)) {
    return(NULL)
  }
  if (inherits(warnings, "condition")) {
    return(conditionMessage(warnings))
  }
  if (is.list(warnings)) {
    warning_names <- names(warnings) %||% rep("", length(warnings))
    lines <- vapply(seq_along(warnings), function(i) {
      message <- trimws(warning_names[[i]] %||% "")
      w <- warnings[[i]]

      value <- if (inherits(w, "condition")) {
        conditionMessage(w)
      } else if (is.call(w) || is.language(w)) {
        paste(deparse(w), collapse = " ")
      } else {
        ai_collapse_text(w)
      }
      value <- trimws(value %||% "")

      if (nzchar(message) && nzchar(value) && !identical(message, value)) {
        paste0("- ", message, " [call: ", value, "]")
      } else if (nzchar(message)) {
        paste0("- ", message)
      } else if (nzchar(value)) {
        paste0("- ", value)
      } else {
        ""
      }
    }, character(1))
    lines <- lines[nzchar(trimws(lines))]
    if (length(lines) == 0) {
      return(NULL)
    }
    return(paste(lines, collapse = "\n"))
  } else {
    values <- ai_safe_text(warnings)
  }
  values <- values[nzchar(trimws(values))]
  if (length(values) == 0) {
    return(NULL)
  }
  labels <- names(values)
  if (is.null(labels)) {
    labels <- rep("", length(values))
  }
  lines <- vapply(seq_along(values), function(i) {
    label <- labels[[i]] %||% ""
    if (nzchar(label)) {
      paste0("- ", label, ": ", values[[i]])
    } else {
      paste0("- ", values[[i]])
    }
  }, character(1))
  paste(lines, collapse = "\n")
}

#' @keywords internal
ai_read_last_warnings <- function(max_age_secs = 300) {
  warnings <- get0("last.warning", envir = baseenv(), inherits = FALSE)
  if (is.null(warnings)) {
    warnings <- get0("last.warning", envir = globalenv(), inherits = FALSE)
  }
  if (is.null(warnings)) {
    warnings <- get0(".Last.warning", envir = baseenv(), inherits = FALSE)
  }
  if (is.null(warnings)) {
    warnings <- get0(".Last.warning", envir = globalenv(), inherits = FALSE)
  }

  # Track warning timestamp if we have warnings
  if (!is.null(warnings)) {
    tracked_warnings <- ai_error_tracker_env$last_warnings
    tracked_time <- ai_error_tracker_env$last_warnings_time

    # Check if these are new warnings
    warnings_text <- ai_format_warning_object(warnings)
    if (!is.null(warnings_text) && nzchar(warnings_text)) {
      if (!identical(warnings_text, tracked_warnings)) {
        # New warnings detected
        ai_error_tracker_env$last_warnings <- warnings_text
        ai_error_tracker_env$last_warnings_time <- Sys.time()
      } else if (!is.null(tracked_time)) {
        # Same warnings, check age
        age_secs <- as.numeric(difftime(Sys.time(), tracked_time, units = "secs"))
        if (is.finite(max_age_secs) && age_secs > max_age_secs) {
          return(NULL)  # Warnings are too old
        }
      }
    }
  }

  ai_format_warning_object(warnings)
}

#' @keywords internal
ai_warning_indicates_package_install_failure <- function(warnings_text) {
  if (is.null(warnings_text) || !nzchar(warnings_text)) {
    return(FALSE)
  }
  patterns <- c(
    "installation of package .* had non-zero exit status",
    "dependency .* is not available",
    "Skipping [0-9]+ packages? not available",
    "package .* is not available"
  )
  any(vapply(patterns, function(pattern) {
    grepl(pattern, warnings_text, ignore.case = TRUE)
  }, logical(1)))
}

#' @keywords internal
ai_rstudio_available <- function() {
  requireNamespace("rstudioapi", quietly = TRUE) &&
    isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))
}

#' @keywords internal
ai_get_rstudio_document_context <- function() {
  if (!ai_rstudio_available()) {
    return(NULL)
  }

  ctx <- tryCatch(rstudioapi::getActiveDocumentContext(), error = function(e) NULL)
  if (is.null(ctx) || is.null(ctx$contents)) {
    ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  }
  if (is.null(ctx) || is.null(ctx$contents)) {
    return(NULL)
  }

  contents <- ai_collapse_text(ctx$contents)
  selection <- character(0)
  if (is.list(ctx$selection) && length(ctx$selection) > 0) {
    selection <- vapply(ctx$selection, function(sel) {
      ai_collapse_text(sel$text %||% character(0))
    }, character(1))
    selection <- selection[nzchar(trimws(selection))]
  }

  list(
    source = "rstudio_active_document",
    path = ctx$path %||% "",
    contents = contents,
    selection = if (length(selection) > 0) paste(selection, collapse = "\n\n") else ""
  )
}

#' @keywords internal
ai_collect_script_context <- function(script = NULL) {
  if (!is.null(script)) {
    script_text <- ai_collapse_text(script)
    script_path <- ""
    source <- "argument"
    if (length(script) == 1 && nzchar(script_text)) {
      candidate <- path.expand(script_text)
      if (file.exists(candidate) && !dir.exists(candidate)) {
        script_path <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
        script_text <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
        source <- "file"
      }
    }
    return(list(
      source = source,
      path = script_path,
      contents = script_text,
      selection = ""
    ))
  }

  ai_get_rstudio_document_context()
}

#' @keywords internal
ai_object_detail <- function(object) {
  dims <- tryCatch(dim(object), error = function(e) NULL)
  if (!is.null(dims)) {
    return(paste0("dim=", paste(dims, collapse = "x")))
  }
  if (is.data.frame(object)) {
    return(sprintf("rows=%d cols=%d", nrow(object), ncol(object)))
  }
  if (is.list(object)) {
    return(sprintf("length=%d", length(object)))
  }
  if (is.atomic(object)) {
    return(sprintf("length=%d", length(object)))
  }
  ""
}

#' @keywords internal
ai_collect_object_summaries <- function(envir = globalenv(), limit = Inf) {
  names_vec <- tryCatch(ls(envir, all.names = FALSE), error = function(e) character(0))
  if (length(names_vec) == 0) {
    return(data.frame(
      name = character(0),
      class = character(0),
      type = character(0),
      size = character(0),
      detail = character(0),
      stringsAsFactors = FALSE
    ))
  }
  if (is.finite(limit) && length(names_vec) > limit) {
    names_vec <- names_vec[seq_len(limit)]
  }

  rows <- lapply(names_vec, function(name) {
    tryCatch({
      object <- get(name, envir = envir, inherits = FALSE)
      data.frame(
        name = name,
        class = paste(class(object), collapse = ", "),
        type = typeof(object),
        size = format(utils::object.size(object), units = "auto"),
        detail = ai_object_detail(object),
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        name = name,
        class = "<error>",
        type = "<error>",
        size = "",
        detail = conditionMessage(e),
        stringsAsFactors = FALSE
      )
    })
  })

  do.call(rbind, rows)
}

#' @keywords internal
ai_read_command_history <- function(max_lines = 10) {
  read_history_file <- function(path) {
    if (is.null(path) || !file.exists(path)) {
      return(character(0))
    }
    tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
  }

  candidates <- list()
  tmp_history <- tempfile("aisdk-r-history-", fileext = ".Rhistory")
  on.exit(unlink(tmp_history), add = TRUE)
  saved <- tryCatch({
    utils::savehistory(tmp_history)
    TRUE
  }, error = function(e) FALSE)
  if (isTRUE(saved)) {
    candidates[[length(candidates) + 1L]] <- read_history_file(tmp_history)
  }

  candidates[[length(candidates) + 1L]] <- read_history_file(file.path(getwd(), ".Rhistory"))
  candidates[[length(candidates) + 1L]] <- read_history_file(file.path(path.expand("~"), ".Rhistory"))

  for (lines in candidates) {
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]
    lines <- lines[!grepl("ask_ai\\(", lines)]
    if (length(lines) > 0) {
      recent_lines <- utils::tail(lines, max_lines)
      return(paste(recent_lines, collapse = "\n"))
    }
  }

  NULL
}

#' Collect Context for `ask_ai()`
#'
#' Collect recent error details, traceback output, warnings, active script
#' context, session information, and lightweight workspace object summaries.
#'
#' @param script Optional script text or path. If `NULL`, RStudio active
#'   document context is used when available.
#' @param error Optional error message. Defaults to `geterrmessage()`.
#' @param traceback Optional traceback text. Defaults to `traceback()` output.
#' @param warnings Optional warning text or warning object. Defaults to
#'   R's `last.warning` context when present.
#' @param include Character vector of sections to include.
#' @param max_context_chars Maximum formatted context characters. Defaults to
#'   `Inf`, meaning no explicit truncation.
#' @param max_error_age_secs Maximum age in seconds for errors/warnings to be
#'   included. Defaults to 300 (5 minutes). Set to `Inf` to include all errors.
#' @param include_history Whether to include recent command history. Defaults to
#'   `TRUE`.
#' @param max_history_lines Maximum number of recent command history lines to
#'   include. Defaults to 10.
#' @return A structured context list with class `aisdk_ai_context`.
#' @export
collect_ai_context <- function(script = NULL,
                               error = NULL,
                               traceback = NULL,
                               warnings = NULL,
                               include = c("error", "traceback", "warnings", "script", "session", "objects", "history"),
                               max_context_chars = Inf,
                               max_error_age_secs = 300,
                               include_history = TRUE,
                               max_history_lines = 10) {
  include <- unique(include %||% character(0))

  ctx <- list(
    collected_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
    working_dir = normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    error = NULL,
    traceback = NULL,
    warnings = NULL,
    script = NULL,
    session = NULL,
    objects = NULL,
    history = NULL,
    max_context_chars = max_context_chars
  )

  if ("error" %in% include) {
    ctx$error <- ai_collapse_text(error %||% ai_read_last_error(max_age_secs = max_error_age_secs))
  }
  if ("traceback" %in% include) {
    ctx$traceback <- ai_collapse_text(traceback %||% ai_read_traceback())
  }
  if ("warnings" %in% include) {
    ctx$warnings <- ai_collapse_text(ai_format_warning_object(warnings) %||% ai_read_last_warnings(max_age_secs = max_error_age_secs))
  }
  if (is.null(error) &&
      nzchar(ctx$error %||% "") &&
      ai_warning_indicates_package_install_failure(ctx$warnings %||% "")) {
    ctx$stale_error <- ctx$error
    ctx$error <- ""
    if (is.null(traceback)) {
      ctx$traceback <- ""
    }
  }
  if ("script" %in% include) {
    ctx$script <- ai_collect_script_context(script)
  }
  if ("session" %in% include) {
    ctx$session <- paste(utils::capture.output(utils::sessionInfo()), collapse = "\n")
  }
  if ("objects" %in% include) {
    ctx$objects <- ai_collect_object_summaries(globalenv())
  }
  if ("history" %in% include && isTRUE(include_history)) {
    ctx$history <- ai_read_command_history(max_lines = max_history_lines)
  }

  class(ctx) <- "aisdk_ai_context"
  ctx
}

#' @keywords internal
format_ai_objects <- function(objects) {
  if (is.null(objects) || nrow(objects) == 0) {
    return("")
  }
  apply(objects, 1, function(row) {
    detail <- row[["detail"]] %||% ""
    suffix <- if (nzchar(detail)) paste0(" | ", detail) else ""
    sprintf("- %s | class=%s | type=%s | size=%s%s",
            row[["name"]], row[["class"]], row[["type"]], row[["size"]], suffix)
  }) |>
    paste(collapse = "\n")
}

#' @keywords internal
format_ai_context <- function(context) {
  if (!inherits(context, "aisdk_ai_context")) {
    rlang::abort("`context` must be an aisdk_ai_context object.")
  }

  script <- context$script
  script_lines <- character(0)
  if (!is.null(script) && nzchar(script$contents %||% "")) {
    script_lines <- c(
      "[script_context_begin]",
      paste0("source: ", script$source %||% "unknown"),
      if (nzchar(script$path %||% "")) paste0("path: ", script$path) else NULL,
      if (nzchar(script$selection %||% "")) c(
        "[selection_begin]",
        script$selection,
        "[selection_end]"
      ) else NULL,
      "[contents_begin]",
      script$contents,
      "[contents_end]",
      "[script_context_end]"
    )
  }

  sections <- c(
    "[aisdk_r_context_begin]",
    paste0("collected_at: ", context$collected_at %||% ""),
    paste0("working_dir: ", context$working_dir %||% ""),
    if (nzchar(context$error %||% "")) c("[last_error_begin]", context$error, "[last_error_end]") else NULL,
    if (nzchar(context$stale_error %||% "")) c("[stale_error_ignored_begin]", context$stale_error, "[stale_error_ignored_end]") else NULL,
    if (nzchar(context$traceback %||% "")) c("[traceback_begin]", context$traceback, "[traceback_end]") else NULL,
    if (nzchar(context$warnings %||% "")) c("[warnings_begin]", context$warnings, "[warnings_end]") else NULL,
    if (nzchar(context$history %||% "")) c("[recent_commands_begin]", context$history, "[recent_commands_end]") else NULL,
    script_lines,
    if (!is.null(context$objects)) c("[objects_begin]", format_ai_objects(context$objects), "[objects_end]") else NULL,
    if (nzchar(context$session %||% "")) c("[session_info_begin]", context$session, "[session_info_end]") else NULL,
    if (nzchar(context$additional_context %||% "")) c("[additional_context_begin]", context$additional_context, "[additional_context_end]") else NULL,
    "[aisdk_r_context_end]"
  )

  ai_truncate_text(paste(sections, collapse = "\n"), context$max_context_chars %||% Inf)
}

#' @keywords internal
build_ask_ai_prompt <- function(context, user_prompt = NULL, skill = NULL) {
  context_text <- format_ai_context(context)

  # Check if we have command history but no error/warning
  has_history <- !is.null(context$history) && nzchar(context$history)
  has_error <- !is.null(context$error) && nzchar(context$error)
  has_warning <- !is.null(context$warnings) && nzchar(context$warnings)

  # Adjust prompt based on what context we have
  if (has_history && !has_error && !has_warning) {
    analysis_prompt <- "Please analyze the recent R commands and session context. The commands may have produced errors or warnings that weren't captured. Diagnose any likely issues, then suggest concrete next steps."
  } else {
    analysis_prompt <- "Please analyze the following R error/session context. Diagnose the likely cause first, then suggest concrete next steps. If information is missing, say exactly what to inspect next."
  }

  lines <- c(
    if (!is.null(skill) && nzchar(skill)) paste0("@", skill) else NULL,
    analysis_prompt,
    if (!is.null(user_prompt) && nzchar(user_prompt)) c("", "[user_request_begin]", user_prompt, "[user_request_end]") else NULL,
    "",
    context_text
  )
  paste(lines, collapse = "\n")
}

#' Ask aisdk About the Current R Error Context
#'
#' Collects the recent R error/session context and opens `console_chat()` with
#' that context as the first user message. In RStudio, this function also reads
#' the active source document when available, making it suitable as an Addin
#' binding.
#'
#' @param prompt Optional user instruction to add above the collected context.
#' @param model Optional model string, `LanguageModelV1`, or `ChatSession`.
#' @param skills Skill paths, `"auto"`, or a `SkillRegistry` passed to
#'   `console_chat()`.
#' @param skill Optional skill name to force for the initial turn.
#' @param context Optional additional context text, or an `aisdk_ai_context`
#'   object to reuse.
#' @param startup_dir R session startup directory for project-aware context.
#' @param working_dir Working directory for sandboxed console tools.
#' @param sandbox_mode Sandbox mode for the console agent.
#' @param stream Whether to stream model output.
#' @param verbose Whether to show debug console output.
#' @param show_context If `TRUE`, print and return the initial prompt without
#'   launching `console_chat()`.
#' @param max_context_chars Maximum formatted context characters. Defaults to
#'   `Inf`, meaning no explicit truncation.
#' @param max_error_age_secs Maximum age in seconds for errors/warnings to be
#'   included. Defaults to 300 (5 minutes). Set to `Inf` to include all errors
#'   regardless of age.
#' @param confirm_stale_errors If `TRUE` (default), show a warning and prompt
#'   for confirmation when errors/warnings are detected but appear stale.
#' @param ... Additional arguments passed to `collect_ai_context()`.
#' @return Invisibly returns the `ChatSession` from `console_chat()`, or a
#'   preview list when `show_context = TRUE`.
#' @export
ask_ai <- function(prompt = NULL,
                   model = NULL,
                   skills = "auto",
                   skill = NULL,
                   context = NULL,
                   startup_dir = getwd(),
                   working_dir = tempdir(),
                   sandbox_mode = "permissive",
                   stream = TRUE,
                   verbose = FALSE,
                   show_context = FALSE,
                   max_context_chars = Inf,
                   max_error_age_secs = 300,
                   confirm_stale_errors = TRUE,
                   ...) {
  if (inherits(context, "aisdk_ai_context")) {
    ai_context <- context
    ai_context$max_context_chars <- max_context_chars
  } else {
    ai_context <- collect_ai_context(
      max_context_chars = max_context_chars,
      max_error_age_secs = max_error_age_secs,
      ...
    )
    ai_context$additional_context <- ai_collapse_text(context)
  }

  # Check for stale errors/warnings and confirm with user
  if (isTRUE(confirm_stale_errors) && !isTRUE(show_context)) {
    has_error <- !is.null(ai_context$error) && nzchar(ai_context$error)
    has_warnings <- !is.null(ai_context$warnings) && nzchar(ai_context$warnings)
    has_history <- !is.null(ai_context$history) && nzchar(ai_context$history)

    if (has_error || has_warnings || has_history) {
      # Check if the error/warning is from geterrmessage() (not user-provided)
      user_provided_error <- !missing(context) && (
        (is.list(context) && !is.null(context$error)) ||
        (!is.null(list(...)$error))
      )

      if (!user_provided_error) {
        cat("\n")
        cat(paste0("\u250c\u2500 Detected Context \u2500", strrep("\u2500", 44), "\u2510\n"))
        if (has_error) {
          error_preview <- substr(ai_context$error, 1, 200)
          if (nchar(ai_context$error) > 200) error_preview <- paste0(error_preview, "...")
          cat("\u2502 Error: ", error_preview, "\n", sep = "")
        }
        if (has_warnings) {
          warning_preview <- substr(ai_context$warnings, 1, 200)
          if (nchar(ai_context$warnings) > 200) warning_preview <- paste0(warning_preview, "...")
          cat("\u2502 Warning: ", warning_preview, "\n", sep = "")
        }
        if (!has_error && nzchar(ai_context$stale_error %||% "")) {
          stale_preview <- substr(ai_context$stale_error, 1, 160)
          if (nchar(ai_context$stale_error) > 160) stale_preview <- paste0(stale_preview, "...")
          cat("\u2502 Ignored stale error: ", stale_preview, "\n", sep = "")
        }
        if (has_history) {
          history_lines <- unlist(strsplit(ai_context$history, "\n", fixed = TRUE), use.names = FALSE)
          history_preview <- utils::tail(history_lines[nzchar(trimws(history_lines))], 3)
          history_preview <- paste(history_preview, collapse = " | ")
          if (nchar(history_preview) > 200) history_preview <- paste0(substr(history_preview, 1, 200), "...")
          cat("\u2502 Recent commands: ", history_preview, "\n", sep = "")
        }
        cat("\u2514\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2518\n")
        cat("\n")

        response <- readline("Use this error/warning context? [Y/n/clear]: ")
        response <- tolower(trimws(response))

        if (response == "clear" || response == "c") {
          clear_error_context()
          cat("\u2713 Error context cleared. Restarting ask_ai() without error context...\n\n")
          return(ask_ai(
            prompt = prompt,
            model = model,
            skills = skills,
            skill = skill,
            context = context,
            startup_dir = startup_dir,
            working_dir = working_dir,
            sandbox_mode = sandbox_mode,
            stream = stream,
            verbose = verbose,
            show_context = show_context,
            max_context_chars = max_context_chars,
            max_error_age_secs = max_error_age_secs,
            confirm_stale_errors = FALSE,  # Don't prompt again
            ...
          ))
        } else if (response == "n" || response == "no") {
          # Remove error/warning context
          ai_context$error <- NULL
          ai_context$traceback <- NULL
          ai_context$warnings <- NULL
          cat("\u2713 Proceeding without error/warning context.\n\n")
        } else {
          cat("\u2713 Using detected error/warning context.\n\n")
        }
      }
    }
  }

  initial_prompt <- build_ask_ai_prompt(ai_context, user_prompt = prompt, skill = skill)

  if (isTRUE(show_context)) {
    cat(initial_prompt, "\n", sep = "")
    return(invisible(list(context = ai_context, prompt = initial_prompt)))
  }

  console_chat(
    session = model,
    skills = skills,
    working_dir = working_dir,
    startup_dir = startup_dir,
    sandbox_mode = sandbox_mode,
    stream = stream,
    verbose = verbose,
    initial_prompt = initial_prompt
  )
}

#' Clear Error Context for ask_ai()
#'
#' Clears the tracked error and warning context used by `ask_ai()`. This is
#' useful when you want to start a fresh debugging session without stale
#' error messages from previous operations.
#'
#' @return Invisibly returns `TRUE`.
#' @export
#' @examples
#' \dontrun{
#' # Clear stale errors before starting a new debugging session
#' clear_error_context()
#' ask_ai()
#' }
clear_error_context <- function() {
  current_error <- tryCatch(trimws(geterrmessage() %||% ""), error = function(e) "")
  ai_error_tracker_env$last_error <- NULL
  ai_error_tracker_env$last_error_time <- NULL
  ai_error_tracker_env$last_warnings <- NULL
  ai_error_tracker_env$last_warnings_time <- NULL
  ai_error_tracker_env$ignored_error <- if (nzchar(current_error)) current_error else NULL

  if (exists("last.warning", envir = baseenv(), inherits = FALSE)) {
    assign("last.warning", NULL, envir = baseenv())
  }
  if (exists("last.warning", envir = globalenv(), inherits = FALSE)) {
    rm("last.warning", envir = globalenv())
  }
  if (exists(".Last.warning", envir = baseenv(), inherits = FALSE)) {
    assign(".Last.warning", NULL, envir = baseenv())
  }
  if (exists(".Last.warning", envir = globalenv(), inherits = FALSE)) {
    rm(".Last.warning", envir = globalenv())
  }

  invisible(TRUE)
}
