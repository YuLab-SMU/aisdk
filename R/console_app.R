#' @title Console App State Helpers
#' @description
#' Internal helpers for the incremental console TUI architecture. These
#' functions centralize view mode, capability detection, per-turn transcript
#' state, and append-only status/timeline rendering.
#' @name console_app
#' @keywords internal
NULL

console_app_overlay_counter <- local({
  state <- new.env(parent = emptyenv())
  state$next_id <- 1L
  function() {
    id <- sprintf("overlay-%d", state$next_id)
    state$next_id <- state$next_id + 1L
    id
  }
})

#' @keywords internal
normalize_console_view_mode <- function(view_mode = "clean") {
  mode <- tolower(view_mode %||% "clean")
  if (!mode %in% c("clean", "inspect", "debug")) {
    rlang::abort("view_mode must be one of 'clean', 'inspect', or 'debug'.")
  }
  mode
}

#' @keywords internal
console_view_mode_tool_log_mode <- function(view_mode) {
  if (identical(normalize_console_view_mode(view_mode), "debug")) {
    "detailed"
  } else {
    "compact"
  }
}

#' @keywords internal
console_view_mode_show_thinking <- function(view_mode) {
  identical(normalize_console_view_mode(view_mode), "debug")
}

#' @keywords internal
detect_console_capabilities <- function() {
  has_cli <- requireNamespace("cli", quietly = TRUE)
  ansi_colors <- if (has_cli) {
    tryCatch(cli::num_ansi_colors(), error = function(e) 1L)
  } else {
    1L
  }

  list(
    interactive = interactive(),
    ansi = isTRUE(ansi_colors > 1L),
    unicode = isTRUE(l10n_info()[["UTF-8"]]),
    cursor_addressing = interactive() && isTRUE(ansi_colors > 1L),
    bracketed_paste = FALSE,
    truecolor = isTRUE(ansi_colors >= 256L),
    inline_images = FALSE,
    ime_safe_cursor = FALSE
  )
}

#' @keywords internal
create_console_app_state <- function(session,
                                     agent_enabled = FALSE,
                                     sandbox_mode = "permissive",
                                     stream_enabled = TRUE,
                                     local_execution_enabled = FALSE,
                                     view_mode = "clean",
                                     capabilities = detect_console_capabilities()) {
  state <- new.env(parent = emptyenv())
  state$session <- session
  state$agent_enabled <- isTRUE(agent_enabled)
  state$sandbox_mode <- sandbox_mode
  state$stream_enabled <- isTRUE(stream_enabled)
  state$local_execution_enabled <- isTRUE(local_execution_enabled)
  state$view_mode <- normalize_console_view_mode(view_mode)
  state$capabilities <- capabilities
  state$phase <- "idle"
  state$tool_state <- "idle"
  state$turns <- list()
  state$current_turn_id <- NULL
  state$next_turn_id <- 1L
  state$next_tool_id <- 1L
  state$overlay_stack <- list()
  state$focus_target <- "composer"
  state$last_rendered_frame <- NULL
  console_app_sync_session(state, session)
  state$local_execution_enabled <- isTRUE(local_execution_enabled)
  state
}

#' @keywords internal
console_app_sync_session <- function(state, session = state$session) {
  state$session <- session
  state$model_id <- session$get_model_id() %||% "(not set)"
  state$local_execution_enabled <- isTRUE(session$get_envir()$.local_mode)
  invisible(state)
}

#' @keywords internal
console_app_set_view_mode <- function(state, view_mode) {
  state$view_mode <- normalize_console_view_mode(view_mode)
  invisible(state)
}

#' @keywords internal
console_app_set_stream_enabled <- function(state, stream_enabled) {
  state$stream_enabled <- isTRUE(stream_enabled)
  invisible(state)
}

#' @keywords internal
console_app_set_local_execution_enabled <- function(state, enabled) {
  state$local_execution_enabled <- isTRUE(enabled)
  invisible(state)
}

#' @keywords internal
console_app_status_snapshot <- function(state) {
  list(
    model_id = state$model_id %||% "(not set)",
    sandbox_mode = state$sandbox_mode %||% "unknown",
    view_mode = state$view_mode %||% "clean",
    stream_enabled = isTRUE(state$stream_enabled),
    local_execution_enabled = isTRUE(state$local_execution_enabled),
    tool_state = state$tool_state %||% "idle",
    phase = state$phase %||% "idle"
  )
}

#' @keywords internal
build_console_status_line <- function(state) {
  paste(build_console_status_segments(state, compact = FALSE), collapse = " | ")
}

#' @keywords internal
build_console_status_segments <- function(state, compact = FALSE) {
  snapshot <- console_app_status_snapshot(state)
  context <- get_console_context_metrics(state$session)
  context_fields <- character(0)
  if (!is.null(context)) {
    suffix <- if (isTRUE(context$estimated)) "(est)" else ""
    context_label <- paste0("Ctx", suffix)
    used_label <- paste0("Used", suffix)
    left_label <- paste0("Left", suffix)
    if (!is.na(context$context_window)) {
      context_fields <- c(
        context_fields,
        sprintf("%s: %s", context_label, format_console_token_compact(context$context_window)),
        sprintf("%s: %s", used_label, format_console_token_compact(context$used_tokens)),
        sprintf("%s: %s", left_label, format_console_token_compact(context$remaining_tokens))
      )
    } else if (!is.na(context$used_tokens)) {
      context_fields <- c(
        context_fields,
        sprintf("%s: %s", used_label, format_console_token_compact(context$used_tokens))
      )
    }
    if (!is.na(context$max_output)) {
      context_fields <- c(
        context_fields,
        sprintf("Out: %s", format_console_token_compact(context$max_output))
      )
    }
  }

  model_value <- if (compact) compact_text_preview(snapshot$model_id, width = 28) else snapshot$model_id
  c(
    sprintf("Model: %s", model_value),
    context_fields,
    sprintf(if (compact) "Sb: %s" else "Sandbox: %s", snapshot$sandbox_mode),
    sprintf(if (compact) "View: %s" else "View: %s", snapshot$view_mode),
    sprintf(if (compact) "Strm: %s" else "Stream: %s", if (snapshot$stream_enabled) "on" else "off"),
    sprintf(if (compact) "Local: %s" else "Local: %s", if (snapshot$local_execution_enabled) "on" else "off"),
    sprintf(if (compact) "Tools: %s" else "Tools: %s", snapshot$tool_state)
  )
}

#' @keywords internal
pack_console_status_segments <- function(segments, width) {
  lines <- character(0)
  current <- character(0)

  for (segment in segments) {
    candidate <- if (length(current) == 0) {
      segment
    } else {
      paste(c(current, segment), collapse = " | ")
    }

    if (length(current) > 0 && nchar(candidate, type = "width") > width) {
      lines <- c(lines, paste(current, collapse = " | "))
      current <- segment
    } else {
      current <- c(current, segment)
    }
  }

  if (length(current) > 0) {
    lines <- c(lines, paste(current, collapse = " | "))
  }

  lines
}

#' @keywords internal
build_console_status_lines <- function(state, width = getOption("width", 80)) {
  width <- max(40L, as.integer(width %||% 80L))

  if (width >= 110L) {
    return(build_console_status_line(state))
  }

  compact <- width < 70L
  segments <- build_console_status_segments(state, compact = compact)

  primary_idx <- c(1L, grep("^Ctx|^Used|^Left|^Out:", segments), grep("^View:", segments), grep("^Tools:", segments))
  primary_idx <- unique(primary_idx[primary_idx >= 1L & primary_idx <= length(segments)])
  secondary_idx <- setdiff(seq_along(segments), primary_idx)

  c(
    pack_console_status_segments(segments[primary_idx], width = width),
    pack_console_status_segments(segments[secondary_idx], width = width)
  )
}

#' @keywords internal
render_console_status_bar <- function(state, width = getOption("width", 80)) {
  lines <- build_console_status_lines(state, width = width)

  if (!requireNamespace("cli", quietly = TRUE)) {
    cat(paste0(lines, "\n"), sep = "")
    return(invisible(lines))
  }

  for (line in lines) {
    cli::cli_text(cli::col_grey(line))
  }
  cli::cli_rule()
  invisible(lines)
}

#' @keywords internal
console_app_get_active_overlay <- function(state) {
  if (length(state$overlay_stack) == 0) {
    return(NULL)
  }
  state$overlay_stack[[length(state$overlay_stack)]]
}

#' @keywords internal
console_app_open_overlay <- function(state, type, title, lines, payload = NULL, focus_target = NULL) {
  overlay <- list(
    id = console_app_overlay_counter(),
    type = type,
    title = title,
    lines = lines %||% character(0),
    payload = payload,
    focus_target = focus_target %||% paste0("overlay:", type),
    opened_at = Sys.time()
  )

  state$overlay_stack[[length(state$overlay_stack) + 1L]] <- overlay
  state$focus_target <- overlay$focus_target
  invisible(overlay)
}

#' @keywords internal
console_app_close_overlay <- function(state, overlay_id = NULL) {
  if (length(state$overlay_stack) == 0) {
    state$focus_target <- "composer"
    return(invisible(NULL))
  }

  if (is.null(overlay_id)) {
    state$overlay_stack <- state$overlay_stack[-length(state$overlay_stack)]
  } else {
    keep <- vapply(state$overlay_stack, function(item) !identical(item$id, overlay_id), logical(1))
    state$overlay_stack <- state$overlay_stack[keep]
  }

  active <- console_app_get_active_overlay(state)
  state$focus_target <- if (is.null(active)) "composer" else active$focus_target
  invisible(active)
}

#' @keywords internal
console_app_close_overlay_by_type <- function(state, type) {
  if (length(state$overlay_stack) == 0) {
    state$focus_target <- "composer"
    return(invisible(NULL))
  }

  keep <- vapply(state$overlay_stack, function(item) !identical(item$type, type), logical(1))
  state$overlay_stack <- state$overlay_stack[keep]
  active <- console_app_get_active_overlay(state)
  state$focus_target <- if (is.null(active)) "composer" else active$focus_target
  invisible(active)
}

#' @keywords internal
build_console_overlay_box <- function(state, overlay = console_app_get_active_overlay(state)) {
  if (is.null(overlay)) {
    return(character(0))
  }

  unicode <- isTRUE(state$capabilities$unicode)
  top_left <- if (unicode) "\u256d" else "+"
  top_right <- if (unicode) "\u256e" else "+"
  bottom_left <- if (unicode) "\u2570" else "+"
  bottom_right <- if (unicode) "\u256f" else "+"
  horizontal <- if (unicode) "\u2500" else "-"
  vertical <- if (unicode) "\u2502" else "|"

  content <- c(
    sprintf("Type: %s", overlay$type %||% "overlay"),
    overlay$lines %||% character(0),
    "Close: /inspect close"
  )

  width <- max(nchar(c(overlay$title %||% "Overlay", content), type = "width"), 24L)
  border <- paste0(strrep(horizontal, width + 2L))
  title_line <- sprintf("%s %-*s %s", vertical, width, overlay$title %||% "Overlay", vertical)
  content_lines <- vapply(content, function(line) {
    sprintf("%s %-*s %s", vertical, width, line, vertical)
  }, character(1))

  c(
    paste0(top_left, border, top_right),
    title_line,
    paste0(vertical, strrep(" ", width + 2L), vertical),
    content_lines,
    paste0(bottom_left, border, bottom_right)
  )
}

#' @keywords internal
render_console_overlay <- function(state, overlay = console_app_get_active_overlay(state)) {
  lines <- build_console_overlay_box(state, overlay)
  if (!length(lines)) {
    return(invisible(lines))
  }

  if (!requireNamespace("cli", quietly = TRUE)) {
    cat(paste0(lines, "\n"), sep = "")
    return(invisible(lines))
  }

  cli::cli_text("")
  for (line in lines) {
    cli::cli_text(cli::col_yellow(line))
  }
  invisible(lines)
}

#' @keywords internal
console_app_start_turn <- function(state, user_input) {
  turn_id <- state$next_turn_id
  state$next_turn_id <- state$next_turn_id + 1L

  turn <- list(
    turn_id = turn_id,
    started_at = Sys.time(),
    ended_at = NULL,
    phase = "thinking",
    user_text = user_input %||% "",
    assistant_text = "",
    tool_calls = list(),
    warnings = character(),
    messages = character(),
    elapsed_ms = NULL
  )

  state$current_turn_id <- turn_id
  state$turns[[as.character(turn_id)]] <- turn
  state$phase <- "thinking"
  state$tool_state <- "idle"
  invisible(turn)
}

#' @keywords internal
console_app_get_current_turn <- function(state) {
  turn_id <- state$current_turn_id
  if (is.null(turn_id)) {
    return(NULL)
  }
  state$turns[[as.character(turn_id)]]
}

#' @keywords internal
console_app_update_current_turn <- function(state, turn) {
  if (is.null(turn$turn_id)) {
    return(invisible(state))
  }
  state$turns[[as.character(turn$turn_id)]] <- turn
  invisible(state)
}

#' @keywords internal
console_app_append_assistant_text <- function(state, text) {
  if (is.null(text) || !nzchar(text)) {
    return(invisible(state))
  }

  turn <- console_app_get_current_turn(state)
  if (is.null(turn)) {
    return(invisible(state))
  }

  turn$assistant_text <- paste0(turn$assistant_text %||% "", text)
  if (length(turn$tool_calls) > 0) {
    state$phase <- "rendering"
  }
  turn$phase <- state$phase
  console_app_update_current_turn(state, turn)
}

#' @keywords internal
console_app_record_tool_start <- function(state, name, arguments) {
  turn <- console_app_get_current_turn(state)
  if (is.null(turn)) {
    return(invisible(state))
  }

  tool_id <- sprintf("tool-%d", state$next_tool_id)
  state$next_tool_id <- state$next_tool_id + 1L

  turn$tool_calls[[length(turn$tool_calls) + 1L]] <- list(
    tool_id = tool_id,
    name = name,
    status = "running",
    start_time = Sys.time(),
    end_time = NULL,
    elapsed_ms = NULL,
    args_summary = compact_tool_start_label(name, arguments),
    result_summary = NULL,
    messages = character(0),
    warnings = character(0),
    raw_args = arguments,
    raw_result = NULL
  )

  turn$phase <- "tool_running"
  state$phase <- "tool_running"
  state$tool_state <- "running"
  console_app_update_current_turn(state, turn)
}

#' @keywords internal
extract_console_tool_diagnostics <- function(raw_result, rendered_result = raw_result) {
  messages <- character(0)
  warnings <- character(0)

  if (is.list(raw_result)) {
    messages <- c(messages, raw_result$messages %||% character(0))
    warnings <- c(warnings, raw_result$warnings %||% character(0))
  }

  if (is.character(raw_result)) {
    messages <- c(messages, attr(raw_result, "aisdk_messages", exact = TRUE) %||% character(0))
    warnings <- c(warnings, attr(raw_result, "aisdk_warnings", exact = TRUE) %||% character(0))
  }

  text_lines <- character(0)
  if (is.character(rendered_result)) {
    text_lines <- unlist(strsplit(rendered_result, "\n", fixed = TRUE), use.names = FALSE)
  } else if (is.character(raw_result)) {
    text_lines <- unlist(strsplit(raw_result, "\n", fixed = TRUE), use.names = FALSE)
  }

  if (length(text_lines)) {
    msg_lines <- grep("^Message:\\s*", text_lines, value = TRUE)
    warn_lines <- grep("^Warning:\\s*", text_lines, value = TRUE)
    if (length(msg_lines)) {
      messages <- c(messages, sub("^Message:\\s*", "", msg_lines))
    }
    if (length(warn_lines)) {
      warnings <- c(warnings, sub("^Warning:\\s*", "", warn_lines))
    }
  }

  list(
    messages = unique(trimws(messages[nzchar(trimws(messages))])),
    warnings = unique(trimws(warnings[nzchar(trimws(warnings))]))
  )
}

#' @keywords internal
console_app_record_tool_result <- function(state, name, result, success = TRUE, raw_result = result) {
  turn <- console_app_get_current_turn(state)
  if (is.null(turn) || length(turn$tool_calls) == 0) {
    return(invisible(state))
  }

  failed <- tool_result_failed(result, success)
  match_idx <- NULL
  for (i in rev(seq_along(turn$tool_calls))) {
    item <- turn$tool_calls[[i]]
    if (identical(item$name, name) && identical(item$status, "running")) {
      match_idx <- i
      break
    }
  }
  if (is.null(match_idx)) {
    match_idx <- length(turn$tool_calls)
  }

  item <- turn$tool_calls[[match_idx]]
  item$end_time <- Sys.time()
  item$status <- if (failed) "failed" else "done"
  item$result_summary <- compact_tool_result_label(name, result, success = !failed)
  item$raw_result <- raw_result

  diagnostics <- extract_console_tool_diagnostics(raw_result, rendered_result = result)
  item$messages <- diagnostics$messages
  item$warnings <- diagnostics$warnings

  if (!is.null(item$start_time) && !is.null(item$end_time)) {
    item$elapsed_ms <- as.numeric(difftime(item$end_time, item$start_time, units = "secs")) * 1000
  }

  turn$tool_calls[[match_idx]] <- item
  turn$messages <- unique(c(turn$messages %||% character(0), diagnostics$messages))
  turn$warnings <- unique(c(turn$warnings %||% character(0), diagnostics$warnings))
  turn$phase <- if (failed) "error" else "rendering"
  state$phase <- turn$phase
  state$tool_state <- if (failed) "error" else "idle"
  console_app_update_current_turn(state, turn)
}

#' @keywords internal
console_app_finish_turn <- function(state, failed = FALSE) {
  turn <- console_app_get_current_turn(state)
  if (is.null(turn)) {
    state$phase <- if (failed) "error" else "idle"
    state$tool_state <- if (failed) "error" else "idle"
    return(invisible(state))
  }

  turn$ended_at <- Sys.time()
  turn$phase <- if (failed) "error" else "done"
  if (!is.null(turn$started_at) && !is.null(turn$ended_at)) {
    turn$elapsed_ms <- as.numeric(difftime(turn$ended_at, turn$started_at, units = "secs")) * 1000
  }

  state$phase <- if (failed) "error" else "idle"
  if (!failed && identical(state$tool_state, "running")) {
    state$tool_state <- "idle"
  }

  console_app_update_current_turn(state, turn)
}

#' @keywords internal
format_console_tool_timeline <- function(turn) {
  if (is.null(turn) || length(turn$tool_calls) == 0) {
    return(character(0))
  }

  vapply(seq_along(turn$tool_calls), function(i) {
    tool <- turn$tool_calls[[i]]
    duration <- if (!is.null(tool$elapsed_ms) && is.finite(tool$elapsed_ms)) {
      sprintf(" (%.0f ms)", tool$elapsed_ms)
    } else {
      ""
    }
    paste0(
      i, ". ", tool$name,
      " [", tool$status, "] ",
      tool$args_summary %||% paste0("Running ", tool$name),
      if (!is.null(tool$result_summary)) paste0(" -> ", tool$result_summary) else "",
      if (length(tool$messages %||% character(0))) paste0(" | messages: ", length(tool$messages)) else "",
      if (length(tool$warnings %||% character(0))) paste0(" | warnings: ", length(tool$warnings)) else "",
      duration
    )
  }, character(1))
}

#' @keywords internal
render_console_tool_timeline <- function(state, turn = console_app_get_current_turn(state)) {
  lines <- format_console_tool_timeline(turn)
  if (length(lines) == 0) {
    return(invisible(lines))
  }

  if (!requireNamespace("cli", quietly = TRUE)) {
    cat("Tool Timeline\n", sep = "")
    cat(paste0("- ", lines, "\n"), sep = "")
    return(invisible(lines))
  }

  cli::cli_h2("Tool Timeline")
  cli::cli_ul(lines)
  invisible(lines)
}

#' @keywords internal
console_app_get_last_turn <- function(state) {
  if (length(state$turns) == 0) {
    return(NULL)
  }

  turn_ids <- suppressWarnings(as.integer(names(state$turns)))
  if (all(is.na(turn_ids))) {
    return(state$turns[[length(state$turns)]])
  }

  state$turns[[as.character(max(turn_ids, na.rm = TRUE))]]
}

#' @keywords internal
console_app_get_turn_by_id <- function(state, turn_id) {
  if (is.null(turn_id)) {
    return(NULL)
  }
  state$turns[[as.character(turn_id)]]
}

#' @keywords internal
format_console_turn_detail <- function(turn) {
  if (is.null(turn)) {
    return(character(0))
  }

  elapsed <- if (!is.null(turn$elapsed_ms) && is.finite(turn$elapsed_ms)) {
    sprintf("%.0f ms", turn$elapsed_ms)
  } else {
    "n/a"
  }

  assistant_preview <- compact_text_preview(turn$assistant_text %||% "", width = 120)
  user_preview <- compact_text_preview(turn$user_text %||% "", width = 120)
  tool_lines <- format_console_tool_timeline(turn)

  c(
    sprintf("Turn: %s", turn$turn_id %||% "unknown"),
    sprintf("Phase: %s", turn$phase %||% "unknown"),
    sprintf("Elapsed: %s", elapsed),
    sprintf("User: %s", user_preview),
    sprintf("Assistant: %s", assistant_preview),
    if (length(turn$messages %||% character(0))) paste0("Messages: ", paste(turn$messages, collapse = " | ")) else "Messages: none",
    if (length(turn$warnings %||% character(0))) paste0("Warnings: ", paste(turn$warnings, collapse = " | ")) else "Warnings: none",
    if (length(tool_lines)) "Timeline:" else "Timeline: none",
    if (length(tool_lines)) paste0("- ", tool_lines) else character(0)
  )
}

#' @keywords internal
format_console_tool_detail <- function(turn, tool_index) {
  if (is.null(turn) || length(turn$tool_calls) == 0) {
    return(character(0))
  }

  if (length(tool_index) == 0 || is.na(tool_index) || tool_index < 1 || tool_index > length(turn$tool_calls)) {
    return(character(0))
  }

  tool <- turn$tool_calls[[tool_index]]
  elapsed <- if (!is.null(tool$elapsed_ms) && is.finite(tool$elapsed_ms)) {
    sprintf("%.0f ms", tool$elapsed_ms)
  } else {
    "n/a"
  }

  args_preview <- tryCatch(
    compact_text_preview(safe_to_json(tool$raw_args, auto_unbox = TRUE), width = 160),
    error = function(e) compact_text_preview(tool$args_summary %||% "", width = 160)
  )
  result_preview <- if (is.null(tool$raw_result)) {
    compact_text_preview(tool$result_summary %||% "", width = 160)
  } else {
    tryCatch(
      if (is.character(tool$raw_result)) {
        compact_text_preview(tool$raw_result, width = 160)
      } else {
        compact_text_preview(safe_to_json(tool$raw_result, auto_unbox = TRUE), width = 160)
      },
      error = function(e) compact_text_preview(tool$result_summary %||% "", width = 160)
    )
  }

  c(
    sprintf("Tool: %s", tool$name %||% "unknown"),
    sprintf("Status: %s", tool$status %||% "unknown"),
    sprintf("Elapsed: %s", elapsed),
    sprintf("Args summary: %s", tool$args_summary %||% "none"),
    sprintf("Result summary: %s", tool$result_summary %||% "none"),
    if (length(tool$messages %||% character(0))) paste0("Messages: ", paste(tool$messages, collapse = " | ")) else "Messages: none",
    if (length(tool$warnings %||% character(0))) paste0("Warnings: ", paste(tool$warnings, collapse = " | ")) else "Warnings: none",
    paste0("Args raw: ", args_preview),
    paste0("Result raw: ", result_preview)
  )
}

#' @keywords internal
build_console_inspector_lines <- function(turn, tool_index = NULL) {
  lines <- if (is.null(tool_index)) {
    format_console_turn_detail(turn)
  } else {
    format_console_tool_detail(turn, tool_index)
  }

  if (!length(lines)) {
    return(lines)
  }

  navigation <- if (is.null(tool_index)) {
    if (length(turn$tool_calls) > 0) {
      c("Navigation: /inspect next opens the first tool", "Navigation: /inspect tool <index> opens a specific tool")
    } else {
      "Navigation: no tool entries are available for this turn"
    }
  } else {
    c("Navigation: /inspect prev | /inspect next", "Navigation: /inspect turn returns to the turn summary")
  }

  c(lines, navigation)
}

#' @keywords internal
render_console_turn_inspector <- function(state, turn = console_app_get_last_turn(state), tool_index = NULL) {
  if (is.null(turn)) {
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_alert_info("No turns are available to inspect yet.")
    } else {
      cat("No turns are available to inspect yet.\n", sep = "")
    }
    return(invisible(character(0)))
  }

  lines <- build_console_inspector_lines(turn, tool_index = tool_index)

  if (!length(lines)) {
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_alert_warning("Requested inspection target is not available.")
    } else {
      cat("Requested inspection target is not available.\n", sep = "")
    }
    return(invisible(character(0)))
  }

  if (!requireNamespace("cli", quietly = TRUE)) {
    cat(paste0(lines, "\n"), sep = "")
    return(invisible(lines))
  }

  cli::cli_h2(if (is.null(tool_index)) "Turn Inspector" else sprintf("Tool Inspector #%d", tool_index))
  cli::cli_ul(lines)
  invisible(lines)
}

#' @keywords internal
console_app_open_turn_overlay <- function(state, turn = console_app_get_last_turn(state), tool_index = NULL) {
  if (is.null(turn)) {
    return(NULL)
  }

  lines <- build_console_inspector_lines(turn, tool_index = tool_index)
  if (!length(lines)) {
    return(NULL)
  }

  console_app_close_overlay_by_type(state, "inspector")
  console_app_open_overlay(
    state = state,
    type = "inspector",
    title = if (is.null(tool_index)) "Inspector Overlay" else sprintf("Inspector Overlay: Tool %d", tool_index),
    lines = lines,
    payload = list(turn_id = turn$turn_id %||% NULL, tool_index = tool_index)
  )
}

#' @keywords internal
console_app_refresh_inspector_overlay <- function(state) {
  overlay <- console_app_get_active_overlay(state)
  if (is.null(overlay) || !identical(overlay$type, "inspector")) {
    return(NULL)
  }

  turn <- console_app_get_turn_by_id(state, overlay$payload$turn_id %||% NULL)
  if (is.null(turn)) {
    return(NULL)
  }

  tool_index <- overlay$payload$tool_index %||% NULL
  overlay$lines <- build_console_inspector_lines(turn, tool_index = tool_index)
  overlay$title <- if (is.null(tool_index)) "Inspector Overlay" else sprintf("Inspector Overlay: Tool %d", tool_index)
  state$overlay_stack[[length(state$overlay_stack)]] <- overlay
  overlay
}

#' @keywords internal
console_app_navigate_inspector <- function(state, direction = c("next", "prev")) {
  direction <- match.arg(direction)
  overlay <- console_app_get_active_overlay(state)
  if (is.null(overlay) || !identical(overlay$type, "inspector")) {
    return(NULL)
  }

  turn <- console_app_get_turn_by_id(state, overlay$payload$turn_id %||% NULL)
  if (is.null(turn) || length(turn$tool_calls) == 0) {
    return(NULL)
  }

  step <- if (identical(direction, "next")) 1L else -1L
  current_index <- overlay$payload$tool_index %||% 0L
  target_index <- if (current_index <= 0L) {
    if (step > 0L) 1L else length(turn$tool_calls)
  } else {
    current_index + step
  }

  if (target_index < 1L || target_index > length(turn$tool_calls)) {
    return(NULL)
  }

  console_app_open_turn_overlay(state, turn = turn, tool_index = target_index)
}
