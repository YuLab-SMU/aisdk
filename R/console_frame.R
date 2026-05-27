#' @title Console Frame Helpers
#' @description
#' Internal helpers for building and rendering a structured console frame from
#' `ConsoleAppState`. This is the first step toward region ownership and later
#' diff rendering, while still using an append-only renderer today.
#' @name console_frame
#' @keywords internal
NULL

#' @keywords internal
build_console_frame <- function(state,
                                turn = console_app_get_current_turn(state),
                                overlay = console_app_get_active_overlay(state)) {
  status_lines <- c(
    build_console_status_lines(state),
    strrep("\u2500", max(40L, min(getOption("width", 80), 120L)))
  )

  timeline_lines <- if (identical(state$view_mode, "inspect")) {
    format_console_tool_timeline(turn)
  } else {
    character(0)
  }

  overlay_lines <- build_console_overlay_box(state, overlay)

  list(
    status = list(type = "status", tone = "muted", lines = status_lines),
    transcript = list(type = "transcript", lines = character(0)),
    timeline = list(type = "timeline", tone = "subtle", lines = timeline_lines),
    overlay = list(type = "overlay", tone = "primary", lines = overlay_lines),
    meta = list(
      view_mode = state$view_mode,
      phase = state$phase,
      focus_target = state$focus_target,
      has_overlay = length(overlay_lines) > 0
    )
  )
}

#' @keywords internal
console_frame_section_changed <- function(previous_frame, frame, name, force = FALSE) {
  if (isTRUE(force) || is.null(previous_frame)) {
    return(TRUE)
  }

  !identical(previous_frame[[name]]$lines %||% character(0), frame[[name]]$lines %||% character(0))
}

#' @keywords internal
render_console_frame <- function(frame,
                                 state = NULL,
                                 sections = c("status", "timeline", "overlay"),
                                 force = FALSE,
                                 capabilities = NULL) {
  has_cli <- requireNamespace("cli", quietly = TRUE)
  previous_frame <- if (!is.null(state)) state$last_rendered_frame %||% NULL else NULL

  render_section <- function(lines, color_fn = identity) {
    if (!length(lines)) {
      return(invisible(NULL))
    }

    for (line in lines) {
      rendered <- if (has_cli) color_fn(line) else line
      cat(rendered, "\n", sep = "")
    }
    invisible(NULL)
  }

  should_render_section <- function(name) {
    if (!name %in% sections) {
      return(FALSE)
    }
    console_frame_section_changed(previous_frame, frame, name, force = force)
  }

  if (should_render_section("status")) {
    render_section(frame$status$lines %||% character(0), cli::col_grey)
  }

  if (should_render_section("timeline") && length(frame$timeline$lines %||% character(0))) {
    header <- if (has_cli) cli::col_grey("timeline") else "timeline"
    cat(header, "\n", sep = "")
    prefix <- if (has_cli) "  \u00b7 " else "  - "
    for (line in frame$timeline$lines) {
      rendered <- if (has_cli) cli::col_grey(paste0(prefix, line)) else paste0(prefix, line)
      cat(rendered, "\n", sep = "")
    }
  }

  if (should_render_section("overlay") && length(frame$overlay$lines %||% character(0))) {
    cat("\n")
    render_section(frame$overlay$lines, if (has_cli) cli::col_yellow else identity)
  }

  if (!is.null(state)) {
    state$last_rendered_frame <- frame
  }

  invisible(frame)
}
