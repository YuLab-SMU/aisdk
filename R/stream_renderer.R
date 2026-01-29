#' @title Stream Renderer: Enhanced CLI output
#' @description
#' Internal utilities for rendering streaming output and tool execution
#' status in the R console using the cli package.
#' @name stream_renderer
#' @keywords internal
NULL

#' @title Create a Stream Renderer
#' @description
#' Creates an environment to manage the state of a streaming response,
#' including thinking indicators and tool execution status.
#' @return A list of functions for rendering.
#' @keywords internal
create_stream_renderer <- function() {

  state <- new.env(parent = emptyenv())
  state$first_chunk <- TRUE
  state$is_thinking <- FALSE
  state$current_tool <- NULL

  has_cli <- requireNamespace("cli", quietly = TRUE)

  # Create the markdown rendering stream processor
  mk_renderer <- create_markdown_stream_renderer()

  # Helper to clear a line (simple version)

  clear_line <- function() {
    cat("\r", paste(rep(" ", getOption("width", 80)), collapse = ""), "\r", sep = "")
  }

  # Reset state for new generation step (used in multi-step agent loops)
  reset_for_new_step <- function() {
    state$first_chunk <- TRUE
    state$is_thinking <- FALSE
    state$current_tool <- NULL
    # Reset the markdown renderer state
    mk_renderer$reset()
  }

  # Check if thinking indicator should be shown
  show_thinking <- should_show_thinking()

  # 1. Thinking Indicator
  start_thinking <- function() {
    if (!interactive() || state$is_thinking || !show_thinking) return()
    state$is_thinking <- TRUE
    
    if (has_cli) {
      cat(cli::col_grey(cli::symbol$ellipsis, " Thinking..."))
    } else {
      cat("... Thinking")
    }
    utils::flush.console()
  }

  stop_thinking <- function() {
    if (!state$is_thinking || !show_thinking) return()
    # Clear the thinking line
    cat("\r", paste(rep(" ", 20), collapse = ""), "\r", sep = "")
    state$is_thinking <- FALSE
    utils::flush.console()
  }

  # 2. Text Streaming
  process_chunk <- function(text, done) {
    if (state$is_thinking) {
      stop_thinking()
    }
    
    if (done) {
      mk_renderer$process_chunk(NULL, done = TRUE)
      return()
    }
    
    if (is.null(text) || !nzchar(text)) return()

    if (state$first_chunk) {
      state$first_chunk <- FALSE
    }
    
    # Delegate to markdown renderer
    mk_renderer$process_chunk(text, done = FALSE)
    utils::flush.console()
  }

  # 3. Tool Execution
  render_tool_start <- function(name, arguments) {
    stop_thinking()
    state$current_tool <- name
    cli_tool_start(name, arguments)
    utils::flush.console()
  }

  render_tool_result <- function(name, result, success = TRUE) {
    state$current_tool <- NULL
    cli_tool_result(name, result, success)
    utils::flush.console()
  }

  list(
    start_thinking = start_thinking,
    stop_thinking = stop_thinking,
    process_chunk = process_chunk,
    render_tool_start = render_tool_start,
    render_tool_result = render_tool_result,
    reset_for_new_step = reset_for_new_step
  )
}
