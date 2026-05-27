#' @title Agent Output Renderer (UI-agnostic contract)
#' @description
#' `Renderer` is the canonical, UI-agnostic contract for presenting an agent's
#' streaming output. The agent runtime (driven by [stream_text()] /
#' `run_agent_runtime()`) emits agent-output events purely through these
#' semantic methods, so the *same* agent-output logic can be rendered in a
#' terminal (the built-in cli backend, [create_stream_renderer()]), a web app
#' (e.g. \pkg{aisdk.shiny}), a test/replay harness ([create_capture_renderer()]),
#' or any custom UI -- by implementing this interface.
#'
#' The agent-output event model captures what an agent *does* as it responds:
#' \itemize{
#'   \item \strong{text} -- streamed assistant text, delivered incrementally via
#'     `process_chunk(text, done)`; the final call usually has `text = NULL` and
#'     `done = TRUE`.
#'   \item \strong{thinking} -- a reasoning stream bracketed by `start_thinking()`
#'     and `stop_thinking()`.
#'   \item \strong{tools} -- tool execution: `render_tool_start(name, arguments)`
#'     followed by `render_tool_result(name, result, success, raw_result)`.
#'   \item \strong{steps} -- multi-step ReAct loop boundaries, `reset_for_new_step()`.
#' }
#'
#' Every method is a no-op in the base class, so the base class is itself a usable
#' null renderer and a subclass need only override the events it cares about.
#' @keywords internal
#' @export
Renderer <- R6::R6Class(
  "Renderer",
  lock_objects = FALSE,
  public = list(
    #' @description Render a chunk of streamed assistant text.
    #' @param text A character chunk, or `NULL` on the final (done) call.
    #' @param done `TRUE` when the current text segment is complete.
    #' @return Invisibly `NULL`.
    process_chunk = function(text, done = FALSE) invisible(NULL),
    #' @description Signal the start of a thinking/reasoning stream.
    #' @return Invisibly `NULL`.
    start_thinking = function() invisible(NULL),
    #' @description Signal the end of a thinking/reasoning stream.
    #' @return Invisibly `NULL`.
    stop_thinking = function() invisible(NULL),
    #' @description Render the start of a tool call.
    #' @param name Tool name.
    #' @param arguments Tool arguments (a list).
    #' @return Invisibly `NULL`.
    render_tool_start = function(name, arguments) invisible(NULL),
    #' @description Render a tool result.
    #' @param name Tool name.
    #' @param result The display result.
    #' @param success Whether the tool call succeeded.
    #' @param raw_result The raw (unformatted) result.
    #' @return Invisibly `NULL`.
    render_tool_result = function(name, result, success = TRUE, raw_result = NULL) invisible(NULL),
    #' @description Reset transient render state at a new ReAct step boundary.
    #' @return Invisibly `NULL`.
    reset_for_new_step = function() invisible(NULL)
  )
)

#' Create a null (no-op) agent-output renderer
#'
#' Returns a [Renderer] that discards every event. Use it for headless or
#' library contexts where agent output should not be displayed.
#' @return A [Renderer].
#' @export
create_null_renderer <- function() {
  Renderer$new()
}

#' @title Capturing Agent Output Renderer
#' @description A [Renderer] that records every agent-output event into an
#'   in-memory log instead of displaying it -- useful for testing, replay, and
#'   reusing agent-output logic in non-terminal UIs.
#' @keywords internal
#' @export
CaptureRenderer <- R6::R6Class(
  "CaptureRenderer",
  inherit = Renderer,
  lock_objects = FALSE,
  public = list(
    #' @field log A list of recorded events, each a list with a `type` field.
    log = NULL,
    #' @description Create a capturing renderer with an empty log.
    initialize = function() {
      self$log <- list()
    },
    #' @description Record a streamed text chunk.
    #' @param text A character chunk, or `NULL`.
    #' @param done `TRUE` when the text segment is complete.
    process_chunk = function(text, done = FALSE) {
      self$log[[length(self$log) + 1L]] <- list(type = "text", text = text, done = done)
      invisible(NULL)
    },
    #' @description Record the start of a thinking stream.
    start_thinking = function() {
      self$log[[length(self$log) + 1L]] <- list(type = "thinking_start")
      invisible(NULL)
    },
    #' @description Record the end of a thinking stream.
    stop_thinking = function() {
      self$log[[length(self$log) + 1L]] <- list(type = "thinking_stop")
      invisible(NULL)
    },
    #' @description Record the start of a tool call.
    #' @param name Tool name.
    #' @param arguments Tool arguments.
    render_tool_start = function(name, arguments) {
      self$log[[length(self$log) + 1L]] <- list(type = "tool_start", name = name, arguments = arguments)
      invisible(NULL)
    },
    #' @description Record a tool result.
    #' @param name Tool name.
    #' @param result The display result.
    #' @param success Whether the tool call succeeded.
    #' @param raw_result The raw result.
    render_tool_result = function(name, result, success = TRUE, raw_result = NULL) {
      self$log[[length(self$log) + 1L]] <- list(type = "tool_result", name = name, result = result, success = success)
      invisible(NULL)
    },
    #' @description Record a ReAct step boundary.
    reset_for_new_step = function() {
      self$log[[length(self$log) + 1L]] <- list(type = "step_reset")
      invisible(NULL)
    },
    #' @description Return the recorded events.
    #' @return A list of recorded events.
    events = function() self$log
  )
)

#' Create a capturing agent-output renderer
#'
#' Returns a [CaptureRenderer] that records agent-output events instead of
#' displaying them. Call its `events()` method to retrieve the recorded events.
#' @return A [CaptureRenderer].
#' @export
create_capture_renderer <- function() {
  CaptureRenderer$new()
}
