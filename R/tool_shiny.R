#' @title Reactive Tool
#' @description
#' Create a tool that can modify Shiny reactive values.
#' This is a wrapper around the standard `tool()` function that provides
#' additional documentation and conventions for Shiny integration.
#'
#' The execute function receives `rv` (reactiveValues) and `session` as
#' the first two arguments, followed by any tool-specific parameters.
#'
#' @param name The name of the tool.
#' @param description A description of what the tool does.
#' @param parameters A schema object defining the tool's parameters.
#' @param execute A function to execute. First two args are `rv` and `session`.
#' @return A Tool object ready for use with aiChatServer.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#' # Create a tool that modifies a reactive value
#' update_resolution_tool <- reactive_tool(
#'   name = "update_resolution",
#'   description = "Update the plot resolution",
#'   parameters = z_object(
#'     resolution = z_number() |> z_describe("New resolution value (50-500)")
#'   ),
#'   execute = function(rv, session, resolution) {
#'     rv$resolution <- resolution
#'     paste0("Resolution updated to ", resolution)
#'   }
#' )
#'
#' # Use with aiChatServer by wrapping the execute function
#' server <- function(input, output, session) {
#'   rv <- reactiveValues(resolution = 100)
#'
#'   # Wrap the tool to inject rv and session
#'   wrapped_tools <- wrap_reactive_tools(
#'     list(update_resolution_tool),
#'     rv = rv,
#'     session = session
#'   )
#'
#'   aiChatServer("chat", model = "openai:gpt-4o", tools = wrapped_tools)
#' }
#' }
#' }
reactive_tool <- function(name, description, parameters, execute) {
  if (!is.function(execute)) {
    rlang::abort("`execute` must be a function.")
  }

  fml_names <- names(formals(execute)) %||% character(0)
  if (length(fml_names) < 2 || !identical(fml_names[1:2], c("rv", "session"))) {
    rlang::abort("`execute` for reactive_tool() must start with arguments `rv` and `session`.")
  }

  tool_obj <- Tool$new(
    name = name,
    description = description,
    parameters = parameters,
    execute = function(args) {
      rlang::abort("Reactive tools must be wrapped with wrap_reactive_tools() before use.")
    }
  )
  class(tool_obj) <- unique(c("ReactiveTool", class(tool_obj)))
  attr(tool_obj, "reactive") <- TRUE
  attr(tool_obj, "reactive_execute") <- execute
  tool_obj
}

#' @title Wrap Reactive Tools
#' @description
#' Wraps reactive tools to inject reactiveValues and session into their
#' execute functions. Call this in your Shiny server before passing tools
#' to aiChatServer.
#'
#' @param tools List of Tool objects, possibly including ReactiveTool objects.
#' @param rv The reactiveValues object to inject.
#' @param session The Shiny session object to inject.
#' @return List of wrapped Tool objects ready for use.
#' @export
wrap_reactive_tools <- function(tools, rv, session) {
  lapply(tools, function(t) {
    if (inherits(t, "ReactiveTool")) {
      original_execute <- attr(t, "reactive_execute", exact = TRUE)
      if (!is.function(original_execute)) {
        rlang::abort("ReactiveTool is missing its original execute function.")
      }
      Tool$new(
        name = t$name,
        description = t$description,
        parameters = t$parameters,
        execute = function(args) {
          args <- args[setdiff(names(args), c(".envir", "rv", "session"))]
          fml_names <- names(formals(original_execute)) %||% character(0)
          if (!("..." %in% fml_names)) {
            allowed <- setdiff(fml_names, c("rv", "session"))
            args <- args[names(args) %in% allowed]
          }
          ordered <- c(list(rv = rv, session = session), args)
          do.call(original_execute, ordered)
        }
      )
    } else {
      t
    }
  })
}
