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
#' \dontrun{
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
reactive_tool <- function(name, description, parameters, execute) {
  # Store the execute function with metadata
  structure(
    tool(
      name = name,
      description = description,
      parameters = parameters,
      execute = execute  # Will be wrapped later
    ),
    class = c("ReactiveTool", "Tool"),
    reactive = TRUE
  )
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
      # Create new tool with wrapped execute
      original_execute <- t$execute
      tool(
        name = t$name,
        description = t$description,
        parameters = t$parameters,
        execute = function(...) {
          original_execute(rv, session, ...)
        }
      )
    } else {
      t
    }
  })
}
