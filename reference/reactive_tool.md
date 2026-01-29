# Reactive Tool

Create a tool that can modify Shiny reactive values. This is a wrapper
around the standard
[`tool()`](https://YuLab-SMU.github.io/aisdk/reference/tool_factory.md)
function that provides additional documentation and conventions for
Shiny integration.

The execute function receives `rv` (reactiveValues) and `session` as the
first two arguments, followed by any tool-specific parameters.

## Usage

``` r
reactive_tool(name, description, parameters, execute)
```

## Arguments

- name:

  The name of the tool.

- description:

  A description of what the tool does.

- parameters:

  A schema object defining the tool's parameters.

- execute:

  A function to execute. First two args are `rv` and `session`.

## Value

A Tool object ready for use with aiChatServer.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a tool that modifies a reactive value
update_resolution_tool <- reactive_tool(
  name = "update_resolution",
  description = "Update the plot resolution",
  parameters = z_object(
    resolution = z_number() |> z_describe("New resolution value (50-500)")
  ),
  execute = function(rv, session, resolution) {
    rv$resolution <- resolution
    paste0("Resolution updated to ", resolution)
  }
)

# Use with aiChatServer by wrapping the execute function
server <- function(input, output, session) {
  rv <- reactiveValues(resolution = 100)

  # Wrap the tool to inject rv and session
  wrapped_tools <- wrap_reactive_tools(
    list(update_resolution_tool),
    rv = rv,
    session = session
  )

  aiChatServer("chat", model = "openai:gpt-4o", tools = wrapped_tools)
}
} # }
```
