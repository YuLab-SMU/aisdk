# Create an MCP Server

Convenience function to create an MCP server.

## Usage

``` r
create_mcp_server(name = "r-mcp-server", version = "0.1.0")
```

## Arguments

- name:

  Server name

- version:

  Server version

## Value

An McpServer object

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a server with a custom tool
server <- create_mcp_server("my-r-server")

# Add a tool
server$add_tool(tool(
  name = "calculate",
  description = "Perform a calculation",
  parameters = z_object(
    expression = z_string(description = "R expression to evaluate")
  ),
  execute = function(args) {
    eval(parse(text = args$expression))
  }
))

# Start listening (blocking)
server$listen()
} # }
```
