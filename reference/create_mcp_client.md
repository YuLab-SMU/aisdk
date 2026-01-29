# Create an MCP Client

Convenience function to create and connect to an MCP server.

## Usage

``` r
create_mcp_client(command, args = character(), env = NULL)
```

## Arguments

- command:

  The command to run the MCP server

- args:

  Command arguments

- env:

  Environment variables

## Value

An McpClient object

## Examples

``` r
if (FALSE) { # \dontrun{
# Connect to GitHub MCP server
client <- create_mcp_client(
  "npx",
  c("-y", "@modelcontextprotocol/server-github"),
  env = c(GITHUB_PERSONAL_ACCESS_TOKEN = Sys.getenv("GITHUB_TOKEN"))
)

# List available tools
tools <- client$list_tools()

# Use tools with generate_text
result <- generate_text(
  model = "openai:gpt-4o",
  prompt = "List my GitHub repos",
  tools = client$as_sdk_tools()
)

client$close()
} # }
```
