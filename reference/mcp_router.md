# Create MCP Router

Factory function to create an MCP router for aggregating multiple
servers.

## Usage

``` r
mcp_router()
```

## Value

An McpRouter object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create router
router <- mcp_router()

# Connect to multiple MCP servers
router$connect("github", "npx", c("-y", "@modelcontextprotocol/server-github"))
router$connect("filesystem", "npx", c("-y", "@modelcontextprotocol/server-filesystem"))

# Use aggregated tools with generate_text
result <- generate_text(
  model = "openai:gpt-4o",
  prompt = "List my GitHub repos and save to a file",
  tools = router$as_sdk_tools()
)

# Hot-swap: remove a server
router$remove_client("github")

# Cleanup
router$close()
} # }
```
