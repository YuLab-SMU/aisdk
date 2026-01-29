# Connectivity (MCP)

The [Model Context Protocol
(MCP)](https://modelcontextprotocol.io/introduction) is a standard for
connecting AI systems to data sources. **aisdk** includes a built-in MCP
Client and Server.

## Using the MCP Client

You can connect to any MCP-compliant server (Node.js, Python, or R) and
use its tools.

### Example: GitHub MCP Server

This example connects to the official GitHub MCP server to let the AI
interact with your repositories.

1.  **Prerequisites**:
    - Node.js installed (to run the server).
    - `GITHUB_TOKEN` environment variable set.
2.  **Connect Client**:

``` r
library(aisdk)

# Start the Node.js GitHub server
github_client <- create_mcp_client(
  command = "npx",
  args = c("-y", "@modelcontextprotocol/server-github"),
  env = c(GITHUB_PERSONAL_ACCESS_TOKEN = Sys.getenv("GITHUB_TOKEN"))
)

# List available tools
tools <- github_client$list_tools()
print(tools[[1]]$name) 
# e.g., "create_issue", "list_pull_requests"...
```

3.  **Use with AI**:

Convert the MCP tools to aisdk Tools and pass them to the model.

``` r
sdk_tools <- github_client$as_sdk_tools()

# Create a session (manages history and tool loops)
# max_steps=10 enables automatic ReAct loop for tool calling
chat <- create_chat_session(
  model = "openai:gpt-4o",
  tools = sdk_tools,
  max_steps = 10
)

# Option 1: Run programmatically
result <- chat$send("List my recent pull requests")
print(result$text)

# Option 2: Run interactively with streaming
# console_chat(chat)
```

4.  **Cleanup**:

``` r
github_client$close()
```
