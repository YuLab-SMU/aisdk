# MCP Client

Connect to and communicate with an MCP server process.

## Details

Manages connection to an external MCP server via stdio.

## Public fields

- `process`:

  The processx process object

- `server_info`:

  Information about the connected server

- `capabilities`:

  Server capabilities

## Methods

### Public methods

- [`McpClient$new()`](#method-McpClient-new)

- [`McpClient$list_tools()`](#method-McpClient-list_tools)

- [`McpClient$call_tool()`](#method-McpClient-call_tool)

- [`McpClient$list_resources()`](#method-McpClient-list_resources)

- [`McpClient$read_resource()`](#method-McpClient-read_resource)

- [`McpClient$is_alive()`](#method-McpClient-is_alive)

- [`McpClient$close()`](#method-McpClient-close)

- [`McpClient$as_sdk_tools()`](#method-McpClient-as_sdk_tools)

- [`McpClient$clone()`](#method-McpClient-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MCP Client

#### Usage

    McpClient$new(command, args = character(), env = NULL)

#### Arguments

- `command`:

  The command to run (e.g., "npx", "python")

- `args`:

  Command arguments (e.g., c("-y",
  "@modelcontextprotocol/server-github"))

- `env`:

  Environment variables as a named character vector

#### Returns

A new McpClient object

------------------------------------------------------------------------

### Method `list_tools()`

List available tools from the MCP server

#### Usage

    McpClient$list_tools()

#### Returns

A list of tool definitions

------------------------------------------------------------------------

### Method `call_tool()`

Call a tool on the MCP server

#### Usage

    McpClient$call_tool(name, arguments = list())

#### Arguments

- `name`:

  The tool name

- `arguments`:

  Tool arguments as a named list

#### Returns

The tool result

------------------------------------------------------------------------

### Method `list_resources()`

List available resources from the MCP server

#### Usage

    McpClient$list_resources()

#### Returns

A list of resource definitions

------------------------------------------------------------------------

### Method `read_resource()`

Read a resource from the MCP server

#### Usage

    McpClient$read_resource(uri)

#### Arguments

- `uri`:

  The resource URI

#### Returns

The resource contents

------------------------------------------------------------------------

### Method `is_alive()`

Check if the MCP server process is alive

#### Usage

    McpClient$is_alive()

#### Returns

TRUE if alive, FALSE otherwise

------------------------------------------------------------------------

### Method [`close()`](https://rdrr.io/r/base/connections.html)

Close the MCP client connection

#### Usage

    McpClient$close()

------------------------------------------------------------------------

### Method `as_sdk_tools()`

Convert MCP tools to SDK Tool objects

#### Usage

    McpClient$as_sdk_tools()

#### Returns

A list of Tool objects

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    McpClient$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
