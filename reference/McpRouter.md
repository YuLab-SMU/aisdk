# MCP Router Class

A virtual MCP server that aggregates tools from multiple downstream MCP
servers into a unified interface. Supports hot-swapping and skill
negotiation.

## Public fields

- `clients`:

  List of connected MCP clients.

- `tool_map`:

  Mapping of tool names to their source clients.

- `capabilities`:

  Aggregated capabilities from all clients.

## Methods

### Public methods

- [`McpRouter$new()`](#method-McpRouter-new)

- [`McpRouter$add_client()`](#method-McpRouter-add_client)

- [`McpRouter$connect()`](#method-McpRouter-connect)

- [`McpRouter$remove_client()`](#method-McpRouter-remove_client)

- [`McpRouter$list_tools()`](#method-McpRouter-list_tools)

- [`McpRouter$call_tool()`](#method-McpRouter-call_tool)

- [`McpRouter$as_sdk_tools()`](#method-McpRouter-as_sdk_tools)

- [`McpRouter$negotiate()`](#method-McpRouter-negotiate)

- [`McpRouter$status()`](#method-McpRouter-status)

- [`McpRouter$close()`](#method-McpRouter-close)

- [`McpRouter$print()`](#method-McpRouter-print)

- [`McpRouter$clone()`](#method-McpRouter-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MCP Router.

#### Usage

    McpRouter$new()

#### Returns

A new McpRouter object.

------------------------------------------------------------------------

### Method `add_client()`

Add an MCP client to the router.

#### Usage

    McpRouter$add_client(name, client)

#### Arguments

- `name`:

  Unique name for this client.

- `client`:

  An McpClient object.

#### Returns

Self (invisibly).

------------------------------------------------------------------------

### Method `connect()`

Connect to an MCP server and add it to the router.

#### Usage

    McpRouter$connect(name, command, args = character(), env = NULL)

#### Arguments

- `name`:

  Unique name for this connection.

- `command`:

  Command to run the MCP server.

- `args`:

  Command arguments.

- `env`:

  Environment variables.

#### Returns

Self (invisibly).

------------------------------------------------------------------------

### Method `remove_client()`

Remove an MCP client from the router (hot-swap out).

#### Usage

    McpRouter$remove_client(name)

#### Arguments

- `name`:

  Name of the client to remove.

#### Returns

Self (invisibly).

------------------------------------------------------------------------

### Method `list_tools()`

List all available tools across all connected clients.

#### Usage

    McpRouter$list_tools()

#### Returns

A list of tool definitions.

------------------------------------------------------------------------

### Method `call_tool()`

Call a tool, routing to the appropriate client.

#### Usage

    McpRouter$call_tool(name, arguments = list())

#### Arguments

- `name`:

  Tool name.

- `arguments`:

  Tool arguments.

#### Returns

The tool result.

------------------------------------------------------------------------

### Method `as_sdk_tools()`

Get all tools as SDK Tool objects for use with generate_text.

#### Usage

    McpRouter$as_sdk_tools()

#### Returns

A list of Tool objects.

------------------------------------------------------------------------

### Method `negotiate()`

Negotiate capabilities with a specific client.

#### Usage

    McpRouter$negotiate(client_name)

#### Arguments

- `client_name`:

  Name of the client.

#### Returns

A list of negotiated capabilities.

------------------------------------------------------------------------

### Method `status()`

Get router status.

#### Usage

    McpRouter$status()

#### Returns

A list with status information.

------------------------------------------------------------------------

### Method [`close()`](https://rdrr.io/r/base/connections.html)

Close all client connections.

#### Usage

    McpRouter$close()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for McpRouter.

#### Usage

    McpRouter$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    McpRouter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
