# MCP Server

Expose R functions as MCP tools to external clients.

## Details

Serves R tools and resources via MCP protocol over stdio.

## Public fields

- `name`:

  Server name

- `version`:

  Server version

- `tools`:

  Registered tools

- `resources`:

  Registered resources

## Methods

### Public methods

- [`McpServer$new()`](#method-McpServer-new)

- [`McpServer$add_tool()`](#method-McpServer-add_tool)

- [`McpServer$add_resource()`](#method-McpServer-add_resource)

- [`McpServer$listen()`](#method-McpServer-listen)

- [`McpServer$process_message()`](#method-McpServer-process_message)

- [`McpServer$clone()`](#method-McpServer-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new MCP Server

#### Usage

    McpServer$new(name = "r-mcp-server", version = "0.1.0")

#### Arguments

- `name`:

  Server name

- `version`:

  Server version

#### Returns

A new McpServer object

------------------------------------------------------------------------

### Method `add_tool()`

Add a tool to the server

#### Usage

    McpServer$add_tool(tool)

#### Arguments

- `tool`:

  A Tool object from the SDK

#### Returns

self (for chaining)

------------------------------------------------------------------------

### Method `add_resource()`

Add a resource to the server

#### Usage

    McpServer$add_resource(
      uri,
      name,
      description = "",
      mime_type = "text/plain",
      read_fn
    )

#### Arguments

- `uri`:

  Resource URI

- `name`:

  Resource name

- `description`:

  Resource description

- `mime_type`:

  MIME type

- `read_fn`:

  Function that returns the resource content

#### Returns

self (for chaining)

------------------------------------------------------------------------

### Method `listen()`

Start listening for MCP requests on stdin/stdout This is a blocking
call.

#### Usage

    McpServer$listen()

------------------------------------------------------------------------

### Method `process_message()`

Process a single MCP message (for testing)

#### Usage

    McpServer$process_message(json_str)

#### Arguments

- `json_str`:

  The JSON-RPC message

#### Returns

The response, or NULL for notifications

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    McpServer$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
