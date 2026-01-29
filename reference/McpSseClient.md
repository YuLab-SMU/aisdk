# MCP SSE Client

Connect to an MCP server via Server-Sent Events (SSE).

## Details

Manages connection to a remote MCP server via SSE transport.

## Super class

[`aisdk::McpClient`](https://YuLab-SMU.github.io/aisdk/reference/McpClient.md)
-\> `McpSseClient`

## Public fields

- `endpoint`:

  The POST endpoint for sending messages (received from SSE init)

- `auth_headers`:

  Authentication headers

## Methods

### Public methods

- [`McpSseClient$new()`](#method-McpSseClient-new)

- [`McpSseClient$clone()`](#method-McpSseClient-clone)

Inherited methods

- [`aisdk::McpClient$as_sdk_tools()`](https://YuLab-SMU.github.io/aisdk/reference/McpClient.html#method-as_sdk_tools)
- [`aisdk::McpClient$call_tool()`](https://YuLab-SMU.github.io/aisdk/reference/McpClient.html#method-call_tool)
- [`aisdk::McpClient$close()`](https://YuLab-SMU.github.io/aisdk/reference/McpClient.html#method-close)
- [`aisdk::McpClient$is_alive()`](https://YuLab-SMU.github.io/aisdk/reference/McpClient.html#method-is_alive)
- [`aisdk::McpClient$list_resources()`](https://YuLab-SMU.github.io/aisdk/reference/McpClient.html#method-list_resources)
- [`aisdk::McpClient$list_tools()`](https://YuLab-SMU.github.io/aisdk/reference/McpClient.html#method-list_tools)
- [`aisdk::McpClient$read_resource()`](https://YuLab-SMU.github.io/aisdk/reference/McpClient.html#method-read_resource)

------------------------------------------------------------------------

### Method `new()`

Create a new MCP SSE Client

#### Usage

    McpSseClient$new(url, headers = list())

#### Arguments

- `url`:

  The SSE endpoint URL

- `headers`:

  named list of headers (e.g. for auth)

#### Returns

A new McpSseClient object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    McpSseClient$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
