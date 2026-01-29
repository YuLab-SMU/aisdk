# Create MCP initialize request

Create MCP initialize request

## Usage

``` r
mcp_initialize_request(
  client_info,
  capabilities = structure(list(), names = character(0)),
  id = 1L
)
```

## Arguments

- client_info:

  List with name and version

- capabilities:

  Client capabilities

- id:

  Request ID

## Value

A JSON-RPC request for initialize
