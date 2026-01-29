# Create a JSON-RPC 2.0 error response object

Create a JSON-RPC 2.0 error response object

## Usage

``` r
jsonrpc_error(code, message, id = NULL, data = NULL)
```

## Arguments

- code:

  The error code (integer)

- message:

  The error message

- id:

  The request ID this is responding to (can be NULL)

- data:

  Optional additional error data

## Value

A list representing the JSON-RPC error response
