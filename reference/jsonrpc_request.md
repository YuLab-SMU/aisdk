# Create a JSON-RPC 2.0 request object

Create a JSON-RPC 2.0 request object

## Usage

``` r
jsonrpc_request(method, params = NULL, id = NULL)
```

## Arguments

- method:

  The method name

- params:

  The parameters (list or NULL)

- id:

  The request ID (integer or string)

## Value

A list representing the JSON-RPC request
