# Stream from Anthropic API

Makes a streaming POST request to Anthropic and processes their SSE
format. Anthropic uses event types like `content_block_delta` instead of
OpenAI's format. Also handles OpenAI-compatible format for proxy
servers.

## Usage

``` r
stream_anthropic(url, headers, body, callback)
```

## Arguments

- url:

  The API endpoint URL.

- headers:

  A named list of HTTP headers.

- body:

  The request body (will be converted to JSON).

- callback:

  A function called for each text delta.

## Value

A GenerateResult object.
