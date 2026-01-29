# Stream from API

Makes a streaming POST request and processes Server-Sent Events (SSE)
using httr2. Implements robust error recovery for malformed SSE data.

## Usage

``` r
stream_from_api(url, headers, body, callback)
```

## Arguments

- url:

  The API endpoint URL.

- headers:

  A named list of HTTP headers.

- body:

  The request body (will be converted to JSON).

- callback:

  A function called for each parsed SSE data chunk.
