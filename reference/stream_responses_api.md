# Stream from Responses API

Makes a streaming POST request to OpenAI Responses API and processes SSE
events. The Responses API uses different event types than Chat
Completions.

## Usage

``` r
stream_responses_api(url, headers, body, callback)
```

## Arguments

- url:

  The API endpoint URL.

- headers:

  A named list of HTTP headers.

- body:

  The request body (will be converted to JSON).

- callback:

  A function called for each event: callback(event_type, data, done).
