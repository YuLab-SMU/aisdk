# Post to API with Retry

Makes a POST request to an API endpoint with automatic retry on failure.
Implements exponential backoff and respects `retry-after` headers.

## Usage

``` r
post_to_api(
  url,
  headers,
  body,
  max_retries = 2,
  initial_delay_ms = 2000,
  backoff_factor = 2
)
```

## Arguments

- url:

  The API endpoint URL.

- headers:

  A named list of HTTP headers.

- body:

  The request body (will be converted to JSON).

- max_retries:

  Maximum number of retries (default: 2).

- initial_delay_ms:

  Initial delay in milliseconds (default: 2000).

- backoff_factor:

  Multiplier for delay on each retry (default: 2).

## Value

The parsed JSON response.
