# Get or Create Session

Retrieves the current chat session from the cache, or creates a new one.
Sessions persist across chunks within a single knit process.

## Usage

``` r
get_or_create_session(options)
```

## Arguments

- options:

  Chunk options containing potential `model` specification.

## Value

A ChatSession object.
