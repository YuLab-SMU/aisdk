# Safe Serialization to JSON

Standardized internal helper for JSON serialization with common
defaults.

## Usage

``` r
safe_to_json(x, auto_unbox = TRUE, ...)
```

## Arguments

- x:

  Object to serialize.

- auto_unbox:

  Whether to automatically unbox single-element vectors. Default TRUE.

- ...:

  Additional arguments to jsonlite::toJSON.

## Value

A JSON string.
