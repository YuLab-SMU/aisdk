# JSON Utilities

Provides robust utilities for parsing potentially truncated or malformed
JSON strings, commonly encountered in streaming LLM outputs.

A robust utility that uses a finite state machine to close open
brackets, braces, and quotes to make a truncated JSON string valid for
parsing.

## Usage

``` r
fix_json(json_str)
```

## Arguments

- json_str:

  A potentially truncated JSON string.

## Value

A repaired JSON string.

## Examples

``` r
fix_json('{"name": "Gene...')
#> [1] "{\"name\": \"Gene...\"}"
fix_json('[1, 2, {"a":')
#> [1] "[1, 2, {\"a\":}]"
```
