# Safe JSON Parser

Parses a JSON string, attempting to repair it using `fix_json` if the
initial parse fails.

## Usage

``` r
safe_parse_json(text)
```

## Arguments

- text:

  A JSON string.

## Value

A parsed R object (list, vector, etc.) or NULL if parsing fails even
after repair.

## Examples

``` r
safe_parse_json('{"a": 1}')
#> $a
#> [1] 1
#> 
safe_parse_json('{"a": 1,')
#> NULL
```
