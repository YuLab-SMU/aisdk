# Migrate Legacy Code

Provides migration guidance for legacy code patterns.

## Usage

``` r
migrate_pattern(pattern)
```

## Arguments

- pattern:

  The legacy pattern to migrate from.

## Value

A list with old_pattern, new_pattern, and example.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get migration guidance for ChatSession
guidance <- migrate_pattern("ChatSession")
cat(guidance$example)
} # }
```
