# Create Empty-Aware Schema Wrapper

Wraps a schema to explicitly handle empty values. Adds `_empty` metadata
for frontend rendering decisions.

## Usage

``` r
z_empty_aware(schema, empty_behavior = "skip")
```

## Arguments

- schema:

  Base z_schema.

- empty_behavior:

  How frontend should handle empty: "skip", "placeholder", "inherit".

## Value

Enhanced z_schema.
