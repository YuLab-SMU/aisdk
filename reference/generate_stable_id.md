# Generate Stable ID

Generates a stable unique identifier for a plot element.

## Usage

``` r
generate_stable_id(type, ..., prefix = NULL)
```

## Arguments

- type:

  Type of element (e.g., "layer", "guide").

- ...:

  Components to include in the ID hash.

- prefix:

  Optional prefix for the ID.

## Value

A stable ID string.
