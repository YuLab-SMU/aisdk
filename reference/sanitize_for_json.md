# Sanitize Object for JSON Serialization

Standardizes R objects for consistent JSON serialization, especially for
ggplot2 elements like units and margins.

## Usage

``` r
sanitize_for_json(x, plot_dims = list(width = 8, height = 6))
```

## Arguments

- x:

  Object to sanitize.

- plot_dims:

  Optional list with width and height in inches.

## Value

A sanitized list or vector.
