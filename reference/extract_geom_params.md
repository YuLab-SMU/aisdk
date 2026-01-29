# Extract Geom Parameters from ggproto Object

Dynamically extracts parameter information from a ggplot2 geom. This
handles the "scattered definitions" problem by reading from source.

## Usage

``` r
extract_geom_params(geom_name)
```

## Arguments

- geom_name:

  Name of the geom (e.g., "point", "line").

## Value

List with default_aes, required_aes, optional_aes, extra_params.
