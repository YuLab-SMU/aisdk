# Create Schema for ggtree Function

Specialized wrapper around create_schema_from_func for ggtree/ggplot2
functions. Handles common mapping and data arguments specifically.

## Usage

``` r
create_z_ggtree(func, layer = NULL)
```

## Arguments

- func:

  The R function (e.g., geom_tiplab).

- layer:

  Optional ggplot2 Layer object. If provided, its parameters (aes_params
  and geom_params) will be used to override the schema defaults. This is
  useful for creating "Edit Mode" forms for existing plot layers.

## Value

A z_object schema.
