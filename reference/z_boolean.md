# Create Boolean Schema

Create a JSON Schema for boolean type.

## Usage

``` r
z_boolean(description = NULL, nullable = FALSE, default = NULL)
```

## Arguments

- description:

  Optional description of the field.

- nullable:

  If TRUE, allows null values.

- default:

  Optional default value.

## Value

A list representing JSON Schema for boolean.

## Examples

``` r
z_boolean(description = "Whether to include details")
#> <z_schema>
#> {
#>   "type": "boolean",
#>   "description": "Whether to include details"
#> } 
```
