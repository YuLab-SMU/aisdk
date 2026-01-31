# Create Any Schema

Create a JSON Schema that accepts any JSON value.

## Usage

``` r
z_any(description = NULL, nullable = TRUE, default = NULL)
```

## Arguments

- description:

  Optional description of the field.

- nullable:

  If TRUE, allows null values.

- default:

  Optional default value.

## Value

A list representing JSON Schema for any value.

## Examples

``` r
z_any(description = "Flexible input")
#> <z_schema>
#> {
#>   "type": ["string", "number", "integer", "boolean", "object", "array", "null"],
#>   "description": "Flexible input"
#> } 
```
