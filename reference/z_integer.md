# Create Integer Schema

Create a JSON Schema for integer type.

## Usage

``` r
z_integer(
  description = NULL,
  nullable = FALSE,
  default = NULL,
  minimum = NULL,
  maximum = NULL
)
```

## Arguments

- description:

  Optional description of the field.

- nullable:

  If TRUE, allows null values.

- default:

  Optional default value.

- minimum:

  Optional minimum value.

- maximum:

  Optional maximum value.

## Value

A list representing JSON Schema for integer.

## Examples

``` r
z_integer(description = "Number of items", minimum = 0)
#> <z_schema>
#> {
#>   "type": "integer",
#>   "description": "Number of items",
#>   "minimum": 0
#> } 
```
