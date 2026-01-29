# Create Number Schema

Create a JSON Schema for number (floating point) type.

## Usage

``` r
z_number(
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

A list representing JSON Schema for number.

## Examples

``` r
z_number(description = "Temperature value", minimum = -100, maximum = 100)
#> <z_schema>
#> {
#>   "type": "number",
#>   "description": "Temperature value",
#>   "minimum": -100,
#>   "maximum": 100
#> } 
```
