# Create String Schema

Create a JSON Schema for string type.

## Usage

``` r
z_string(description = NULL, nullable = FALSE, default = NULL)
```

## Arguments

- description:

  Optional description of the field.

- nullable:

  If TRUE, allows null values.

- default:

  Optional default value.

## Value

A list representing JSON Schema for string.

## Examples

``` r
z_string(description = "The city name")
#> <z_schema>
#> {
#>   "type": "string",
#>   "description": "The city name"
#> } 
```
