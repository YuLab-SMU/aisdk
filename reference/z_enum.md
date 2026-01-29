# Create Enum Schema

Create a JSON Schema for string enum type.

## Usage

``` r
z_enum(values, description = NULL, nullable = FALSE, default = NULL)
```

## Arguments

- values:

  Character vector of allowed values.

- description:

  Optional description of the field.

- nullable:

  If TRUE, allows null values.

- default:

  Optional default value.

## Value

A list representing JSON Schema for enum.

## Examples

``` r
z_enum(c("celsius", "fahrenheit"), description = "Temperature unit")
#> <z_schema>
#> {
#>   "type": "string",
#>   "enum": [
#>     "celsius",
#>     "fahrenheit"
#>   ],
#>   "description": "Temperature unit"
#> } 
```
