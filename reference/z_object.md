# Create Object Schema

Create a JSON Schema for object type. This is the primary schema builder
for defining tool parameters.

## Usage

``` r
z_object(
  ...,
  .description = NULL,
  .required = NULL,
  .additional_properties = FALSE
)
```

## Arguments

- ...:

  Named arguments where names are property names and values are z_schema
  objects created by z\_\* functions.

- .description:

  Optional description of the object.

- .required:

  Character vector of required field names. If NULL (default), all
  fields are considered required.

- .additional_properties:

  Whether to allow additional properties. Default FALSE.

## Value

A list representing JSON Schema for object.

## Examples

``` r
z_object(
  location = z_string(description = "City name, e.g., Beijing"),
  unit = z_enum(c("celsius", "fahrenheit"), description = "Temperature unit")
)
#> <z_schema>
#> {
#>   "type": "object",
#>   "properties": {
#>     "location": {
#>       "type": "string",
#>       "description": "City name, e.g., Beijing"
#>     },
#>     "unit": {
#>       "type": "string",
#>       "enum": [
#>         "celsius",
#>         "fahrenheit"
#>       ],
#>       "description": "Temperature unit"
#>     }
#>   },
#>   "required": [
#>     "location",
#>     "unit"
#>   ],
#>   "additionalProperties": false
#> } 
```
