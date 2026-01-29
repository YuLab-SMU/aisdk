# Create Array Schema

Create a JSON Schema for array type.

## Usage

``` r
z_array(
  items,
  description = NULL,
  nullable = FALSE,
  default = NULL,
  min_items = NULL,
  max_items = NULL
)
```

## Arguments

- items:

  Schema for array items (created by z\_\* functions).

- description:

  Optional description of the field.

- nullable:

  If TRUE, allows null values.

- default:

  Optional default value.

- min_items:

  Optional minimum number of items.

- max_items:

  Optional maximum number of items.

## Value

A list representing JSON Schema for array.

## Examples

``` r
z_array(z_string(), description = "List of names")
#> <z_schema>
#> {
#>   "type": "array",
#>   "items": {
#>     "type": "string"
#>   },
#>   "description": "List of names"
#> } 
```
