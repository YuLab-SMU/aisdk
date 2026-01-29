# Convert Schema to JSON

Convert a z_schema object to a JSON string suitable for API calls.
Handles the R-specific auto_unbox issues properly.

## Usage

``` r
schema_to_json(schema, pretty = FALSE)
```

## Arguments

- schema:

  A z_schema object created by z\_\* functions.

- pretty:

  If TRUE, format JSON with indentation.

## Value

A JSON string.

## Examples

``` r
schema <- z_object(
  name = z_string(description = "User name")
)
cat(schema_to_json(schema, pretty = TRUE))
#> {
#>   "type": "object",
#>   "properties": {
#>     "name": {
#>       "type": "string",
#>       "description": "User name"
#>     }
#>   },
#>   "required": [
#>     "name"
#>   ],
#>   "additionalProperties": false
#> }
```
