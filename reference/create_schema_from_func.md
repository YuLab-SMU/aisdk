# Create Schema from Function

Inspects an R function and generates a z_object schema based on its
arguments and default values.

## Usage

``` r
create_schema_from_func(
  func,
  include_args = NULL,
  exclude_args = NULL,
  params = NULL,
  func_name = NULL,
  type_mode = c("infer", "any")
)
```

## Arguments

- func:

  The R function to inspect.

- include_args:

  Optional character vector of argument names to include. If provided,
  only these arguments will be included in the schema.

- exclude_args:

  Optional character vector of argument names to exclude.

- params:

  Optional named list of parameter values to use as defaults. This
  allows overriding the function's default values (e.g., with values
  extracted from an existing plot layer).

- func_name:

  Optional string of the function name to look up documentation. If not
  provided, attempts to infer from 'func' symbol.

- type_mode:

  How to assign parameter types. "infer" (default) uses default values
  to infer types. "any" uses z_any() for all parameters.

## Value

A z_object schema.

## Examples

``` r
if (FALSE) { # \dontrun{
my_func <- function(a = 1, b = "text", c = TRUE) {}
schema <- create_schema_from_func(my_func)
print(schema)

# Override defaults
schema_override <- create_schema_from_func(my_func, params = list(a = 99))
} # }
```
