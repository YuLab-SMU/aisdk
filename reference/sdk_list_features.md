# List Feature Flags

List all available feature flags and their current values.

## Usage

``` r
sdk_list_features()
```

## Value

A named list of feature flags.

## Examples

``` r
# \donttest{
if (interactive()) {
# See all feature flags
print(sdk_list_features())
}
# }
```
