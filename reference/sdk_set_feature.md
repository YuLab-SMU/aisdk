# Set Feature Flag

Set a feature flag value. Use this to enable/disable SDK features.

## Usage

``` r
sdk_set_feature(flag, value)
```

## Arguments

- flag:

  Name of the feature flag.

- value:

  Value to set.

## Value

Invisible previous value.

## Examples

``` r
if (FALSE) { # \dontrun{
# Disable shared session for legacy compatibility
sdk_set_feature("use_shared_session", FALSE)

# Enable legacy tool format
sdk_set_feature("legacy_tool_format", TRUE)
} # }
```
