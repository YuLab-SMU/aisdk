# Get Feature Flag

Get the current value of a feature flag.

## Usage

``` r
sdk_feature(flag, default = NULL)
```

## Arguments

- flag:

  Name of the feature flag.

- default:

  Default value if flag not set.

## Value

The flag value.

## Examples

``` r
# \donttest{
if (interactive()) {
# Check if shared session is enabled
if (sdk_feature("use_shared_session")) {
  session <- create_shared_session(model = "openai:gpt-4o")
}
} else {
  session <- create_chat_session(model = "openai:gpt-4o")
}
# }
```
