# Check SDK Version Compatibility

Check if code is compatible with the current SDK version and suggest
migration steps if needed.

## Usage

``` r
check_sdk_compatibility(code_version)
```

## Arguments

- code_version:

  Version string the code was written for.

## Value

A list with compatible (logical) and suggestions (character vector).

## Examples

``` r
if (FALSE) { # \dontrun{
result <- check_sdk_compatibility("0.8.0")
if (!result$compatible) {
  cat("Migration needed:\n")
  cat(paste(result$suggestions, collapse = "\n"))
}
} # }
```
