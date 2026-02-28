# Create Stepfun Provider

Factory function to create a Stepfun provider.

## Usage

``` r
create_stepfun(api_key = NULL, base_url = NULL, headers = NULL)
```

## Arguments

- api_key:

  Stepfun API key. Defaults to STEPFUN_API_KEY env var.

- base_url:

  Base URL for API calls. Defaults to https://api.stepfun.com/v1.

- headers:

  Optional additional headers.

## Value

A StepfunProvider object.

## Supported Models

Stepfun provides various models such as:

- **Standard**: "step-1-8k", "step-1-32k", "step-1-128k", "step-1-256k",
  "step-2-16k"

## Examples

``` r
# \donttest{
if (interactive()) {
stepfun <- create_stepfun()
model <- stepfun$language_model("step-1-8k")
result <- generate_text(model, "Explain quantum computing in one sentence.")
}
# }
```
