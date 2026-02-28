# Create xAI Provider

Factory function to create an xAI provider.

## Usage

``` r
create_xai(api_key = NULL, base_url = NULL, headers = NULL)
```

## Arguments

- api_key:

  xAI API key. Defaults to XAI_API_KEY env var.

- base_url:

  Base URL for API calls. Defaults to https://api.x.ai/v1.

- headers:

  Optional additional headers.

## Value

A XAIProvider object.

## Supported Models

xAI provides Grok models:

- **Grok**: "grok-beta", "grok-2-1212", "grok-4-1-fast-reasoning", etc.

## Examples

``` r
# \donttest{
if (interactive()) {
xai <- create_xai()
model <- xai$language_model("grok-4-1-fast-reasoning")
result <- generate_text(model, "Explain quantum computing in one sentence.")
}
# }
```
