# Create Anthropic Provider

Factory function to create an Anthropic provider.

## Usage

``` r
create_anthropic(
  api_key = NULL,
  base_url = NULL,
  api_version = NULL,
  headers = NULL,
  name = NULL
)
```

## Arguments

- api_key:

  Anthropic API key. Defaults to ANTHROPIC_API_KEY env var.

- base_url:

  Base URL for API calls. Defaults to https://api.anthropic.com/v1.

- api_version:

  Anthropic API version header. Defaults to "2023-06-01".

- headers:

  Optional additional headers.

- name:

  Optional provider name override.

## Value

An AnthropicProvider object.

## Examples

``` r
if (FALSE) { # \dontrun{
anthropic <- create_anthropic(api_key = "sk-ant-...")
model <- anthropic$language_model("claude-sonnet-4-20250514")
} # }
```
