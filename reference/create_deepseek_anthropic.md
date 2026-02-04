# Create DeepSeek Provider (Anthropic API Format)

Factory function to create a DeepSeek provider using the
Anthropic-compatible API. This allows you to use DeepSeek models with
the Anthropic API format.

## Usage

``` r
create_deepseek_anthropic(api_key = NULL, headers = NULL)
```

## Arguments

- api_key:

  DeepSeek API key. Defaults to DEEPSEEK_API_KEY env var.

- headers:

  Optional additional headers.

## Value

An AnthropicProvider object configured for DeepSeek.

## Details

DeepSeek provides an Anthropic-compatible endpoint at
`https://api.deepseek.com/anthropic`. This convenience function wraps
[`create_anthropic()`](https://YuLab-SMU.github.io/aisdk/reference/create_anthropic.md)
with DeepSeek-specific defaults.

Note: When using an unsupported model name, the API backend will
automatically map it to `deepseek-chat`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Use DeepSeek via Anthropic API format
deepseek <- create_deepseek_anthropic()
model <- deepseek$language_model("deepseek-chat")
result <- generate_text(model, "Hello!")

# This is useful for tools that expect Anthropic API format
# such as Claude Code integration
} # }
```
