# Create Volcengine/Ark Provider

Convenience function to create a Volcengine (火山引擎) provider using
the Ark API. This is a wrapper around create_openai with
Volcengine-specific defaults.

## Usage

``` r
create_volcengine(api_key = NULL, base_url = NULL, headers = NULL)
```

## Arguments

- api_key:

  Volcengine API key. Defaults to ARK_API_KEY env var.

- base_url:

  Base URL for API calls. Defaults to
  https://ark.cn-beijing.volces.com/api/v3.

- headers:

  Optional additional headers.

## Value

An OpenAIProvider object configured for Volcengine.

## Token Limit Parameters for Volcengine Responses API

Volcengine's Responses API has two mutually exclusive token limit
parameters:

- `max_output_tokens`: Total limit including reasoning + answer (default
  mapping)

- `max_tokens` (API level): Answer-only limit, excluding reasoning

The SDK's unified `max_tokens` parameter maps to `max_output_tokens` by
default, which is the **safe choice** to prevent runaway reasoning
costs.

For advanced users who want answer-only limits:

- Use `max_answer_tokens` parameter to explicitly set answer-only limit

- Use `max_output_tokens` parameter to explicitly set total limit

## Examples

``` r
if (FALSE) { # \dontrun{
volcengine <- create_volcengine()

# Chat API (standard models)
model <- volcengine$language_model("doubao-1-5-pro-256k-250115")
result <- generate_text(model, "你好")

# Responses API (reasoning models like DeepSeek)
model <- volcengine$responses_model("deepseek-r1-250120")

# Default: max_tokens limits total output (reasoning + answer)
result <- model$generate(messages = msgs, max_tokens = 2000)

# Advanced: limit only the answer part (reasoning can be longer)
result <- model$generate(messages = msgs, max_answer_tokens = 500)

# Advanced: explicitly set total limit
result <- model$generate(messages = msgs, max_output_tokens = 3000)
} # }
```
