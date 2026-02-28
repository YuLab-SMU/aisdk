# Create Volcengine/Ark Provider

Factory function to create a Volcengine (火山引擎) provider using the
Ark API.

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

A VolcengineProvider object.

## Supported Models

Volcengine Ark platform hosts a variety of models:

- **Doubao (豆包)**: ByteDance's proprietary models (e.g.,
  "doubao-1-5-pro-256k-250115")

- **DeepSeek**: DeepSeek models hosted on Volcengine (e.g.,
  "deepseek-r1-250120")

- Other third-party models available on the platform

## API Formats

Volcengine supports both Chat Completions API and Responses API:

- `language_model()`: Uses Chat Completions API (standard)

- `responses_model()`: Uses Responses API (for reasoning models)

- `smart_model()`: Auto-selects based on model ID

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
# \donttest{
if (interactive()) {
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

# Smart model selection (auto-detects best API)
model <- volcengine$smart_model("deepseek-r1-250120")
}
# }
```
