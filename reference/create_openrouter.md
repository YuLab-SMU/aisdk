# Create OpenRouter Provider

Factory function to create an OpenRouter provider.

## Usage

``` r
create_openrouter(api_key = NULL, base_url = NULL, headers = NULL)
```

## Arguments

- api_key:

  OpenRouter API key. Defaults to OPENROUTER_API_KEY env var.

- base_url:

  Base URL for API calls. Defaults to https://openrouter.ai/api/v1.

- headers:

  Optional additional headers.

## Value

An OpenRouterProvider object.

## Supported Models

OpenRouter provides access to hundreds of models from many providers:

- **OpenAI**: "openai/gpt-4o", "openai/o1"

- **Anthropic**: "anthropic/claude-sonnet-4-20250514"

- **Google**: "google/gemini-2.5-pro"

- **DeepSeek**: "deepseek/deepseek-r1", "deepseek/deepseek-chat-v3-0324"

- **Meta**: "meta-llama/llama-4-maverick"

- And many more at https://openrouter.ai/models

## Examples

``` r
# \donttest{
if (interactive()) {
openrouter <- create_openrouter()

# Access any model via a unified API
model <- openrouter$language_model("openai/gpt-4o")
result <- generate_text(model, "Hello!")

# Reasoning model
model <- openrouter$language_model("deepseek/deepseek-r1")
result <- generate_text(model, "Solve: 15 * 23")
print(result$reasoning)
}
# }
```
