# Create DeepSeek Provider

Factory function to create a DeepSeek provider.

DeepSeek offers two main models:

- **deepseek-chat**: Standard chat model (DeepSeek-V3.2 non-thinking
  mode)

- **deepseek-reasoner**: Reasoning model with chain-of-thought
  (DeepSeek-V3.2 thinking mode)

## Usage

``` r
create_deepseek(api_key = NULL, base_url = NULL, headers = NULL)
```

## Arguments

- api_key:

  DeepSeek API key. Defaults to DEEPSEEK_API_KEY env var.

- base_url:

  Base URL. Defaults to "https://api.deepseek.com".

- headers:

  Optional additional headers.

## Value

A DeepSeekProvider object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with deepseek-chat
deepseek <- create_deepseek()
model <- deepseek$language_model("deepseek-chat")
result <- generate_text(model, "Hello!")

# Using deepseek-reasoner for chain-of-thought reasoning
model_reasoner <- deepseek$language_model("deepseek-reasoner")
result <- model_reasoner$generate(
    messages = list(list(role = "user", content = "Solve: What is 15 * 23?")),
    max_tokens = 500
)
print(result$text) # Final answer
print(result$reasoning) # Chain-of-thought reasoning

# Streaming with reasoning
stream_text(model_reasoner, "Explain quantum entanglement step by step")
} # }
```
