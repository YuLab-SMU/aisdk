# Create OpenAI Provider

Factory function to create an OpenAI provider.

## Usage

``` r
create_openai(
  api_key = NULL,
  base_url = NULL,
  organization = NULL,
  project = NULL,
  headers = NULL,
  name = NULL,
  disable_stream_options = FALSE
)
```

## Arguments

- api_key:

  OpenAI API key. Defaults to OPENAI_API_KEY env var.

- base_url:

  Base URL for API calls. Defaults to https://api.openai.com/v1.

- organization:

  Optional OpenAI organization ID.

- project:

  Optional OpenAI project ID.

- headers:

  Optional additional headers.

- name:

  Optional provider name override (for compatible APIs).

- disable_stream_options:

  Disable stream_options parameter (for providers like Volcengine that
  don't support it).

## Value

An OpenAIProvider object.

## Token Limit Parameters

The SDK provides a unified `max_tokens` parameter that automatically
maps to the correct API field based on the model and API type:

- **Chat API (standard models)**: `max_tokens` -\> `max_tokens`

- **Chat API (o1/o3 models)**: `max_tokens` -\> `max_completion_tokens`

- **Responses API**: `max_tokens` -\> `max_output_tokens` (total:
  reasoning + answer)

For advanced users who need fine-grained control:

- `max_completion_tokens`: Explicitly set completion tokens (Chat API,
  o1/o3)

- `max_output_tokens`: Explicitly set total output limit (Responses API)

- `max_answer_tokens`: Limit answer only, excluding reasoning (Responses
  API, Volcengine-specific)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with Chat Completions API
openai <- create_openai(api_key = "sk-...")
model <- openai$language_model("gpt-4o")
result <- generate_text(model, "Hello!")

# Using Responses API for reasoning models
openai <- create_openai()
model <- openai$responses_model("o1")
result <- generate_text(model, "Solve this math problem...")
print(result$reasoning)  # Access chain-of-thought

# Smart model selection (auto-detects best API)
model <- openai$smart_model("o3-mini")  # Uses Responses API
model <- openai$smart_model("gpt-4o")   # Uses Chat Completions API

# Token limits - unified interface
# For standard models: limits generated content
result <- model$generate(messages = msgs, max_tokens = 1000)

# For o1/o3 models: automatically maps to max_completion_tokens
model_o1 <- openai$language_model("o1")
result <- model_o1$generate(messages = msgs, max_tokens = 2000)

# For Responses API: automatically maps to max_output_tokens (total limit)
model_resp <- openai$responses_model("o1")
result <- model_resp$generate(messages = msgs, max_tokens = 2000)

# Advanced: explicitly control answer-only limit (Volcengine Responses API)
result <- model_resp$generate(messages = msgs, max_answer_tokens = 500)

# Multi-turn conversation with Responses API
model <- openai$responses_model("o1")
result1 <- generate_text(model, "What is 2+2?")
result2 <- generate_text(model, "Now multiply that by 3")  # Remembers context
model$reset()  # Start fresh conversation

# For Volcengine/Ark API
volcengine <- create_openai(
  api_key = Sys.getenv("ARK_API_KEY"),
  base_url = "https://ark.cn-beijing.volces.com/api/v3",
  name = "volcengine",
  disable_stream_options = TRUE
)
} # }
```
