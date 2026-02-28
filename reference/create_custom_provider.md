# Create a custom provider

Creates a dynamic wrapper around existing model classes (OpenAI,
Anthropic) based on user-provided configuration. The returned provider
can be registered in the global `ProviderRegistry`.

## Usage

``` r
create_custom_provider(
  provider_name,
  base_url,
  api_key = NULL,
  api_format = c("chat_completions", "responses", "anthropic_messages"),
  use_max_completion_tokens = FALSE
)
```

## Arguments

- provider_name:

  The identifier name for this custom provider (e.g.
  "my_custom_openai_proxy").

- base_url:

  The base URL for the API endpoint.

- api_key:

  The API key for authentication. If NULL, defaults to checking
  environmental variables.

- api_format:

  The underlying API format to use. Supports "chat_completions" (OpenAI
  default), "responses" (OpenAI Responses API), and "anthropic_messages"
  (Anthropic Messages API).

- use_max_completion_tokens:

  A boolean flag. If TRUE, injects the `is_reasoning_model` capability
  to ensure the model uses `max_completion_tokens` instead of
  `max_tokens`.

## Value

A custom provider object with a `language_model(model_id)` method.
