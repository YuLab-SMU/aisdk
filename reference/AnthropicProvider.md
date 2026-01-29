# Anthropic Provider Class

Provider class for Anthropic. Can create language models.

## Public fields

- `specification_version`:

  Provider spec version.

## Methods

### Public methods

- [`AnthropicProvider$new()`](#method-AnthropicProvider-new)

- [`AnthropicProvider$enable_caching()`](#method-AnthropicProvider-enable_caching)

- [`AnthropicProvider$language_model()`](#method-AnthropicProvider-language_model)

- [`AnthropicProvider$clone()`](#method-AnthropicProvider-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the Anthropic provider.

#### Usage

    AnthropicProvider$new(
      api_key = NULL,
      base_url = NULL,
      api_version = NULL,
      headers = NULL,
      name = NULL
    )

#### Arguments

- `api_key`:

  Anthropic API key. Defaults to ANTHROPIC_API_KEY env var.

- `base_url`:

  Base URL for API calls. Defaults to https://api.anthropic.com/v1.

- `api_version`:

  Anthropic API version header. Defaults to "2023-06-01".

- `headers`:

  Optional additional headers.

- `name`:

  Optional provider name override.

------------------------------------------------------------------------

### Method `enable_caching()`

Enable or disable prompt caching.

#### Usage

    AnthropicProvider$enable_caching(enable = TRUE)

#### Arguments

- `enable`:

  Logical.

------------------------------------------------------------------------

### Method `language_model()`

Create a language model.

#### Usage

    AnthropicProvider$language_model(model_id = "claude-sonnet-4-20250514")

#### Arguments

- `model_id`:

  The model ID (e.g., "claude-sonnet-4-20250514",
  "claude-3-5-sonnet-20241022").

#### Returns

An AnthropicLanguageModel object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AnthropicProvider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
