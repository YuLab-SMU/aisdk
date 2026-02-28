# OpenRouter Provider Class

Provider class for OpenRouter.

## Super class

[`aisdk::OpenAIProvider`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.md)
-\> `OpenRouterProvider`

## Methods

### Public methods

- [`OpenRouterProvider$new()`](#method-OpenRouterProvider-new)

- [`OpenRouterProvider$language_model()`](#method-OpenRouterProvider-language_model)

- [`OpenRouterProvider$clone()`](#method-OpenRouterProvider-clone)

Inherited methods

- [`aisdk::OpenAIProvider$embedding_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-embedding_model)
- [`aisdk::OpenAIProvider$responses_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-responses_model)
- [`aisdk::OpenAIProvider$smart_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-smart_model)

------------------------------------------------------------------------

### Method `new()`

Initialize the OpenRouter provider.

#### Usage

    OpenRouterProvider$new(api_key = NULL, base_url = NULL, headers = NULL)

#### Arguments

- `api_key`:

  OpenRouter API key. Defaults to OPENROUTER_API_KEY env var.

- `base_url`:

  Base URL. Defaults to https://openrouter.ai/api/v1.

- `headers`:

  Optional additional headers.

------------------------------------------------------------------------

### Method `language_model()`

Create a language model.

#### Usage

    OpenRouterProvider$language_model(model_id = NULL)

#### Arguments

- `model_id`:

  The model ID (e.g., "openai/gpt-4o",
  "anthropic/claude-sonnet-4-20250514", "deepseek/deepseek-r1",
  "google/gemini-2.5-pro").

#### Returns

An OpenRouterLanguageModel object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OpenRouterProvider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
