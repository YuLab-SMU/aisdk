# xAI Provider Class

Provider class for xAI.

## Super class

[`aisdk::OpenAIProvider`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.md)
-\> `XAIProvider`

## Methods

### Public methods

- [`XAIProvider$new()`](#method-XAIProvider-new)

- [`XAIProvider$language_model()`](#method-XAIProvider-language_model)

- [`XAIProvider$clone()`](#method-XAIProvider-clone)

Inherited methods

- [`aisdk::OpenAIProvider$embedding_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-embedding_model)
- [`aisdk::OpenAIProvider$responses_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-responses_model)
- [`aisdk::OpenAIProvider$smart_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-smart_model)

------------------------------------------------------------------------

### Method `new()`

Initialize the xAI provider.

#### Usage

    XAIProvider$new(api_key = NULL, base_url = NULL, headers = NULL)

#### Arguments

- `api_key`:

  xAI API key. Defaults to XAI_API_KEY env var.

- `base_url`:

  Base URL. Defaults to https://api.x.ai/v1.

- `headers`:

  Optional additional headers.

------------------------------------------------------------------------

### Method `language_model()`

Create a language model.

#### Usage

    XAIProvider$language_model(model_id = NULL)

#### Arguments

- `model_id`:

  The model ID (e.g., "grok-4-1-fast-reasoning").

#### Returns

A XAILanguageModel object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    XAIProvider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
