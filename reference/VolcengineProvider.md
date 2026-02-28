# Volcengine Provider Class

Provider class for Volcengine (火山引擎) Ark platform.

## Super class

[`aisdk::OpenAIProvider`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.md)
-\> `VolcengineProvider`

## Methods

### Public methods

- [`VolcengineProvider$new()`](#method-VolcengineProvider-new)

- [`VolcengineProvider$language_model()`](#method-VolcengineProvider-language_model)

- [`VolcengineProvider$clone()`](#method-VolcengineProvider-clone)

Inherited methods

- [`aisdk::OpenAIProvider$embedding_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-embedding_model)
- [`aisdk::OpenAIProvider$responses_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-responses_model)
- [`aisdk::OpenAIProvider$smart_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-smart_model)

------------------------------------------------------------------------

### Method `new()`

Initialize the Volcengine provider.

#### Usage

    VolcengineProvider$new(api_key = NULL, base_url = NULL, headers = NULL)

#### Arguments

- `api_key`:

  Volcengine API key. Defaults to ARK_API_KEY env var.

- `base_url`:

  Base URL. Defaults to https://ark.cn-beijing.volces.com/api/v3.

- `headers`:

  Optional additional headers.

------------------------------------------------------------------------

### Method `language_model()`

Create a language model.

#### Usage

    VolcengineProvider$language_model(model_id = NULL)

#### Arguments

- `model_id`:

  The model ID (e.g., "doubao-1-5-pro-256k-250115").

#### Returns

A VolcengineLanguageModel object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    VolcengineProvider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
