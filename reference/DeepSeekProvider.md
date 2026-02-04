# DeepSeek Provider Class

Provider class for DeepSeek.

## Super class

[`aisdk::OpenAIProvider`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.md)
-\> `DeepSeekProvider`

## Methods

### Public methods

- [`DeepSeekProvider$new()`](#method-DeepSeekProvider-new)

- [`DeepSeekProvider$language_model()`](#method-DeepSeekProvider-language_model)

- [`DeepSeekProvider$clone()`](#method-DeepSeekProvider-clone)

Inherited methods

- [`aisdk::OpenAIProvider$embedding_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-embedding_model)
- [`aisdk::OpenAIProvider$responses_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-responses_model)
- [`aisdk::OpenAIProvider$smart_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-smart_model)

------------------------------------------------------------------------

### Method `new()`

Initialize the DeepSeek provider.

#### Usage

    DeepSeekProvider$new(api_key = NULL, base_url = NULL, headers = NULL)

#### Arguments

- `api_key`:

  DeepSeek API key. Defaults to DEEPSEEK_API_KEY env var.

- `base_url`:

  Base URL. Defaults to https://api.deepseek.com.

- `headers`:

  Optional additional headers.

------------------------------------------------------------------------

### Method `language_model()`

Create a language model.

#### Usage

    DeepSeekProvider$language_model(model_id = NULL)

#### Arguments

- `model_id`:

  The model ID (e.g., "deepseek-chat", "deepseek-reasoner").

#### Returns

A DeepSeekLanguageModel object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DeepSeekProvider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
