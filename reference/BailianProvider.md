# Bailian Provider Class

Provider class for Alibaba Cloud Bailian (百炼) / DashScope platform.

## Super class

[`aisdk::OpenAIProvider`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.md)
-\> `BailianProvider`

## Methods

### Public methods

- [`BailianProvider$new()`](#method-BailianProvider-new)

- [`BailianProvider$language_model()`](#method-BailianProvider-language_model)

- [`BailianProvider$clone()`](#method-BailianProvider-clone)

Inherited methods

- [`aisdk::OpenAIProvider$embedding_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-embedding_model)
- [`aisdk::OpenAIProvider$responses_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-responses_model)
- [`aisdk::OpenAIProvider$smart_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-smart_model)

------------------------------------------------------------------------

### Method `new()`

Initialize the Bailian provider.

#### Usage

    BailianProvider$new(api_key = NULL, base_url = NULL, headers = NULL)

#### Arguments

- `api_key`:

  DashScope API key. Defaults to DASHSCOPE_API_KEY env var.

- `base_url`:

  Base URL. Defaults to
  https://dashscope.aliyuncs.com/compatible-mode/v1.

- `headers`:

  Optional additional headers.

------------------------------------------------------------------------

### Method `language_model()`

Create a language model.

#### Usage

    BailianProvider$language_model(model_id = NULL)

#### Arguments

- `model_id`:

  The model ID (e.g., "qwen-plus", "qwen-turbo", "qwq-32b").

#### Returns

A BailianLanguageModel object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BailianProvider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
