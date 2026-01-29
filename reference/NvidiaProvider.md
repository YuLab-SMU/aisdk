# NVIDIA Provider Class

Provider class for NVIDIA.

## Super class

[`aisdk::OpenAIProvider`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.md)
-\> `NvidiaProvider`

## Methods

### Public methods

- [`NvidiaProvider$new()`](#method-NvidiaProvider-new)

- [`NvidiaProvider$language_model()`](#method-NvidiaProvider-language_model)

- [`NvidiaProvider$clone()`](#method-NvidiaProvider-clone)

Inherited methods

- [`aisdk::OpenAIProvider$embedding_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-embedding_model)
- [`aisdk::OpenAIProvider$responses_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-responses_model)
- [`aisdk::OpenAIProvider$smart_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-smart_model)

------------------------------------------------------------------------

### Method `new()`

Initialize the NVIDIA provider.

#### Usage

    NvidiaProvider$new(api_key = NULL, base_url = NULL, headers = NULL)

#### Arguments

- `api_key`:

  NVIDIA API key. Defaults to NVIDIA_API_KEY env var.

- `base_url`:

  Base URL. Defaults to https://integrate.api.nvidia.com/v1.

- `headers`:

  Optional additional headers.

------------------------------------------------------------------------

### Method `language_model()`

Create a language model.

#### Usage

    NvidiaProvider$language_model(model_id)

#### Arguments

- `model_id`:

  The model ID (e.g., "z-ai/glm4.7").

#### Returns

A NvidiaLanguageModel object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    NvidiaProvider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
