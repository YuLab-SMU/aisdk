# Stepfun Provider Class

Provider class for Stepfun.

## Super class

[`aisdk::OpenAIProvider`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.md)
-\> `StepfunProvider`

## Methods

### Public methods

- [`StepfunProvider$new()`](#method-StepfunProvider-new)

- [`StepfunProvider$language_model()`](#method-StepfunProvider-language_model)

- [`StepfunProvider$clone()`](#method-StepfunProvider-clone)

Inherited methods

- [`aisdk::OpenAIProvider$embedding_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-embedding_model)
- [`aisdk::OpenAIProvider$responses_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-responses_model)
- [`aisdk::OpenAIProvider$smart_model()`](https://YuLab-SMU.github.io/aisdk/reference/OpenAIProvider.html#method-smart_model)

------------------------------------------------------------------------

### Method `new()`

Initialize the Stepfun provider.

#### Usage

    StepfunProvider$new(api_key = NULL, base_url = NULL, headers = NULL)

#### Arguments

- `api_key`:

  Stepfun API key. Defaults to STEPFUN_API_KEY env var.

- `base_url`:

  Base URL. Defaults to https://api.stepfun.com/v1.

- `headers`:

  Optional additional headers.

------------------------------------------------------------------------

### Method `language_model()`

Create a language model.

#### Usage

    StepfunProvider$language_model(model_id = NULL)

#### Arguments

- `model_id`:

  The model ID (e.g., "step-1-8k").

#### Returns

A StepfunLanguageModel object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    StepfunProvider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
