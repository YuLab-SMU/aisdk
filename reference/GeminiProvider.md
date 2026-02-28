# Gemini Provider Class

Provider class for Google Gemini.

## Public fields

- `specification_version`:

  Provider spec version.

## Methods

### Public methods

- [`GeminiProvider$new()`](#method-GeminiProvider-new)

- [`GeminiProvider$language_model()`](#method-GeminiProvider-language_model)

- [`GeminiProvider$clone()`](#method-GeminiProvider-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the Gemini provider.

#### Usage

    GeminiProvider$new(
      api_key = NULL,
      base_url = NULL,
      headers = NULL,
      name = NULL
    )

#### Arguments

- `api_key`:

  Gemini API key. Defaults to GEMINI_API_KEY env var.

- `base_url`:

  Base URL for API calls. Defaults to
  https://generativelanguage.googleapis.com/v1beta/models.

- `headers`:

  Optional additional headers.

- `name`:

  Optional provider name override.

------------------------------------------------------------------------

### Method `language_model()`

Create a language model.

#### Usage

    GeminiProvider$language_model(model_id = "gemini-2.5-flash")

#### Arguments

- `model_id`:

  The model ID (e.g., "gemini-1.5-pro", "gemini-1.5-flash",
  "gemini-2.0-flash").

#### Returns

A GeminiLanguageModel object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GeminiProvider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
