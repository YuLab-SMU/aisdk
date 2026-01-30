# OpenAI Provider Class

Provider class for OpenAI. Can create language and embedding models.

## Public fields

- `specification_version`:

  Provider spec version.

## Methods

### Public methods

- [`OpenAIProvider$new()`](#method-OpenAIProvider-new)

- [`OpenAIProvider$language_model()`](#method-OpenAIProvider-language_model)

- [`OpenAIProvider$responses_model()`](#method-OpenAIProvider-responses_model)

- [`OpenAIProvider$smart_model()`](#method-OpenAIProvider-smart_model)

- [`OpenAIProvider$embedding_model()`](#method-OpenAIProvider-embedding_model)

- [`OpenAIProvider$clone()`](#method-OpenAIProvider-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the OpenAI provider.

#### Usage

    OpenAIProvider$new(
      api_key = NULL,
      base_url = NULL,
      organization = NULL,
      project = NULL,
      headers = NULL,
      name = NULL,
      disable_stream_options = FALSE
    )

#### Arguments

- `api_key`:

  OpenAI API key. Defaults to OPENAI_API_KEY env var.

- `base_url`:

  Base URL for API calls. Defaults to https://api.openai.com/v1.

- `organization`:

  Optional OpenAI organization ID.

- `project`:

  Optional OpenAI project ID.

- `headers`:

  Optional additional headers.

- `name`:

  Optional provider name override (for compatible APIs).

- `disable_stream_options`:

  Disable stream_options parameter (for providers that don't support
  it).

------------------------------------------------------------------------

### Method `language_model()`

Create a language model.

#### Usage

    OpenAIProvider$language_model(model_id = Sys.getenv("OPENAI_MODEL", "gpt-4o"))

#### Arguments

- `model_id`:

  The model ID (e.g., "gpt-4o", "gpt-4o-mini").

#### Returns

An OpenAILanguageModel object.

------------------------------------------------------------------------

### Method `responses_model()`

Create a language model using the Responses API.

#### Usage

    OpenAIProvider$responses_model(model_id)

#### Arguments

- `model_id`:

  The model ID (e.g., "o1", "o3-mini", "gpt-4o").

#### Details

The Responses API is designed for:

- Models with built-in reasoning (o1, o3 series)

- Stateful multi-turn conversations (server maintains history)

- Advanced features like structured outputs

The model maintains conversation state internally via response IDs. Call
`model$reset()` to start a fresh conversation.

#### Returns

An OpenAIResponsesLanguageModel object.

------------------------------------------------------------------------

### Method `smart_model()`

Smart model factory that automatically selects the best API.

#### Usage

    OpenAIProvider$smart_model(
      model_id,
      api_format = c("auto", "chat", "responses")
    )

#### Arguments

- `model_id`:

  The model ID.

- `api_format`:

  API format to use: "auto" (default), "chat", or "responses".

#### Details

When `api_format = "auto"` (default), the method automatically selects:

- Responses API for reasoning models (o1, o3, o1-mini, o3-mini)

- Chat Completions API for all other models (gpt-4o, gpt-4, etc.)

You can override this by explicitly setting `api_format`.

#### Returns

A language model object (either OpenAILanguageModel or
OpenAIResponsesLanguageModel).

------------------------------------------------------------------------

### Method `embedding_model()`

Create an embedding model.

#### Usage

    OpenAIProvider$embedding_model(model_id = "text-embedding-3-small")

#### Arguments

- `model_id`:

  The model ID (e.g., "text-embedding-3-small").

#### Returns

An OpenAIEmbeddingModel object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OpenAIProvider$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
