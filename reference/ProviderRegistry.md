# Provider Registry

Manages registered providers and allows accessing models by ID.

## Methods

### Public methods

- [`ProviderRegistry$new()`](#method-ProviderRegistry-new)

- [`ProviderRegistry$register()`](#method-ProviderRegistry-register)

- [`ProviderRegistry$language_model()`](#method-ProviderRegistry-language_model)

- [`ProviderRegistry$embedding_model()`](#method-ProviderRegistry-embedding_model)

- [`ProviderRegistry$list_providers()`](#method-ProviderRegistry-list_providers)

- [`ProviderRegistry$clone()`](#method-ProviderRegistry-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the registry.

#### Usage

    ProviderRegistry$new(separator = ":")

#### Arguments

- `separator`:

  The separator between provider and model IDs (default: ":").

------------------------------------------------------------------------

### Method `register()`

Register a provider.

#### Usage

    ProviderRegistry$register(id, provider)

#### Arguments

- `id`:

  The provider ID (e.g., "openai").

- `provider`:

  The provider object (must have `language_model` method).

------------------------------------------------------------------------

### Method `language_model()`

Get a language model by ID.

#### Usage

    ProviderRegistry$language_model(id)

#### Arguments

- `id`:

  Model ID in the format "provider:model" (e.g., "openai:gpt-4o").

#### Returns

A LanguageModelV1 object.

------------------------------------------------------------------------

### Method `embedding_model()`

Get an embedding model by ID.

#### Usage

    ProviderRegistry$embedding_model(id)

#### Arguments

- `id`:

  Model ID in the format "provider:model".

#### Returns

An EmbeddingModelV1 object.

------------------------------------------------------------------------

### Method `list_providers()`

List all registered provider IDs.

#### Usage

    ProviderRegistry$list_providers()

#### Returns

A character vector of provider IDs.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ProviderRegistry$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
