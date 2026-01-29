# Embedding Model V1 (Abstract Base Class)

Abstract interface for embedding models.

## Public fields

- `specification_version`:

  The version of this specification.

- `provider`:

  The provider identifier.

- `model_id`:

  The model identifier.

## Methods

### Public methods

- [`EmbeddingModelV1$new()`](#method-EmbeddingModelV1-new)

- [`EmbeddingModelV1$do_embed()`](#method-EmbeddingModelV1-do_embed)

- [`EmbeddingModelV1$clone()`](#method-EmbeddingModelV1-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the embedding model.

#### Usage

    EmbeddingModelV1$new(provider, model_id)

#### Arguments

- `provider`:

  Provider name.

- `model_id`:

  Model ID.

------------------------------------------------------------------------

### Method `do_embed()`

Generate embeddings for a value. Abstract method.

#### Usage

    EmbeddingModelV1$do_embed(value)

#### Arguments

- `value`:

  A character string or vector to embed.

#### Returns

A list with embeddings.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    EmbeddingModelV1$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
