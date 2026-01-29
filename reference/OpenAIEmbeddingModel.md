# OpenAI Embedding Model

Embedding model implementation for OpenAI's embeddings API.

## Super class

[`aisdk::EmbeddingModelV1`](https://YuLab-SMU.github.io/aisdk/reference/EmbeddingModelV1.md)
-\> `OpenAIEmbeddingModel`

## Methods

### Public methods

- [`OpenAIEmbeddingModel$new()`](#method-OpenAIEmbeddingModel-new)

- [`OpenAIEmbeddingModel$do_embed()`](#method-OpenAIEmbeddingModel-do_embed)

- [`OpenAIEmbeddingModel$clone()`](#method-OpenAIEmbeddingModel-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the OpenAI embedding model.

#### Usage

    OpenAIEmbeddingModel$new(model_id, config)

#### Arguments

- `model_id`:

  The model ID (e.g., "text-embedding-3-small").

- `config`:

  Configuration list.

------------------------------------------------------------------------

### Method `do_embed()`

Generate embeddings for a value.

#### Usage

    OpenAIEmbeddingModel$do_embed(value)

#### Arguments

- `value`:

  A character string or vector to embed.

#### Returns

A list with embeddings and usage.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OpenAIEmbeddingModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
