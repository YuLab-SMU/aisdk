# Create Embeddings

Generate embeddings for text using an embedding model.

## Usage

``` r
create_embeddings(model, value, registry = NULL)
```

## Arguments

- model:

  Either an EmbeddingModelV1 object, or a string ID like
  "openai:text-embedding-3-small".

- value:

  A character string or vector to embed.

- registry:

  Optional ProviderRegistry to use.

## Value

A list with embeddings and usage information.

## Examples

``` r
if (FALSE) { # \dontrun{
model <- create_openai()$embedding_model("text-embedding-3-small")
result <- create_embeddings(model, "Hello, world!")
print(length(result$embeddings[[1]]))
} # }
```
