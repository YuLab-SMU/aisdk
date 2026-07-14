#' @title OpenAI Embedding Model
#' @description
#' Embedding model implementation for OpenAI's embeddings API.
#' @keywords internal
OpenAIEmbeddingModel <- R6::R6Class(
  "OpenAIEmbeddingModel",
  inherit = EmbeddingModelV1,
  private = list(
    config = NULL
  ),
  public = list(
    #' @description Initialize the OpenAI embedding model.
    #' @param model_id The model ID (e.g., "text-embedding-3-small").
    #' @param config Configuration list.
    initialize = function(model_id, config) {
      super$initialize(provider = config$provider_name %||% "openai", model_id = model_id)
      private$config <- config
    },

    #' @description Generate embeddings for a value.
    #' @param value A character string or vector to embed.
    #' @return A list with embeddings and usage.
    do_embed = function(value) {
      url <- api_endpoint_urls(private$config, "/embeddings")
      headers <- list(
        `Content-Type` = "application/json",
        Authorization = paste("Bearer", private$config$api_key)
      )

      body <- list(
        model = self$model_id,
        input = value
      )

      response <- post_to_api(
        url,
        headers,
        body,
        timeout_seconds = private$config$timeout_seconds,
        total_timeout_seconds = private$config$total_timeout_seconds,
        first_byte_timeout_seconds = private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = private$config$connect_timeout_seconds,
        idle_timeout_seconds = private$config$idle_timeout_seconds
      )

      list(
        embeddings = lapply(response$data, function(x) x$embedding),
        usage = response$usage
      )
    }
  )
)
