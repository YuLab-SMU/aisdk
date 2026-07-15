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
    #' @param value A character string or vector to embed. A vector is embedded
    #'   in a single batched request.
    #' @param ... Provider options merged into the request body, e.g.
    #'   `dimensions` (Matryoshka output-dimension truncation, text-embedding-3
    #'   only), `encoding_format`, or `user`.
    #' @return A list with embeddings and usage.
    do_embed = function(value, ...) {
      opts <- list(...)
      url <- api_endpoint_urls(private$config, "/embeddings")
      headers <- list(
        `Content-Type` = "application/json",
        Authorization = paste("Bearer", private$config$api_key)
      )

      body <- list(
        model = self$model_id,
        input = value
      )

      # dimensions must be an integer for the OpenAI embeddings API (jsonlite
      # would otherwise emit 256.0, which the endpoint rejects).
      if (!is.null(opts$dimensions)) {
        opts$dimensions <- as.integer(opts$dimensions)
      }
      opts <- opts[!vapply(opts, is.null, logical(1))]
      if (length(opts) > 0) {
        body <- utils::modifyList(body, opts)
      }

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
