#' @title Utilities: Middleware System
#' @description
#' Provides middleware functionality to wrap and enhance language models.
#' Middleware can transform parameters and wrap generate/stream operations.
#' @name utils_middleware
NULL

# Suppress R CMD check notes for R6 private references
utils::globalVariables("private")

#' @title Middleware (Base Class)
#' @description
#' Defines a middleware that can intercept and modify model operations.
#' @export
Middleware <- R6::R6Class(
  "Middleware",
  public = list(
    #' @field name A descriptive name for this middleware.
    name = "base_middleware",

    #' @description Transform parameters before calling the model.
    #' @param params The original call parameters.
    #' @param type Either "generate" or "stream".
    #' @param model The model being called.
    #' @return The transformed parameters.
    transform_params = function(params, type, model) {
      params # Default: no transformation
    },

    #' @description Wrap the generate operation.
    #' @param do_generate A function that calls the model's do_generate.
    #' @param params The (potentially transformed) parameters.
    #' @param model The model being called.
    #' @return The result of the generation.
    wrap_generate = function(do_generate, params, model) {
      do_generate() # Default: no wrapping
    },

    #' @description Wrap the stream operation.
    #' @param do_stream A function that calls the model's do_stream.
    #' @param params The (potentially transformed) parameters.
    #' @param model The model being called.
    #' @param callback The streaming callback function.
    wrap_stream = function(do_stream, params, model, callback) {
      do_stream(callback) # Default: no wrapping
    }
  )
)

#' @title Wrap Language Model with Middleware
#' @description
#' Wraps a LanguageModelV1 with one or more middleware instances.
#' Middleware is applied in order: first middleware transforms first,
#' last middleware wraps closest to the model.
#'
#' @param model A LanguageModelV1 object.
#' @param middleware A single Middleware object or a list of Middleware objects.
#' @param model_id Optional custom model ID.
#' @param provider_id Optional custom provider ID.
#' @return A new LanguageModelV1 object with middleware applied.
#' @export
wrap_language_model <- function(model, middleware, model_id = NULL, provider_id = NULL) {
  if (!is.list(middleware)) {
    middleware <- list(middleware)
  }

  # Apply middleware in reverse order (last wraps closest to model)
  wrapped_model <- model
  for (mw in rev(middleware)) {
    wrapped_model <- create_wrapped_model(wrapped_model, mw, model_id, provider_id)
  }

  wrapped_model
}

#' @keywords internal
create_wrapped_model <- function(model, middleware, model_id = NULL, provider_id = NULL) {
  # Create a new R6 class that wraps the model
  WrappedModel <- R6::R6Class(
    "WrappedModel",
    inherit = LanguageModelV1,
    private = list(
      inner_model = NULL,
      middleware = NULL
    ),
    public = list(
      initialize = function(inner_model, middleware, model_id, provider_id) {
        private$inner_model <- inner_model
        private$middleware <- middleware
        self$provider <- provider_id %||% inner_model$provider
        self$model_id <- model_id %||% inner_model$model_id
      },

      do_generate = function(params) {
        mw <- private$middleware
        inner <- private$inner_model

        # Transform params
        transformed <- mw$transform_params(params, "generate", inner)

        # Create the do_generate closure
        do_generate_fn <- function() {
          inner$do_generate(transformed)
        }

        # Wrap and call
        mw$wrap_generate(do_generate_fn, transformed, inner)
      },

      do_stream = function(params, callback) {
        mw <- private$middleware
        inner <- private$inner_model

        # Transform params
        transformed <- mw$transform_params(params, "stream", inner)

        # Create the do_stream closure
        do_stream_fn <- function(cb) {
          inner$do_stream(transformed, cb)
        }

        # Wrap and call
        mw$wrap_stream(do_stream_fn, transformed, inner, callback)
      }
    )
  )

  WrappedModel$new(model, middleware, model_id, provider_id)
}
