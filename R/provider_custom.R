#' @title Custom Provider Factory
#' @description
#' A dynamic factory for creating custom provider instances.
#' This allows users to instantiate a model provider at runtime by configuring
#' the endpoint (`base_url`), credentials (`api_key`), network protocol/routing (`api_format`),
#' and specific capabilities (`use_max_completion_tokens`), without writing a new Provider class.
#'
#' @name provider_custom
NULL

#' @title Create a custom provider
#' @description
#' Creates a dynamic wrapper around existing model classes (OpenAI, Anthropic)
#' based on user-provided configuration. The returned provider can be registered
#' in the global `ProviderRegistry`.
#'
#' @param provider_name The identifier name for this custom provider (e.g. "my_custom_openai_proxy").
#' @param base_url The base URL for the API endpoint.
#' @param api_key The API key for authentication. If NULL, defaults to checking environmental variables.
#' @param api_format The underlying API format to use. Supports "chat_completions" (OpenAI default),
#' "responses" (OpenAI Responses API), and "anthropic_messages" (Anthropic Messages API).
#' @param use_max_completion_tokens A boolean flag. If TRUE, injects the `is_reasoning_model` capability
#' to ensure the model uses `max_completion_tokens` instead of `max_tokens`.
#'
#' @return A custom provider object with a `language_model(model_id)` method.
#' @export
create_custom_provider <- function(
  provider_name,
  base_url,
  api_key = NULL,
  api_format = c("chat_completions", "responses", "anthropic_messages"),
  use_max_completion_tokens = FALSE
) {
    api_format <- match.arg(api_format)

    if (is.null(provider_name) || trimws(provider_name) == "") {
        rlang::abort("`provider_name` must be a non-empty string.")
    }

    if (is.null(base_url) || trimws(base_url) == "") {
        rlang::abort("`base_url` must be a valid URL string.")
    }

    # Build the base configuration to be injected into the underlying model
    config <- list(
        api_key = api_key,
        base_url = base_url
    )

    # Inject capabilities
    # Currently, the only configurable capability in this factory is `is_reasoning_model`.
    capabilities <- list(
        is_reasoning_model = isTRUE(use_max_completion_tokens)
    )

    # Dynamically generate the Provider wrapper class
    CustomProvider <- R6::R6Class(
        classname = paste0("CustomProvider_", provider_name),
        public = list(
            name = NULL,
            initialize = function(name) {
                self$name <- name
            },

            # Get a Language Model instance for this custom provider.
            # model_id: The specific model to use (e.g. "gpt-4o")
            # Returns a LanguageModelV1 object instance.
            language_model = function(model_id) {
                # Core routing logic to instantiate the correct V2 abstract class

                if (api_format == "chat_completions") {
                    return(OpenAILanguageModel$new(
                        model_id = model_id,
                        config = config,
                        capabilities = capabilities
                    ))
                } else if (api_format == "responses") {
                    return(OpenAIResponsesLanguageModel$new(
                        model_id = model_id,
                        config = config,
                        capabilities = capabilities
                    ))
                } else if (api_format == "anthropic_messages") {
                    return(AnthropicLanguageModel$new(
                        model_id = model_id,
                        config = config,
                        capabilities = capabilities
                    ))
                } else {
                    rlang::abort(paste0("Unsupported api_format: ", api_format))
                }
            }
        )
    )

    return(CustomProvider$new(name = provider_name))
}
