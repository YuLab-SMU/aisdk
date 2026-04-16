#' @name provider_openrouter
#' @title OpenRouter Provider
#' @description
#' Implementation for OpenRouter, a unified API gateway for multiple LLM providers.
#' OpenRouter API is OpenAI-compatible and provides access to models from OpenAI,
#' Anthropic, Google, Meta, Mistral, DeepSeek, and many more.
#' @keywords internal
NULL

#' @title OpenRouter Language Model Class
#' @description
#' Language model implementation for OpenRouter's chat completions API.
#' Inherits from OpenAI model but adds support for OpenRouter-specific features
#' like reasoning content extraction from reasoning models.
#' @keywords internal
OpenRouterLanguageModel <- R6::R6Class(
    "OpenRouterLanguageModel",
    inherit = OpenAILanguageModel,
    public = list(
        #' @description Parse the API response into a GenerateResult.
        #' Overrides parent to extract reasoning_content from reasoning models.
        #' @param response The parsed API response.
        #' @return A GenerateResult object.
        parse_response = function(response) {
            result <- super$parse_response(response)

            # Extract reasoning content (DeepSeek-R1, QwQ, etc. via OpenRouter)
            choice <- response$choices[[1]]
            result$reasoning <- choice$message$reasoning_content

            result
        }
    )
)

#' @title OpenRouter Provider Class
#' @description
#' Provider class for OpenRouter.
#' @export
OpenRouterProvider <- R6::R6Class(
    "OpenRouterProvider",
    inherit = OpenAIProvider,
    public = list(
        #' @description Initialize the OpenRouter provider.
        #' @param api_key OpenRouter API key. Defaults to OPENROUTER_API_KEY env var.
        #' @param base_url Base URL. Defaults to https://openrouter.ai/api/v1.
        #' @param headers Optional additional headers.
        #' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
        #' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
        #' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
        #' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
        #' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
        initialize = function(api_key = NULL,
                              base_url = NULL,
                              headers = NULL,
                              timeout_seconds = NULL,
                              total_timeout_seconds = NULL,
                              first_byte_timeout_seconds = NULL,
                              connect_timeout_seconds = NULL,
                              idle_timeout_seconds = NULL) {
            suppressWarnings(
                super$initialize(
                    api_key = api_key %||% Sys.getenv("OPENROUTER_API_KEY"),
                    base_url = base_url %||% Sys.getenv("OPENROUTER_BASE_URL", "https://openrouter.ai/api/v1"),
                    headers = headers,
                    name = "openrouter",
                    timeout_seconds = timeout_seconds,
                    total_timeout_seconds = total_timeout_seconds,
                    first_byte_timeout_seconds = first_byte_timeout_seconds,
                    connect_timeout_seconds = connect_timeout_seconds,
                    idle_timeout_seconds = idle_timeout_seconds
                )
            )

            if (nchar(private$config$api_key) == 0) {
                rlang::warn("OpenRouter API key not set. Set OPENROUTER_API_KEY env var or pass api_key parameter.")
            }
        },

        #' @description Create a language model.
        #' @param model_id The model ID (e.g., "openai/gpt-4o", "anthropic/claude-sonnet-4-20250514",
        #'   "deepseek/deepseek-r1", "google/gemini-2.5-pro").
        #' @return An OpenRouterLanguageModel object.
        language_model = function(model_id = NULL) {
            model_id <- model_id %||% Sys.getenv("OPENROUTER_MODEL")
            if (is.null(model_id) || model_id == "") {
                rlang::abort("Model ID not provided and OPENROUTER_MODEL environment variable not set.")
            }
            OpenRouterLanguageModel$new(model_id, private$config)
        },

        #' @description Create an image model.
        #' @param model_id The model ID (e.g., "openai/gpt-image-1").
        #' @return An OpenAIImageModel object.
        image_model = function(model_id = NULL) {
            model_id <- model_id %||% Sys.getenv("OPENROUTER_IMAGE_MODEL", unset = "")
            if (is.null(model_id) || model_id == "") {
                model_id <- "openai/gpt-image-1"
            }
            OpenAIImageModel$new(model_id, private$config)
        }
    )
)

#' @title Create OpenRouter Provider
#' @description
#' Factory function to create an OpenRouter provider.
#'
#' @section Supported Models:
#' OpenRouter provides access to hundreds of models from many providers:
#' \itemize{
#'   \item \strong{OpenAI}: "openai/gpt-4o", "openai/o1"
#'   \item \strong{Anthropic}: "anthropic/claude-sonnet-4-20250514"
#'   \item \strong{Google}: "google/gemini-2.5-pro"
#'   \item \strong{DeepSeek}: "deepseek/deepseek-r1", "deepseek/deepseek-chat-v3-0324"
#'   \item \strong{Meta}: "meta-llama/llama-4-maverick"
#'   \item And many more at https://openrouter.ai/models
#' }
#'
#' @param api_key OpenRouter API key. Defaults to OPENROUTER_API_KEY env var.
#' @param base_url Base URL for API calls. Defaults to https://openrouter.ai/api/v1.
#' @param headers Optional additional headers.
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
#' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
#' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
#' @return An OpenRouterProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#' openrouter <- create_openrouter()
#'
#' # Access any model via a unified API
#' model <- openrouter$language_model("openai/gpt-4o")
#' result <- generate_text(model, "Hello!")
#'
#' # Reasoning model
#' model <- openrouter$language_model("deepseek/deepseek-r1")
#' result <- generate_text(model, "Solve: 15 * 23")
#' print(result$reasoning)
#' }
#' }
create_openrouter <- function(api_key = NULL,
                              base_url = NULL,
                              headers = NULL,
                              timeout_seconds = NULL,
                              total_timeout_seconds = NULL,
                              first_byte_timeout_seconds = NULL,
                              connect_timeout_seconds = NULL,
                              idle_timeout_seconds = NULL) {
    OpenRouterProvider$new(
        api_key = api_key,
        base_url = base_url,
        headers = headers,
        timeout_seconds = timeout_seconds,
        total_timeout_seconds = total_timeout_seconds,
        first_byte_timeout_seconds = first_byte_timeout_seconds,
        connect_timeout_seconds = connect_timeout_seconds,
        idle_timeout_seconds = idle_timeout_seconds
    )
}
