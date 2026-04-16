#' @name provider_aihubmix
#' @title AiHubMix Provider
#' @description
#' Implementation for AiHubMix models.
#' AiHubMix API is OpenAI-compatible, but provides extended support for
#' features like Claude's extended thinking and prompt caching.
#' @keywords internal
NULL

#' @title AiHubMix Language Model Class
#' @description
#' Language model implementation for AiHubMix's chat completions API.
#' Inherits from OpenAILanguageModel as AiHubMix provides an OpenAI-compatible API.
#' @keywords internal
AiHubMixLanguageModel <- R6::R6Class(
    "AiHubMixLanguageModel",
    inherit = OpenAILanguageModel,
    public = list(
        #' @description Parse the API response into a GenerateResult.
        #' Overrides parent to extract AiHubMix-specific reasoning fields.
        #' @param response The parsed API response.
        #' @return A GenerateResult object.
        parse_response = function(response) {
            # Use parent's parsing for standard fields
            result <- super$parse_response(response)

            # Extract reasoning content/details (AhHubMix specific)
            choice <- response$choices[[1]]

            # reasoning_content (string)
            if (!is.null(choice$message$reasoning_content)) {
                result$reasoning <- choice$message$reasoning_content
            }

            # Allow raw_response to contain reasoning_details for multi-turn passing
            # as mentioned in the docs: response.choices[0].message.reasoning_details
            if (!is.null(choice$message$reasoning_details)) {
                # Store reasoning_details natively in the result object
                # or rely on raw_response. Here we ensure it's captured in raw_response.
            }

            result
        },

        #' @description Build the request payload for non-streaming generation.
        #' Overrides parent to process caching and reasoning parameters.
        #' @param params A list of call options.
        #' @return A list with url, headers, and body.
        build_payload = function(params) {
            payload <- super$build_payload(params)

            # Pass reasoning mapping parameters if user provides them
            # e.g., budget_tokens, reasoning_effort
            if (!is.null(params$reasoning_effort)) {
                payload$body$reasoning_effort <- params$reasoning_effort
            }
            if (!is.null(params$reasoning)) {
                payload$body$reasoning <- params$reasoning
            }
            if (!is.null(params$budget_tokens)) {
                payload$body$budget_tokens <- params$budget_tokens
            }

            payload
        },

        #' @description Build the request payload for streaming generation.
        #' Overrides parent to process caching and reasoning parameters.
        #' @param params A list of call options.
        #' @return A list with url, headers, and body.
        build_stream_payload = function(params) {
            payload <- super$build_stream_payload(params)

            # Pass reasoning mapping parameters if user provides them
            if (!is.null(params$reasoning_effort)) {
                payload$body$reasoning_effort <- params$reasoning_effort
            }
            if (!is.null(params$reasoning)) {
                payload$body$reasoning <- params$reasoning
            }
            if (!is.null(params$budget_tokens)) {
                payload$body$budget_tokens <- params$budget_tokens
            }

            payload
        }
    )
)

#' @title AiHubMix Provider Class
#' @description
#' Provider class for AiHubMix.
#' @export
AiHubMixProvider <- R6::R6Class(
    "AiHubMixProvider",
    inherit = OpenAIProvider,
    public = list(
        #' @description Initialize the AiHubMix provider.
        #' @param api_key AiHubMix API key. Defaults to AIHUBMIX_API_KEY env var.
        #' @param base_url Base URL. Defaults to https://aihubmix.com/v1.
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
                    api_key = api_key %||% Sys.getenv("AIHUBMIX_API_KEY"),
                    base_url = base_url %||% Sys.getenv("AIHUBMIX_BASE_URL", "https://aihubmix.com/v1"),
                    headers = headers,
                    name = "aihubmix",
                    timeout_seconds = timeout_seconds,
                    total_timeout_seconds = total_timeout_seconds,
                    first_byte_timeout_seconds = first_byte_timeout_seconds,
                    connect_timeout_seconds = connect_timeout_seconds,
                    idle_timeout_seconds = idle_timeout_seconds
                )
            )

            if (nchar(private$config$api_key) == 0) {
                rlang::warn("AiHubMix API key not set. Set AIHUBMIX_API_KEY env var or pass api_key parameter.")
            }
        },

        #' @description Create a language model.
        #' @param model_id The model ID (e.g., "claude-sonnet-3-5", "claude-opus-3", "gpt-4o").
        #' @return An AiHubMixLanguageModel object.
        language_model = function(model_id = NULL) {
            model_id <- model_id %||% Sys.getenv("AIHUBMIX_MODEL", unset = "")
            if (is.null(model_id) || model_id == "") {
                model_id <- "claude-3-5-sonnet-20241022"
            }
            AiHubMixLanguageModel$new(model_id, private$config)
        },

        #' @description Create an image model.
        #' @param model_id The model ID (e.g., "gpt-image-1").
        #' @return An OpenAIImageModel object.
        image_model = function(model_id = NULL) {
            model_id <- model_id %||% Sys.getenv("AIHUBMIX_IMAGE_MODEL", unset = "")
            if (is.null(model_id) || model_id == "") {
                model_id <- "gpt-image-1"
            }
            OpenAIImageModel$new(model_id, private$config)
        }
    )
)

#' @title Create AiHubMix Provider
#' @description
#' Factory function to create an AiHubMix provider.
#'
#' AiHubMix provides a unified API for various models including Claude, OpenAI, Gemini, etc.
#'
#' @param api_key AiHubMix API key. Defaults to AIHUBMIX_API_KEY env var.
#' @param base_url Base URL for API calls. Defaults to https://aihubmix.com/v1.
#' @param headers Optional additional headers.
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
#' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
#' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
#' @return An AiHubMixProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'     aihubmix <- create_aihubmix()
#'     model <- aihubmix$language_model("claude-sonnet-3-5")
#'     result <- generate_text(model, "Explain quantum computing in one sentence.")
#' }
#' }
create_aihubmix <- function(api_key = NULL,
                            base_url = NULL,
                            headers = NULL,
                            timeout_seconds = NULL,
                            total_timeout_seconds = NULL,
                            first_byte_timeout_seconds = NULL,
                            connect_timeout_seconds = NULL,
                            idle_timeout_seconds = NULL) {
    AiHubMixProvider$new(
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

#' @title Create AiHubMix Provider (Anthropic API Format)
#' @description
#' Factory function to create an AiHubMix provider using the Anthropic-compatible API.
#' This allows you to use AiHubMix Claude models with the native Anthropic API format,
#' unlocking advanced features like Prompt Caching.
#'
#' @details
#' AiHubMix provides an Anthropic-compatible endpoint at `https://aihubmix.com/v1`.
#' This convenience function wraps `create_anthropic()` with AiHubMix-specific defaults.
#'
#' @param api_key AiHubMix API key. Defaults to AIHUBMIX_API_KEY env var.
#' @param extended_caching Logical. If TRUE, enables the 1-hour beta cache for Claude.
#' @param headers Optional additional headers.
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
#' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
#' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
#' @return An AnthropicProvider object configured for AiHubMix.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'     # Use AiHubMix via Anthropic API format (unlocks caching)
#'     aihubmix_claude <- create_aihubmix_anthropic()
#'     model <- aihubmix_claude$language_model("claude-3-5-sonnet-20241022")
#'     result <- generate_text(model, "Hello Claude!")
#' }
#' }
create_aihubmix_anthropic <- function(api_key = NULL,
                                      extended_caching = FALSE,
                                      headers = NULL,
                                      timeout_seconds = NULL,
                                      total_timeout_seconds = NULL,
                                      first_byte_timeout_seconds = NULL,
                                      connect_timeout_seconds = NULL,
                                      idle_timeout_seconds = NULL) {
    h <- headers %||% list()
    if (isTRUE(extended_caching)) {
        h$`anthropic-beta` <- "extended-cache-ttl-2025-04-11"
    }

    provider <- create_anthropic(
        api_key = api_key %||% Sys.getenv("AIHUBMIX_API_KEY"),
        base_url = "https://aihubmix.com/v1",
        name = "aihubmix",
        headers = h,
        timeout_seconds = timeout_seconds,
        total_timeout_seconds = total_timeout_seconds,
        first_byte_timeout_seconds = first_byte_timeout_seconds,
        connect_timeout_seconds = connect_timeout_seconds,
        idle_timeout_seconds = idle_timeout_seconds
    )
    # Enable caching automatically if using this native wrapper
    # since it's the primary reason to use it.
    provider$enable_caching(TRUE)
    provider
}

#' @title Create AiHubMix Provider (Gemini API Format)
#' @description
#' Factory function to create an AiHubMix provider using the Gemini-compatible API.
#' This allows you to use Gemini models with the native Gemini API structure.
#'
#' @details
#' AiHubMix provides a Gemini-compatible endpoint at `https://aihubmix.com/gemini/v1beta/models`.
#' This convenience function wraps `create_gemini()` with AiHubMix-specific defaults.
#'
#' @param api_key AiHubMix API key. Defaults to AIHUBMIX_API_KEY env var.
#' @param headers Optional additional headers.
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
#' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
#' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
#' @return A GeminiProvider object configured for AiHubMix.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'     # Use AiHubMix via Gemini API format
#'     aihubmix_gemini <- create_aihubmix_gemini()
#'     model <- aihubmix_gemini$language_model("gemini-2.5-flash")
#'     result <- generate_text(model, "Hello Gemini!")
#' }
#' }
create_aihubmix_gemini <- function(api_key = NULL,
                                   headers = NULL,
                                   timeout_seconds = NULL,
                                   total_timeout_seconds = NULL,
                                   first_byte_timeout_seconds = NULL,
                                   connect_timeout_seconds = NULL,
                                   idle_timeout_seconds = NULL) {
    create_gemini(
        api_key = api_key %||% Sys.getenv("AIHUBMIX_API_KEY"),
        base_url = "https://aihubmix.com/gemini/v1beta/models",
        name = "aihubmix",
        headers = headers,
        timeout_seconds = timeout_seconds,
        total_timeout_seconds = total_timeout_seconds,
        first_byte_timeout_seconds = first_byte_timeout_seconds,
        connect_timeout_seconds = connect_timeout_seconds,
        idle_timeout_seconds = idle_timeout_seconds
    )
}
