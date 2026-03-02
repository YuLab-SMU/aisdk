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
        initialize = function(api_key = NULL,
                              base_url = NULL,
                              headers = NULL) {
            suppressWarnings(
                super$initialize(
                    api_key = api_key %||% Sys.getenv("AIHUBMIX_API_KEY"),
                    base_url = base_url %||% Sys.getenv("AIHUBMIX_BASE_URL", "https://aihubmix.com/v1"),
                    headers = headers,
                    name = "aihubmix"
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
                            headers = NULL) {
    AiHubMixProvider$new(
        api_key = api_key,
        base_url = base_url,
        headers = headers
    )
}
