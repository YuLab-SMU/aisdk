#' @name provider_volcengine
#' @title Volcengine (火山引擎) Provider
#' @description
#' Implementation for Volcengine Ark (火山方舟) hosted models.
#' Volcengine API is OpenAI-compatible with support for reasoning models (e.g., Doubao, DeepSeek).
#' @keywords internal
NULL

#' @title Volcengine Language Model Class
#' @description
#' Language model implementation for Volcengine's chat completions API.
#' Inherits from OpenAI model but adds support for Volcengine-specific features
#' like reasoning content extraction from models that support `reasoning_content`.
#' @keywords internal
VolcengineLanguageModel <- R6::R6Class(
    "VolcengineLanguageModel",
    inherit = OpenAILanguageModel,
    public = list(
        #' @description Parse the API response into a GenerateResult.
        #' Overrides parent to extract Volcengine-specific reasoning_content.
        #' @param response The parsed API response.
        #' @return A GenerateResult object.
        parse_response = function(response) {
            # Use parent's parsing for standard fields
            result <- super$parse_response(response)

            # Extract reasoning content (Doubao thinking models, DeepSeek-R1 via Volcengine, etc.)
            choice <- response$choices[[1]]
            result$reasoning <- choice$message$reasoning_content

            result
        }
    )
)

#' @title Volcengine Provider Class
#' @description
#' Provider class for Volcengine (火山引擎) Ark platform.
#' @export
VolcengineProvider <- R6::R6Class(
    "VolcengineProvider",
    inherit = OpenAIProvider,
    public = list(
        #' @description Initialize the Volcengine provider.
        #' @param api_key Volcengine API key. Defaults to ARK_API_KEY env var.
        #' @param base_url Base URL. Defaults to https://ark.cn-beijing.volces.com/api/v3.
        #' @param headers Optional additional headers.
        initialize = function(api_key = NULL,
                              base_url = NULL,
                              headers = NULL) {
            # Suppress parent class warning since we do our own check
            suppressWarnings(
                super$initialize(
                    api_key = api_key %||% Sys.getenv("ARK_API_KEY"),
                    base_url = base_url %||% Sys.getenv("ARK_BASE_URL", "https://ark.cn-beijing.volces.com/api/v3"),
                    headers = headers,
                    name = "volcengine",
                    disable_stream_options = TRUE
                )
            )

            if (nchar(private$config$api_key) == 0) {
                rlang::warn("Volcengine API key not set. Set ARK_API_KEY env var or pass api_key parameter.")
            }
        },

        #' @description Create a language model.
        #' @param model_id The model ID (e.g., "doubao-1-5-pro-256k-250115").
        #' @return A VolcengineLanguageModel object.
        language_model = function(model_id = NULL) {
            model_id <- model_id %||% Sys.getenv("ARK_MODEL")
            if (is.null(model_id) || model_id == "") {
                rlang::abort("Model ID not provided and ARK_MODEL environment variable not set.")
            }
            VolcengineLanguageModel$new(model_id, private$config)
        }
    )
)

#' @title Create Volcengine/Ark Provider
#' @description
#' Factory function to create a Volcengine (火山引擎) provider using the Ark API.
#'
#' @section Supported Models:
#' @eval generate_model_docs("volcengine")
#'
#' @section API Formats:
#' Volcengine supports both Chat Completions API and Responses API:
#' \itemize{
#'   \item \code{language_model()}: Uses Chat Completions API (standard)
#'   \item \code{responses_model()}: Uses Responses API (for reasoning models)
#'   \item \code{smart_model()}: Auto-selects based on model ID
#' }
#'
#' @section Token Limit Parameters for Volcengine Responses API:
#' Volcengine's Responses API has two mutually exclusive token limit parameters:
#'
#' \itemize{
#'   \item \code{max_output_tokens}: Total limit including reasoning + answer (default mapping)
#'   \item \code{max_tokens} (API level): Answer-only limit, excluding reasoning
#' }
#'
#' The SDK's unified \code{max_tokens} parameter maps to \code{max_output_tokens} by default,
#' which is the \strong{safe choice} to prevent runaway reasoning costs.
#'
#' For advanced users who want answer-only limits:
#' \itemize{
#'   \item Use \code{max_answer_tokens} parameter to explicitly set answer-only limit
#'   \item Use \code{max_output_tokens} parameter to explicitly set total limit
#' }
#'
#' @param api_key Volcengine API key. Defaults to ARK_API_KEY env var.
#' @param base_url Base URL for API calls. Defaults to https://ark.cn-beijing.volces.com/api/v3.
#' @param headers Optional additional headers.
#' @return A VolcengineProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'     volcengine <- create_volcengine()
#'
#'     # Chat API (standard models)
#'     model <- volcengine$language_model("doubao-1-5-pro-256k-250115")
#'     result <- generate_text(model, "你好")
#'
#'     # Responses API (reasoning models like DeepSeek)
#'     model <- volcengine$responses_model("deepseek-r1-250120")
#'
#'     # Default: max_tokens limits total output (reasoning + answer)
#'     result <- model$generate(messages = msgs, max_tokens = 2000)
#'
#'     # Advanced: limit only the answer part (reasoning can be longer)
#'     result <- model$generate(messages = msgs, max_answer_tokens = 500)
#'
#'     # Smart model selection (auto-detects best API)
#'     model <- volcengine$smart_model("deepseek-r1-250120")
#' }
#' }
create_volcengine <- function(api_key = NULL,
                              base_url = NULL,
                              headers = NULL) {
    VolcengineProvider$new(
        api_key = api_key,
        base_url = base_url,
        headers = headers
    )
}
