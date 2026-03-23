#' @name provider_bailian
#' @title Alibaba Cloud Bailian Provider
#' @description
#' Implementation for Alibaba Cloud Bailian (DashScope) hosted models.
#' DashScope API is OpenAI-compatible with support for Qwen series models
#' including reasoning models (QwQ, Qwen3 etc.).
#' @keywords internal
NULL

#' @title Bailian Language Model Class
#' @description
#' Language model implementation for Alibaba Cloud DashScope's chat completions API.
#' Inherits from OpenAI model but adds support for DashScope-specific features
#' like reasoning content extraction from Qwen reasoning models.
#' @keywords internal
BailianLanguageModel <- R6::R6Class(
    "BailianLanguageModel",
    inherit = OpenAILanguageModel,
    public = list(
        #' @description Parse the API response into a GenerateResult.
        #' Overrides parent to extract DashScope-specific reasoning_content.
        #' @param response The parsed API response.
        #' @return A GenerateResult object.
        parse_response = function(response) {
            # Use parent's parsing for standard fields
            result <- super$parse_response(response)

            # Extract reasoning content (QwQ, Qwen reasoning models etc.)
            choice <- response$choices[[1]]
            result$reasoning <- choice$message$reasoning_content

            result
        }
    )
)

#' @title Bailian Provider Class
#' @description
#' Provider class for Alibaba Cloud Bailian / DashScope platform.
#' @export
BailianProvider <- R6::R6Class(
    "BailianProvider",
    inherit = OpenAIProvider,
    public = list(
        #' @description Initialize the Bailian provider.
        #' @param api_key DashScope API key. Defaults to DASHSCOPE_API_KEY env var.
        #' @param base_url Base URL. Defaults to https://dashscope.aliyuncs.com/compatible-mode/v1.
        #' @param headers Optional additional headers.
        initialize = function(api_key = NULL,
                              base_url = NULL,
                              headers = NULL) {
            # Suppress parent class warning since we do our own check
            suppressWarnings(
                super$initialize(
                    api_key = api_key %||% Sys.getenv("DASHSCOPE_API_KEY"),
                    base_url = base_url %||% Sys.getenv("DASHSCOPE_BASE_URL", "https://dashscope.aliyuncs.com/compatible-mode/v1"),
                    headers = headers,
                    name = "bailian"
                )
            )

            if (nchar(private$config$api_key) == 0) {
                rlang::warn("DashScope API key not set. Set DASHSCOPE_API_KEY env var or pass api_key parameter.")
            }
        },

        #' @description Create a language model.
        #' @param model_id The model ID (e.g., "qwen-plus", "qwen-turbo", "qwq-32b").
        #' @return A BailianLanguageModel object.
        language_model = function(model_id = NULL) {
            model_id <- model_id %||% Sys.getenv("DASHSCOPE_MODEL", "qwen-plus")
            BailianLanguageModel$new(model_id, private$config)
        }
    )
)

#' @title Create Alibaba Cloud Bailian Provider
#' @description
#' Factory function to create an Alibaba Cloud Bailian provider using the DashScope API.
#'
#' @section Supported Models:
#' DashScope platform hosts Qwen series and other models:
#' \itemize{
#'   \item \strong{qwen-plus}: Balanced performance model
#'   \item \strong{qwen-turbo}: Fast & cost-effective model
#'   \item \strong{qwen-max}: Most capable model
#'   \item \strong{qwq-32b}: Reasoning model with chain-of-thought
#'   \item \strong{qwen-vl-plus}: Vision-language model
#'   \item Other third-party models available on the platform
#' }
#'
#' @param api_key DashScope API key. Defaults to DASHSCOPE_API_KEY env var.
#' @param base_url Base URL for API calls. Defaults to https://dashscope.aliyuncs.com/compatible-mode/v1.
#' @param headers Optional additional headers.
#' @return A BailianProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#' bailian <- create_bailian()
#'
#' # Standard chat model
#' model <- bailian$language_model("qwen-plus")
#' result <- generate_text(model, "Hello")
#'
#' # Reasoning model (QwQ with chain-of-thought)
#' model <- bailian$language_model("qwq-32b")
#' result <- generate_text(model, "Solve: What is 15 * 23?")
#' print(result$reasoning) # Chain-of-thought reasoning
#'
#' # Default model (qwen-plus)
#' model <- bailian$language_model()
#' }
#' }
create_bailian <- function(api_key = NULL,
                           base_url = NULL,
                           headers = NULL) {
    BailianProvider$new(
        api_key = api_key,
        base_url = base_url,
        headers = headers
    )
}
