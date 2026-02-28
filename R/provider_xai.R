#' @name provider_xai
#' @title xAI Provider
#' @description
#' Implementation for xAI (Grok) models.
#' xAI API is OpenAI-compatible.
#' @keywords internal
NULL

#' @title xAI Language Model Class
#' @description
#' Language model implementation for xAI's chat completions API.
#' Inherits from OpenAILanguageModel as xAI provides An OpenAI-compatible API.
#' @keywords internal
XAILanguageModel <- R6::R6Class(
    "XAILanguageModel",
    inherit = OpenAILanguageModel,
    public = list(
        # Currently XAI uses the standard OpenAI response format, so we can inherit directly.
        # We override just to provide a dedicated Class for xAI models.
    )
)

#' @title xAI Provider Class
#' @description
#' Provider class for xAI.
#' @export
XAIProvider <- R6::R6Class(
    "XAIProvider",
    inherit = OpenAIProvider,
    public = list(
        #' @description Initialize the xAI provider.
        #' @param api_key xAI API key. Defaults to XAI_API_KEY env var.
        #' @param base_url Base URL. Defaults to https://api.x.ai/v1.
        #' @param headers Optional additional headers.
        initialize = function(api_key = NULL,
                              base_url = NULL,
                              headers = NULL) {
            suppressWarnings(
                super$initialize(
                    api_key = api_key %||% Sys.getenv("XAI_API_KEY"),
                    base_url = base_url %||% Sys.getenv("XAI_BASE_URL", "https://api.x.ai/v1"),
                    headers = headers,
                    name = "xai"
                )
            )

            if (nchar(private$config$api_key) == 0) {
                rlang::warn("xAI API key not set. Set XAI_API_KEY env var or pass api_key parameter.")
            }
        },

        #' @description Create a language model.
        #' @param model_id The model ID (e.g., "grok-4-1-fast-reasoning").
        #' @return A XAILanguageModel object.
        language_model = function(model_id = NULL) {
            model_id <- model_id %||% Sys.getenv("XAI_MODEL", unset = "")
            if (is.null(model_id) || model_id == "") {
                model_id <- "grok-4o"
            }
            XAILanguageModel$new(model_id, private$config)
        }
    )
)

#' @title Create xAI Provider
#' @description
#' Factory function to create an xAI provider.
#'
#' @section Supported Models:
#' xAI provides Grok models:
#' \itemize{
#'   \item \strong{Grok}: "grok-beta", "grok-2-1212", "grok-4-1-fast-reasoning", etc.
#' }
#'
#' @param api_key xAI API key. Defaults to XAI_API_KEY env var.
#' @param base_url Base URL for API calls. Defaults to https://api.x.ai/v1.
#' @param headers Optional additional headers.
#' @return A XAIProvider object.
#' @export
#' @examples
#' \donttest{
#' xai <- create_xai()
#' model <- xai$language_model("grok-4-1-fast-reasoning")
#' result <- generate_text(model, "Explain quantum computing in one sentence.")
#' }
create_xai <- function(api_key = NULL,
                       base_url = NULL,
                       headers = NULL) {
    XAIProvider$new(
        api_key = api_key,
        base_url = base_url,
        headers = headers
    )
}
