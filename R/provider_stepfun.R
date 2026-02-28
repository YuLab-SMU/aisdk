#' @name provider_stepfun
#' @title Stepfun Provider
#' @description
#' Implementation for Stepfun (阶跃星辰) models.
#' Stepfun API is OpenAI-compatible.
#' @keywords internal
NULL

#' @title Stepfun Language Model Class
#' @description
#' Language model implementation for Stepfun's chat completions API.
#' Inherits from OpenAILanguageModel as Stepfun provides an OpenAI-compatible API.
#' @keywords internal
StepfunLanguageModel <- R6::R6Class(
    "StepfunLanguageModel",
    inherit = OpenAILanguageModel,
    public = list(
        # Currently Stepfun uses the standard OpenAI response format, so we can inherit directly.
        # We override just to provide a dedicated Class for Stepfun models.
    )
)

#' @title Stepfun Provider Class
#' @description
#' Provider class for Stepfun.
#' @export
StepfunProvider <- R6::R6Class(
    "StepfunProvider",
    inherit = OpenAIProvider,
    public = list(
        #' @description Initialize the Stepfun provider.
        #' @param api_key Stepfun API key. Defaults to STEPFUN_API_KEY env var.
        #' @param base_url Base URL. Defaults to https://api.stepfun.com/v1.
        #' @param headers Optional additional headers.
        initialize = function(api_key = NULL,
                              base_url = NULL,
                              headers = NULL) {
            suppressWarnings(
                super$initialize(
                    api_key = api_key %||% Sys.getenv("STEPFUN_API_KEY"),
                    base_url = base_url %||% Sys.getenv("STEPFUN_BASE_URL", "https://api.stepfun.com/v1"),
                    headers = headers,
                    name = "stepfun"
                )
            )

            if (nchar(private$config$api_key) == 0) {
                rlang::warn("Stepfun API key not set. Set STEPFUN_API_KEY env var or pass api_key parameter.")
            }
        },

        #' @description Create a language model.
        #' @param model_id The model ID (e.g., "step-1-8k").
        #' @return A StepfunLanguageModel object.
        language_model = function(model_id = NULL) {
            model_id <- model_id %||% Sys.getenv("STEPFUN_MODEL", unset = "")
            if (is.null(model_id) || model_id == "") {
                model_id <- "step-1-8k"
            }
            StepfunLanguageModel$new(model_id, private$config)
        }
    )
)

#' @title Create Stepfun Provider
#' @description
#' Factory function to create a Stepfun provider.
#'
#' @section Supported Models:
#' Stepfun provides various models such as:
#' \itemize{
#'   \item \strong{Standard}: "step-1-8k", "step-1-32k", "step-1-128k", "step-1-256k", "step-2-16k"
#' }
#'
#' @param api_key Stepfun API key. Defaults to STEPFUN_API_KEY env var.
#' @param base_url Base URL for API calls. Defaults to https://api.stepfun.com/v1.
#' @param headers Optional additional headers.
#' @return A StepfunProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#' stepfun <- create_stepfun()
#' model <- stepfun$language_model("step-1-8k")
#' result <- generate_text(model, "Explain quantum computing in one sentence.")
#' }
#' }
create_stepfun <- function(api_key = NULL,
                           base_url = NULL,
                           headers = NULL) {
    StepfunProvider$new(
        api_key = api_key,
        base_url = base_url,
        headers = headers
    )
}
