#' @name provider_stepfun
#' @title Stepfun Provider
#' @description
#' Implementation for Stepfun models.
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
    private = list(
        # Override to fix response_format for Stepfun API limits
        process_response_format = function(params) {
            if (!is.null(params$response_format)) {
                # If it's a schema (has type / properties) or explicitly set to json_schema,
                # Stepfun doesn't support structured outputs (json_schema). It only supports json_object.
                # So we must downgrade it to list(type = "json_object") and inject the schema into the prompt.
                orig_format <- params$response_format

                # Convert the schema to character string for the prompt
                schema_json <- tryCatch(
                    if (is.character(orig_format)) orig_format else jsonlite::toJSON(orig_format, auto_unbox = TRUE),
                    error = function(e) "{}"
                )

                instruction <- paste(
                    "You must return your output strictly in valid JSON format.",
                    "The JSON must adhere to the following schema:\n",
                    schema_json
                )

                # Inject into the first system message, or add a new one
                msgs <- params$messages
                if (length(msgs) > 0 && msgs[[1]]$role == "system") {
                    msgs[[1]]$content <- paste(msgs[[1]]$content, "\n\n", instruction)
                } else {
                    msgs <- c(list(list(role = "system", content = instruction)), msgs)
                }
                params$messages <- msgs

                # Stepfun API only accepts type="text" or type="json_object", and sometimes neither
                # It evaluates purely based on prompt injection, so we strip it.
                params$response_format <- NULL
            }
            params
        }
    ),
    public = list(
        #' @description Build the payload for the Stepfun API.
        #' @param params A list of parameters for the API call.
        build_payload = function(params) {
            params <- private$process_response_format(params)
            super$build_payload(params)
        },
        #' @description Build the stream payload for the Stepfun API.
        #' @param params A list of parameters for the API call.
        build_stream_payload = function(params) {
            params <- private$process_response_format(params)
            super$build_stream_payload(params)
        }
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
        #' @param model_id The model ID (e.g., "step-3.5-flash").
        #' @return A StepfunLanguageModel object.
        language_model = function(model_id = NULL) {
            model_id <- model_id %||% Sys.getenv("STEPFUN_MODEL", unset = "")
            if (is.null(model_id) || model_id == "") {
                model_id <- "step-3.5-flash"
            }
            StepfunLanguageModel$new(model_id, private$config)
        }
    )
)

#' @title Create Stepfun Provider
#' @description
#' Factory function to create a Stepfun provider.
#'
#' @eval generate_model_docs("stepfun")
#'
#' @param api_key Stepfun API key. Defaults to STEPFUN_API_KEY env var.
#' @param base_url Base URL for API calls. Defaults to https://api.stepfun.com/v1.
#' @param headers Optional additional headers.
#' @return A StepfunProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'     stepfun <- create_stepfun()
#'     model <- stepfun$language_model("step-1-8k")
#'     result <- generate_text(model, "Explain quantum computing in one sentence.")
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
