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

#' @title Stepfun Image Model Class
#' @description
#' Image model implementation for Stepfun image generation and editing APIs.
#' @keywords internal
StepfunImageModel <- R6::R6Class(
    "StepfunImageModel",
    inherit = ImageModelV1,
    private = list(
        config = NULL,
        get_headers = function(include_content_type = TRUE) {
            h <- list(
                Authorization = paste("Bearer", private$config$api_key)
            )
            if (include_content_type) {
                h$`Content-Type` <- "application/json"
            }
            if (!is.null(private$config$headers)) {
                h <- c(h, private$config$headers)
            }
            h
        },
        parse_image_response = function(response, output_dir = tempdir(), prefix = "stepfun_image") {
            images <- list()

            if (!is.null(response$data) && length(response$data) > 0) {
                for (item in response$data) {
                    artifact <- list(
                        revised_prompt = item$revised_prompt %||% NULL
                    )

                    if (!is.null(item$b64_json)) {
                        artifact$bytes <- base64enc::base64decode(item$b64_json)
                        artifact$media_type <- "image/png"
                    } else if (!is.null(item$image)) {
                        artifact$bytes <- base64enc::base64decode(item$image)
                        artifact$media_type <- "image/png"
                    } else if (!is.null(item$url)) {
                        artifact$uri <- item$url
                    }

                    images <- c(images, list(artifact))
                }
            }

            finalize_image_artifacts(images, output_dir = output_dir, prefix = prefix)
        }
    ),
    public = list(
        #' @description Initialize the Stepfun image model.
        #' @param model_id The model ID.
        #' @param config Configuration list.
        initialize = function(model_id, config) {
            super$initialize(
                provider = config$provider_name %||% "stepfun",
                model_id = model_id,
                capabilities = list(
                    image_output = TRUE,
                    image_edit = TRUE
                )
            )
            private$config <- config
        },

        #' @description Generate images.
        #' @param params A list of call options.
        #' @return A GenerateImageResult object.
        do_generate_image = function(params) {
            if (is.null(params$prompt) || !nzchar(params$prompt)) {
                rlang::abort("`prompt` must be a non-empty string.")
            }

            body <- list(
                model = self$model_id,
                prompt = params$prompt,
                response_format = params$response_format %||% "b64_json"
            )
            if (!is.null(params$n)) body$n <- params$n
            if (!is.null(params$size)) body$size <- params$size
            if (!is.null(params$seed)) body$seed <- params$seed

            handled <- c("prompt", "output_dir", "response_format", "n", "size", "seed")
            extra <- params[setdiff(names(params), handled)]
            if (length(extra) > 0) {
                body <- utils::modifyList(body, extra)
            }
            body <- body[!sapply(body, is.null)]

            response <- post_to_api(
                paste0(private$config$base_url, "/images/generations"),
                private$get_headers(),
                body
            )

            GenerateImageResult$new(
                images = private$parse_image_response(
                    response,
                    output_dir = params$output_dir %||% tempdir(),
                    prefix = "stepfun_image"
                ),
                raw_response = response
            )
        },

        #' @description Edit images.
        #' @param params A list of call options.
        #' @return A GenerateImageResult object.
        do_edit_image = function(params) {
            if (is.null(params$image)) {
                rlang::abort("`image` must be supplied for Stepfun image editing.")
            }
            if (!identical(self$model_id, "step-1x-edit")) {
                rlang::abort("Stepfun image editing currently requires the `step-1x-edit` model.")
            }
            if (!is.null(params$mask)) {
                rlang::abort("Stepfun image editing via aisdk does not support `mask` yet.")
            }

            image_path <- materialize_image_upload(
                params$image,
                output_dir = params$output_dir %||% tempdir(),
                prefix = "stepfun_image"
            )

            body <- list(
                model = self$model_id,
                image = curl::form_file(image_path),
                prompt = params$prompt %||% "Edit this image.",
                response_format = params$response_format %||% "b64_json"
            )
            if (!is.null(params$n)) body$n <- as.character(params$n)
            if (!is.null(params$size)) body$size <- params$size

            handled <- c("image", "prompt", "output_dir", "response_format", "n", "size")
            extra <- params[setdiff(names(params), handled)]
            if (length(extra) > 0) {
                body <- c(body, extra)
            }
            body <- body[!sapply(body, is.null)]

            response <- post_multipart_to_api(
                paste0(private$config$base_url, "/images/edits"),
                private$get_headers(include_content_type = FALSE),
                body
            )

            GenerateImageResult$new(
                images = private$parse_image_response(
                    response,
                    output_dir = params$output_dir %||% tempdir(),
                    prefix = "stepfun_edit"
                ),
                raw_response = response
            )
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
        },

        #' @description Create an image model.
        #' @param model_id The model ID (e.g., "step-1x-medium", "step-1x-edit").
        #' @return A StepfunImageModel object.
        image_model = function(model_id = NULL) {
            model_id <- model_id %||% Sys.getenv("STEPFUN_IMAGE_MODEL", unset = "")
            if (is.null(model_id) || model_id == "") {
                model_id <- "step-1x-medium"
            }
            StepfunImageModel$new(model_id, private$config)
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
