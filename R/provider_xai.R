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
        #' @description Parse the API response into a GenerateResult.
        #' Overrides parent to extract xAI-specific reasoning_content.
        #' @param response The parsed API response.
        #' @return A GenerateResult object.
        parse_response = function(response) {
            # Use parent's parsing for standard fields
            result <- super$parse_response(response)

            # Extract reasoning content (Grok reasoning models)
            if (!is.null(response$choices[[1]]$message$reasoning_content)) {
                result$reasoning <- response$choices[[1]]$message$reasoning_content
            }

            result
        }
    )
)

#' @title xAI Image Model Class
#' @description
#' Image model implementation for xAI image generation and editing APIs.
#' @keywords internal
XAIImageModel <- R6::R6Class(
    "XAIImageModel",
    inherit = ImageModelV1,
    private = list(
        config = NULL,
        get_headers = function() {
            h <- list(
                `Content-Type` = "application/json",
                Authorization = paste("Bearer", private$config$api_key)
            )
            if (!is.null(private$config$headers)) {
                h <- c(h, private$config$headers)
            }
            h
        },
        parse_image_response = function(response, output_dir = tempdir(), prefix = "xai_image") {
            images <- list()

            if (!is.null(response$data) && length(response$data) > 0) {
                for (item in response$data) {
                    artifact <- list(
                        revised_prompt = item$revised_prompt %||% NULL
                    )

                    if (!is.null(item$b64_json)) {
                        artifact$bytes <- base64enc::base64decode(item$b64_json)
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
        #' @description Initialize the xAI image model.
        #' @param model_id The model ID.
        #' @param config Configuration list.
        initialize = function(model_id, config) {
            super$initialize(
                provider = config$provider_name %||% "xai",
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
            if (!is.null(params$quality)) body$quality <- params$quality
            if (!is.null(params$response_format)) body$response_format <- params$response_format

            handled <- c("prompt", "output_dir", "response_format", "n", "size", "quality")
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
                    prefix = "xai_image"
                ),
                raw_response = response
            )
        },

        #' @description Edit images.
        #' @param params A list of call options.
        #' @return A GenerateImageResult object.
        do_edit_image = function(params) {
            if (is.null(params$image)) {
                rlang::abort("`image` must be supplied for xAI image editing.")
            }

            images <- params$image
            if (!is.list(images)) {
                images <- as.list(images)
            }
            normalized <- lapply(images, function(img) {
                list(
                    type = "image_url",
                    url = normalize_image_input_to_url_like(img)
                )
            })

            body <- list(
                model = self$model_id,
                prompt = params$prompt %||% "Edit this image.",
                response_format = params$response_format %||% "b64_json"
            )
            if (length(normalized) == 1) {
                body$image <- normalized[[1]]
            } else {
                body$images <- normalized
            }
            if (!is.null(params$n)) body$n <- params$n
            if (!is.null(params$size)) body$size <- params$size

            handled <- c("image", "prompt", "output_dir", "response_format", "n", "size")
            extra <- params[setdiff(names(params), handled)]
            if (length(extra) > 0) {
                body <- utils::modifyList(body, extra)
            }
            body <- body[!sapply(body, is.null)]

            response <- post_to_api(
                paste0(private$config$base_url, "/images/edits"),
                private$get_headers(),
                body
            )

            GenerateImageResult$new(
                images = private$parse_image_response(
                    response,
                    output_dir = params$output_dir %||% tempdir(),
                    prefix = "xai_edit"
                ),
                raw_response = response
            )
        }
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
                    api_key = api_key %||% Sys.getenv("XAI_API_KEY"),
                    base_url = base_url %||% Sys.getenv("XAI_BASE_URL", "https://api.x.ai/v1"),
                    headers = headers,
                    name = "xai",
                    timeout_seconds = timeout_seconds,
                    total_timeout_seconds = total_timeout_seconds,
                    first_byte_timeout_seconds = first_byte_timeout_seconds,
                    connect_timeout_seconds = connect_timeout_seconds,
                    idle_timeout_seconds = idle_timeout_seconds
                )
            )

            if (nchar(private$config$api_key) == 0) {
                rlang::warn("xAI API key not set. Set XAI_API_KEY env var or pass api_key parameter.")
            }
        },

        #' @description Create a language model.
        #' @param model_id The model ID (e.g., "grok-beta", "grok-2-1212").
        #' @return A XAILanguageModel object.
        language_model = function(model_id = NULL) {
            model_id <- model_id %||% Sys.getenv("XAI_MODEL", unset = "")
            if (is.null(model_id) || model_id == "") {
                model_id <- "grok-beta"
            }
            XAILanguageModel$new(model_id, private$config)
        },

        #' @description Create an image model.
        #' @param model_id The model ID (e.g., "grok-2-image").
        #' @return A XAIImageModel object.
        image_model = function(model_id = NULL) {
            model_id <- model_id %||% Sys.getenv("XAI_IMAGE_MODEL", unset = "")
            if (is.null(model_id) || model_id == "") {
                model_id <- "grok-2-image"
            }
            XAIImageModel$new(model_id, private$config)
        }
    )
)

#' @title Create xAI Provider
#' @description
#' Factory function to create an xAI provider.
#'
#' @eval generate_model_docs("xai")
#'
#' @param api_key xAI API key. Defaults to XAI_API_KEY env var.
#' @param base_url Base URL for API calls. Defaults to https://api.x.ai/v1.
#' @param headers Optional additional headers.
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
#' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
#' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
#' @return A XAIProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'     xai <- create_xai()
#'     model <- xai$language_model("grok-beta")
#'     result <- generate_text(model, "Explain quantum computing in one sentence.")
#' }
#' }
create_xai <- function(api_key = NULL,
                       base_url = NULL,
                       headers = NULL,
                       timeout_seconds = NULL,
                       total_timeout_seconds = NULL,
                       first_byte_timeout_seconds = NULL,
                       connect_timeout_seconds = NULL,
                       idle_timeout_seconds = NULL) {
    XAIProvider$new(
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
