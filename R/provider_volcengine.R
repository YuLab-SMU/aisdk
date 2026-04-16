#' @name provider_volcengine
#' @title Volcengine Provider
#' @description
#' Implementation for Volcengine Ark hosted models.
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

#' @title Volcengine Image Model Class
#' @description
#' Image model implementation for Volcengine Ark image generation models such as
#' Doubao Seedream. Volcengine exposes these models through an OpenAI-compatible
#' image generation endpoint.
#' @keywords internal
VolcengineImageModel <- R6::R6Class(
    "VolcengineImageModel",
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
        build_body = function(params) {
            body <- list(
                model = self$model_id,
                prompt = params$prompt %||% "Edit this image.",
                response_format = params$response_format %||% "b64_json"
            )

            if (!is.null(params$image)) {
                images <- params$image
                if (!is.list(images)) {
                    images <- as.list(images)
                }
                normalized <- lapply(images, normalize_image_input_for_json)
                body$image <- if (length(normalized) == 1) normalized[[1]] else normalized
            }

            if (!is.null(params$n)) body$n <- params$n
            if (!is.null(params$size)) body$size <- params$size
            if (!is.null(params$guidance_scale)) body$guidance_scale <- params$guidance_scale
            if (!is.null(params$seed)) body$seed <- params$seed
            if (!is.null(params$watermark)) body$watermark <- params$watermark

            handled <- c("prompt", "image", "output_dir", "response_format", "n", "size", "guidance_scale", "seed", "watermark")
            extra <- params[setdiff(names(params), handled)]
            if (length(extra) > 0) {
                body <- utils::modifyList(body, extra)
            }

            body[!sapply(body, is.null)]
        },
        parse_image_response = function(response, output_dir = tempdir(), prefix = "volcengine_image") {
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
        #' @description Initialize the Volcengine image model.
        #' @param model_id The model ID (e.g., "doubao-seedream-5-0").
        #' @param config Configuration list.
        initialize = function(model_id, config) {
            super$initialize(
                provider = config$provider_name %||% "volcengine",
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

            url <- paste0(private$config$base_url, "/images/generations")
            response <- post_to_api(url, private$get_headers(), private$build_body(params))

            GenerateImageResult$new(
                images = private$parse_image_response(
                    response,
                    output_dir = params$output_dir %||% tempdir(),
                    prefix = "volcengine_image"
                ),
                usage = response$usage %||% NULL,
                raw_response = response
            )
        },

        #' @description Edit images by providing one or more reference images.
        #' @param params A list of call options.
        #' @return A GenerateImageResult object.
        do_edit_image = function(params) {
            if (is.null(params$image)) {
                rlang::abort("`image` must be supplied for Volcengine image editing.")
            }
            if (is.null(params$prompt) || !nzchar(params$prompt)) {
                params$prompt <- "Edit this image."
            }
            if (!is.null(params$mask)) {
                rlang::abort("Volcengine image editing via aisdk does not support `mask` yet.")
            }

            self$do_generate_image(params)
        }
    )
)

#' @title Volcengine Provider Class
#' @description
#' Provider class for the Volcengine Ark platform.
#' @export
VolcengineProvider <- R6::R6Class(
    "VolcengineProvider",
    inherit = OpenAIProvider,
    public = list(
        #' @description Initialize the Volcengine provider.
        #' @param api_key Volcengine API key. Defaults to ARK_API_KEY env var.
        #' @param base_url Base URL. Defaults to https://ark.cn-beijing.volces.com/api/v3.
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
            # Suppress parent class warning since we do our own check
            suppressWarnings(
                super$initialize(
                    api_key = api_key %||% Sys.getenv("ARK_API_KEY"),
                    base_url = base_url %||% Sys.getenv("ARK_BASE_URL", "https://ark.cn-beijing.volces.com/api/v3"),
                    headers = headers,
                    name = "volcengine",
                    timeout_seconds = timeout_seconds,
                    total_timeout_seconds = total_timeout_seconds,
                    first_byte_timeout_seconds = first_byte_timeout_seconds,
                    connect_timeout_seconds = connect_timeout_seconds,
                    idle_timeout_seconds = idle_timeout_seconds,
                    disable_stream_options = TRUE
                )
            )

            if (nchar(private$config$api_key) == 0) {
                rlang::warn("Volcengine API key not set. Set ARK_API_KEY env var or pass api_key parameter.")
            }
        },

        #' @description Create a language model.
        #' @param model_id The model ID (e.g., "doubao-1-5-pro-256k-250115" or "gpt-4o").
        #' @return A VolcengineLanguageModel object.
        language_model = function(model_id = NULL) {
            model_id <- model_id %||% Sys.getenv("ARK_MODEL")
            if (is.null(model_id) || model_id == "") {
                rlang::abort("Model ID not provided and ARK_MODEL environment variable not set.")
            }

            # Mapping for common model IDs to Volcengine/Ark equivalents
            mapping <- list(
                "gpt-4o" = "doubao-1-5-pro-256k-250115",
                "gpt-4o-mini" = "doubao-1-5-lite-128k-250115",
                "deepseek-chat" = "deepseek-v3",
                "deepseek-reasoner" = "deepseek-r1"
            )

            if (model_id %in% names(mapping)) {
                model_id <- mapping[[model_id]]
            }

            VolcengineLanguageModel$new(model_id, private$config)
        },

        #' @description Create an image model.
        #' @param model_id The model ID (e.g., "doubao-seedream-5-0").
        #' @return A VolcengineImageModel object.
        image_model = function(model_id = Sys.getenv("ARK_IMAGE_MODEL", "doubao-seedream-5-0")) {
            VolcengineImageModel$new(model_id, private$config)
        }
    )
)

#' @title Create Volcengine/Ark Provider
#' @description
#' Factory function to create a Volcengine provider using the Ark API.
#'
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
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
#' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
#' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
#' @return A VolcengineProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'     volcengine <- create_volcengine()
#'
#'     # Chat API (standard models)
#'     model <- volcengine$language_model("doubao-1-5-pro-256k-250115")
#'     result <- generate_text(model, "Hello")
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
                              headers = NULL,
                              timeout_seconds = NULL,
                              total_timeout_seconds = NULL,
                              first_byte_timeout_seconds = NULL,
                              connect_timeout_seconds = NULL,
                              idle_timeout_seconds = NULL) {
    VolcengineProvider$new(
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
