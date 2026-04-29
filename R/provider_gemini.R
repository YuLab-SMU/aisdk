#' @name provider_gemini
#' @title Gemini Provider
#' @description
#' Implementation for Google Gemini models via REST API.
#' @keywords internal
NULL

#' @title Gemini Language Model Class
#' @description
#' Language model implementation for Gemini's generateContent API.
#' @keywords internal
GeminiLanguageModel <- R6::R6Class(
    "GeminiLanguageModel",
    inherit = LanguageModelV1,
    private = list(
        config = NULL,
        parse_candidate_parts = function(parts) {
            text_content <- ""
            tool_calls <- NULL
            images <- NULL

            for (part in parts) {
                if (!is.null(part$text)) {
                    text_content <- paste0(text_content, part$text)
                } else if (!is.null(part$functionCall)) {
                    if (is.null(tool_calls)) tool_calls <- list()
                    tool_calls <- c(tool_calls, list(list(
                        id = paste0("call_", part$functionCall$name, "_", sample(10000:99999, 1)),
                        name = part$functionCall$name,
                        arguments = part$functionCall$args
                    )))
                } else if (!is.null(part$inlineData)) {
                    if (is.null(images)) images <- list()
                    images <- c(images, list(list(
                        media_type = part$inlineData$mimeType %||% "application/octet-stream",
                        bytes = base64enc::base64decode(part$inlineData$data)
                    )))
                } else if (!is.null(part$inline_data)) {
                    if (is.null(images)) images <- list()
                    images <- c(images, list(list(
                        media_type = part$inline_data$mimeType %||% "application/octet-stream",
                        bytes = base64enc::base64decode(part$inline_data$data)
                    )))
                } else if (!is.null(part$fileData)) {
                    if (is.null(images)) images <- list()
                    images <- c(images, list(list(
                        media_type = part$fileData$mimeType %||% "application/octet-stream",
                        uri = part$fileData$fileUri
                    )))
                }
            }

            list(
                text = text_content,
                tool_calls = tool_calls,
                images = images
            )
        },
        get_headers = function() {
            h <- list(
                `Content-Type` = "application/json"
            )
            if (!is.null(private$config$headers)) {
                h <- c(h, private$config$headers)
            }
            h
        },

        format_content_parts = function(content) {
            if (is.null(content)) {
                return(list())
            }

            if (is.character(content)) {
                return(list(list(text = content)))
            }

            translate_message_content(content, target = "gemini")
        },

        # Format normalized messages to Gemini contents array
        format_messages = function(messages) {
            contents <- list()
            system_instruction <- NULL

            for (msg in messages) {
                if (msg$role == "system") {
                    system_instruction <- list(
                        parts = list(list(text = if (is.character(msg$content) || is.null(msg$content)) {
                            msg$content %||% ""
                        } else {
                            content_blocks_to_text(msg$content, arg_name = "system")
                        }))
                    )
                } else if (msg$role == "user") {
                    contents <- c(contents, list(list(
                        role = "user",
                        parts = private$format_content_parts(msg$content)
                    )))
                } else if (msg$role == "assistant") {
                    parts <- private$format_content_parts(msg$content)
                    if (!is.null(msg$tool_calls) && length(msg$tool_calls) > 0) {
                        for (tc in msg$tool_calls) {
                            parts <- c(parts, list(list(
                                functionCall = list(
                                    name = tc$name,
                                    args = if (is.character(tc$arguments)) parse_tool_arguments(tc$arguments, tool_name = tc$name) else tc$arguments
                                )
                            )))
                        }
                    }
                    contents <- c(contents, list(list(
                        role = "model",
                        parts = parts
                    )))
                } else if (msg$role == "tool") {
                    contents <- c(contents, list(list(
                        role = "function",
                        parts = list(list(
                            functionResponse = list(
                                name = msg$tool_name %||% "unknown_tool",
                                response = list(result = if (is.character(msg$content)) {
                                    # Try parse to prevent double encoding JSON strings if it's already a JSON string
                                    tryCatch(jsonlite::fromJSON(msg$content, simplifyVector = FALSE), error = function(e) msg$content)
                                } else {
                                    msg$content
                                })
                            )
                        ))
                    )))
                }
            }

            list(
                contents = contents,
                system_instruction = system_instruction
            )
        },

        # Map generation config parameters
        build_generation_config = function(params) {
            config <- list()
            if (!is.null(params$temperature)) config$temperature <- params$temperature
            if (!is.null(params$top_p)) config$topP <- params$top_p
            if (!is.null(params$top_k)) config$topK <- params$top_k
            if (!is.null(params$max_tokens)) config$maxOutputTokens <- params$max_tokens
            if (!is.null(params$stop_sequences)) config$stopSequences <- params$stop_sequences
            if (!is.null(params$response_modalities)) config$responseModalities <- params$response_modalities
            if (!is.null(params$image_config)) config$imageConfig <- params$image_config
            if (length(config) == 0) {
                return(NULL)
            }
            config
        }
    ),
    public = list(
        #' @description Initialize the Gemini language model.
        #' @param model_id The model ID (e.g., "gemini-1.5-pro").
        #' @param config Configuration list with api_key, base_url, headers, etc.
        initialize = function(model_id, config) {
            super$initialize(provider = config$provider_name %||% "gemini", model_id = model_id)
            private$config <- config
        },

        #' @description Get the configuration list.
        #' @return A list with provider configuration.
        get_config = function() {
            private$config
        },

        #' @description Build the request payload for generation
        #' @param params A list of call options.
        #' @param stream Whether to build for streaming
        #' @return A list with url, headers, and body.
        build_payload_internal = function(params, stream = FALSE) {
            endpoint <- if (stream) "streamGenerateContent?alt=sse&key=" else "generateContent?key="

            base <- private$config$base_url
            if (!grepl("/models$", base)) {
                base <- paste0(base, "/models")
            }
            url <- paste0(base, "/", self$model_id, ":", endpoint, private$config$api_key)
            headers <- private$get_headers()

            formatted <- private$format_messages(params$messages)

            body <- list(
                contents = formatted$contents
            )

            if (!is.null(formatted$system_instruction)) {
                body$systemInstruction <- formatted$system_instruction
            }

            gen_config <- private$build_generation_config(params)
            if (!is.null(gen_config)) {
                body$generationConfig <- gen_config
            }

            # Tools
            if (!is.null(params$tools) && length(params$tools) > 0) {
                function_declarations <- list()
                other_tools <- list()

                for (t in params$tools) {
                    if (inherits(t, "Tool")) {
                        # Get openai format as it's JSON Schema compliant
                        api_fmt <- t$to_api_format("openai")
                        function_declarations <- c(function_declarations, list(list(
                            name = api_fmt$`function`$name,
                            description = api_fmt$`function`$description,
                            parameters = api_fmt$`function`$parameters
                        )))
                    } else if (is.list(t) && !is.null(names(t))) {
                        # Pass through special tool objects like list(google_search = list())
                        other_tools <- c(other_tools, list(t))
                    } else {
                        function_declarations <- c(function_declarations, list(t))
                    }
                }

                body_tools <- list()
                if (length(function_declarations) > 0) {
                    body_tools <- c(body_tools, list(list(functionDeclarations = function_declarations)))
                }
                if (length(other_tools) > 0) {
                    body_tools <- c(body_tools, other_tools)
                }

                if (length(body_tools) > 0) {
                    body$tools <- body_tools
                }
            }

            # Remove NULLs
            body <- body[!sapply(body, is.null)]

            list(url = url, headers = headers, body = body)
        },

        #' @description Generate text (non-streaming).
        #' @param params A list of call options including messages, temperature, etc.
        #' @return A GenerateResult object.
        do_generate = function(params) {
            payload <- self$build_payload_internal(params, stream = FALSE)
            response <- post_to_api(
                payload$url,
                payload$headers,
                payload$body,
                timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
                total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
                first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
                connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
                idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
            )

            # Parse Gemini response format
            text_content <- ""
            tool_calls <- NULL
            images <- NULL
            finish_reason <- NULL
            usage <- NULL

            if (!is.null(response$candidates) && length(response$candidates) > 0) {
                candidate <- response$candidates[[1]]
                finish_reason <- candidate$finishReason

                if (!is.null(candidate$content) && !is.null(candidate$content$parts)) {
                    parsed <- private$parse_candidate_parts(candidate$content$parts)
                    text_content <- parsed$text
                    tool_calls <- parsed$tool_calls
                    images <- parsed$images
                }
            }

            if (!is.null(response$usageMetadata)) {
                usage <- list(
                    prompt_tokens = response$usageMetadata$promptTokenCount,
                    completion_tokens = response$usageMetadata$candidatesTokenCount,
                    total_tokens = response$usageMetadata$totalTokenCount
                )
            }

            result <- GenerateResult$new(
                text = text_content,
                usage = usage,
                finish_reason = finish_reason,
                raw_response = response,
                tool_calls = tool_calls
            )
            if (!is.null(images)) {
                result$images <- images
            }
            result
        },

        #' @description Generate text (streaming).
        #' @param params A list of call options.
        #' @param callback A function called for each chunk: callback(text, done).
        #' @return A GenerateResult object.
        do_stream = function(params, callback) {
            payload <- self$build_payload_internal(params, stream = TRUE)
            agg <- SSEAggregator$new(callback)

            # Gemini returns SSE events where `data` contains the JSON representation of GenerateContentResponse
            stream_from_api(
                payload$url,
                payload$headers,
                payload$body,
                callback = function(data, done) {
                    if (done) {
                        agg$on_done()
                        return()
                    }

                    # Each data chunk is a GenerateContentResponse
                    agg$on_raw_response(data)

                    if (!is.null(data$candidates) && length(data$candidates) > 0) {
                        candidate <- data$candidates[[1]]

                        if (!is.null(candidate$content) && !is.null(candidate$content$parts)) {
                            for (part in candidate$content$parts) {
                                if (!is.null(part$text) && nzchar(part$text)) {
                                    agg$on_text_delta(part$text)
                                } else if (!is.null(part$functionCall)) {
                                    # Gemini doesn't chunk function calls the same way, it just sends the whole call
                                    # Mock an OpenAI tool call format to reuse SSEAggregator's tool tracking
                                    tc_mock <- list(list(
                                        index = length(data$candidates) - 1,
                                        id = paste0("call_", part$functionCall$name, "_", sample(10000:99999, 1)),
                                        `function` = list(
                                            name = part$functionCall$name,
                                            arguments = jsonlite::toJSON(part$functionCall$args, auto_unbox = TRUE)
                                        )
                                    ))
                                    agg$on_tool_call_delta(tc_mock)
                                }
                            }
                        }

                        if (!is.null(candidate$finishReason)) {
                            agg$on_finish_reason(candidate$finishReason)
                        }
                    }

                    if (!is.null(data$usageMetadata)) {
                        agg$on_usage(list(
                            prompt_tokens = data$usageMetadata$promptTokenCount,
                            completion_tokens = data$usageMetadata$candidatesTokenCount,
                            total_tokens = data$usageMetadata$totalTokenCount
                        ))
                    }
                },
                timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
                total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
                first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
                connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
                idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
            )

            agg$build_result()
        },

        #' @description Format a tool execution result for Gemini's API.
        #' @param tool_call_id The ID of the tool call (unused in Gemini but present for interface compatibility).
        #' @param tool_name The name of the tool.
        #' @param result_content The result content from executing the tool.
        #' @return A list formatted as a message for Gemini API.
        format_tool_result = function(tool_call_id, tool_name, result_content) {
            list(
                role = "tool",
                tool_call_id = tool_call_id, # Retained for aisdk internal tracking, filtered in format_messages
                tool_name = tool_name,
                content = result_content
            )
        },

        #' @description Get the message format for Gemini.
        get_history_format = function() {
            "gemini"
        }
    )
)

#' @title Gemini Image Model Class
#' @description
#' Image model implementation for Gemini image generation models.
#' @keywords internal
GeminiImageModel <- R6::R6Class(
    "GeminiImageModel",
    inherit = ImageModelV1,
    private = list(
        config = NULL,
        get_headers = function() {
            h <- list(
                `Content-Type` = "application/json"
            )
            if (!is.null(private$config$headers)) {
                h <- c(h, private$config$headers)
            }
            h
        },
        build_generation_config = function(params) {
            config <- list(
                responseModalities = params$response_modalities %||% list("IMAGE")
            )
            if (!is.null(params$size)) {
                config$imageConfig <- utils::modifyList(
                    config$imageConfig %||% list(),
                    list(size = params$size)
                )
            }
            if (!is.null(params$image_config)) {
                config$imageConfig <- utils::modifyList(
                    config$imageConfig %||% list(),
                    params$image_config
                )
            }
            config
        },
        build_payload = function(params) {
            base <- private$config$base_url
            if (!grepl("/models$", base)) {
                base <- paste0(base, "/models")
            }

            url <- paste0(base, "/", self$model_id, ":generateContent?key=", private$config$api_key)
            headers <- private$get_headers()

            parts <- list()
            if (!is.null(params$prompt) && nzchar(params$prompt)) {
                parts <- c(parts, list(list(text = params$prompt)))
            }
            if (!is.null(params$image)) {
                images <- params$image
                if (!is.list(images)) {
                    images <- as.list(images)
                }
                for (img in images) {
                    parts <- c(parts, translate_blocks_gemini(list(input_image(img))))
                }
            }

            body <- list(
                contents = list(list(
                    role = "user",
                    parts = parts
                )),
                generationConfig = private$build_generation_config(params)
            )

            body <- body[!sapply(body, is.null)]
            list(url = url, headers = headers, body = body)
        }
    ),
    public = list(
        #' @description Initialize the Gemini image model.
        #' @param model_id The model ID.
        #' @param config Configuration list with api_key, base_url, headers, etc.
        initialize = function(model_id, config) {
            super$initialize(
                provider = config$provider_name %||% "gemini",
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

            payload <- private$build_payload(params)
            response <- post_to_api(
                payload$url,
                payload$headers,
                payload$body,
                timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
                total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
                first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
                connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
                idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
            )

            parsed_images <- list()
            text_content <- ""
            finish_reason <- NULL
            usage <- NULL

            if (!is.null(response$candidates) && length(response$candidates) > 0) {
                candidate <- response$candidates[[1]]
                finish_reason <- candidate$finishReason
            }

            if (!is.null(response$candidates) && length(response$candidates) > 0) {
                candidate <- response$candidates[[1]]
                if (!is.null(candidate$content$parts)) {
                    for (part in candidate$content$parts) {
                        if (!is.null(part$text)) {
                            text_content <- paste0(text_content, part$text)
                        } else if (!is.null(part$inlineData)) {
                            parsed_images <- c(parsed_images, list(list(
                                media_type = part$inlineData$mimeType %||% "application/octet-stream",
                                bytes = base64enc::base64decode(part$inlineData$data)
                            )))
                        } else if (!is.null(part$inline_data)) {
                            parsed_images <- c(parsed_images, list(list(
                                media_type = part$inline_data$mimeType %||% "application/octet-stream",
                                bytes = base64enc::base64decode(part$inline_data$data)
                            )))
                        }
                    }
                }
            }

            if (!is.null(response$usageMetadata)) {
                usage <- list(
                    prompt_tokens = response$usageMetadata$promptTokenCount,
                    completion_tokens = response$usageMetadata$candidatesTokenCount,
                    total_tokens = response$usageMetadata$totalTokenCount
                )
            }

            images <- finalize_image_artifacts(
                parsed_images,
                output_dir = params$output_dir %||% tempdir(),
                prefix = "gemini_image"
            )

            GenerateImageResult$new(
                images = images,
                text = text_content,
                usage = usage,
                finish_reason = finish_reason,
                raw_response = response
            )
        },

        #' @description Edit images.
        #' @param params A list of call options.
        #' @return A GenerateImageResult object.
        do_edit_image = function(params) {
            if (is.null(params$image)) {
                rlang::abort("`image` must be supplied for Gemini image editing.")
            }
            if (!is.null(params$mask)) {
                rlang::abort("Gemini image editing via aisdk does not support `mask` yet.")
            }

            if (is.null(params$prompt) || !nzchar(params$prompt)) {
                params$prompt <- "Edit this image."
            }

            self$do_generate_image(params)
        }
    )
)

#' @title Gemini Provider Class
#' @description
#' Provider class for Google Gemini.
#' @export
GeminiProvider <- R6::R6Class(
    "GeminiProvider",
    public = list(
        #' @field specification_version Provider spec version.
        specification_version = "v1",

        #' @description Initialize the Gemini provider.
        #' @param api_key Gemini API key. Defaults to GEMINI_API_KEY env var.
        #' @param base_url Base URL for API calls. Defaults to https://generativelanguage.googleapis.com/v1beta/models.
        #' @param headers Optional additional headers.
        #' @param name Optional provider name override.
        #' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
        #' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
        #' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
        #' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
        #' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
        initialize = function(api_key = NULL,
                              base_url = NULL,
                              headers = NULL,
                              name = NULL,
                              timeout_seconds = NULL,
                              total_timeout_seconds = NULL,
                              first_byte_timeout_seconds = NULL,
                              connect_timeout_seconds = NULL,
                              idle_timeout_seconds = NULL) {
            private$config <- list(
                api_key = api_key %||% Sys.getenv("GEMINI_API_KEY"),
                base_url = sub("/$", "", base_url %||% Sys.getenv("GEMINI_BASE_URL", "https://generativelanguage.googleapis.com/v1beta/models")),
                headers = headers,
                provider_name = name %||% "gemini",
                timeout_seconds = timeout_seconds,
                total_timeout_seconds = total_timeout_seconds,
                first_byte_timeout_seconds = first_byte_timeout_seconds,
                connect_timeout_seconds = connect_timeout_seconds,
                idle_timeout_seconds = idle_timeout_seconds
            )

            if (nchar(private$config$api_key) == 0) {
                rlang::warn("Gemini API key not set. Set GEMINI_API_KEY env var or pass api_key parameter.")
            }
        },

        #' @description Create a language model.
        #' @param model_id The model ID (e.g., "gemini-1.5-pro", "gemini-1.5-flash", "gemini-2.0-flash").
        #' @return A GeminiLanguageModel object.
        language_model = function(model_id = Sys.getenv("GEMINI_MODEL", "gemini-2.5-flash")) {
            GeminiLanguageModel$new(model_id, private$config)
        },

        #' @description Create an image model.
        #' @param model_id The model ID for image generation.
        #' @return A GeminiImageModel object.
        image_model = function(model_id = Sys.getenv("GEMINI_IMAGE_MODEL", "gemini-2.5-flash-image")) {
            GeminiImageModel$new(model_id, private$config)
        }
    ),
    private = list(
        config = NULL
    )
)

#' @title Create Gemini Provider
#' @description
#' Factory function to create a Gemini provider.
#'
#' @eval generate_model_docs("gemini")
#'
#' @param api_key Gemini API key. Defaults to GEMINI_API_KEY env var.
#' @param base_url Base URL for API calls. Defaults to https://generativelanguage.googleapis.com/v1beta/models.
#' @param headers Optional additional headers.
#' @param name Optional provider name override.
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
#' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
#' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
#' @return A GeminiProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'     gemini <- create_gemini(api_key = "AIza...")
#'     model <- gemini$language_model("gemini-2.5-flash")
#' }
#' }
create_gemini <- function(api_key = NULL,
                          base_url = NULL,
                          headers = NULL,
                          name = NULL,
                          timeout_seconds = NULL,
                          total_timeout_seconds = NULL,
                          first_byte_timeout_seconds = NULL,
                          connect_timeout_seconds = NULL,
                          idle_timeout_seconds = NULL) {
    GeminiProvider$new(
        api_key = api_key,
        base_url = base_url,
        headers = headers,
        name = name,
        timeout_seconds = timeout_seconds,
        total_timeout_seconds = total_timeout_seconds,
        first_byte_timeout_seconds = first_byte_timeout_seconds,
        connect_timeout_seconds = connect_timeout_seconds,
        idle_timeout_seconds = idle_timeout_seconds
    )
}
