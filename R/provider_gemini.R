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
        get_headers = function() {
            h <- list(
                `Content-Type` = "application/json"
            )
            if (!is.null(private$config$headers)) {
                h <- c(h, private$config$headers)
            }
            h
        },

        # Format OpenAI-style messages to Gemini contents array
        format_messages = function(messages) {
            contents <- list()
            system_instruction <- NULL

            for (msg in messages) {
                if (msg$role == "system") {
                    system_instruction <- list(
                        parts = list(list(text = msg$content))
                    )
                } else if (msg$role == "user") {
                    contents <- c(contents, list(list(
                        role = "user",
                        parts = list(list(text = msg$content))
                    )))
                } else if (msg$role == "assistant") {
                    parts <- list()
                    if (!is.null(msg$content) && nzchar(msg$content)) {
                        parts <- c(parts, list(list(text = msg$content)))
                    }
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
            # For gemini, api key is passed as query param `key`
            endpoint <- if (stream) "streamGenerateContent?alt=sse&key=" else "generateContent?key="
            url <- paste0(private$config$base_url, "/", self$model_id, ":", endpoint, private$config$api_key)
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
                function_declarations <- lapply(params$tools, function(t) {
                    if (inherits(t, "Tool")) {
                        # Get openai format as it's JSON Schema compliant
                        api_fmt <- t$to_api_format("openai")
                        list(
                            name = api_fmt$`function`$name,
                            description = api_fmt$`function`$description,
                            parameters = api_fmt$`function`$parameters
                        )
                    } else {
                        t
                    }
                })
                body$tools <- list(list(functionDeclarations = function_declarations))
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
            response <- post_to_api(payload$url, payload$headers, payload$body)

            # Parse Gemini response format
            text_content <- ""
            tool_calls <- NULL
            finish_reason <- NULL
            usage <- NULL

            if (!is.null(response$candidates) && length(response$candidates) > 0) {
                candidate <- response$candidates[[1]]
                finish_reason <- candidate$finishReason

                if (!is.null(candidate$content) && !is.null(candidate$content$parts)) {
                    for (part in candidate$content$parts) {
                        if (!is.null(part$text)) {
                            text_content <- paste0(text_content, part$text)
                        } else if (!is.null(part$functionCall)) {
                            if (is.null(tool_calls)) tool_calls <- list()
                            tool_calls <- c(tool_calls, list(list(
                                id = paste0("call_", part$functionCall$name, "_", sample(10000:99999, 1)), # Gemini has no tool call ID, mock one
                                name = part$functionCall$name,
                                arguments = part$functionCall$args
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

            GenerateResult$new(
                text = text_content,
                usage = usage,
                finish_reason = finish_reason,
                raw_response = response,
                tool_calls = tool_calls
            )
        },

        #' @description Generate text (streaming).
        #' @param params A list of call options.
        #' @param callback A function called for each chunk: callback(text, done).
        #' @return A GenerateResult object.
        do_stream = function(params, callback) {
            payload <- self$build_payload_internal(params, stream = TRUE)
            agg <- SSEAggregator$new(callback)

            # Gemini returns SSE events where `data` contains the JSON representation of GenerateContentResponse
            stream_from_api(payload$url, payload$headers, payload$body, callback = function(data, done) {
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
            })

            agg$finalize()
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
        initialize = function(api_key = NULL,
                              base_url = NULL,
                              headers = NULL,
                              name = NULL) {
            private$config <- list(
                api_key = api_key %||% Sys.getenv("GEMINI_API_KEY"),
                base_url = sub("/$", "", base_url %||% Sys.getenv("GEMINI_BASE_URL", "https://generativelanguage.googleapis.com/v1beta/models")),
                headers = headers,
                provider_name = name %||% "gemini"
            )

            if (nchar(private$config$api_key) == 0) {
                rlang::warn("Gemini API key not set. Set GEMINI_API_KEY env var or pass api_key parameter.")
            }
        },

        #' @description Create a language model.
        #' @param model_id The model ID (e.g., "gemini-1.5-pro", "gemini-1.5-flash", "gemini-2.0-flash").
        #' @return A GeminiLanguageModel object.
        language_model = function(model_id = "gemini-2.5-flash") {
            GeminiLanguageModel$new(model_id, private$config)
        }
    ),
    private = list(
        config = NULL
    )
)

#' @title Create Gemini Provider
#' @description
#' Factory function to create a Gemini provider.
#' @param api_key Gemini API key. Defaults to GEMINI_API_KEY env var.
#' @param base_url Base URL for API calls. Defaults to https://generativelanguage.googleapis.com/v1beta/models.
#' @param headers Optional additional headers.
#' @param name Optional provider name override.
#' @return A GeminiProvider object.
#' @export
#' @examples
#' \donttest{
#' gemini <- create_gemini(api_key = "AIza...")
#' model <- gemini$language_model("gemini-1.5-pro")
#' }
create_gemini <- function(api_key = NULL,
                          base_url = NULL,
                          headers = NULL,
                          name = NULL) {
    GeminiProvider$new(
        api_key = api_key,
        base_url = base_url,
        headers = headers,
        name = name
    )
}
