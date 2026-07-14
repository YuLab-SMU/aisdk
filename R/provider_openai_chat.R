#' @title OpenAI Language Model Class
#' @description
#' Language model implementation for OpenAI's chat completions API.
#' Exported so that OpenAI-compatible providers in companion packages
#' (e.g. \pkg{aisdk.providers}) can inherit from it across package boundaries.
#' @keywords internal
#' @export
OpenAILanguageModel <- R6::R6Class(
  "OpenAILanguageModel",
  inherit = LanguageModelV1,
  private = list(
    config = NULL,
    get_headers = function() {
      h <- list(`Content-Type` = "application/json")
      if (nzchar(private$config$api_key %||% "")) {
        h$Authorization <- paste("Bearer", private$config$api_key)
      }
      if (!is.null(private$config$organization)) {
        h$`OpenAI-Organization` <- private$config$organization
      }
      if (!is.null(private$config$headers)) {
        h <- c(h, private$config$headers)
      }
      h
    },

    # Process response_format for OpenAI-compatible APIs
    process_response_format = function(params) {
      if (!is.null(params$response_format)) {
        fmt <- params$response_format

        # If it's a z_schema object, convert to OpenAI json_schema format
        if (inherits(fmt, "z_schema")) {
          # Use json_schema if supported, otherwise fallback to json_object + prompt injection
          if (!isTRUE(private$config$disable_json_schema)) {
            schema_list <- schema_to_list(fmt)
            schema_name <- params$response_format_name %||% "output_schema"

            params$response_format <- list(
              type = "json_schema",
              json_schema = list(
                name = schema_name,
                strict = TRUE,
                schema = schema_list
              )
            )
          } else {
            # Fallback for providers that don't support native structured output
            schema_json <- schema_to_json(fmt)
            instruction <- paste(
              "Return your output strictly as a JSON object adhering to this schema:\n",
              schema_json
            )

            msgs <- params$messages
            if (length(msgs) > 0 && msgs[[1]]$role == "system") {
              msgs[[1]]$content <- paste(msgs[[1]]$content, "\n\n", instruction)
            } else {
              msgs <- c(list(list(role = "system", content = instruction)), msgs)
            }
            params$messages <- msgs
            params$response_format <- list(type = "json_object")
          }
        } else if (is.character(fmt)) {
          # Support string shorthands
          if (fmt == "json_object") {
            params$response_format <- list(type = "json_object")
          } else if (fmt == "text") {
            params$response_format <- list(type = "text")
          }
        }
      }
      params
    },

    translate_chat_messages = function(messages) {
      lapply(messages, function(msg) {
        translated <- msg
        if (!is.null(msg$content) && !identical(msg$role, "tool")) {
          translated$content <- translate_message_content(msg$content, target = "openai_chat")
        }
        translated
      })
    },

    # Reasoning models (o-series, gpt-5) reject sampling params with HTTP 400
    # ("Unsupported value: 'temperature' does not support X with this model"),
    # so they are silently skipped for those models.
    # See: https://platform.openai.com/docs/guides/reasoning
    add_chat_sampling_params = function(body, params, is_reasoning) {
      for (nm in c("temperature", "top_p", "presence_penalty", "frequency_penalty")) {
        if (!is.null(params[[nm]]) && !is_reasoning) {
          body[[nm]] <- params[[nm]]
        }
      }
      body
    },

    # Smart token limit mapping (capability-driven): an explicit
    # `max_completion_tokens` always wins; the unified `max_tokens` becomes
    # `max_completion_tokens` for reasoning models (which reject `max_tokens`)
    # and stays `max_tokens` otherwise.
    add_chat_token_limit = function(body, params, is_reasoning) {
      if (!is.null(params[["max_completion_tokens"]])) {
        body$max_completion_tokens <- params[["max_completion_tokens"]]
      } else if (!is.null(params[["max_tokens"]])) {
        if (is_reasoning) {
          body$max_completion_tokens <- params[["max_tokens"]]
        } else {
          body$max_tokens <- params[["max_tokens"]]
        }
      }
      body
    },

    format_chat_tools = function(tools) {
      lapply(unname(tools), function(t) {
        if (inherits(t, "Tool")) {
          t$to_api_format("openai")
        } else {
          t
        }
      })
    },

    # Pass through params not consumed above (e.g. seed, logprobs, the
    # normalized response_format) so provider-specific options reach the API
    # unchanged. Sampling params never appear here: they are in handled_params.
    merge_chat_extra_params = function(body, params) {
      handled_params <- c(
        "messages", "temperature", "top_p", "presence_penalty", "frequency_penalty",
        "max_tokens", "max_completion_tokens",
        "tools", "stream", "model",
        "timeout_seconds", "total_timeout_seconds", "first_byte_timeout_seconds",
        "connect_timeout_seconds", "idle_timeout_seconds"
      )
      extra_params <- params[setdiff(names(params), handled_params)]
      if (length(extra_params) > 0) {
        body <- utils::modifyList(body, extra_params)
      }
      body
    },

    # Shared body builder for the streaming and non-streaming chat payloads.
    build_chat_body = function(params, stream = FALSE) {
      body <- list(
        model = self$model_id,
        messages = private$translate_chat_messages(params$messages),
        stream = stream
      )
      is_reasoning <- self$has_capability("is_reasoning_model")
      body <- private$add_chat_sampling_params(body, params, is_reasoning)
      body <- private$add_chat_token_limit(body, params, is_reasoning)
      if (!is.null(params$tools) && length(params$tools) > 0) {
        body$tools <- private$format_chat_tools(params$tools)
      }
      body <- private$merge_chat_extra_params(body, params)
      # Added after the extra-params merge so a caller-supplied stream_options
      # wins over the default; some providers reject stream_options entirely
      # (config$disable_stream_options).
      if (isTRUE(stream) &&
          is.null(body[["stream_options"]]) &&
          !isTRUE(private$config$disable_stream_options)) {
        body$stream_options <- list(include_usage = TRUE)
      }
      body[!sapply(body, is.null)]
    }
  ),
  public = list(
    #' @description Initialize the OpenAI language model.
    #' @param model_id The model ID (e.g., "gpt-4o").
    #' @param config Configuration list with api_key, base_url, headers, etc.
    #' @param capabilities Optional list of capability flags.
    initialize = function(model_id, config, capabilities = list()) {
      # Auto-detect reasoning model capability if not explicitly set
      if (is.null(capabilities$is_reasoning_model)) {
        capabilities$is_reasoning_model <- grepl("^o[0-9]|^gpt-5", model_id, ignore.case = TRUE)
      }
      super$initialize(
        provider = config$provider_name %||% "openai",
        model_id = model_id,
        capabilities = capabilities
      )
      private$config <- config
    },

    #' @description Get the configuration list.
    #' @return A list with provider configuration.
    get_config = function() {
      private$config
    },

    #' @description Build the request payload for non-streaming generation.
    #' Subclasses can override to customize payload construction.
    #' @param params A list of call options.
    #' @return A list with url, headers, and body.
    build_payload = function(params) {
      params <- private$process_response_format(params)
      list(
        url = api_endpoint_urls(private$config, "/chat/completions"),
        headers = private$get_headers(),
        body = private$build_chat_body(params, stream = FALSE)
      )
    },

    #' @description Execute the API request.
    #' @param url The API endpoint URL.
    #' @param headers A named list of HTTP headers.
    #' @param body The request body.
    #' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
    #' @param total_timeout_seconds Optional total request timeout override in seconds.
    #' @param first_byte_timeout_seconds Optional time-to-first-byte timeout override in seconds.
    #' @param connect_timeout_seconds Optional connection timeout override in seconds.
    #' @param idle_timeout_seconds Optional stall timeout override in seconds.
    #' @return The parsed API response.
    execute_request = function(url, headers, body,
                               timeout_seconds = NULL,
                               total_timeout_seconds = NULL,
                               first_byte_timeout_seconds = NULL,
                               connect_timeout_seconds = NULL,
                               idle_timeout_seconds = NULL) {
      post_to_api(
        url,
        headers,
        body,
        timeout_seconds = timeout_seconds %||% private$config$timeout_seconds,
        total_timeout_seconds = total_timeout_seconds %||% private$config$total_timeout_seconds,
        first_byte_timeout_seconds = first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = connect_timeout_seconds %||% private$config$connect_timeout_seconds,
        idle_timeout_seconds = idle_timeout_seconds %||% private$config$idle_timeout_seconds
      )
    },

    #' @description Parse the API response into a GenerateResult.
    #' Subclasses can override to extract provider-specific fields (e.g., reasoning_content).
    #' @param response The parsed API response.
    #' @return A GenerateResult object.
    parse_response = function(response) {
      choice <- response$choices[[1]]

      # Parse tool_calls if present
      tool_calls <- NULL
      if (!is.null(choice$message$tool_calls)) {
        tool_calls <- lapply(choice$message$tool_calls, function(tc) {
          list(
            id = tc$id,
            name = tc$`function`$name,
            arguments = parse_tool_arguments(tc$`function`$arguments, tool_name = tc$`function`$name)
          )
        })
      }

      GenerateResult$new(
        text = choice$message$content %||% "",
        usage = response$usage,
        finish_reason = choice$finish_reason,
        raw_response = response,
        tool_calls = tool_calls
      )
    },

    #' @description Generate text (non-streaming). Uses template method pattern.
    #' @param params A list of call options including messages, temperature, etc.
    #' @return A GenerateResult object.
    do_generate = function(params) {
      payload <- self$build_payload(params)
      response <- self$execute_request(
        payload$url,
        payload$headers,
        payload$body,
        timeout_seconds = params$timeout_seconds,
        total_timeout_seconds = params$total_timeout_seconds,
        first_byte_timeout_seconds = params$first_byte_timeout_seconds,
        connect_timeout_seconds = params$connect_timeout_seconds,
        idle_timeout_seconds = params$idle_timeout_seconds
      )
      self$parse_response(response)
    },

    #' @description Build the request payload for streaming generation.
    #' Subclasses can override to customize stream payload construction.
    #' @param params A list of call options.
    #' @return A list with url, headers, and body.
    build_stream_payload = function(params) {
      params <- private$process_response_format(params)
      list(
        url = api_endpoint_urls(private$config, "/chat/completions"),
        headers = private$get_headers(),
        body = private$build_chat_body(params, stream = TRUE)
      )
    },

    #' @description Generate text (streaming).
    #' @param params A list of call options.
    #' @param callback A function called for each chunk: callback(text, done).
    #' @return A GenerateResult object.
    do_stream = function(params, callback) {
      payload <- self$build_stream_payload(params)
      agg <- SSEAggregator$new(callback)

      stream_from_api(
        payload$url,
        payload$headers,
        payload$body,
        callback = function(data, done) {
          map_openai_chunk(data, done, agg)
        },
        timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
        total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
        first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
        idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
      )

      agg$build_result()
    },

    #' @description Format a tool execution result for OpenAI's API.
    #' @param tool_call_id The ID of the tool call.
    #' @param tool_name The name of the tool (not used by OpenAI but kept for interface consistency).
    #' @param result_content The result content from executing the tool.
    #' @return A list formatted as a message for OpenAI's API.
    format_tool_result = function(tool_call_id, tool_name, result_content) {
      list(
        role = "tool",
        tool_call_id = tool_call_id,
        content = if (is.character(result_content)) result_content else safe_to_json(result_content, auto_unbox = TRUE)
      )
    },

    #' @description Get the message format for OpenAI.
    get_history_format = function() {
      "openai"
    }
  )
)
