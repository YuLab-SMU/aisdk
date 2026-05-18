#' @name provider_openai
#' @title OpenAI Provider
#' @description
#' Implementation for OpenAI models.
#' @keywords internal
NULL

#' @title OpenAI Language Model Class
#' @description
#' Language model implementation for OpenAI's chat completions API.
#' @keywords internal
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
      url <- paste0(private$config$base_url, "/chat/completions")
      headers <- private$get_headers()

      body <- list(
        model = self$model_id,
        messages = private$translate_chat_messages(params$messages),
        stream = FALSE
      )

      # Reasoning models (o-series, gpt-5) reject sampling params with HTTP 400
      # ("Unsupported value: 'temperature' does not support X with this model").
      # See: https://platform.openai.com/docs/guides/reasoning
      is_reasoning <- self$has_capability("is_reasoning_model")
      if (!is.null(params$temperature) && !is_reasoning) {
        body$temperature <- params$temperature
      }
      if (!is.null(params$top_p) && !is_reasoning) {
        body$top_p <- params$top_p
      }
      if (!is.null(params$presence_penalty) && !is_reasoning) {
        body$presence_penalty <- params$presence_penalty
      }
      if (!is.null(params$frequency_penalty) && !is_reasoning) {
        body$frequency_penalty <- params$frequency_penalty
      }

      # ==========================================================
      # Smart Token Limit Mapping (capability-driven)
      # ==========================================================
      explicit_completion_tokens <- params$max_completion_tokens

      if (!is.null(explicit_completion_tokens)) {
        body$max_completion_tokens <- explicit_completion_tokens
      } else if (!is.null(params$max_tokens)) {
        if (is_reasoning) {
          body$max_completion_tokens <- params$max_tokens
        } else {
          body$max_tokens <- params$max_tokens
        }
      }

      # Add tools if provided
      if (!is.null(params$tools) && length(params$tools) > 0) {
        tools_list <- unname(params$tools)
        body$tools <- lapply(tools_list, function(t) {
          if (inherits(t, "Tool")) {
            t$to_api_format("openai")
          } else {
            t
          }
        })
      }

      # Pass through any extra parameters
      handled_params <- c(
        "messages", "temperature", "top_p", "presence_penalty", "frequency_penalty",
        "max_tokens", "max_completion_tokens",
        "tools", "stream", "model",
        "timeout_seconds", "total_timeout_seconds", "first_byte_timeout_seconds",
        "connect_timeout_seconds", "idle_timeout_seconds"
      )
      extra_params <- params[setdiff(names(params), handled_params)]
      if (length(extra_params) > 0) {
        if (is_reasoning) {
          extra_params <- extra_params[setdiff(names(extra_params),
            c("temperature", "top_p", "presence_penalty", "frequency_penalty"))]
        }
        body <- utils::modifyList(body, extra_params)
      }

      body <- body[!sapply(body, is.null)]

      list(url = url, headers = headers, body = body)
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
      url <- paste0(private$config$base_url, "/chat/completions")
      headers <- private$get_headers()

      body <- list(
        model = self$model_id,
        messages = private$translate_chat_messages(params$messages),
        stream = TRUE
      )

      # Reasoning models (o-series, gpt-5) reject sampling params — silently skip.
      is_reasoning <- self$has_capability("is_reasoning_model")
      if (!is.null(params$temperature) && !is_reasoning) {
        body$temperature <- params$temperature
      }
      if (!is.null(params$top_p) && !is_reasoning) {
        body$top_p <- params$top_p
      }
      if (!is.null(params$presence_penalty) && !is_reasoning) {
        body$presence_penalty <- params$presence_penalty
      }
      if (!is.null(params$frequency_penalty) && !is_reasoning) {
        body$frequency_penalty <- params$frequency_penalty
      }

      # Smart Token Limit Mapping (capability-driven)
      explicit_completion_tokens <- params$max_completion_tokens
      if (!is.null(explicit_completion_tokens)) {
        body$max_completion_tokens <- explicit_completion_tokens
      } else if (!is.null(params$max_tokens)) {
        if (is_reasoning) {
          body$max_completion_tokens <- params$max_tokens
        } else {
          body$max_tokens <- params$max_tokens
        }
      }

      # Pass through any extra parameters
      handled_params <- c(
        "messages", "temperature", "top_p", "presence_penalty", "frequency_penalty",
        "max_tokens", "max_completion_tokens",
        "tools", "stream", "model",
        "timeout_seconds", "total_timeout_seconds", "first_byte_timeout_seconds",
        "connect_timeout_seconds", "idle_timeout_seconds"
      )
      extra_params <- params[setdiff(names(params), handled_params)]
      if (length(extra_params) > 0) {
        if (is_reasoning) {
          extra_params <- extra_params[setdiff(names(extra_params),
            c("temperature", "top_p", "presence_penalty", "frequency_penalty"))]
        }
        body <- utils::modifyList(body, extra_params)
      }

      # Add tools if provided
      if (!is.null(params$tools) && length(params$tools) > 0) {
        tools_list <- unname(params$tools)
        body$tools <- lapply(tools_list, function(t) {
          if (inherits(t, "Tool")) {
            t$to_api_format("openai")
          } else {
            t
          }
        })
      }

      # Add stream_options only if not disabled
      if (is.null(body$stream_options) && !isTRUE(private$config$disable_stream_options)) {
        body$stream_options <- list(include_usage = TRUE)
      }

      body <- body[!sapply(body, is.null)]

      list(url = url, headers = headers, body = body)
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

#' @title OpenAI Responses Language Model Class
#' @description
#' Language model implementation for OpenAI's Responses API.
#' This API is designed for stateful multi-turn conversations where the server
#' maintains conversation history, and supports advanced features like:
#' - Built-in reasoning/thinking (for o1, o3 models)
#' - Server-side conversation state management via response IDs
#' - Structured output items (reasoning, message, tool calls)
#'
#' The Responses API uses a different request/response format than Chat Completions:
#' - Request: `input` field instead of `messages`, optional `previous_response_id`
#' - Response: `output` array with typed items instead of `choices`
#'
#' @keywords internal
#' @export
OpenAIResponsesLanguageModel <- R6::R6Class(
  "OpenAIResponsesLanguageModel",
  inherit = LanguageModelV1,
  private = list(
    config = NULL,
    last_response_id = NULL,
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

    # Convert standard messages format to Responses API input format
    format_input = function(messages) {
      # Responses API accepts input in multiple formats:
      # 1. Simple string: "Hello"
      # 2. Array of input items with roles
      # We convert standard messages to the array format

      input_items <- list()
      system_instructions <- NULL

      for (msg in messages) {
        if (msg$role == "system") {
          # System messages become instructions parameter
          system_instructions <- if (is.character(msg$content) || is.null(msg$content)) {
            msg$content
          } else {
            content_blocks_to_text(msg$content, arg_name = "system")
          }
        } else if (msg$role == "user") {
          input_items <- c(input_items, list(list(
            type = "message",
            role = "user",
            content = translate_message_content(msg$content, target = "openai_responses")
          )))
        } else if (msg$role == "assistant") {
          input_items <- c(input_items, list(list(
            type = "message",
            role = "assistant",
            content = translate_message_content(msg$content, target = "openai_responses")
          )))
        } else if (msg$role == "tool") {
          # Tool results in Responses API format
          input_items <- c(input_items, list(list(
            type = "function_call_output",
            call_id = msg$tool_call_id,
            output = msg$content
          )))
        }
      }

      list(
        input = input_items,
        instructions = system_instructions
      )
    },

    # Parse Responses API output array into GenerateResult
    parse_output = function(response) {
      output_items <- response$output %||% list()

      text_content <- ""
      reasoning_content <- NULL
      tool_calls <- NULL

      for (item in output_items) {
        item_type <- item$type

        if (item_type == "reasoning") {
          # Reasoning/thinking content
          # Support both OpenAI format (item$content) and Volcano Ark format (item$summary)
          if (!is.null(item$summary) && is.list(item$summary)) {
            # Volcano Ark format: summary array contains summary_text objects
            for (block in item$summary) {
              if (!is.null(block$text)) {
                reasoning_content <- paste0(reasoning_content %||% "", block$text)
              }
            }
          } else if (is.list(item$content)) {
            # OpenAI format: content array of text blocks
            for (block in item$content) {
              if (!is.null(block$text)) {
                reasoning_content <- paste0(reasoning_content %||% "", block$text)
              }
            }
          } else if (!is.null(item$content)) {
            reasoning_content <- item$content
          }
        } else if (item_type == "message") {
          # Final message content
          if (is.list(item$content)) {
            for (block in item$content) {
              if (!is.null(block$text)) {
                text_content <- paste0(text_content, block$text)
              }
            }
          } else if (is.character(item$content)) {
            text_content <- paste0(text_content, item$content)
          }
        } else if (item_type == "function_call") {
          # Tool/function calls
          if (is.null(tool_calls)) tool_calls <- list()
          tool_calls <- c(tool_calls, list(list(
            id = item$call_id %||% item$id,
            name = item$name,
            arguments = if (is.character(item$arguments)) {
              parse_tool_arguments(item$arguments, tool_name = item$name)
            } else {
              item$arguments
            }
          )))
        }
      }

      list(
        text = text_content,
        reasoning = reasoning_content,
        tool_calls = tool_calls
      )
    },

    # Build the Responses-API `reasoning` body block from flat (reasoning_effort,
    # reasoning_summary) and/or nested (reasoning = list(...)) inputs. Returns
    # NULL when neither was supplied — callers should skip setting body$reasoning
    # in that case so the API can apply its own default.
    build_responses_reasoning_block = function(params) {
      nested <- params$reasoning
      if (!is.null(nested) && !is.list(nested)) {
        rlang::abort("`reasoning` must be a named list (e.g. list(effort = \"low\", summary = \"auto\")).")
      }
      effort <- params$reasoning_effort %||% nested$effort
      summary <- params$reasoning_summary %||% nested$summary
      extras <- if (is.list(nested)) nested[setdiff(names(nested), c("effort", "summary"))] else list()
      if (is.null(effort) && is.null(summary) && length(extras) == 0) {
        return(NULL)
      }
      out <- list()
      if (!is.null(effort))  out$effort  <- effort
      if (!is.null(summary)) out$summary <- summary
      for (nm in names(extras)) out[[nm]] <- extras[[nm]]
      out
    },

    # Normalize `include` (character vector or list) into a list of strings, the
    # shape the Responses API expects. Returns NULL if no include was provided.
    build_responses_include_field = function(params) {
      inc <- params$include
      if (is.null(inc)) return(NULL)
      as.list(unlist(inc, use.names = FALSE))
    },

    # Accept conversation as either a string id or a list with $id (the shape
    # the server returns from POST /v1/conversations). Returns the bare id
    # string or NULL.
    resolve_conversation_id = function(conversation) {
      if (is.null(conversation)) return(NULL)
      if (is.character(conversation) && length(conversation) == 1 && nzchar(conversation)) {
        return(conversation)
      }
      if (is.list(conversation) && !is.null(conversation$id) && nzchar(conversation$id)) {
        return(conversation$id)
      }
      rlang::abort("`conversation` must be a conversation id string or a list with `$id`.")
    }
  ),
  public = list(
    #' @description Initialize the OpenAI Responses language model.
    #' @param model_id The model ID (e.g., "o1", "o3-mini", "gpt-4o").
    #' @param config Configuration list with api_key, base_url, headers, etc.
    #' @param capabilities Optional list of capability flags.
    initialize = function(model_id, config, capabilities = list()) {
      # Auto-detect reasoning model so downstream `has_capability()` calls work.
      # Matches OpenAI o-series (o1, o3, o4-mini, ...), gpt-5 family (gpt-5,
      # gpt-5-pro, gpt-5-mini, ...), and proxy variants like gpt-5.4-mini.
      if (is.null(capabilities$is_reasoning_model)) {
        capabilities$is_reasoning_model <- grepl(
          "^o[0-9]|^gpt-5", model_id, ignore.case = TRUE
        )
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

    #' @description Get the last response ID (for debugging/advanced use).
    #' @return The last response ID or NULL.
    get_last_response_id = function() {
      private$last_response_id
    },

    #' @description Reset the conversation state (clear response ID).
    #' Call this to start a fresh conversation.
    reset = function() {
      private$last_response_id <- NULL
      invisible(self)
    },

    #' @description Generate text (non-streaming) using Responses API.
    #'
    #' Reasoning controls (`reasoning_effort`, `reasoning_summary`, nested
    #' `reasoning = list(effort, summary, ...)`) are translated into the
    #' Responses-API `body$reasoning` block. The `include` argument (character
    #' vector or list) is forwarded to `body$include` — pass
    #' `c("reasoning.encrypted_content")` to keep reasoning tokens across
    #' stateless turns. Any other unrecognized parameter falls through into
    #' the body via `modifyList`, matching the previous escape-hatch behavior.
    #'
    #' @param params A list of call options including messages, temperature, etc.
    #' @return A GenerateResult object.
    do_generate = function(params) {
      url <- paste0(private$config$base_url, "/responses")
      headers <- private$get_headers()

      # Convert messages to Responses API format
      formatted <- private$format_input(params$messages)

      body <- list(
        model = self$model_id,
        input = formatted$input
      )

      # Add system instructions if present
      if (!is.null(formatted$instructions)) {
        body$instructions <- formatted$instructions
      }

      # Inject previous_response_id for multi-turn (stateful conversation)
      if (!is.null(private$last_response_id)) {
        body$previous_response_id <- private$last_response_id
      }

      # Optional sampling parameters.
      # Reasoning models on the Responses API (gpt-5, o-series, and most
      # gpt-5.x variants on third-party proxies) reject `temperature`/`top_p`
      # with HTTP 400 ("Unsupported value: 'temperature' ... Only the default
      # value is supported"). We silently skip these for any model exposed via
      # the Responses API since that surface is reasoning-only on OpenAI.
      # See: https://platform.openai.com/docs/guides/reasoning
      is_reasoning <- self$has_capability("is_reasoning_model")
      if (!is.null(params$temperature) && !is_reasoning) {
        body$temperature <- params$temperature
      }
      if (!is.null(params$top_p) && !is_reasoning) {
        body$top_p <- params$top_p
      }
      if (!is.null(params$presence_penalty) && !is_reasoning) {
        body$presence_penalty <- params$presence_penalty
      }
      if (!is.null(params$frequency_penalty) && !is_reasoning) {
        body$frequency_penalty <- params$frequency_penalty
      }

      # ==========================================================
      # Smart Token Limit Mapping for Responses API
      # Volcengine Responses API has two mutually exclusive parameters:
      # - max_output_tokens: Total limit (reasoning + answer) - SAFE DEFAULT
      # - max_tokens: Answer-only limit (excludes reasoning)
      # SDK maps unified `max_tokens` to `max_output_tokens` for safety
      # ==========================================================
      # Check for explicit overrides (escape hatch for advanced users)
      explicit_output_tokens <- params$max_output_tokens # Total limit
      explicit_answer_tokens <- params$max_answer_tokens # Answer-only limit

      if (!is.null(explicit_output_tokens)) {
        # User explicitly wants total limit (reasoning + answer)
        body$max_output_tokens <- explicit_output_tokens
      } else if (!is.null(explicit_answer_tokens)) {
        # User explicitly wants answer-only limit (Volcengine-specific)
        # This uses the API's max_tokens field which excludes reasoning
        body$max_tokens <- explicit_answer_tokens
      } else if (!is.null(params$max_tokens)) {
        # Default behavior: map SDK's max_tokens to max_output_tokens (total limit)
        # This is the SAFE default - prevents runaway reasoning costs
        body$max_output_tokens <- params$max_tokens
      }

      # Add tools if provided (Responses API format)
      if (!is.null(params$tools) && length(params$tools) > 0) {
        tools_list <- unname(params$tools)
        body$tools <- lapply(tools_list, function(t) {
          if (inherits(t, "Tool")) {
            # Convert to Responses API tool format
            api_fmt <- t$to_api_format("openai")
            list(
              type = "function",
              name = api_fmt$`function`$name,
              description = api_fmt$`function`$description,
              parameters = api_fmt$`function`$parameters
            )
          } else {
            t
          }
        })
      }

      # Reasoning controls: support flat (reasoning_effort, reasoning_summary)
      # and nested (reasoning = list(...)) forms, plus the `include` field for
      # stateless reasoning continuity (e.g. "reasoning.encrypted_content").
      reasoning_block <- private$build_responses_reasoning_block(params)
      if (!is.null(reasoning_block)) {
        body$reasoning <- reasoning_block
      }
      include_field <- private$build_responses_include_field(params)
      if (!is.null(include_field)) {
        body$include <- include_field
      }
      # Optional server-side conversation handle. Accepts a string id or a
      # list with `$id`. When set, OpenAI manages message history server-side
      # — the caller is responsible for sending only the new turn rather than
      # the full transcript, otherwise tokens are paid twice.
      conv_id <- private$resolve_conversation_id(params$conversation)
      if (!is.null(conv_id)) {
        body$conversation <- conv_id
      }

      # Pass through extra parameters (e.g., store)
      handled_params <- c(
        "messages", "temperature", "top_p", "presence_penalty", "frequency_penalty",
        "max_tokens", "max_output_tokens", "max_answer_tokens",
        "tools", "stream", "model",
        "reasoning", "reasoning_effort", "reasoning_summary", "include",
        "conversation",
        "timeout_seconds", "total_timeout_seconds", "first_byte_timeout_seconds",
        "connect_timeout_seconds", "idle_timeout_seconds"
      )
      extra_params <- params[setdiff(names(params), handled_params)]
      if (length(extra_params) > 0) {
        if (is_reasoning) {
          extra_params <- extra_params[setdiff(names(extra_params),
            c("temperature", "top_p", "presence_penalty", "frequency_penalty"))]
        }
        body <- utils::modifyList(body, extra_params)
      }

      # Remove NULL entries
      body <- body[!sapply(body, is.null)]

      response <- post_to_api(
        url,
        headers,
        body,
        timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
        total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
        first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
        idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
      )

      # Update internal state with new response ID
      private$last_response_id <- response$id

      # Parse the output
      parsed <- private$parse_output(response)

      # Build usage info
      usage <- NULL
      if (!is.null(response$usage)) {
        usage <- list(
          prompt_tokens = response$usage$input_tokens %||% response$usage$prompt_tokens,
          completion_tokens = response$usage$output_tokens %||% response$usage$completion_tokens,
          total_tokens = response$usage$total_tokens
        )
        # Include reasoning tokens if available
        if (!is.null(response$usage$output_tokens_details$reasoning_tokens)) {
          usage$reasoning_tokens <- response$usage$output_tokens_details$reasoning_tokens
        }
      }

      GenerateResult$new(
        text = parsed$text,
        reasoning = parsed$reasoning,
        usage = usage,
        finish_reason = response$status %||% response$stop_reason,
        raw_response = response,
        tool_calls = parsed$tool_calls,
        response_id = response$id
      )
    },

    #' @description Generate text (streaming) using Responses API.
    #' @param params A list of call options.
    #' @param callback A function called for each chunk: callback(text, done).
    #' @return A GenerateResult object.
    do_stream = function(params, callback) {
      url <- paste0(private$config$base_url, "/responses")
      headers <- private$get_headers()

      # Convert messages to Responses API format
      formatted <- private$format_input(params$messages)

      body <- list(
        model = self$model_id,
        input = formatted$input,
        stream = TRUE
      )

      # Add system instructions if present
      if (!is.null(formatted$instructions)) {
        body$instructions <- formatted$instructions
      }

      # Inject previous_response_id for multi-turn
      if (!is.null(private$last_response_id)) {
        body$previous_response_id <- private$last_response_id
      }

      # Optional sampling parameters — silently skip for reasoning models
      # (see notes in do_generate above).
      is_reasoning <- self$has_capability("is_reasoning_model")
      if (!is.null(params$temperature) && !is_reasoning) {
        body$temperature <- params$temperature
      }
      if (!is.null(params$top_p) && !is_reasoning) {
        body$top_p <- params$top_p
      }
      if (!is.null(params$presence_penalty) && !is_reasoning) {
        body$presence_penalty <- params$presence_penalty
      }
      if (!is.null(params$frequency_penalty) && !is_reasoning) {
        body$frequency_penalty <- params$frequency_penalty
      }

      # Smart Token Limit Mapping (same logic as do_generate)
      explicit_output_tokens <- params$max_output_tokens
      explicit_answer_tokens <- params$max_answer_tokens
      if (!is.null(explicit_output_tokens)) {
        body$max_output_tokens <- explicit_output_tokens
      } else if (!is.null(explicit_answer_tokens)) {
        body$max_tokens <- explicit_answer_tokens
      } else if (!is.null(params$max_tokens)) {
        body$max_output_tokens <- params$max_tokens
      }

      # Add tools if provided
      if (!is.null(params$tools) && length(params$tools) > 0) {
        tools_list <- unname(params$tools)
        body$tools <- lapply(tools_list, function(t) {
          if (inherits(t, "Tool")) {
            api_fmt <- t$to_api_format("openai")
            list(
              type = "function",
              name = api_fmt$`function`$name,
              description = api_fmt$`function`$description,
              parameters = api_fmt$`function`$parameters
            )
          } else {
            t
          }
        })
      }

      # Reasoning controls (mirrors do_generate).
      reasoning_block <- private$build_responses_reasoning_block(params)
      if (!is.null(reasoning_block)) {
        body$reasoning <- reasoning_block
      }
      include_field <- private$build_responses_include_field(params)
      if (!is.null(include_field)) {
        body$include <- include_field
      }
      conv_id <- private$resolve_conversation_id(params$conversation)
      if (!is.null(conv_id)) {
        body$conversation <- conv_id
      }

      # Pass through extra parameters
      handled_params <- c(
        "messages", "temperature", "top_p", "presence_penalty", "frequency_penalty",
        "max_tokens", "max_output_tokens", "max_answer_tokens",
        "tools", "stream", "model",
        "reasoning", "reasoning_effort", "reasoning_summary", "include",
        "conversation",
        "timeout_seconds", "total_timeout_seconds", "first_byte_timeout_seconds",
        "connect_timeout_seconds", "idle_timeout_seconds"
      )
      extra_params <- params[setdiff(names(params), handled_params)]
      if (length(extra_params) > 0) {
        if (is_reasoning) {
          extra_params <- extra_params[setdiff(names(extra_params),
            c("temperature", "top_p", "presence_penalty", "frequency_penalty"))]
        }
        body <- utils::modifyList(body, extra_params)
      }

      body <- body[!sapply(body, is.null)]

      # State for streaming - use environment for mutable state
      stream_state <- new.env(parent = emptyenv())
      stream_state$full_text <- ""
      stream_state$full_reasoning <- ""
      stream_state$is_reasoning <- FALSE
      stream_state$tool_calls_acc <- list()
      stream_state$finish_reason <- NULL
      stream_state$full_usage <- NULL
      stream_state$response_id <- NULL
      stream_state$last_response <- NULL

      stream_responses_api(
        url,
        headers,
        body,
        callback = function(event_type, data, done) {
          if (done) {
            if (stream_state$is_reasoning) {
              callback("\n</think>\n\n", done = FALSE)
              stream_state$is_reasoning <- FALSE
            }
            callback(NULL, done = TRUE)
          } else {
            stream_state$last_response <- data

            # Debug: Print event type (enable with options(aisdk.debug = TRUE))
            if (isTRUE(getOption("aisdk.debug", FALSE))) {
              message("[DEBUG] Event: ", event_type, " | Data keys: ", paste(names(data), collapse = ", "))
            }

            # Handle different event types from Responses API
            if (event_type == "response.created") {
              stream_state$response_id <- data$response$id
            } else if (event_type == "response.output_item.added") {
              # New output item started
              item <- data$item
              if (!is.null(item) && item$type == "reasoning") {
                if (!stream_state$is_reasoning) {
                  callback("<think>\n", done = FALSE)
                  stream_state$is_reasoning <- TRUE
                }
                # Some providers may include initial content in the added event
                if (!is.null(item$content) && is.list(item$content)) {
                  for (block in item$content) {
                    if (!is.null(block$text) && nchar(block$text) > 0) {
                      stream_state$full_reasoning <- paste0(stream_state$full_reasoning, block$text)
                      callback(block$text, done = FALSE)
                    }
                  }
                }
                # Also check for summary field (Volcano Ark format)
                if (!is.null(item$summary) && is.list(item$summary)) {
                  for (block in item$summary) {
                    if (!is.null(block$text) && nchar(block$text) > 0) {
                      stream_state$full_reasoning <- paste0(stream_state$full_reasoning, block$text)
                      callback(block$text, done = FALSE)
                    }
                  }
                }
              }
            } else if (event_type == "response.content_part.delta" ||
              event_type == "response.output_text.delta") {
              # Text content delta
              # Support both formats:
              # - OpenAI: delta is object with text field
              # - Volcano Ark: delta is string directly
              delta_text <- NULL
              if (is.character(data$delta)) {
                delta_text <- data$delta
              } else if (is.list(data$delta)) {
                delta_text <- data$delta$text
              }
              if (!is.null(delta_text) && nchar(delta_text) > 0) {
                if (stream_state$is_reasoning) {
                  callback("\n</think>\n\n", done = FALSE)
                  stream_state$is_reasoning <- FALSE
                }
                stream_state$full_text <- paste0(stream_state$full_text, delta_text)
                callback(delta_text, done = FALSE)
              }
            } else if (event_type == "response.reasoning.delta" ||
              event_type == "response.reasoning_summary_text.delta" ||
              event_type == "response.reasoning_summary.delta" ||
              event_type == "response.reasoning_content.delta") {
              # Reasoning content delta
              # Support multiple formats:
              # - OpenAI: response.reasoning.delta (delta is object with text field)
              # - Volcano Ark: response.reasoning_summary_text.delta (delta is string directly)
              delta_reasoning <- NULL
              if (is.character(data$delta)) {
                # Volcano Ark format: delta is a string directly
                delta_reasoning <- data$delta
              } else if (is.list(data$delta)) {
                # OpenAI format: delta is an object with text field
                delta_reasoning <- data$delta$text %||% data$delta$summary_text
              }
              if (!is.null(delta_reasoning) && nchar(delta_reasoning) > 0) {
                if (!stream_state$is_reasoning) {
                  callback("<think>\n", done = FALSE)
                  stream_state$is_reasoning <- TRUE
                }
                stream_state$full_reasoning <- paste0(stream_state$full_reasoning, delta_reasoning)
                callback(delta_reasoning, done = FALSE)
              }
            } else if (grepl("^response\\.reasoning", event_type) && grepl("delta$", event_type)) {
              # Catch-all for any other reasoning delta event types
              delta_reasoning <- NULL
              if (is.character(data$delta)) {
                delta_reasoning <- data$delta
              } else if (is.list(data$delta)) {
                delta_reasoning <- data$delta$text %||% data$delta$summary_text
              }
              if (!is.null(delta_reasoning) && nchar(delta_reasoning) > 0) {
                if (!stream_state$is_reasoning) {
                  callback("<think>\n", done = FALSE)
                  stream_state$is_reasoning <- TRUE
                }
                stream_state$full_reasoning <- paste0(stream_state$full_reasoning, delta_reasoning)
                callback(delta_reasoning, done = FALSE)
              }
            } else if (event_type == "response.function_call_arguments.delta") {
              # Tool call arguments streaming
              idx <- (data$output_index %||% 0) + 1
              if (length(stream_state$tool_calls_acc) < idx) {
                stream_state$tool_calls_acc[[idx]] <- list(id = "", name = "", arguments = "")
              }
              if (!is.null(data$delta)) {
                stream_state$tool_calls_acc[[idx]]$arguments <- paste0(
                  stream_state$tool_calls_acc[[idx]]$arguments,
                  data$delta
                )
              }
            } else if (event_type == "response.output_item.done") {
              # Output item completed
              item <- data$item
              if (!is.null(item)) {
                if (item$type == "function_call") {
                  idx <- (data$output_index %||% 0) + 1
                  if (length(stream_state$tool_calls_acc) < idx) {
                    stream_state$tool_calls_acc[[idx]] <- list(id = "", name = "", arguments = "")
                  }
                  stream_state$tool_calls_acc[[idx]]$id <- item$call_id %||% item$id %||% ""
                  stream_state$tool_calls_acc[[idx]]$name <- item$name %||% ""
                  if (!is.null(item$arguments) && is.character(item$arguments)) {
                    stream_state$tool_calls_acc[[idx]]$arguments <- item$arguments
                  }
                } else if (item$type == "reasoning") {
                  # Handle reasoning content from done event (Volcano Ark may send full content here)
                  # Only process if we haven't accumulated reasoning from delta events
                  if (nchar(stream_state$full_reasoning) == 0) {
                    reasoning_text <- NULL
                    # Try summary field first (Volcano Ark format)
                    if (!is.null(item$summary) && is.list(item$summary)) {
                      for (block in item$summary) {
                        if (!is.null(block$text)) {
                          reasoning_text <- paste0(reasoning_text %||% "", block$text)
                        }
                      }
                    }
                    # Try content field (OpenAI format)
                    if (is.null(reasoning_text) && !is.null(item$content) && is.list(item$content)) {
                      for (block in item$content) {
                        if (!is.null(block$text)) {
                          reasoning_text <- paste0(reasoning_text %||% "", block$text)
                        }
                      }
                    }
                    if (!is.null(reasoning_text) && nchar(reasoning_text) > 0) {
                      if (!stream_state$is_reasoning) {
                        callback("<think>\n", done = FALSE)
                        stream_state$is_reasoning <- TRUE
                      }
                      stream_state$full_reasoning <- reasoning_text
                      callback(reasoning_text, done = FALSE)
                    }
                  }
                  # End reasoning section when reasoning item is done
                  if (stream_state$is_reasoning) {
                    callback("\n</think>\n\n", done = FALSE)
                    stream_state$is_reasoning <- FALSE
                  }
                }
              }
            } else if (event_type == "response.completed" || event_type == "response.done") {
              # Response completed - contains full response with output array
              if (!is.null(data$response)) {
                stream_state$response_id <- data$response$id
                stream_state$finish_reason <- data$response$status
                if (!is.null(data$response$usage)) {
                  stream_state$full_usage <- list(
                    prompt_tokens = data$response$usage$input_tokens,
                    completion_tokens = data$response$usage$output_tokens,
                    total_tokens = data$response$usage$total_tokens
                  )
                }
                # If no text/reasoning accumulated from deltas, parse from full output
                # Some providers (e.g., Volcano Ark) may not send delta events
                if (nzchar(stream_state$full_text) == FALSE || nzchar(stream_state$full_reasoning) == FALSE) {
                  parsed <- private$parse_output(data$response)
                  if (nzchar(stream_state$full_text) == FALSE) {
                    stream_state$full_text <- parsed$text
                  }
                  if (nzchar(stream_state$full_reasoning) == FALSE && !is.null(parsed$reasoning)) {
                    stream_state$full_reasoning <- parsed$reasoning
                  }
                }
              }
            }
          }
        },
        timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
        total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
        first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
        idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
      )

      # Update internal state
      if (!is.null(stream_state$response_id)) {
        private$last_response_id <- stream_state$response_id
      }

      # Finalize tool calls
      final_tool_calls <- NULL
      if (length(stream_state$tool_calls_acc) > 0) {
        final_tool_calls <- lapply(stream_state$tool_calls_acc, function(tc) {
          list(
            id = tc$id,
            name = tc$name,
            arguments = parse_tool_arguments(tc$arguments, tool_name = tc$name)
          )
        })
        final_tool_calls <- Filter(function(tc) nzchar(tc$name %||% ""), final_tool_calls)
        if (length(final_tool_calls) == 0) final_tool_calls <- NULL
      }

      GenerateResult$new(
        text = stream_state$full_text,
        reasoning = if (nzchar(stream_state$full_reasoning)) stream_state$full_reasoning else NULL,
        usage = stream_state$full_usage,
        finish_reason = stream_state$finish_reason,
        raw_response = stream_state$last_response,
        tool_calls = final_tool_calls,
        response_id = stream_state$response_id
      )
    },

    #' @description Format a tool execution result for Responses API.
    #' @param tool_call_id The ID of the tool call.
    #' @param tool_name The name of the tool.
    #' @param result_content The result content from executing the tool.
    #' @return A list formatted as a message for Responses API.
    format_tool_result = function(tool_call_id, tool_name, result_content) {
      # For Responses API, tool results are sent as function_call_output in input
      list(
        role = "tool",
        tool_call_id = tool_call_id,
        content = if (is.character(result_content)) result_content else safe_to_json(result_content, auto_unbox = TRUE)
      )
    },

    #' @description Get the message format for Responses API.
    get_history_format = function() {
      "openai_responses"
    }
  )
)

#' @title Stream from Responses API
#' @description
#' Makes a streaming POST request to OpenAI Responses API and processes SSE events.
#' The Responses API uses different event types than Chat Completions.
#'
#' @param url The API endpoint URL.
#' @param headers A named list of HTTP headers.
#' @param body The request body (will be converted to JSON).
#' @param callback A function called for each event: callback(event_type, data, done).
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total stream timeout in seconds.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds.
#' @param connect_timeout_seconds Optional connection-establishment timeout in seconds.
#' @param idle_timeout_seconds Optional stall timeout in seconds.
#' @keywords internal
stream_responses_api <- function(url, headers, body, callback,
                                 timeout_seconds = NULL,
                                 total_timeout_seconds = NULL,
                                 first_byte_timeout_seconds = NULL,
                                 connect_timeout_seconds = NULL,
                                 idle_timeout_seconds = NULL) {
  timeout_config <- resolve_request_timeout_config(
    timeout_seconds = timeout_seconds,
    total_timeout_seconds = total_timeout_seconds,
    first_byte_timeout_seconds = first_byte_timeout_seconds,
    connect_timeout_seconds = connect_timeout_seconds,
    idle_timeout_seconds = idle_timeout_seconds,
    request_type = "stream"
  )
  req <- httr2::request(url)
  req <- httr2::req_headers(req, !!!headers)
  req <- prepare_json_post_request(req, body)
  req <- apply_request_timeout_config(req, timeout_config)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)

  resp <- httr2::req_perform_connection(req)
  on.exit(close(resp), add = TRUE)

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    error_text <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(e) "Unknown error (could not read body)"
    )
    rlang::abort(c(
      paste0("API request failed with status ", status),
      "i" = paste0("URL: ", url),
      "x" = error_text
    ), class = "aisdk_api_error")
  }

  while (!httr2::resp_stream_is_complete(resp)) {
    event <- tryCatch(
      httr2::resp_stream_sse(resp),
      error = function(e) NULL
    )

    if (is.null(event)) next

    event_type <- event$type
    data_str <- event$data

    if (!is.null(data_str) && nzchar(data_str)) {
      if (data_str == "[DONE]") {
        callback(NULL, NULL, done = TRUE)
        break
      }

      tryCatch(
        {
          data <- jsonlite::fromJSON(data_str, simplifyVector = FALSE)
          # Use the event type from the parsed data if available
          actual_type <- data$type %||% event_type
          callback(actual_type, data, done = FALSE)
        },
        error = function(e) {
          # Skip malformed JSON
        }
      )
    }
  }

  callback(NULL, NULL, done = TRUE)
}

#' @title OpenAI Embedding Model
#' @description
#' Embedding model implementation for OpenAI's embeddings API.
#' @keywords internal
OpenAIEmbeddingModel <- R6::R6Class(
  "OpenAIEmbeddingModel",
  inherit = EmbeddingModelV1,
  private = list(
    config = NULL
  ),
  public = list(
    #' @description Initialize the OpenAI embedding model.
    #' @param model_id The model ID (e.g., "text-embedding-3-small").
    #' @param config Configuration list.
    initialize = function(model_id, config) {
      super$initialize(provider = config$provider_name %||% "openai", model_id = model_id)
      private$config <- config
    },

    #' @description Generate embeddings for a value.
    #' @param value A character string or vector to embed.
    #' @return A list with embeddings and usage.
    do_embed = function(value) {
      url <- paste0(private$config$base_url, "/embeddings")
      headers <- list(
        `Content-Type` = "application/json",
        Authorization = paste("Bearer", private$config$api_key)
      )

      body <- list(
        model = self$model_id,
        input = value
      )

      response <- post_to_api(
        url,
        headers,
        body,
        timeout_seconds = private$config$timeout_seconds,
        total_timeout_seconds = private$config$total_timeout_seconds,
        first_byte_timeout_seconds = private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = private$config$connect_timeout_seconds,
        idle_timeout_seconds = private$config$idle_timeout_seconds
      )

      list(
        embeddings = lapply(response$data, function(x) x$embedding),
        usage = response$usage
      )
    }
  )
)

#' @title OpenAI Image Model
#' @description
#' Image model implementation for OpenAI's image generation and editing APIs.
#' @keywords internal
OpenAIImageModel <- R6::R6Class(
  "OpenAIImageModel",
  inherit = ImageModelV1,
  private = list(
    config = NULL,
    last_response_id = NULL,
    get_headers = function(include_content_type = TRUE) {
      h <- list()
      if (nzchar(private$config$api_key %||% "")) {
        h$Authorization <- paste("Bearer", private$config$api_key)
      }
      if (include_content_type) {
        h$`Content-Type` <- "application/json"
      }
      if (!is.null(private$config$organization)) {
        h$`OpenAI-Organization` <- private$config$organization
      }
      if (!is.null(private$config$headers)) {
        h <- c(h, private$config$headers)
      }
      h
    },
    supports_image_response_format = function() {
      provider_name <- tolower(private$config$provider_name %||% "openai")
      base_url <- tolower(private$config$base_url %||% "")
      if (identical(provider_name, "aihubmix")) {
        return(FALSE)
      }
      if (grepl("aihubmix\\.com", base_url)) {
        return(FALSE)
      }
      TRUE
    },
    is_aihubmix_compatible = function() {
      provider_name <- tolower(private$config$provider_name %||% "openai")
      base_url <- tolower(private$config$base_url %||% "")
      identical(provider_name, "aihubmix") || grepl("aihubmix\\.com", base_url)
    },
    aihubmix_size_from_dimensions = function(width = NULL, height = NULL) {
      width <- suppressWarnings(as.numeric(width))
      height <- suppressWarnings(as.numeric(height))
      if (
        length(width) != 1 || length(height) != 1 ||
          is.na(width) || is.na(height) ||
          width <= 0 || height <= 0
      ) {
        return(NULL)
      }

      ratio <- width / height
      if (ratio > 1.15) {
        return("1536x1024")
      }
      if (ratio < 0.87) {
        return("1024x1536")
      }
      "1024x1024"
    },
    normalize_aihubmix_generation_params = function(params) {
      if (!private$is_aihubmix_compatible()) {
        return(params)
      }

      if (is.null(params$size)) {
        params$size <- private$aihubmix_size_from_dimensions(
          width = params$width %||% NULL,
          height = params$height %||% NULL
        )
      }

      if (!is.null(params$size) && identical(tolower(as.character(params$size)[[1]]), "auto")) {
        params$size <- NULL
      }

      if (!is.null(params$transparent_background) && is.null(params$background)) {
        params$background <- if (isTRUE(params$transparent_background)) "transparent" else "opaque"
      }

      params$width <- NULL
      params$height <- NULL
      params$transparent_background <- NULL
      params
    },
    is_gpt_image_2 = function() {
      grepl("(^|/)gpt-image-2($|-)", self$model_id %||% "", perl = TRUE)
    },
    is_gpt_image_family = function() {
      grepl("(^|/)(gpt-image-2|gpt-image-1(\\.5|-mini)?|chatgpt-image-latest)($|-)", self$model_id %||% "", perl = TRUE)
    },
    validate_image_params = function(params, request_type = c("generate", "edit")) {
      request_type <- match.arg(request_type)

      if (!is.null(params$output_compression)) {
        compression <- suppressWarnings(as.numeric(params$output_compression))
        if (is.na(compression) || compression < 0 || compression > 100) {
          rlang::abort("`output_compression` must be a number between 0 and 100.")
        }
        fmt <- tolower(params$output_format %||% "")
        if (!fmt %in% c("jpeg", "jpg", "webp")) {
          rlang::abort("`output_compression` requires `output_format = 'jpeg'` or `output_format = 'webp'`.")
        }
      }

      if (!is.null(params$input_fidelity)) {
        if (request_type != "edit") {
          rlang::abort("`input_fidelity` is only supported for image editing workflows.")
        }
        if (private$is_gpt_image_2()) {
          rlang::abort("`input_fidelity` is fixed for `gpt-image-2` and cannot be overridden.")
        }
      }

      if (!is.null(params$response_format) && private$is_gpt_image_family()) {
        fmt <- tolower(as.character(params$response_format)[[1]])
        if (identical(fmt, "url")) {
          rlang::abort("GPT image models return base64 image payloads; `response_format = 'url'` is not supported.")
        }
      }

      invisible(TRUE)
    },
    build_generation_body = function(params) {
      params <- private$normalize_aihubmix_generation_params(params)
      private$validate_image_params(params, request_type = "generate")

      body <- list(
        model = self$model_id,
        prompt = params$prompt
      )
      if (private$supports_image_response_format()) {
        body$response_format <- params$response_format %||% "b64_json"
      }

      if (!is.null(params$n)) body$n <- params$n
      if (!is.null(params$size)) body$size <- params$size
      if (!is.null(params$quality)) body$quality <- params$quality
      if (!is.null(params$background)) body$background <- params$background
      if (!is.null(params$moderation)) body$moderation <- params$moderation
      if (!is.null(params$output_format)) body$output_format <- params$output_format
      if (!is.null(params$output_compression)) body$output_compression <- params$output_compression

      handled <- c(
        "prompt", "output_dir", "response_format", "n", "size", "quality",
        "background", "moderation", "output_format", "output_compression",
        "timeout_seconds", "total_timeout_seconds", "first_byte_timeout_seconds",
        "connect_timeout_seconds", "idle_timeout_seconds"
      )
      extra <- params[setdiff(names(params), handled)]
      if (length(extra) > 0) {
        body <- utils::modifyList(body, extra)
      }

      body[!sapply(body, is.null)]
    },
    build_responses_tool_config = function(params) {
      # Build the `image_generation` tool config block for the Responses-API
      # fallback. Mirrors the field subset documented for the tool; keeps the
      # same normalize + validate sequence as build_generation_body so behavior
      # is consistent across the two paths.
      params <- private$normalize_aihubmix_generation_params(params)
      private$validate_image_params(params, request_type = "generate")

      tool <- list(
        type = "image_generation",
        model = params$image_model %||% self$model_id
      )
      if (!is.null(params$quality))            tool$quality <- params$quality
      if (!is.null(params$size))               tool$size <- params$size
      if (!is.null(params$output_format))      tool$output_format <- params$output_format
      if (!is.null(params$output_compression)) tool$output_compression <- params$output_compression
      if (!is.null(params$background))         tool$background <- params$background
      if (!is.null(params$moderation))         tool$moderation <- params$moderation
      if (!is.null(params$n))                  tool$n <- params$n

      tool[!sapply(tool, is.null)]
    },
    build_responses_edit_tool_config = function(params, mask_data_url = NULL) {
      # Edit-flavored variant of build_responses_tool_config: sets
      # action = "edit" explicitly (the tool's `auto` default also works,
      # but explicit is safer), forwards `input_fidelity` (only allowed for
      # edits per validate_image_params), and adds `input_image_mask` when a
      # mask was supplied. The source image goes into the request `input`
      # array as an input_image block — not into the tool config.
      private$validate_image_params(params, request_type = "edit")

      tool <- list(
        type = "image_generation",
        action = "edit",
        model = params$image_model %||% self$model_id
      )
      if (!is.null(params$quality))            tool$quality <- params$quality
      if (!is.null(params$size))               tool$size <- params$size
      if (!is.null(params$output_format))      tool$output_format <- params$output_format
      if (!is.null(params$output_compression)) tool$output_compression <- params$output_compression
      if (!is.null(params$background))         tool$background <- params$background
      if (!is.null(params$moderation))         tool$moderation <- params$moderation
      if (!is.null(params$input_fidelity))     tool$input_fidelity <- params$input_fidelity
      if (!is.null(mask_data_url)) {
        tool$input_image_mask <- list(image_url = mask_data_url)
      }

      tool[!sapply(tool, is.null)]
    },
    build_edit_body = function(params) {
      upload_dir <- params$output_dir %||% tempdir()
      private$validate_image_params(params, request_type = "edit")
      image_inputs <- coerce_image_inputs(params$image)
      image_paths <- lapply(seq_along(image_inputs), function(i) {
        materialize_image_upload(
          image_inputs[[i]],
          output_dir = upload_dir,
          prefix = sprintf("openai_image_%02d", i)
        )
      })

      body <- list(
        model = self$model_id,
        prompt = params$prompt %||% "Edit this image."
      )
      if (private$supports_image_response_format()) {
        body$response_format <- params$response_format %||% "b64_json"
      }

      if (length(image_paths) == 1) {
        body$image <- curl::form_file(image_paths[[1]])
      } else {
        body <- c(
          body,
          stats::setNames(lapply(image_paths, curl::form_file), rep("image[]", length(image_paths)))
        )
      }

      if (!is.null(params$mask)) {
        mask_path <- materialize_image_upload(params$mask, output_dir = upload_dir, prefix = "openai_mask")
        body$mask <- curl::form_file(mask_path)
      }

      if (!is.null(params$n)) body$n <- as.character(params$n)
      if (!is.null(params$size)) body$size <- params$size
      if (!is.null(params$quality)) body$quality <- params$quality
      if (!is.null(params$background)) body$background <- params$background
      if (!is.null(params$output_format)) body$output_format <- params$output_format
      if (!is.null(params$output_compression)) body$output_compression <- params$output_compression
      if (!is.null(params$input_fidelity)) body$input_fidelity <- params$input_fidelity

      handled <- c(
        "image", "mask", "prompt", "output_dir", "response_format", "n",
        "size", "quality", "background", "output_format", "output_compression",
        "input_fidelity",
        "timeout_seconds", "total_timeout_seconds", "first_byte_timeout_seconds",
        "connect_timeout_seconds", "idle_timeout_seconds"
      )
      extra <- params[setdiff(names(params), handled)]
      if (length(extra) > 0) {
        body <- c(body, extra)
      }

      body[!sapply(body, is.null)]
    },
    parse_image_response = function(response,
                                    output_dir = tempdir(),
                                    prefix = "openai_image",
                                    requested_output_format = NULL) {
      images <- list()

      if (!is.null(response$data) && length(response$data) > 0) {
        for (item in response$data) {
          artifact <- list(
            revised_prompt = item$revised_prompt %||% NULL
          )

          if (!is.null(item$b64_json)) {
            artifact$bytes <- base64enc::base64decode(item$b64_json)
            artifact$media_type <- switch(item$output_format %||% requested_output_format %||% "",
              png = "image/png",
              jpeg = "image/jpeg",
              jpg = "image/jpeg",
              webp = "image/webp",
              "image/png"
            )
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
    #' @description Initialize the OpenAI image model.
    #' @param model_id The model ID (e.g., "gpt-image-2", "gpt-image-1.5").
    #' @param config Configuration list.
    initialize = function(model_id, config) {
      super$initialize(
        provider = config$provider_name %||% "openai",
        model_id = model_id,
        capabilities = list(
          image_output = TRUE,
          image_edit = TRUE
        )
      )
      private$config <- config
    },

    #' @description Generate images.
    #'
    #' Tries the classic `POST /v1/images/generations` endpoint first. If that
    #' returns a 404 with `invalid_api_path` / "not available" — the signal
    #' some OpenAI-compatible proxies emit when they only expose the newer
    #' Responses API — falls back to `POST /v1/responses` with the
    #' `image_generation` tool and decodes the returned base64 image.
    #'
    #' On the fallback path, the standard image params (`quality`, `size`,
    #' `output_format`, `output_compression`, `background`, `moderation`, `n`)
    #' are forwarded into the tool config, and a `previous_response_id` from a
    #' prior fallback call is auto-attached so multi-turn edits ("now make it
    #' realistic") work the same as on the language-model path. Use
    #' `get_last_response_id()` / `reset()` to inspect or clear that state.
    #'
    #' @param params A list of call options.
    #' @return A GenerateImageResult object.
    do_generate_image = function(params) {
      if (is.null(params$prompt) || !nzchar(params$prompt)) {
        rlang::abort("`prompt` must be a non-empty string.")
      }

      classic <- tryCatch(
        self$do_generate_image_classic(params),
        error = function(e) e
      )
      if (!inherits(classic, "error")) {
        return(classic)
      }

      if (self$looks_like_missing_classic_endpoint(classic)) {
        message(
          "OpenAI image generation: classic /v1/images/generations is unreachable on this endpoint. ",
          "Falling back to /v1/responses with the `image_generation` tool."
        )
        return(self$do_generate_image_via_responses(params))
      }

      stop(classic)
    },

    #' @description Generate images via the classic `POST /v1/images/generations`
    #'   endpoint. Called by `do_generate_image()`; exposed for callers that want
    #'   to bypass the Responses-API fallback on proxies they trust.
    #' @param params A list of call options (see `do_generate_image`).
    #' @return A GenerateImageResult object.
    do_generate_image_classic = function(params) {
      url <- paste0(private$config$base_url, "/images/generations")
      headers <- private$get_headers(include_content_type = TRUE)
      body <- private$build_generation_body(params)
      response <- post_to_api(
        url,
        headers,
        body,
        timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
        total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
        first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
        idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
      )

      GenerateImageResult$new(
        images = private$parse_image_response(
          response,
          output_dir = params$output_dir %||% tempdir(),
          prefix = "openai_image",
          requested_output_format = body$output_format %||% NULL
        ),
        usage = response$usage %||% NULL,
        raw_response = response
      )
    },

    #' @description Generate images via `POST /v1/responses` with the
    #'   `image_generation` tool. Used as a fallback when the classic
    #'   `/v1/images/generations` endpoint is unreachable (e.g. OpenAI-compatible
    #'   proxies that only expose the Responses API).
    #' @param params A list of call options (see `do_generate_image`).
    #' @return A GenerateImageResult object.
    do_generate_image_via_responses = function(params) {
      url <- paste0(private$config$base_url, "/responses")
      # Force identity transfer-encoding: some OpenAI-compatible proxies
      # advertise gzip but send a malformed Content-Encoding header on the
      # /v1/responses route. With `Accept-Encoding: identity` the proxy
      # streams uncompressed and httr2 parses cleanly.
      headers <- c(
        private$get_headers(include_content_type = TRUE),
        list(`Accept-Encoding` = "identity")
      )
      body <- list(
        model = self$model_id,
        input = params$prompt,
        tools = list(private$build_responses_tool_config(params))
      )
      if (!is.null(private$last_response_id)) {
        body$previous_response_id <- private$last_response_id
      }
      response <- post_to_api(
        url,
        headers,
        body,
        timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
        total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
        first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
        idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
      )

      # Record the conversation id even before parsing output, so a callback
      # like "now make it realistic" can chain off this turn even if the
      # current response yielded no image.
      if (!is.null(response$id)) {
        private$last_response_id <- response$id
      }

      requested_format <- tolower(as.character(params$output_format %||% "png")[[1]])
      media_type <- switch(requested_format,
        png = "image/png",
        jpeg = "image/jpeg",
        jpg = "image/jpeg",
        webp = "image/webp",
        "image/png"
      )

      images <- list()
      for (item in response$output %||% list()) {
        if (identical(item$type %||% "", "image_generation_call") && !is.null(item$result)) {
          images <- c(images, list(list(
            bytes = base64enc::base64decode(item$result),
            media_type = media_type,
            revised_prompt = item$revised_prompt %||% NULL
          )))
        }
      }

      if (!length(images)) {
        rlang::abort(c(
          "Responses API returned no `image_generation_call` output.",
          i = "The proxy accepted the request but produced no image; this is usually a model or prompt issue."
        ))
      }

      GenerateImageResult$new(
        images = finalize_image_artifacts(
          images,
          output_dir = params$output_dir %||% tempdir(),
          prefix = "openai_image_responses"
        ),
        usage = response$usage %||% NULL,
        raw_response = response
      )
    },

    #' @description Heuristic check used by `do_generate_image()` /
    #'   `do_edit_image()` to decide whether a classic-endpoint error looks
    #'   like "endpoint not available" on the proxy, in which case the
    #'   Responses-API fallback is taken. Matches both the `/images/generations`
    #'   and `/images/edits` paths.
    #' @param err An error condition raised by the classic-endpoint call.
    #' @return `TRUE` if the error message matches the "missing endpoint" shape.
    looks_like_missing_classic_endpoint = function(err) {
      msg <- conditionMessage(err) %||% ""
      isTRUE(grepl("404", msg, fixed = TRUE)) &&
        (grepl("invalid_api_path", msg, fixed = TRUE) ||
         grepl("not available", msg, fixed = TRUE) ||
         grepl("images/(generations|edits)", msg))
    },

    #' @description Return the most recent Responses-API response id captured
    #' during the `/v1/responses` fallback path. Used to chain multi-turn
    #' image edits via `previous_response_id`.
    #' @return Character scalar or `NULL` if no fallback call has succeeded yet.
    get_last_response_id = function() {
      private$last_response_id
    },

    #' @description Clear any stored `previous_response_id`, ending the current
    #' multi-turn image session on the Responses-API fallback path.
    #' @return The model, invisibly.
    reset = function() {
      private$last_response_id <- NULL
      invisible(self)
    },

    #' @description Edit images.
    #'
    #' Tries the classic `POST /v1/images/edits` multipart endpoint first.
    #' If that returns the same "missing endpoint" 404 signal handled by
    #' `do_generate_image()`, falls back to `POST /v1/responses` with the
    #' source image inlined as an `input_image` data URL and the optional
    #' mask passed via `input_image_mask` on the `image_generation` tool.
    #'
    #' The Responses fallback accepts a single source image per turn
    #' (multi-reference edit is classic-only). Image params (`quality`,
    #' `size`, `output_format`, `background`, `output_compression`,
    #' `moderation`, `input_fidelity`) are forwarded into the tool config,
    #' and `previous_response_id` is auto-attached from any prior fallback
    #' call so iterative edits chain.
    #'
    #' @param params A list of call options.
    #' @return A GenerateImageResult object.
    do_edit_image = function(params) {
      if (is.null(params$image)) {
        rlang::abort("`image` must be supplied for OpenAI image editing.")
      }

      classic <- tryCatch(
        self$do_edit_image_classic(params),
        error = function(e) e
      )
      if (!inherits(classic, "error")) {
        return(classic)
      }

      if (self$looks_like_missing_classic_endpoint(classic)) {
        message(
          "OpenAI image edit: classic /v1/images/edits is unreachable on this endpoint. ",
          "Falling back to /v1/responses with the `image_generation` tool."
        )
        return(self$do_edit_image_via_responses(params))
      }

      stop(classic)
    },

    #' @description Edit images via the classic `POST /v1/images/edits`
    #'   multipart endpoint. Called by `do_edit_image()`; exposed for callers
    #'   that want to bypass the Responses-API fallback.
    #' @param params A list of call options (see `do_edit_image`).
    #' @return A GenerateImageResult object.
    do_edit_image_classic = function(params) {
      url <- paste0(private$config$base_url, "/images/edits")
      headers <- private$get_headers(include_content_type = FALSE)
      body <- private$build_edit_body(params)
      response <- post_multipart_to_api(
        url,
        headers,
        body,
        timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
        total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
        first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
        idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
      )

      GenerateImageResult$new(
        images = private$parse_image_response(
          response,
          output_dir = params$output_dir %||% tempdir(),
          prefix = "openai_edit",
          requested_output_format = body$output_format %||% NULL
        ),
        usage = response$usage %||% NULL,
        raw_response = response
      )
    },

    #' @description Stream image generation with partial-image previews via
    #'   `POST /v1/responses`. Sets `stream = TRUE` and `partial_images` on
    #'   the `image_generation` tool config; dispatches SSE events to the
    #'   user-supplied `callback` (one per partial frame, one final). Uses
    #'   the same Responses-API path as the non-streaming fallback, with the
    #'   same `previous_response_id` chaining.
    #' @param params A list of call options. The `partial_images` field
    #'   (0–3) controls how many preview frames the API emits before the
    #'   final image; default `2`.
    #' @param callback A function receiving each event list.
    #' @return A GenerateImageResult with the final image.
    do_stream_image = function(params, callback) {
      if (is.null(params$prompt) || !nzchar(params$prompt)) {
        rlang::abort("`prompt` must be a non-empty string.")
      }

      url <- paste0(private$config$base_url, "/responses")
      headers <- c(
        private$get_headers(include_content_type = TRUE),
        list(`Accept-Encoding` = "identity")
      )

      tool_cfg <- private$build_responses_tool_config(params)
      partial_n <- as.integer(params$partial_images %||% 2)
      if (is.na(partial_n) || partial_n < 0 || partial_n > 3) {
        rlang::abort("`partial_images` must be an integer in 0..3.")
      }
      if (partial_n > 0) {
        tool_cfg$partial_images <- partial_n
      }

      body <- list(
        model = self$model_id,
        input = params$prompt,
        stream = TRUE,
        tools = list(tool_cfg)
      )
      if (!is.null(private$last_response_id)) {
        body$previous_response_id <- private$last_response_id
      }

      requested_format <- tolower(as.character(params$output_format %||% "png")[[1]])
      media_type <- switch(requested_format,
        png = "image/png",
        jpeg = "image/jpeg",
        jpg = "image/jpeg",
        webp = "image/webp",
        "image/png"
      )

      state <- new.env(parent = emptyenv())
      state$partial_count <- 0L
      state$final_images <- list()
      state$response_id <- NULL
      state$usage <- NULL

      stream_responses_api(
        url = url,
        headers = headers,
        body = body,
        timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
        total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
        first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
        idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds,
        callback = function(event_type, data, done) {
          if (isTRUE(done)) return(invisible(NULL))
          if (is.null(event_type) || is.null(data)) return(invisible(NULL))

          # Pick up the response id from response.created (or the first event
          # that carries it) so multi-turn chaining works even if completion
          # arrives via a different event shape.
          if (is.null(state$response_id)) {
            state$response_id <- data$response$id %||% data$id %||% NULL
          }

          # Partial previews. The documented event type is
          # `response.image_generation_call.partial_image` with a
          # `partial_image_b64` field; accept a few aliases for proxy
          # quirks.
          if (grepl("partial_image", event_type, fixed = TRUE)) {
            b64 <- data$partial_image_b64 %||% data$b64 %||% data$result
            if (!is.null(b64) && nzchar(b64)) {
              bytes <- base64enc::base64decode(b64)
              state$partial_count <- state$partial_count + 1L
              idx <- as.integer(data$partial_image_index %||% state$partial_count)
              callback(list(
                type = "partial",
                index = idx,
                bytes = bytes,
                media_type = media_type,
                done = FALSE
              ))
            }
            return(invisible(NULL))
          }

          # Per-call completion event.
          if (event_type == "response.image_generation_call.completed" &&
              !is.null(data$result)) {
            state$final_images <- c(state$final_images, list(list(
              bytes = base64enc::base64decode(data$result),
              media_type = media_type,
              revised_prompt = data$revised_prompt %||% NULL
            )))
            return(invisible(NULL))
          }

          # Final response envelope — has usage and any image calls we
          # haven't already collected via the per-call completion event.
          if (event_type == "response.completed" && is.list(data$response)) {
            if (is.null(state$response_id)) state$response_id <- data$response$id
            state$usage <- data$response$usage %||% NULL
            if (!length(state$final_images)) {
              for (item in data$response$output %||% list()) {
                if (identical(item$type %||% "", "image_generation_call") &&
                    !is.null(item$result)) {
                  state$final_images <- c(state$final_images, list(list(
                    bytes = base64enc::base64decode(item$result),
                    media_type = media_type,
                    revised_prompt = item$revised_prompt %||% NULL
                  )))
                }
              }
            }
          }

          invisible(NULL)
        }
      )

      if (!is.null(state$response_id)) {
        private$last_response_id <- state$response_id
      }

      if (!length(state$final_images)) {
        rlang::abort(c(
          "Streaming completed but no final image was received.",
          i = "The API may have sent partials only, or the connection ended before the final image_generation_call event."
        ))
      }

      callback(list(
        type = "completed",
        bytes = state$final_images[[1]]$bytes,
        media_type = media_type,
        done = TRUE
      ))

      GenerateImageResult$new(
        images = finalize_image_artifacts(
          state$final_images,
          output_dir = params$output_dir %||% tempdir(),
          prefix = "openai_image_stream"
        ),
        usage = state$usage,
        raw_response = list(response_id = state$response_id, partial_count = state$partial_count)
      )
    },

    #' @description Edit images via `POST /v1/responses` with the
    #'   `image_generation` tool in edit mode. Inlines the source image as a
    #'   base64 data URL inside an `input_image` block; passes the optional
    #'   mask via the tool's `input_image_mask` field.
    #' @param params A list of call options (see `do_edit_image`).
    #' @return A GenerateImageResult object.
    do_edit_image_via_responses = function(params) {
      url <- paste0(private$config$base_url, "/responses")
      headers <- c(
        private$get_headers(include_content_type = TRUE),
        list(`Accept-Encoding` = "identity")
      )

      upload_dir <- params$output_dir %||% tempdir()
      image_inputs <- coerce_image_inputs(params$image)
      if (length(image_inputs) > 1) {
        rlang::warn(
          "Responses-API image edit accepts a single source image per turn; using only the first. Use the classic endpoint for multi-reference edits."
        )
      }
      src_data_url <- normalize_image_input_to_url_like(image_inputs[[1]])
      if (!grepl("^(data:|https?:)", src_data_url)) {
        # normalize_image_input_to_url_like returns either a URL/data-URI or
        # a bare base64 string — but for the Responses input_image block we
        # always want a data URI. Wrap the bare path case.
        src_data_url <- paste0("data:image/png;base64,", base64enc::base64encode(src_data_url))
      }

      mask_data_url <- NULL
      if (!is.null(params$mask)) {
        mask_inputs <- coerce_image_inputs(params$mask, arg = "`mask`")
        mask_data_url <- normalize_image_input_to_url_like(mask_inputs[[1]])
        if (!grepl("^(data:|https?:)", mask_data_url)) {
          mask_data_url <- paste0("data:image/png;base64,", base64enc::base64encode(mask_data_url))
        }
      }

      body <- list(
        model = self$model_id,
        input = list(list(
          role = "user",
          content = list(
            list(type = "input_text", text = params$prompt %||% "Edit this image."),
            list(type = "input_image", image_url = src_data_url)
          )
        )),
        tools = list(private$build_responses_edit_tool_config(params, mask_data_url))
      )
      if (!is.null(private$last_response_id)) {
        body$previous_response_id <- private$last_response_id
      }
      response <- post_to_api(
        url,
        headers,
        body,
        timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
        total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
        first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
        idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
      )

      if (!is.null(response$id)) {
        private$last_response_id <- response$id
      }

      requested_format <- tolower(as.character(params$output_format %||% "png")[[1]])
      media_type <- switch(requested_format,
        png = "image/png",
        jpeg = "image/jpeg",
        jpg = "image/jpeg",
        webp = "image/webp",
        "image/png"
      )

      images <- list()
      for (item in response$output %||% list()) {
        if (identical(item$type %||% "", "image_generation_call") && !is.null(item$result)) {
          images <- c(images, list(list(
            bytes = base64enc::base64decode(item$result),
            media_type = media_type,
            revised_prompt = item$revised_prompt %||% NULL
          )))
        }
      }

      if (!length(images)) {
        rlang::abort(c(
          "Responses API returned no `image_generation_call` output for the edit request.",
          i = "The proxy accepted the request but produced no edited image; this is usually a model or prompt issue."
        ))
      }

      GenerateImageResult$new(
        images = finalize_image_artifacts(
          images,
          output_dir = upload_dir,
          prefix = "openai_edit_responses"
        ),
        usage = response$usage %||% NULL,
        raw_response = response
      )
    }
  )
)

#' @title OpenAI Provider Class
#' @description
#' Provider class for OpenAI. Can create language and embedding models.
#' @export
OpenAIProvider <- R6::R6Class(
  "OpenAIProvider",
  public = list(
    #' @field specification_version Provider spec version.
    specification_version = "v1",

    #' @description Initialize the OpenAI provider.
    #' @param api_key OpenAI API key. Defaults to OPENAI_API_KEY env var.
    #' @param base_url Base URL for API calls. Defaults to https://api.openai.com/v1.
    #' @param organization Optional OpenAI organization ID.
    #' @param project Optional OpenAI project ID.
    #' @param headers Optional additional headers.
    #' @param name Optional provider name override (for compatible APIs).
    #' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
    #' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
    #' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
    #' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
    #' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
    #' @param disable_stream_options Disable stream_options parameter (for providers that don't support it).
    #' @param api_format Default API surface for `smart_model()` / `model()`:
    #'   "auto" (route reasoning models to Responses, others to Chat — the
    #'   canonical OpenAI behavior), "chat" (always Chat Completions — useful
    #'   for proxies that don't expose /responses, or that surface reasoning
    #'   models like gpt-5.x via /chat/completions), or "responses" (always
    #'   Responses API). The explicit `language_model()` and `responses_model()`
    #'   methods continue to ignore this setting.
    initialize = function(api_key = NULL,
                          base_url = NULL,
                          organization = NULL,
                          project = NULL,
                          headers = NULL,
                          name = NULL,
                          timeout_seconds = NULL,
                          total_timeout_seconds = NULL,
                          first_byte_timeout_seconds = NULL,
                          connect_timeout_seconds = NULL,
                          idle_timeout_seconds = NULL,
                          disable_stream_options = FALSE,
                          api_format = c("auto", "chat", "responses")) {
      api_format <- match.arg(api_format)
      private$config <- list(
        api_key = api_key %||% Sys.getenv("OPENAI_API_KEY"),
        base_url = sub("/$", "", base_url %||% Sys.getenv("OPENAI_BASE_URL", "https://api.openai.com/v1")),
        organization = organization,
        project = project,
        headers = headers,
        provider_name = name %||% "openai",
        timeout_seconds = timeout_seconds,
        total_timeout_seconds = total_timeout_seconds,
        first_byte_timeout_seconds = first_byte_timeout_seconds,
        connect_timeout_seconds = connect_timeout_seconds,
        idle_timeout_seconds = idle_timeout_seconds,
        disable_stream_options = disable_stream_options,
        api_format = api_format
      )

      if (nchar(private$config$api_key) == 0) {
        rlang::warn("OpenAI API key not set. Set OPENAI_API_KEY env var or pass api_key parameter.")
      }
    },

    #' @description Create a language model (always Chat Completions API).
    #' @param model_id The model ID (e.g., "gpt-4o", "gpt-4o-mini").
    #' @return An OpenAILanguageModel object.
    language_model = function(model_id = Sys.getenv("OPENAI_MODEL", "gpt-4o")) {
      OpenAILanguageModel$new(model_id, private$config)
    },

    #' @description Create a language model using the Responses API.
    #' @param model_id The model ID (e.g., "o1", "o3-mini", "gpt-4o").
    #' @return An OpenAIResponsesLanguageModel object.
    #' @details
    #' The Responses API is designed for:
    #' - Models with built-in reasoning (o1, o3 series)
    #' - Stateful multi-turn conversations (server maintains history)
    #' - Advanced features like structured outputs
    #'
    #' The model maintains conversation state internally via response IDs.
    #' Call `model$reset()` to start a fresh conversation.
    responses_model = function(model_id) {
      OpenAIResponsesLanguageModel$new(model_id, private$config)
    },

    #' @description Default-route factory. Picks chat vs responses based on the
    #' `api_format` set in `create_openai()`. Recommended entry point for
    #' callers that don't care which surface is used.
    #' @param model_id The model ID. Defaults to `OPENAI_MODEL` env var.
    #' @return A LanguageModelV1 instance.
    model = function(model_id = Sys.getenv("OPENAI_MODEL", "gpt-4o")) {
      self$smart_model(model_id, api_format = private$config$api_format %||% "auto")
    },

    #' @description Smart model factory that selects the API surface.
    #' @param model_id The model ID.
    #' @param api_format API format: "auto" (default — reasoning → Responses,
    #'   others → Chat), "chat", or "responses". Defaults to the
    #'   `api_format` passed to `create_openai()`.
    #' @return A language model object (either OpenAILanguageModel or OpenAIResponsesLanguageModel).
    #' @details
    #' When `api_format = "auto"`, the method picks:
    #' - Responses API for reasoning models (o1, o3, gpt-5, ...)
    #' - Chat Completions API for everything else
    #'
    #' Override per-call when the provider's default doesn't match the
    #' specific model you're about to use.
    smart_model = function(model_id,
                           api_format = private$config$api_format %||% "auto") {
      api_format <- match.arg(api_format, c("auto", "chat", "responses"))

      if (api_format == "auto") {
        # Reasoning models use Responses API
        # Pattern matches: o1, o3, o1-mini, o3-mini, o1-preview, gpt-5, gpt-5-mini, etc.
        is_reasoning_model <- grepl("^o[0-9]|^gpt-5", model_id, ignore.case = TRUE)

        if (is_reasoning_model) {
          api_format <- "responses"
        } else {
          api_format <- "chat"
        }
      }

      switch(api_format,
        "chat" = self$language_model(model_id),
        "responses" = self$responses_model(model_id),
        stop("Unknown api_format: ", api_format)
      )
    },

    #' @description Create an embedding model.
    #' @param model_id The model ID (e.g., "text-embedding-3-small").
    #' @return An OpenAIEmbeddingModel object.
    embedding_model = function(model_id = "text-embedding-3-small") {
      OpenAIEmbeddingModel$new(model_id, private$config)
    },

    #' @description Create an image model.
    #' @param model_id The model ID (e.g., "gpt-image-2", "gpt-image-1.5").
    #' @return An OpenAIImageModel object.
    image_model = function(model_id = Sys.getenv("OPENAI_IMAGE_MODEL", "gpt-image-2")) {
      OpenAIImageModel$new(model_id, private$config)
    },

    #' @description Create a server-side conversation object via
    #'   `POST /v1/conversations`. Returns the parsed response, including the
    #'   conversation `id` you can pass as `conversation = "conv_..."` to
    #'   `generate_text()` / `stream_text()` so OpenAI manages the message
    #'   history server-side instead of you sending the full transcript each
    #'   turn.
    #' @param items Optional list of initial conversation items, each shaped
    #'   like `list(type = "message", role = "user", content = "Hello!")`.
    #' @param metadata Optional named list (up to 16 keys, values stringified).
    #' @return Parsed response list with at least `id`, `object`, `created_at`,
    #'   `metadata`.
    create_conversation = function(items = NULL, metadata = NULL) {
      body <- list()
      if (!is.null(items))    body$items    <- items
      if (!is.null(metadata)) body$metadata <- metadata
      private$request_conversations_api("POST", path = "", body = body)
    },

    #' @description Retrieve a conversation object by id.
    #' @param conversation_id Conversation id returned from `create_conversation()`.
    #' @return Parsed response list.
    get_conversation = function(conversation_id) {
      if (!is.character(conversation_id) || !nzchar(conversation_id)) {
        rlang::abort("`conversation_id` must be a non-empty string.")
      }
      private$request_conversations_api("GET", path = conversation_id)
    },

    #' @description Delete a conversation object by id. Server-side history
    #'   is irrecoverable after this call.
    #' @param conversation_id Conversation id returned from `create_conversation()`.
    #' @return Parsed response list (typically `list(id, object, deleted)`).
    delete_conversation = function(conversation_id) {
      if (!is.character(conversation_id) || !nzchar(conversation_id)) {
        rlang::abort("`conversation_id` must be a non-empty string.")
      }
      private$request_conversations_api("DELETE", path = conversation_id)
    }
  ),
  private = list(
    config = NULL,
    request_conversations_api = function(method, path = "", body = NULL) {
      base <- private$config$base_url
      url <- paste0(base, "/conversations", if (nzchar(path)) paste0("/", path) else "")
      headers <- list(`Content-Type` = "application/json")
      if (nzchar(private$config$api_key %||% "")) {
        headers$Authorization <- paste("Bearer", private$config$api_key)
      }
      if (!is.null(private$config$organization)) {
        headers$`OpenAI-Organization` <- private$config$organization
      }
      if (!is.null(private$config$headers)) {
        headers <- c(headers, private$config$headers)
      }

      req <- httr2::request(url)
      req <- httr2::req_headers(req, !!!headers)
      req <- httr2::req_method(req, method)
      if (!is.null(body) && length(body) > 0) {
        req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
      }
      req <- httr2::req_error(req, is_error = function(resp) FALSE)

      resp <- httr2::req_perform(req)
      status <- httr2::resp_status(resp)
      if (status >= 400) {
        error_text <- tryCatch(
          httr2::resp_body_string(resp),
          error = function(e) "Unknown error (could not read body)"
        )
        rlang::abort(c(
          paste0("Conversations API ", method, " ", url, " failed with status ", status),
          x = error_text
        ), class = "aisdk_api_error")
      }
      resp_text <- httr2::resp_body_string(resp)
      if (!nzchar(resp_text)) return(invisible(NULL))
      jsonlite::fromJSON(resp_text, simplifyVector = FALSE)
    }
  )
)

#' @title Create OpenAI Provider
#' @description
#' Factory function to create an OpenAI provider.
#'
#' @eval generate_model_docs("openai")
#'
#' @section Token Limit Parameters:
#' The SDK provides a unified `max_tokens` parameter that automatically maps to the
#' correct API field based on the model and API type:
#'
#' \itemize{
#'   \item **Chat API (standard models)**: `max_tokens` -> `max_tokens`
#'   \item **Chat API (o1/o3 models)**: `max_tokens` -> `max_completion_tokens`
#'   \item **Responses API**: `max_tokens` -> `max_output_tokens` (total: reasoning + answer)
#' }
#'
#' For advanced users who need fine-grained control:
#' \itemize{
#'   \item `max_completion_tokens`: Explicitly set completion tokens (Chat API, o1/o3)
#'   \item `max_output_tokens`: Explicitly set total output limit (Responses API)
#'   \item `max_answer_tokens`: Limit answer only, excluding reasoning (Responses API, Volcengine-specific)
#' }
#'
#' @param api_key OpenAI API key. Defaults to OPENAI_API_KEY env var.
#' @param base_url Base URL for API calls. Defaults to https://api.openai.com/v1.
#' @param organization Optional OpenAI organization ID.
#' @param project Optional OpenAI project ID.
#' @param headers Optional additional headers.
#' @param name Optional provider name override (for compatible APIs).
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
#' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
#' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
#' @param disable_stream_options Disable stream_options parameter (for providers like Volcengine that don't support it).
#' @param api_format Default API surface for `smart_model()` / `model()`: `"auto"` (default, picks Chat or Responses based on model), `"chat"` (always Chat Completions), or `"responses"` (always Responses API).
#' @return An OpenAIProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # Basic usage with Chat Completions API
#'   openai <- create_openai(api_key = "sk-...")
#'   model <- openai$language_model("gpt-4o")
#'   result <- generate_text(model, "Hello!")
#'
#'   # Using Responses API for reasoning models
#'   openai <- create_openai()
#'   model <- openai$responses_model("o1")
#'   result <- generate_text(model, "Solve this math problem...")
#'   print(result$reasoning) # Access chain-of-thought
#'
#'   # Smart model selection (auto-detects best API)
#'   model <- openai$smart_model("o3-mini") # Uses Responses API
#'   model <- openai$smart_model("gpt-4o") # Uses Chat Completions API
#'
#'   # Token limits - unified interface
#'   # For standard models: limits generated content
#'   result <- model$generate(messages = msgs, max_tokens = 1000)
#'
#'   # For o1/o3 models: automatically maps to max_completion_tokens
#'   model_o1 <- openai$language_model("o1")
#'   result <- model_o1$generate(messages = msgs, max_tokens = 2000)
#'
#'   # For Responses API: automatically maps to max_output_tokens (total limit)
#'   model_resp <- openai$responses_model("o1")
#'   result <- model_resp$generate(messages = msgs, max_tokens = 2000)
#'
#'   # Advanced: explicitly control answer-only limit (Volcengine Responses API)
#'   result <- model_resp$generate(messages = msgs, max_answer_tokens = 500)
#'
#'   # Multi-turn conversation with Responses API
#'   model <- openai$responses_model("o1")
#'   result1 <- generate_text(model, "What is 2+2?")
#'   result2 <- generate_text(model, "Now multiply that by 3") # Remembers context
#'   model$reset() # Start fresh conversation
#' }
#' }
create_openai <- function(api_key = NULL,
                          base_url = NULL,
                          organization = NULL,
                          project = NULL,
                          headers = NULL,
                          name = NULL,
                          timeout_seconds = NULL,
                          total_timeout_seconds = NULL,
                          first_byte_timeout_seconds = NULL,
                          connect_timeout_seconds = NULL,
                          idle_timeout_seconds = NULL,
                          disable_stream_options = FALSE,
                          api_format = c("auto", "chat", "responses")) {
  api_format <- match.arg(api_format)
  OpenAIProvider$new(
    api_key = api_key,
    base_url = base_url,
    organization = organization,
    project = project,
    headers = headers,
    name = name,
    timeout_seconds = timeout_seconds,
    total_timeout_seconds = total_timeout_seconds,
    first_byte_timeout_seconds = first_byte_timeout_seconds,
    connect_timeout_seconds = connect_timeout_seconds,
    idle_timeout_seconds = idle_timeout_seconds,
    disable_stream_options = disable_stream_options,
    api_format = api_format
  )
}

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
