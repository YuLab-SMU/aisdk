# --- Responses API server-side state helpers --------------------------------
# Shared by the Responses language model and image model. The Responses
# protocol is a spectrum: official OpenAI keeps conversation state server-side
# (via `previous_response_id`), some compatible proxies are HTTP-stateless and
# require the full transcript every turn, and some reject `previous_response_id`
# outright. These helpers gate whether server-side state is used and recognize
# the rejection so callers can self-heal.
#
# `responses_state_mode` (read from config):
#   "stateless" - never send `previous_response_id`; always resend full history.
#   "server"    - send `previous_response_id` when available (send only the new
#                 turn), degrading to stateless if the endpoint rejects it.
#   "auto"      - same as "server" (kept distinct for forward-compatibility /
#                 wizard wording). Default for first-party `create_openai()`.
responses_normalize_state_mode <- function(mode) {
  mode <- tolower(trimws(as.character(mode %||% "stateless")))
  if (length(mode) != 1L || !mode %in% c("stateless", "server", "auto")) {
    mode <- "stateless"
  }
  mode
}

# TRUE when an error indicates the endpoint rejected `previous_response_id`
# (HTTP-stateless Responses proxies), so the caller can drop it and retry with
# the full transcript.
responses_previous_id_rejected <- function(e) {
  msg <- tolower(conditionMessage(e) %||% "")
  grepl("previous_response_id", msg, fixed = TRUE)
}

responses_previous_id_unsupported <- function(e) {
  msg <- tolower(conditionMessage(e) %||% "")
  responses_previous_id_rejected(e) &&
    (
      inherits(e, "aisdk_api_compatibility_error") ||
        grepl("unsupported|not supported|only supported|unknown parameter|unknown_parameter|unrecognized", msg)
    )
}

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
    # Set once an endpoint rejects `previous_response_id`; from then on this
    # model instance stays stateless (resends full history) for the session.
    state_unsupported = FALSE,
    # Whether to send `previous_response_id` this turn (mode-gated + self-heal).
    use_server_state = function() {
      mode <- responses_normalize_state_mode(private$config$responses_state_mode)
      mode %in% c("server", "auto") && !isTRUE(private$state_unsupported)
    },
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
    format_input = function(messages, only_new = FALSE) {
      # Responses API accepts input in multiple formats:
      # 1. Simple string: "Hello"
      # 2. Array of input items with roles
      # We convert standard messages to the array format

      # When `only_new` is TRUE the caller is sending `previous_response_id`, so
      # the server already holds the conversation up to the last assistant turn.
      # Send only the messages after it (keeping any system message, which sets
      # per-call instructions) to avoid paying for the whole transcript twice.
      if (isTRUE(only_new)) {
        last_assistant <- 0L
        for (i in seq_along(messages)) {
          if (identical(messages[[i]]$role, "assistant")) {
            last_assistant <- i
          }
        }
        if (last_assistant > 0L) {
          messages <- Filter(Negate(is.null), lapply(seq_along(messages), function(i) {
            if (i > last_assistant || identical(messages[[i]]$role, "system")) {
              messages[[i]]
            } else {
              NULL
            }
          }))
        }
      }

      input_items <- list()
      system_instructions <- NULL

      append_responses_tool_calls <- function(tool_calls) {
        for (tc in tool_calls %||% list()) {
          fn <- tc$`function` %||% list()
          name <- tc$name %||% fn$name %||% ""
          arguments <- tc$arguments %||% fn$arguments %||% "{}"
          if (!is.character(arguments)) {
            arguments <- safe_to_json(arguments %||% list(), auto_unbox = TRUE)
          } else {
            arguments <- as.character(arguments)
          }
          input_items <<- c(input_items, list(list(
            type = "function_call",
            call_id = tc$call_id %||% tc$id,
            name = name,
            arguments = arguments
          )))
        }
      }

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
          # Assistant turns replayed as input must use `output_text`, not
          # `input_text`. The Responses API rejects assistant content typed as
          # `input_text` (HTTP-stateless proxies surface this as a 5xx), so we
          # build the output content explicitly here rather than via
          # translate_message_content() (which always emits `input_text`).
          assistant_text <- if (is.character(msg$content) || is.null(msg$content)) {
            paste(msg$content %||% "", collapse = "")
          } else {
            content_blocks_to_text(msg$content, arg_name = "assistant")
          }
          if (nzchar(assistant_text)) {
            input_items <- c(input_items, list(list(
              type = "message",
              role = "assistant",
              content = list(list(type = "output_text", text = assistant_text))
            )))
          }
          if (length(msg$tool_calls %||% list()) > 0) {
            append_responses_tool_calls(msg$tool_calls)
          }
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
            item_id = item$id %||% NULL,
            call_id = item$call_id %||% NULL,
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

    responses_thinking_enabled = function(thinking, default = FALSE) {
      if (is.null(thinking)) {
        return(isTRUE(default))
      }
      if (isTRUE(thinking)) {
        return(TRUE)
      }
      if (identical(thinking, FALSE)) {
        return(FALSE)
      }
      if (is.character(thinking) && length(thinking) == 1) {
        return(tolower(trimws(thinking)) %in% c("on", "true", "1", "yes", "enabled"))
      }
      if (is.list(thinking) && !is.null(thinking$type)) {
        return(tolower(as.character(thinking$type)) %in% c("enabled", "on", "auto"))
      }
      FALSE
    },

    # Build the Responses-API `reasoning` body block from flat (reasoning_effort,
    # reasoning_summary), nested (reasoning = list(...)), and the SDK-level
    # `thinking` switch. OpenAI only exposes visible thinking via reasoning
    # summaries; for reasoning models, request the default summary stream
    # unless thinking was explicitly disabled so the existing <think> renderer
    # has content.
    build_responses_reasoning_block = function(params) {
      nested <- list_get_exact(params, "reasoning")
      if (!is.null(nested) && !is.list(nested)) {
        rlang::abort("`reasoning` must be a named list (e.g. list(effort = \"low\", summary = \"auto\")).")
      }
      effort <- list_get_exact(params, "reasoning_effort") %||% nested$effort
      summary <- list_get_exact(params, "reasoning_summary") %||% nested$summary
      if (is.null(summary) &&
          isTRUE(self$has_capability("is_reasoning_model")) &&
          private$responses_thinking_enabled(list_get_exact(params, "thinking"), default = TRUE)) {
        summary <- "auto"
      }
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
    },

    # Shared body builder for the streaming and non-streaming Responses
    # payloads. `use_server_state` controls both how much history goes into
    # `input` (only the new turn when the server already holds the transcript)
    # and whether `previous_response_id` is injected; retries after a rejected
    # `previous_response_id` rebuild with use_server_state = FALSE.
    build_responses_body = function(params, stream = FALSE, use_server_state = FALSE) {
      formatted <- private$format_input(params$messages, only_new = use_server_state)

      body <- list(
        model = self$model_id,
        input = formatted$input
      )
      if (isTRUE(stream)) {
        body$stream <- TRUE
      }

      # Add system instructions if present
      if (!is.null(formatted$instructions)) {
        body$instructions <- formatted$instructions
      }

      # Inject previous_response_id only when server-side state is in effect.
      if (use_server_state) {
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
      for (nm in c("temperature", "top_p", "presence_penalty", "frequency_penalty")) {
        if (!is.null(params[[nm]]) && !is_reasoning) {
          body[[nm]] <- params[[nm]]
        }
      }

      # Smart token limit mapping for the Responses API. Some backends
      # (e.g. Volcengine) expose two mutually exclusive parameters:
      # - max_output_tokens: total limit (reasoning + answer) — safe default
      # - max_tokens: answer-only limit (excludes reasoning)
      # The unified `max_tokens` maps to `max_output_tokens` so runaway
      # reasoning costs stay bounded; explicit overrides win.
      if (!is.null(params[["max_output_tokens"]])) {
        body$max_output_tokens <- params[["max_output_tokens"]]
      } else if (!is.null(params[["max_answer_tokens"]])) {
        body$max_tokens <- params[["max_answer_tokens"]]
      } else if (!is.null(params[["max_tokens"]])) {
        body$max_output_tokens <- params[["max_tokens"]]
      }

      # Add tools if provided (Responses API format)
      if (!is.null(params$tools) && length(params$tools) > 0) {
        body$tools <- lapply(unname(params$tools), function(t) {
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

      # Pass through extra parameters (e.g., store). Sampling params never
      # appear here: they are in handled_params.
      handled_params <- c(
        "messages", "temperature", "top_p", "presence_penalty", "frequency_penalty",
        "max_tokens", "max_output_tokens", "max_answer_tokens",
        "tools", "stream", "model",
        "reasoning", "reasoning_effort", "reasoning_summary", "thinking", "thinking_budget", "include",
        "conversation",
        "timeout_seconds", "total_timeout_seconds", "first_byte_timeout_seconds",
        "connect_timeout_seconds", "idle_timeout_seconds"
      )
      extra_params <- params[setdiff(names(params), handled_params)]
      if (length(extra_params) > 0) {
        body <- utils::modifyList(body, extra_params)
      }

      body[!sapply(body, is.null)]
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
      url <- api_endpoint_urls(private$config, "/responses")
      headers <- private$get_headers()

      # Decide up front whether this turn uses server-side state, since it
      # changes how much history we serialize into `input`.
      use_server_state <- private$use_server_state() && !is.null(private$last_response_id)

      body <- private$build_responses_body(
        params, stream = FALSE, use_server_state = use_server_state
      )

      do_post <- function(req_body) {
        post_to_api(
          url,
          headers,
          req_body,
          timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
          total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
          first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
          connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
          idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
        )
      }

      response <- if (use_server_state) {
        tryCatch(
          do_post(body),
          aisdk_api_error = function(e) {
            if (!responses_previous_id_rejected(e)) stop(e)
            # Endpoint is HTTP-stateless: drop server state, stay stateless for
            # the rest of the session, and retry once with the full transcript.
            private$last_response_id <- NULL
            private$state_unsupported <- responses_previous_id_unsupported(e)
            do_post(private$build_responses_body(params, stream = FALSE))
          }
        )
      } else {
        do_post(body)
      }

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
      url <- api_endpoint_urls(private$config, "/responses")
      headers <- private$get_headers()

      use_server_state <- private$use_server_state() && !is.null(private$last_response_id)

      body <- private$build_responses_body(
        params, stream = TRUE, use_server_state = use_server_state
      )

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

      run_responses_stream <- function(req_body) {
        stream_responses_api(
        url,
        headers,
        req_body,
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
      }

      # The 400 from an HTTP-stateless endpoint is raised before any stream
      # event fires, so we can safely retry the whole stream without history.
      if (use_server_state) {
        tryCatch(
          run_responses_stream(body),
          aisdk_api_error = function(e) {
            if (!responses_previous_id_rejected(e)) stop(e)
            private$last_response_id <- NULL
            private$state_unsupported <- responses_previous_id_unsupported(e)
            run_responses_stream(private$build_responses_body(params, stream = TRUE))
          }
        )
      } else {
        run_responses_stream(body)
      }

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
  # `url` may arrive as a multi-candidate failover vector (api_endpoint_urls
  # returns one per base_url); httr2::request() requires a single string, so a
  # comma-separated base_url used to crash Responses streaming outright. This
  # path does not implement cross-endpoint failover yet, so use the first
  # candidate (matching the non-streaming call's primary endpoint).
  urls <- normalize_api_url_candidates(url)
  if (length(urls) == 0) {
    rlang::abort("`url` must contain at least one non-empty API endpoint URL.")
  }
  url <- urls[[1]]
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

  # Cap consecutive read failures so a mid-stream transport error (connection
  # reset, idle timeout) can't spin this loop at 100% CPU forever: once the
  # transfer is aborted every resp_stream_sse() throws but resp_stream_is_
  # complete() may never turn TRUE. Mirrors stream_from_api's guard.
  consecutive_errors <- 0L
  max_consecutive_errors <- 10L
  done_emitted <- FALSE

  while (!httr2::resp_stream_is_complete(resp)) {
    event <- tryCatch(
      httr2::resp_stream_sse(resp),
      error = function(e) e
    )
    if (inherits(event, "condition")) {
      consecutive_errors <- consecutive_errors + 1L
      if (consecutive_errors >= max_consecutive_errors) {
        rlang::abort(
          c("Too many consecutive SSE read errors on the Responses stream",
            "x" = conditionMessage(event)),
          class = c("aisdk_stream_error", "aisdk_api_error")
        )
      }
      next
    }
    if (is.null(event)) next
    consecutive_errors <- 0L

    event_type <- event$type
    data_str <- event$data

    if (!is.null(data_str) && nzchar(data_str)) {
      if (data_str == "[DONE]") {
        callback(NULL, NULL, done = TRUE)
        done_emitted <- TRUE
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

  # Signal completion once — not twice when the stream ended via "[DONE]"
  # (a double done fires a caller's finalizer twice, and a second call that
  # throws on double-close loses the completed result).
  if (!isTRUE(done_emitted)) {
    callback(NULL, NULL, done = TRUE)
  }
}
