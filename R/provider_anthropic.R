#' @name provider_anthropic
#' @title Anthropic Provider
#' @description
#' Implementation for Anthropic Claude models.
#' @keywords internal
NULL

#' @keywords internal
# Build the usage list from an Anthropic `usage` object, surfacing the cache
# token counts (cache_creation_input_tokens / cache_read_input_tokens) so
# prompt-caching effectiveness is observable — without them a user cannot tell
# whether caching actually hit. total_tokens counts every input token once
# (Anthropic reports cache reads/writes separately from input_tokens).
anthropic_usage_with_cache <- function(usage) {
  usage <- usage %||% list()
  input <- usage$input_tokens %||% 0
  output <- usage$output_tokens %||% 0
  cache_read <- usage$cache_read_input_tokens
  cache_write <- usage$cache_creation_input_tokens
  out <- list(
    prompt_tokens = input,
    completion_tokens = output,
    total_tokens = input + output + (cache_read %||% 0) + (cache_write %||% 0)
  )
  if (!is.null(cache_read)) out$cache_read_input_tokens <- cache_read
  if (!is.null(cache_write)) out$cache_creation_input_tokens <- cache_write
  out
}

#' @keywords internal
# Map a unified reasoning_effort (low/medium/high) to an Anthropic extended-
# thinking token budget. Defaults are indicative and overridable via
# options(aisdk.anthropic_reasoning_budgets = list(low=, medium=, high=)).
reasoning_effort_to_anthropic_budget <- function(effort) {
  effort <- tolower(trimws(as.character(effort %||% "")[[1]]))
  defaults <- list(low = 2048L, medium = 8192L, high = 16384L)
  budgets <- getOption("aisdk.anthropic_reasoning_budgets", defaults)
  if (!is.list(budgets)) budgets <- defaults
  budgets <- utils::modifyList(defaults, budgets)
  val <- budgets[[effort]]
  if (is.null(val)) NULL else as.integer(val)
}

#' @title Anthropic Language Model Class
#' @description
#' Language model implementation for Anthropic's Messages API.
#' @keywords internal
AnthropicLanguageModel <- R6::R6Class(
  "AnthropicLanguageModel",
  inherit = LanguageModelV1,
  private = list(
    config = NULL,
    get_headers = function(context_management = FALSE) {
      h <- list(
        `Content-Type` = "application/json",
        `anthropic-version` = private$config$api_version
      )
      if (nzchar(private$config$api_key %||% "")) {
        h$`x-api-key` <- private$config$api_key
      }

      # Merge custom headers first so we can check if they override defaults
      if (!is.null(private$config$headers)) {
        for (name in names(private$config$headers)) {
          h[[name]] <- private$config$headers[[name]]
        }
      }

      # Collect required beta tokens. Anthropic accepts a comma-separated list
      # in a single `anthropic-beta` header, so prompt caching and context
      # editing can be enabled together (previously only one could be set).
      betas <- character(0)
      if (isTRUE(private$config$enable_caching)) {
        betas <- c(betas, "prompt-caching-2024-07-31")
      }
      if (isTRUE(context_management)) {
        betas <- c(betas, "context-management-2025-06-27")
      }
      if (length(betas) > 0) {
        existing <- if (!is.null(h$`anthropic-beta`)) {
          trimws(strsplit(h$`anthropic-beta`, ",", fixed = TRUE)[[1]])
        } else {
          character(0)
        }
        h$`anthropic-beta` <- paste(unique(c(existing, betas)), collapse = ",")
      }

      h
    },

    # Convert standard messages to Anthropic format
    # Anthropic requires system messages to be passed as a separate `system` parameter
    format_messages = function(messages) {
      system_prompt <- NULL
      system_cache_control <- NULL
      formatted <- list()

      for (msg in messages) {
        if (msg$role == "system") {
          # Anthropic: system message is a top-level parameter
          system_prompt <- if (is.character(msg$content) || is.null(msg$content)) {
            msg$content
          } else {
            content_blocks_to_text(msg$content, arg_name = "system")
          }
          # Check for cache_control in system message
          if (!is.null(msg$cache_control)) {
            system_cache_control <- msg$cache_control
          }
        } else {
          # user and assistant roles
          content <- if (is.character(msg$content) || is.null(msg$content)) {
            msg$content
          } else {
            translate_message_content(msg$content, target = "anthropic")
          }

          # Construct message object
          msg_obj <- list(
            role = msg$role,
            content = content
          )

          # Pass through cache_control if present (for Prompt Caching)
          if (!is.null(msg$cache_control)) {
            # For caching, content might need to be a list of blocks if not already
            if (is.character(content)) {
              msg_obj$content <- list(
                list(
                  type = "text",
                  text = content,
                  cache_control = msg$cache_control
                )
              )
            } else if (is.list(content)) {
              # Assume user already structured it or we need to attach to last block
              # For simplicity, we assume if cache_control is on the message,
              # it's intended for the content block.
              # Users should ideally structure content blocks themselves for advanced caching.
              # But if they passed cache_control on the message level:
              msg_obj$content <- lapply(content, function(block) {
                block$cache_control <- msg$cache_control
                block
              })
            }
          }

          formatted <- c(formatted, list(msg_obj))
        }
      }

      # Format system prompt with cache_control if needed
      if (!is.null(system_prompt)) {
        if (!is.null(system_cache_control)) {
          system_prompt <- list(
            list(
              type = "text",
              text = system_prompt,
              cache_control = system_cache_control
            )
          )
        }
      }

      list(
        system = system_prompt,
        messages = formatted
      )
    },

    # Shared Messages-API body builder for the streaming and non-streaming
    # paths (mirrors the OpenAI provider's build_chat_body). Previously the two
    # paths were hand-maintained and had drifted: the stream path silently
    # dropped top_p, stop_sequences, and the extra-params passthrough (which is
    # how `thinking`, `tool_choice`, etc. reach the body).
    build_messages_body = function(params, stream = FALSE) {
      formatted <- private$format_messages(params$messages)

      body <- list(
        model = self$model_id,
        max_tokens = params$max_tokens %||% 4096 # Anthropic requires max_tokens
      )
      if (isTRUE(stream)) {
        body$stream <- TRUE
      }
      if (!is.null(formatted$system)) {
        body$system <- formatted$system
      }
      body$messages <- formatted$messages

      # Portable reasoning control: a unified `reasoning_effort` (low/medium/
      # high) maps to an Anthropic extended-thinking budget when the caller did
      # not pass an explicit `thinking` block. This makes reasoning_effort work
      # the same as on the OpenAI Responses API.
      reasoning_effort <- list_get_exact(params, "reasoning_effort")
      if (!is.null(reasoning_effort) && is.null(params$thinking)) {
        budget <- reasoning_effort_to_anthropic_budget(reasoning_effort)
        if (!is.null(budget)) {
          params$thinking <- list(type = "enabled", budget_tokens = budget)
          body$thinking <- params$thinking
        }
      }

      # Thinking models require temperature = 1.0. Thinking is enabled via the
      # explicit param or by a "-think" model name (for proxies).
      is_thinking <- !is.null(params$thinking) || grepl("-think", self$model_id, fixed = TRUE)
      if (is_thinking) {
        if (!isTRUE(stream) && !is.null(params$temperature) && params$temperature != 1.0 && interactive()) {
          cli::cli_alert_info("Anthropic thinking models require temperature = 1.0. Overriding {params$temperature}.")
        }
        body$temperature <- 1.0
      } else if (!is.null(params$temperature)) {
        body$temperature <- params$temperature
      }

      if (!is.null(params$top_p)) {
        body$top_p <- params$top_p
      }
      if (!is.null(params$stop_sequences)) {
        body$stop_sequences <- params$stop_sequences
      }

      # Server-side context editing (tool-result / thinking clearing): a
      # lighter-touch lever than client-side compaction. A per-call
      # `context_management` wins over a provider-level default; when either is
      # set, do_generate/do_stream also emit the beta header (see get_headers).
      context_management <- params$context_management %||% private$config$context_management
      if (!is.null(context_management)) {
        body$context_management <- context_management
      }

      # Structured outputs. Anthropic's native JSON-schema constrained decoding
      # lives in `output_config.format` (GA). A z_schema response_format used
      # to fall through the extra-params passthrough into the body verbatim and
      # 400 the request; convert it here, or inject the schema into the system
      # prompt when the endpoint can't do native structured output.
      structured <- private$apply_structured_output(body, params)
      body <- structured$body

      if (!is.null(params$tools) && length(params$tools) > 0) {
        body$tools <- lapply(params$tools, function(t) {
          if (inherits(t, "Tool")) {
            tool_api_fmt <- t$to_api_format("anthropic")
            if (!is.null(t$meta) && !is.null(t$meta$cache_control)) {
              tool_api_fmt$cache_control <- t$meta$cache_control
            }
            tool_api_fmt
          } else {
            t # Assume already in correct format
          }
        })
        # When caching is enabled but the caller marked no breakpoint, cache the
        # tools array automatically: Anthropic caches the prefix up to and
        # including a cache_control block, and tools sit first (before system
        # and messages), so marking the LAST tool caches the whole — typically
        # largest and most stable — tools prefix. Without this, `enable_caching`
        # only sends a beta header and nothing is actually cached.
        already_marked <- any(vapply(
          body$tools, function(x) is.list(x) && !is.null(x$cache_control), logical(1)
        ))
        if (isTRUE(private$config$enable_caching) && !already_marked && length(body$tools) > 0) {
          last <- length(body$tools)
          if (is.list(body$tools[[last]])) {
            body$tools[[last]]$cache_control <- list(type = "ephemeral")
          }
        }
      }

      # Portable tool_choice: map the unified value to Anthropic's object shape.
      # `parallel_tool_calls = FALSE` is carried here as disable_parallel_tool_use
      # (Anthropic has no top-level parallel flag — passed through it would 400).
      tc <- normalize_tool_choice(params$tool_choice, "anthropic", params$parallel_tool_calls)
      if (!is.null(tc)) {
        body$tool_choice <- tc
      }

      # Pass through extra params (thinking, ...). Sampling params and the
      # tool-choice knobs handled above never appear here (see handled_params).
      handled_params <- c(
        "messages", "temperature", "max_tokens", "tools", "stream", "model",
        "system", "top_p", "stop_sequences", "context_management",
        "response_format", "response_format_name", "reasoning_effort",
        "tool_choice", "parallel_tool_calls",
        "timeout_seconds", "total_timeout_seconds", "first_byte_timeout_seconds",
        "connect_timeout_seconds", "idle_timeout_seconds"
      )
      extra_params <- params[setdiff(names(params), handled_params)]
      if (length(extra_params) > 0) {
        body <- utils::modifyList(body, extra_params)
      }

      body[!sapply(body, is.null)]
    },

    # Translate a z_schema `response_format` into Anthropic structured output.
    # Native path: body$output_config = {format: {type:"json_schema", schema}}
    # (constrained decoding, GA). Fallback (config$disable_json_schema, for
    # proxies without it): inject the schema into the system prompt and ask for
    # JSON. Mirrors the OpenAI provider's process_response_format. Returns the
    # updated body (system may be modified on the fallback path).
    apply_structured_output = function(body, params) {
      fmt <- params$response_format
      if (is.null(fmt) || !inherits(fmt, "z_schema")) {
        return(list(body = body))
      }
      schema_list <- schema_to_list(fmt)
      if (!isTRUE(private$config$disable_json_schema)) {
        body$output_config <- list(
          format = list(type = "json_schema", schema = schema_list)
        )
      } else {
        instruction <- paste(
          "Return your output strictly as a JSON object adhering to this schema:\n",
          schema_to_json(fmt)
        )
        body$system <- if (is.null(body$system)) {
          instruction
        } else if (is.character(body$system)) {
          paste(body$system, "\n\n", instruction)
        } else {
          # System is already a content-block list (e.g. cache_control block):
          # append an instruction block rather than corrupt it.
          c(body$system, list(list(type = "text", text = instruction)))
        }
      }
      list(body = body)
    }
  ),
  public = list(
    #' @description Initialize the Anthropic language model.
    #' @param model_id The model ID (e.g., "claude-sonnet-4-20250514").
    #' @param config Configuration list with api_key, base_url, headers, etc.
    #' @param capabilities Optional list of capability flags.
    initialize = function(model_id, config, capabilities = list()) {
      super$initialize(
        provider = config$provider_name %||% "anthropic",
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

    #' @description Count input tokens exactly via Anthropic's
    #'   `/messages/count_tokens` endpoint (free, no generation). Falls back to
    #'   the local estimate only when the endpoint is unreachable (offline).
    #' @param params A list of call options (`messages`, `system`, `tools`, ...).
    #' @return An integer token count.
    count_tokens = function(params) {
      url <- api_endpoint_urls(private$config, "/messages/count_tokens")
      body <- private$build_messages_body(params, stream = FALSE)
      # The count endpoint takes the messages body minus generation-only fields.
      body$max_tokens <- NULL
      body$stream <- NULL
      headers <- private$get_headers(
        context_management = !is.null(body$context_management)
      )

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

      if (is.null(response) || is.null(response$input_tokens)) {
        return(estimate_prompt_tokens(params$messages, params$system))
      }
      as.integer(response$input_tokens)
    },

    #' @description Generate text (non-streaming).
    #' @param params A list of call options including messages, temperature, etc.
    #' @return A GenerateResult object.
    do_generate = function(params) {
      url <- api_endpoint_urls(private$config, "/messages")
      body <- private$build_messages_body(params, stream = FALSE)
      headers <- private$get_headers(
        context_management = !is.null(body$context_management)
      )

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

      # Parse Anthropic response format
      # Response has: id, type, role, content (array), model, stop_reason, usage
      text <- ""
      tool_calls <- NULL

      if (length(response$content) > 0) {
        for (block in response$content) {
          if (block$type == "text") {
            text <- paste0(text, block$text)
          } else if (block$type == "tool_use") {
            # Anthropic tool_use block
            if (is.null(tool_calls)) tool_calls <- list()
            tool_calls <- c(tool_calls, list(list(
              id = block$id,
              name = block$name,
              arguments = block$input
            )))
          }
        }
      }

      GenerateResult$new(
        text = text,
        usage = anthropic_usage_with_cache(response$usage),
        finish_reason = response$stop_reason,
        raw_response = response,
        tool_calls = tool_calls
      )
    },

    #' @description Generate text (streaming).
    #' @param params A list of call options.
    #' @param callback A function called for each chunk: callback(text, done).
    #' @return A GenerateResult object.
    do_stream = function(params, callback) {
      url <- api_endpoint_urls(private$config, "/messages")
      body <- private$build_messages_body(params, stream = TRUE)
      headers <- private$get_headers(
        context_management = !is.null(body$context_management)
      )

      # Anthropic uses different SSE event format
      stream_anthropic(
        url,
        headers,
        body,
        callback,
        timeout_seconds = params$timeout_seconds %||% private$config$timeout_seconds,
        total_timeout_seconds = params$total_timeout_seconds %||% private$config$total_timeout_seconds,
        first_byte_timeout_seconds = params$first_byte_timeout_seconds %||% private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = params$connect_timeout_seconds %||% private$config$connect_timeout_seconds,
        idle_timeout_seconds = params$idle_timeout_seconds %||% private$config$idle_timeout_seconds
      )
    },

    #' @description Format a tool execution result for Anthropic's API.
    #' @param tool_call_id The ID of the tool call (tool_use_id in Anthropic terms).
    #' @param tool_name The name of the tool (not used by Anthropic but kept for interface consistency).
    #' @param result_content The result content from executing the tool.
    #' @return A list formatted as a message for Anthropic's API.
    format_tool_result = function(tool_call_id, tool_name, result_content) {
      list(
        role = "user",
        content = list(
          list(
            type = "tool_result",
            tool_use_id = tool_call_id,
            content = if (is.character(result_content)) result_content else safe_to_json(result_content, auto_unbox = TRUE)
          )
        )
      )
    },

    #' @description Get the message format for Anthropic.
    get_history_format = function() {
      "anthropic"
    }
  )
)

# Helper function to check if arguments are "empty" (NULL or empty list)
is_empty_args <- function(args) {
  if (is.null(args)) {
    return(TRUE)
  }
  if (is.list(args) && length(args) == 0) {
    return(TRUE)
  }
  return(FALSE)
}


#' @title Stream from Anthropic API
#' @description
#' Makes a streaming POST request to Anthropic and processes their SSE format.
#' Anthropic uses event types like `content_block_delta` instead of OpenAI's format.
#' Also handles OpenAI-compatible format for proxy servers.
#'
#' @param url The API endpoint URL.
#' @param headers A named list of HTTP headers.
#' @param body The request body (will be converted to JSON).
#' @param callback A function called for each text delta.
#' @param max_retries Maximum number of connection/first-event retries
#'   before any stream chunk has been delivered.
#' @param initial_delay_ms Initial delay in milliseconds.
#' @param backoff_factor Multiplier for delay on each retry.
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total stream timeout in seconds.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds.
#' @param connect_timeout_seconds Optional connection-establishment timeout in seconds.
#' @param idle_timeout_seconds Optional stall timeout in seconds.
#' @return A GenerateResult object.
#' @keywords internal
stream_anthropic <- function(url, headers, body, callback,
                             max_retries = 5,
                             initial_delay_ms = 2000,
                             backoff_factor = 2,
                             timeout_seconds = NULL,
                             total_timeout_seconds = NULL,
                             first_byte_timeout_seconds = NULL,
                             connect_timeout_seconds = NULL,
                             idle_timeout_seconds = NULL) {
  urls <- normalize_api_url_candidates(url)
  if (length(urls) == 0) {
    rlang::abort("`url` must contain at least one non-empty API endpoint URL.")
  }
  if (length(urls) > 1) {
    return(stream_anthropic_failover(
      urls = urls,
      headers = headers,
      body = body,
      callback = callback,
      max_retries = max_retries,
      initial_delay_ms = initial_delay_ms,
      backoff_factor = backoff_factor,
      timeout_seconds = timeout_seconds,
      total_timeout_seconds = total_timeout_seconds,
      first_byte_timeout_seconds = first_byte_timeout_seconds,
      connect_timeout_seconds = connect_timeout_seconds,
      idle_timeout_seconds = idle_timeout_seconds
    ))
  }
  url <- urls[[1]]

  if (!preflight_internet(url)) {
    return(GenerateResult$new())
  }

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
  req <- httr2::req_error(req, is_error = function(resp) FALSE) # Handle errors manually

  attempt <- 0L
  delay_ms <- initial_delay_ms

  run_stream_attempt <- function() {
    resp <- NULL
    stream_state <- new.env(parent = emptyenv())
    stream_state$delivered_events <- FALSE

    on.exit(stream_response_close(resp), add = TRUE)

    resp <- stream_perform_connection(req)

    status <- stream_response_status(resp)
    if (status >= 400) {
      error_text <- tryCatch(
        stream_response_body_string(resp),
        error = function(e) "Unknown error (could not read body)"
      )
      abort_http_api_error(status = status, url = url, error_body = error_text)
    }

    abort_stream_transport_error <- function(e) {
      stream_class <- if (isTRUE(stream_state$delivered_events)) {
        "aisdk_stream_partial_error"
      } else {
        "aisdk_stream_start_error"
      }
      header <- if (isTRUE(stream_state$delivered_events)) {
        "API stream interrupted after data was received"
      } else {
        "API stream failed before the first event"
      }
      rlang::abort(
        c(
          header,
          "i" = paste0("URL: ", url),
          "x" = conditionMessage(e)
        ),
        class = c(stream_class, request_error_classes(e)),
        parent = e
      )
    }

    agg <- SSEAggregator$new(function(text, done) {
      stream_state$delivered_events <- TRUE
      callback(text, done)
    })

    openai_seen <- FALSE

    debug_opt <- getOption("aisdk.debug", FALSE)
    debug_enabled <- isTRUE(debug_opt) || (is.character(debug_opt) && tolower(debug_opt) %in% c("1", "true", "yes", "on"))
    debug_env <- Sys.getenv("AISDK_DEBUG", "")
    if (nzchar(debug_env) && tolower(debug_env) %in% c("1", "true", "yes", "on")) {
      debug_enabled <- TRUE
    }

    repeat {
      stream_complete <- tryCatch(
        stream_response_is_complete(resp),
        error = function(e) {
          if (is_stream_transport_error(e)) {
            abort_stream_transport_error(e)
          }
          rlang::cnd_signal(e)
        }
      )
      if (isTRUE(stream_complete)) {
        break
      }

      event <- tryCatch(
        stream_response_sse(resp),
        error = function(e) {
          if (is_stream_transport_error(e)) {
            abort_stream_transport_error(e)
          }
          rlang::cnd_signal(e)
        }
      )

      if (is.null(event)) {
        next
      }

      event_type <- event$type
      data_str <- event$data

      event_data <- NULL
      if (!is.null(data_str) && nzchar(data_str)) {
        if (data_str == "[DONE]") {
          break
        }
        tryCatch(
          {
            event_data <- jsonlite::fromJSON(data_str, simplifyVector = FALSE)
          },
          error = function(e) {
            # Skip malformed JSON.
          }
        )
      }

      if (isTRUE(debug_enabled)) {
        debug_evt <- list(
          event_type = event_type,
          data_names = names(event_data),
          has_choices = !is.null(event_data$choices)
        )
        message("aisdk debug: sse_event=", jsonlite::toJSON(debug_evt, auto_unbox = TRUE))
      }

      if (!is.null(event_data)) {
        if (!is.null(event_data$choices)) {
          openai_seen <- TRUE
          map_openai_chunk(event_data, done = FALSE, agg)
        } else {
          should_break <- map_anthropic_chunk(event_type, event_data, agg)
          if (isTRUE(should_break)) {
            break
          }
        }
      }
    }

    if (openai_seen) {
      agg$on_done()
    }

    agg$build_result()
  }

  repeat {
    attempt <- attempt + 1L

    attempt_result <- tryCatch(
      run_stream_attempt(),
      error = function(e) e
    )

    if (!inherits(attempt_result, "error")) {
      return(attempt_result)
    }

    retryable_start_failure <- inherits(attempt_result, "aisdk_stream_start_error") ||
      (!inherits(attempt_result, "aisdk_api_error") && is_stream_transport_error(attempt_result))

    if (retryable_start_failure && attempt <= max_retries) {
      message(sprintf("Network error, retrying stream in %d ms...", delay_ms))
      Sys.sleep(delay_ms / 1000)
      delay_ms <- delay_ms * backoff_factor
      next
    }

    if (retryable_start_failure) {
      abort_retry_api_error(url = url, error = attempt_result)
    }

    rlang::cnd_signal(attempt_result)
  }
}

#' @keywords internal
stream_anthropic_failover <- function(urls, headers, body, callback,
                                      max_retries = 5,
                                      initial_delay_ms = 2000,
                                      backoff_factor = 2,
                                      timeout_seconds = NULL,
                                      total_timeout_seconds = NULL,
                                      first_byte_timeout_seconds = NULL,
                                      connect_timeout_seconds = NULL,
                                      idle_timeout_seconds = NULL) {
  urls <- order_api_url_candidates(urls)
  last_error <- NULL

  for (candidate in urls) {
    result <- tryCatch(
      stream_anthropic(
        url = candidate,
        headers = headers,
        body = body,
        callback = callback,
        max_retries = max_retries,
        initial_delay_ms = initial_delay_ms,
        backoff_factor = backoff_factor,
        timeout_seconds = timeout_seconds,
        total_timeout_seconds = total_timeout_seconds,
        first_byte_timeout_seconds = first_byte_timeout_seconds,
        connect_timeout_seconds = connect_timeout_seconds,
        idle_timeout_seconds = idle_timeout_seconds
      ),
      error = function(e) e
    )

    if (!inherits(result, "error")) {
      mark_api_route_success(candidate)
      return(result)
    }

    last_error <- result
    if (inherits(result, "aisdk_stream_partial_error")) {
      rlang::cnd_signal(result)
    }

    retryable <- inherits(result, "aisdk_stream_start_error") ||
      is_retryable_api_error(result)

    if (!retryable) {
      rlang::cnd_signal(result)
    }

    mark_api_route_failure(candidate, conditionMessage(result))
    if (length(urls) > 1) {
      message("aisdk: API stream route failed before data was received; trying next configured endpoint.")
    }
  }

  if (!is.null(last_error)) {
    abort_retry_api_error(url = paste(urls, collapse = ", "), error = last_error)
  }
  rlang::abort("API stream failed: no API endpoints were attempted.", class = "aisdk_api_error")
}

#' @title Anthropic Provider Class
#' @description
#' Provider class for Anthropic. Can create language models.
#' @export
AnthropicProvider <- R6::R6Class(
  "AnthropicProvider",
  public = list(
    #' @field specification_version Provider spec version.
    specification_version = "v1",

    #' @description Initialize the Anthropic provider.
    #' @param api_key Anthropic API key. Defaults to ANTHROPIC_API_KEY env var.
    #' @param base_url Base URL for API calls. Defaults to https://api.anthropic.com/v1.
    #' @param api_version Anthropic API version header. Defaults to "2023-06-01".
    #' @param headers Optional additional headers.
    #' @param name Optional provider name override.
    #' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
    #' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
    #' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
    #' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
    #' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
    initialize = function(api_key = NULL,
                          base_url = NULL,
                          api_version = NULL,
                          headers = NULL,
                          name = NULL,
                          timeout_seconds = NULL,
                          total_timeout_seconds = NULL,
                          first_byte_timeout_seconds = NULL,
                          connect_timeout_seconds = NULL,
                          idle_timeout_seconds = NULL) {
      raw_base_url <- base_url %||% paste(
        c(
          Sys.getenv("ANTHROPIC_BASE_URL", "https://api.anthropic.com/v1"),
          Sys.getenv("ANTHROPIC_BASE_URLS", unset = "")
        ),
        collapse = ","
      )
      base_urls <- normalize_base_urls(raw_base_url)
      private$config <- list(
        api_key = api_key %||% Sys.getenv("ANTHROPIC_API_KEY"),
        base_url = base_urls[[1]],
        base_urls = base_urls,
        api_version = api_version %||% "2023-06-01",
        headers = headers,
        enable_caching = FALSE, # Default false
        provider_name = name %||% "anthropic",
        timeout_seconds = timeout_seconds,
        total_timeout_seconds = total_timeout_seconds,
        first_byte_timeout_seconds = first_byte_timeout_seconds,
        connect_timeout_seconds = connect_timeout_seconds,
        idle_timeout_seconds = idle_timeout_seconds
      )

      if (nchar(private$config$api_key) == 0) {
        rlang::warn("Anthropic API key not set. Set ANTHROPIC_API_KEY env var or pass api_key parameter.")
      }
    },

    #' @description Enable or disable prompt caching.
    #' @param enable Logical.
    enable_caching = function(enable = TRUE) {
      private$config$enable_caching <- enable
    },

    #' @description Create a language model.
    #' @param model_id The model ID (e.g., "claude-sonnet-4-20250514", "claude-3-5-sonnet-20241022").
    #' @return An AnthropicLanguageModel object.
    language_model = function(model_id = "claude-sonnet-4-20250514") {
      AnthropicLanguageModel$new(model_id, private$config)
    }
  ),
  private = list(
    config = NULL
  )
)

#' @title Create Anthropic Provider
#' @description
#' Factory function to create an Anthropic provider.
#'
#' @eval generate_model_docs("anthropic")
#'
#' @param api_key Anthropic API key. Defaults to ANTHROPIC_API_KEY env var.
#' @param base_url Base URL for API calls. Defaults to https://api.anthropic.com/v1.
#' @param api_version Anthropic API version header. Defaults to "2023-06-01".
#' @param headers Optional additional headers.
#' @param name Optional provider name override.
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
#' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
#' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
#' @return An AnthropicProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   anthropic <- create_anthropic(api_key = "sk-ant-...")
#'   model <- anthropic$language_model("claude-sonnet-4-20250514")
#' }
#' }
create_anthropic <- function(api_key = NULL,
                             base_url = NULL,
                             api_version = NULL,
                             headers = NULL,
                             name = NULL,
                             timeout_seconds = NULL,
                             total_timeout_seconds = NULL,
                             first_byte_timeout_seconds = NULL,
                             connect_timeout_seconds = NULL,
                             idle_timeout_seconds = NULL) {
  AnthropicProvider$new(
    api_key = api_key,
    base_url = base_url,
    api_version = api_version,
    headers = headers,
    name = name,
    timeout_seconds = timeout_seconds,
    total_timeout_seconds = total_timeout_seconds,
    first_byte_timeout_seconds = first_byte_timeout_seconds,
    connect_timeout_seconds = connect_timeout_seconds,
    idle_timeout_seconds = idle_timeout_seconds
  )
}
