#' @name provider_anthropic
#' @title Anthropic Provider
#' @description
#' Implementation for Anthropic Claude models.
#' @keywords internal
NULL

#' @title Anthropic Language Model Class
#' @description
#' Language model implementation for Anthropic's Messages API.
#' @keywords internal
AnthropicLanguageModel <- R6::R6Class(
  "AnthropicLanguageModel",
  inherit = LanguageModelV1,
  private = list(
    config = NULL,
    get_headers = function() {
      h <- list(
        `Content-Type` = "application/json",
        `x-api-key` = private$config$api_key,
        `anthropic-version` = private$config$api_version
      )

      # Add beta header for prompt caching if enabled
      if (!is.null(private$config$enable_caching) && private$config$enable_caching) {
        h$`anthropic-beta` <- "prompt-caching-2024-07-31"
      }

      if (!is.null(private$config$headers)) {
        h <- c(h, private$config$headers)
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
          system_prompt <- msg$content
          # Check for cache_control in system message
          if (!is.null(msg$cache_control)) {
            system_cache_control <- msg$cache_control
          }
        } else {
          # user and assistant roles
          content <- msg$content

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
    }
  ),
  public = list(
    #' @description Initialize the Anthropic language model.
    #' @param model_id The model ID (e.g., "claude-sonnet-4-20250514").
    #' @param config Configuration list with api_key, base_url, headers, etc.
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

    #' @description Generate text (non-streaming).
    #' @param params A list of call options including messages, temperature, etc.
    #' @return A GenerateResult object.
    do_generate = function(params) {
      url <- paste0(private$config$base_url, "/messages")
      headers <- private$get_headers()

      # Format messages for Anthropic API
      formatted <- private$format_messages(params$messages)

      body <- list(
        model = self$model_id,
        max_tokens = params$max_tokens %||% 4096 # Anthropic requires max_tokens
      )

      # Add system prompt if present
      if (!is.null(formatted$system)) {
        body$system <- formatted$system
      }

      body$messages <- formatted$messages

      # Optional parameters
      if (!is.null(params$temperature)) {
        body$temperature <- params$temperature
      }
      if (!is.null(params$top_p)) {
        body$top_p <- params$top_p
      }
      if (!is.null(params$stop_sequences)) {
        body$stop_sequences <- params$stop_sequences
      }

      # Add tools if provided (Anthropic format)
      if (!is.null(params$tools) && length(params$tools) > 0) {
        body$tools <- lapply(params$tools, function(t) {
          if (inherits(t, "Tool")) {
            t$to_api_format("anthropic")
          } else {
            t # Assume already in correct format
          }
        })
      }

      # NEW: Pass through any extra parameters
      handled_params <- c("messages", "temperature", "max_tokens", "tools", "stream", "model", "system", "top_p", "stop_sequences")
      extra_params <- params[setdiff(names(params), handled_params)]
      if (length(extra_params) > 0) {
        body <- utils::modifyList(body, extra_params)
      }

      # Remove NULL entries
      body <- body[!sapply(body, is.null)]

      response <- post_to_api(url, headers, body)

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
        usage = list(
          prompt_tokens = response$usage$input_tokens,
          completion_tokens = response$usage$output_tokens,
          total_tokens = response$usage$input_tokens + response$usage$output_tokens
        ),
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
      url <- paste0(private$config$base_url, "/messages")
      headers <- private$get_headers()

      # Format messages for Anthropic API
      formatted <- private$format_messages(params$messages)

      body <- list(
        model = self$model_id,
        max_tokens = params$max_tokens %||% 4096,
        stream = TRUE
      )

      # Add system prompt if present
      if (!is.null(formatted$system)) {
        body$system <- formatted$system
      }

      body$messages <- formatted$messages

      # Optional parameters
      if (!is.null(params$temperature)) {
        body$temperature <- params$temperature
      }

      # Add tools if provided (Anthropic format)
      if (!is.null(params$tools) && length(params$tools) > 0) {
        body$tools <- lapply(params$tools, function(t) {
          if (inherits(t, "Tool")) {
            t$to_api_format("anthropic")
          } else {
            t # Assume already in correct format
          }
        })
      }

      body <- body[!sapply(body, is.null)]

      # Anthropic uses different SSE event format
      stream_anthropic(url, headers, body, callback)
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
#' @return A GenerateResult object.
#' @keywords internal
stream_anthropic <- function(url, headers, body, callback) {
  req <- httr2::request(url)
  req <- httr2::req_headers(req, !!!headers)
  req <- httr2::req_body_json(req, body)
  req <- httr2::req_error(req, is_error = function(resp) FALSE) # Handle errors manually

  # Establish connection
  resp <- httr2::req_perform_connection(req)

  # Ensure connection is closed when function exits
  on.exit(close(resp), add = TRUE)

  # Check status code immediately
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    # If error, try to read the body to give a helpful message
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

  # Create aggregator for unified state management
  agg <- SSEAggregator$new(callback)

  # Track whether we're seeing OpenAI-proxy format
  openai_seen <- FALSE

  debug_opt <- getOption("aisdk.debug", FALSE)
  debug_enabled <- isTRUE(debug_opt) || (is.character(debug_opt) && tolower(debug_opt) %in% c("1", "true", "yes", "on"))
  debug_env <- Sys.getenv("AISDK_DEBUG", "")
  if (nzchar(debug_env) && tolower(debug_env) %in% c("1", "true", "yes", "on")) {
    debug_enabled <- TRUE
  }

  # Process stream
  while (!httr2::resp_stream_is_complete(resp)) {
    event <- httr2::resp_stream_sse(resp)

    if (is.null(event)) next

    event_type <- event$type
    data_str <- event$data

    # Parse JSON data
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
          # Skip malformed JSON
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

    # Handle Standard Anthropic Events vs OpenAI Proxy Events
    if (!is.null(event_data)) {
      if (!is.null(event_data$choices)) {
        # --- OpenAI-compatible format (Proxy) ---
        openai_seen <- TRUE
        map_openai_chunk(event_data, done = FALSE, agg)
      } else {
        # --- Native Anthropic format ---
        should_break <- map_anthropic_chunk(event_type, event_data, agg)
        if (isTRUE(should_break)) break
      }
    }
  }

  # --- Finalize and Return Result ---
  if (openai_seen) {
    # Close any open reasoning block for OpenAI proxy path
    agg$on_done()
  }

  agg$finalize()
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
    initialize = function(api_key = NULL,
                          base_url = NULL,
                          api_version = NULL,
                          headers = NULL,
                          name = NULL) {
      private$config <- list(
        api_key = api_key %||% Sys.getenv("ANTHROPIC_API_KEY"),
        base_url = sub("/$", "", base_url %||% Sys.getenv("ANTHROPIC_BASE_URL", "https://api.anthropic.com/v1")),
        api_version = api_version %||% "2023-06-01",
        headers = headers,
        enable_caching = FALSE, # Default false
        provider_name = name %||% "anthropic"
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
#' @param api_key Anthropic API key. Defaults to ANTHROPIC_API_KEY env var.
#' @param base_url Base URL for API calls. Defaults to https://api.anthropic.com/v1.
#' @param api_version Anthropic API version header. Defaults to "2023-06-01".
#' @param headers Optional additional headers.
#' @param name Optional provider name override.
#' @return An AnthropicProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#' anthropic <- create_anthropic(api_key = "sk-ant-...")
#' model <- anthropic$language_model("claude-sonnet-4-20250514")
#' }
#' }
create_anthropic <- function(api_key = NULL,
                             base_url = NULL,
                             api_version = NULL,
                             headers = NULL,
                             name = NULL) {
  AnthropicProvider$new(
    api_key = api_key,
    base_url = base_url,
    api_version = api_version,
    headers = headers,
    name = name
  )
}
