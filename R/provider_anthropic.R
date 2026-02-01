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
    initialize = function(model_id, config) {
      super$initialize(provider = config$provider_name %||% "anthropic", model_id = model_id)
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
        max_tokens = params$max_tokens %||% 4096  # Anthropic requires max_tokens
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
            t  # Assume already in correct format
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
            t  # Assume already in correct format
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
  if (is.null(args)) return(TRUE)
  if (is.list(args) && length(args) == 0) return(TRUE)
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

  # State variables
  buffer <- ""
  full_text <- ""
  full_usage <- NULL
  stop_reason <- NULL
  tool_calls_acc <- list()
  tool_args_acc <- list()
  content_blocks_acc <- list()
  last_event <- NULL
  
  # Proxy/OpenAI compatibility state
  openai_seen <- FALSE
  oa_full_text <- ""
  oa_tool_calls_acc <- list()
  oa_finish_reason <- NULL
  oa_full_usage <- NULL
  oa_last_response <- NULL
  oa_is_reasoning <- FALSE
  
  # Anthropic Extended Thinking support
  is_thinking <- FALSE
  thinking_text_acc <- ""

  ensure_index <- function(lst, idx, default) {
    if (length(lst) < idx) {
      for (i in seq(from = length(lst) + 1, to = idx)) {
        lst[[i]] <- default
      }
    }
    lst
  }
  
  debug_opt <- getOption("aisdk.debug", FALSE)
  debug_enabled <- isTRUE(debug_opt) || (is.character(debug_opt) && tolower(debug_opt) %in% c("1", "true", "yes", "on"))
  debug_env <- Sys.getenv("AISDK_DEBUG", "")
  if (nzchar(debug_env) && tolower(debug_env) %in% c("1", "true", "yes", "on")) {
    debug_enabled <- TRUE
  }

  # Process stream
  while (!httr2::resp_stream_is_complete(resp)) {
    # resp_stream_sse returns a list(type=..., data=..., id=..., retry=...) or NULL
    event <- httr2::resp_stream_sse(resp)
    
    if (is.null(event)) next
    
    event_type <- event$type
    data_str <- event$data
    
    # Parse JSON data
    event_data <- NULL
    if (!is.null(data_str) && nzchar(data_str)) {
      if (data_str == "[DONE]") {
         # Usually OpenAI style end
         break
      }
      tryCatch({
        event_data <- jsonlite::fromJSON(data_str, simplifyVector = FALSE)
      }, error = function(e) {
        # Skip malformed JSON
      })
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
        oa_last_response <- event_data
        delta <- event_data$choices[[1]]$delta
        choice <- event_data$choices[[1]]
        
        if (!is.null(choice$finish_reason)) {
          oa_finish_reason <- choice$finish_reason
        }
        
        # Handle reasoning content
        if (!is.null(delta$reasoning_content) && nchar(delta$reasoning_content) > 0) {
          if (!oa_is_reasoning) {
            callback("<think>\n", FALSE)
            oa_is_reasoning <- TRUE
          }
          callback(delta$reasoning_content, FALSE)
        }
        
        if (!is.null(delta$content) && nchar(delta$content) > 0) {
          # Transition from reasoning to content
          if (oa_is_reasoning) {
            callback("\n</think>\n\n", FALSE)
            oa_is_reasoning <- FALSE
          }
          oa_full_text <- paste0(oa_full_text, delta$content)
          callback(delta$content, FALSE)
        }
        
        if (!is.null(delta$tool_calls)) {
          for (tc in delta$tool_calls) {
            idx <- if (!is.null(tc$index)) tc$index + 1 else length(oa_tool_calls_acc) + 1
            oa_tool_calls_acc <- ensure_index(oa_tool_calls_acc, idx, list(id = "", name = "", arguments = "", arguments_is_list = FALSE))
            
            if (!is.null(tc$id)) oa_tool_calls_acc[[idx]]$id <- paste0(oa_tool_calls_acc[[idx]]$id, tc$id)
            
            name_val <- NULL
            if (!is.null(tc$`function`$name)) name_val <- tc$`function`$name
            if (is.null(name_val) && !is.null(tc$name)) name_val <- tc$name
            if (!is.null(name_val)) oa_tool_calls_acc[[idx]]$name <- paste0(oa_tool_calls_acc[[idx]]$name, name_val)
            
            args_val <- NULL
            if (!is.null(tc$`function`$arguments)) args_val <- tc$`function`$arguments
            if (is.null(args_val) && !is.null(tc$arguments)) args_val <- tc$arguments
            
            if (!is.null(args_val)) {
              if (is.list(args_val) && !is.character(args_val)) {
                oa_tool_calls_acc[[idx]]$arguments <- args_val
                oa_tool_calls_acc[[idx]]$arguments_is_list <- TRUE
              } else {
                oa_tool_calls_acc[[idx]]$arguments <- paste0(oa_tool_calls_acc[[idx]]$arguments, args_val)
              }
            }
          }
        }
        
        if (!is.null(event_data$usage)) {
          oa_full_usage <- event_data$usage
        }
        
      } else {
        # --- Native Anthropic format ---
        last_event <- event_data
        
        if (event_type == "content_block_delta") {
          delta <- event_data$delta
          if (!is.null(delta) && delta$type == "text_delta" && !is.null(delta$text)) {
            # Close thinking if needed
            if (is_thinking) {
              callback("\n</think>\n\n", FALSE)
              is_thinking <- FALSE
            }
            
            full_text <- paste0(full_text, delta$text)
            callback(delta$text, FALSE)
            
            idx <- if (is.null(event_data$index)) length(content_blocks_acc) + 1 else event_data$index + 1
            content_blocks_acc <- ensure_index(content_blocks_acc, idx, list(type = "text", text = ""))
            content_blocks_acc[[idx]]$text <- paste0(content_blocks_acc[[idx]]$text, delta$text)
            
          } else if (!is.null(delta) && delta$type == "thinking_delta" && !is.null(delta$thinking)) {
            if (!is_thinking) {
              callback("<think>\n", FALSE)
              is_thinking <- TRUE
            }
            thinking_text_acc <- paste0(thinking_text_acc, delta$thinking)
            callback(delta$thinking, FALSE)
            
          } else if (!is.null(delta) && delta$type == "input_json_delta" && !is.null(delta$partial_json)) {
            idx <- if (is.null(event_data$index)) length(tool_args_acc) + 1 else event_data$index + 1
            tool_args_acc <- ensure_index(tool_args_acc, idx, "")
            tool_args_acc[[idx]] <- paste0(tool_args_acc[[idx]], delta$partial_json)
          }
          
        } else if (event_type == "content_block_start") {
          cb <- event_data$content_block
          idx <- if (is.null(event_data$index)) length(content_blocks_acc) + 1 else event_data$index + 1
          
          if (!is.null(cb) && !is.null(cb$type) && cb$type == "tool_use") {
            tool_calls_acc <- ensure_index(tool_calls_acc, idx, NULL)
            tool_calls_acc[[idx]] <- list(
              id = if (is.null(cb$id)) "" else cb$id,
              name = if (is.null(cb$name)) "" else cb$name,
              arguments = cb$input
            )
            
            content_blocks_acc <- ensure_index(content_blocks_acc, idx, list(type = "tool_use", id = "", name = "", input = NULL))
            content_blocks_acc[[idx]] <- list(type = "tool_use", id = cb$id, name = cb$name, input = cb$input)
            
            tool_args_acc <- ensure_index(tool_args_acc, idx, "")
            
          } else if (!is.null(cb) && !is.null(cb$type) && cb$type == "text") {
            content_blocks_acc <- ensure_index(content_blocks_acc, idx, list(type = "text", text = ""))
            if (!is.null(cb$text)) content_blocks_acc[[idx]]$text <- cb$text
            
          } else if (!is.null(cb) && !is.null(cb$type) && cb$type == "thinking") {
            if (!is_thinking) {
              callback("<think>\n", FALSE)
              is_thinking <- TRUE
            }
          }
          
        } else if (event_type == "content_block_stop") {
          if (is_thinking) {
            callback("\n</think>\n\n", FALSE)
            is_thinking <- FALSE
          }
          
        } else if (event_type == "message_delta") {
          if (!is.null(event_data$delta$stop_reason)) {
            stop_reason <- event_data$delta$stop_reason
          }
          if (!is.null(event_data$usage)) {
             # Anthropic usage is usually partial in delta
             full_usage <- list(
               prompt_tokens = 0, # usually not sent here
               completion_tokens = event_data$usage$output_tokens,
               total_tokens = event_data$usage$output_tokens
             )
          }
          
        } else if (event_type == "message_stop") {
          callback(NULL, TRUE)
          # We can break here, but let's continue to ensure stream loop finishes naturally if connection closes
          break
        }
      }
    }
  }

  # --- Finalize and Return Result ---
  
  if (openai_seen) {
    if (oa_is_reasoning) {
      callback("\n</think>\n\n", FALSE)
    }

    final_tool_calls <- NULL
    if (length(oa_tool_calls_acc) > 0) {
      final_tool_calls <- lapply(oa_tool_calls_acc, function(tc) {
        # Use the robust argument parser from tool.R
        args_val <- if (isTRUE(tc$arguments_is_list)) {
          tc$arguments
        } else {
          parse_tool_arguments(tc$arguments, tool_name = tc$name)
        }
        list(id = tc$id, name = tc$name, arguments = args_val)
      })
      final_tool_calls <- Filter(function(tc) nzchar(tc$name %||% ""), final_tool_calls)
      if (length(final_tool_calls) == 0) final_tool_calls <- NULL
    }
    
    GenerateResult$new(
      text = oa_full_text,
      usage = oa_full_usage,
      finish_reason = oa_finish_reason,
      raw_response = oa_last_response,
      tool_calls = final_tool_calls
    )
    
  } else {
    # Native Anthropic
    final_tool_calls <- NULL
    all_indices <- union(seq_along(tool_calls_acc), seq_along(tool_args_acc))

    if (length(all_indices) > 0) {
      final_tool_calls <- lapply(all_indices, function(idx) {
        tc <- tool_calls_acc[[idx]]
        raw_args <- tool_args_acc[[idx]]

        # Use the robust argument parser
        args_val <- if (!is.null(raw_args) && nzchar(raw_args)) {
          parse_tool_arguments(raw_args, tool_name = tc$name %||% "unknown")
        } else if (!is.null(tc) && !is_empty_args(tc$arguments)) {
          tc$arguments
        } else {
          list()
        }

        list(
          id = if (!is.null(tc) && !is.null(tc$id)) tc$id else "",
          name = if (!is.null(tc) && !is.null(tc$name)) tc$name else "",
          arguments = args_val
        )
      })
      final_tool_calls <- Filter(function(tc) nzchar(tc$name %||% ""), final_tool_calls)
      if (length(final_tool_calls) == 0) final_tool_calls <- NULL
    }
    
    # Reconstruct content blocks
    content_blocks_final <- list()
    max_idx <- max(c(seq_along(content_blocks_acc), 0))
    if (max_idx > 0) {
      for (idx in seq_len(max_idx)) {
         if (idx <= length(content_blocks_acc)) {
            cb <- content_blocks_acc[[idx]]
            content_blocks_final[[length(content_blocks_final) + 1]] <- cb
         }
      }
    }
    
    raw_response_final <- last_event %||% list()
    raw_response_final$content <- content_blocks_final
    
    GenerateResult$new(
      text = full_text,
      usage = full_usage,
      finish_reason = stop_reason,
      raw_response = raw_response_final,
      tool_calls = final_tool_calls
    )
  }
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
#' \dontrun{
#' anthropic <- create_anthropic(api_key = "sk-ant-...")
#' model <- anthropic$language_model("claude-sonnet-4-20250514")
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
