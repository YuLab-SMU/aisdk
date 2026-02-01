#' @title Core API: High-Level Functions
#' @description
#' User-facing high-level API functions for interacting with AI models.
#' @name core_api
NULL

#' @title Generate Text
#' @description
#' Generate text using a language model. This is the primary high-level function
#' for non-streaming text generation.
#'
#' When tools are provided and max_steps > 1, the function will automatically
#' execute tool calls and feed results back to the LLM in a ReAct-style loop
#' until the LLM produces a final response or max_steps is reached.
#'
#' @param model Either a LanguageModelV1 object, or a string ID like "openai:gpt-4o".
#' @param prompt A character string prompt, or a list of messages.
#' @param system Optional system prompt.
#' @param temperature Sampling temperature (0-2). Default 0.7.
#' @param max_tokens Maximum tokens to generate.
#' @param tools Optional list of Tool objects for function calling.
#' @param max_steps Maximum number of generation steps (tool execution loops).
#'   Default 1 (single generation, no automatic tool execution).
#'   Set to higher values (e.g., 5) to enable automatic tool execution.
#' @param skills Optional path to skills directory, or a SkillRegistry object.
#'   When provided, skill tools are auto-injected and skill summaries are added
#'   to the system prompt.
#' @param session Optional ChatSession object. When provided, tool executions
#'   run in the session's environment, enabling cross-agent data sharing.
#' @param hooks Optional HookHandler object for intercepting events.
#' @param registry Optional ProviderRegistry to use (defaults to global registry).
#' @param ... Additional arguments passed to the model.
#' @return A GenerateResult object with text and optionally tool_calls.
#'   When max_steps > 1 and tools are used, the result includes:
#'   \itemize{
#'     \item steps: Number of steps taken
#'     \item all_tool_calls: List of all tool calls made across all steps
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Using hooks
#' my_hooks <- create_hooks(
#'   on_generation_start = function(model, prompt, tools) message("Starting..."),
#'   on_tool_start = function(tool, args) message("Calling tool ", tool$name)
#' )
#' result <- generate_text(model, "...", hooks = my_hooks)
#' }
generate_text <- function(model,
                          prompt,
                          system = NULL,
                          temperature = 0.7,
                          max_tokens = NULL,
                          tools = NULL,
                          max_steps = 1,
                          skills = NULL,
                          session = NULL,
                          hooks = NULL,
                          registry = NULL,
                          ...) {
  # Resolve model from string ID if needed
  model <- resolve_model(model, registry, type = "language")

  # Handle skills parameter
  skill_registry <- NULL
  if (!is.null(skills)) {
    if (is.character(skills)) {
      # skills is a path, scan for skills
      skill_registry <- create_skill_registry(skills)
    } else if (inherits(skills, "SkillRegistry")) {
      skill_registry <- skills
    } else {
      rlang::abort("skills must be a path string or SkillRegistry object.")
    }
    
    # Inject skill summaries into system prompt
    skill_prompt <- skill_registry$generate_prompt_section()
    if (nzchar(skill_prompt)) {
      system <- if (is.null(system)) skill_prompt else paste(system, "\n\n", skill_prompt, sep = "")
    }
    
    # Add skill tools to the tools list
    skill_tools <- create_skill_tools(skill_registry)
    tools <- if (is.null(tools)) skill_tools else c(tools, skill_tools)
  }
  
  # Trigger on_generation_start
  if (!is.null(hooks)) {
    hooks$trigger_generation_start(model, prompt, tools)
  }

  # Build initial messages
  messages <- build_messages(prompt, system)

  # Build base params (tools stay constant across steps)
  base_params <- list(
    temperature = temperature,
    max_tokens = max_tokens,
    tools = tools,
    ...
  )

  # Track all tool calls for debugging/logging
  all_tool_calls <- list()
  step <- 0
  result <- NULL

  # ReAct loop
  tryCatch({
    while (step < max_steps) {
      step <- step + 1

      # Build params with current messages
      params <- c(list(messages = messages), base_params)

      # Call the model
      result <- model$do_generate(params)

      # Check if there are tool calls to process
      if (!is.null(result$tool_calls) && length(result$tool_calls) > 0 && !is.null(tools)) {
        # Store tool calls
        all_tool_calls <- c(all_tool_calls, result$tool_calls)

        # If we've reached max_steps, return without executing
        # If we've reached max_steps, return without executing
        if (step >= max_steps) {
          warning(sprintf("Maximum generation steps (%d) reached. Tool execution stopped.", max_steps))
          break
        }

        # Execute tools (with hooks and optional session environment)
        tool_envir <- if (!is.null(session)) session$get_envir() else NULL
        
        # Log tool calls
        if (interactive()) {
          for (tc in result$tool_calls) {
            print_tool_execution(tc$name, tc$arguments)
          }
        }

        tool_results <- execute_tool_calls(result$tool_calls, tools, hooks, envir = tool_envir)

        # Log tool results (matching one-to-one with calls usually, but execute_tool_calls returns list)
        if (interactive()) {
          for (tr in tool_results) {
            print_tool_result(tr$name, tr$result)
          }
        }

        # Append assistant message with tool_calls to history
        # Note: We need to include the tool_calls in the assistant message for context
        assistant_message <- list(role = "assistant", content = result$text %||% "")
        
        history_format <- model$get_history_format()
        
        # For OpenAI, we need to include tool_calls in the assistant message
        if (history_format == "openai") {
          assistant_message$tool_calls <- lapply(result$tool_calls, function(tc) {
            list(
              id = tc$id,
              type = "function",
              `function` = list(
                name = tc$name,
                arguments = safe_to_json(tc$arguments, auto_unbox = TRUE)
              )
            )
          })
        } else if (history_format == "anthropic") {
          # For Anthropic, tool_use blocks are part of the content
          assistant_message$content <- result$raw_response$content
        }
        
        messages <- c(messages, list(assistant_message))

        # Append tool results to history using provider-specific formatting
        for (tr in tool_results) {
          tool_result_msg <- model$format_tool_result(tr$id, tr$name, tr$result)
          messages <- c(messages, list(tool_result_msg))
        }

        # Continue loop
      } else {
        # No tool calls, we're done
        break
      }
    }
  }, error = handle_network_error)

  # Add step information to result for debugging
  if (max_steps > 1) {
    if (is.null(result)) {
       # If result is NULL here (e.g. error in first loop), initialize it loosely so we return something
       result <- list()
    }
    result$steps <- step
    result$all_tool_calls <- all_tool_calls
  }

  # Trigger on_generation_end
  if (!is.null(hooks)) {
    hooks$trigger_generation_end(result)
  }

  result
}

#' @title Stream Text
#' @description
#' Generate text using a language model with streaming output.
#' This function provides a real-time stream of tokens through a callback.
#'
#' @param model Either a LanguageModelV1 object, or a string ID like "openai:gpt-4o".
#' @param prompt A character string prompt, or a list of messages.
#' @param callback A function called for each text chunk: \code{callback(text, done)}.
#' @param system Optional system prompt.
#' @param temperature Sampling temperature (0-2). Default 0.7.
#' @param max_tokens Maximum tokens to generate.
#' @param tools Optional list of Tool objects for function calling.
#' @param max_steps Maximum number of generation steps (tool execution loops).
#'   Default 1. Set to higher values (e.g., 5) to enable automatic tool execution.
#' @param skills Optional path to skills directory, or a SkillRegistry object.
#' @param session Optional ChatSession object for shared state.
#' @param hooks Optional HookHandler object.
#' @param registry Optional ProviderRegistry to use.
#' @param ... Additional arguments passed to the model.
#' @return A GenerateResult object (accumulated from the stream).
#' @export
#' @examples
#' \dontrun{
#' model <- create_openai()$language_model("gpt-4o")
#' stream_text(model, "Tell me a story", callback = function(text, done) {
#'   if (!done) cat(text)
#' })
#' }
stream_text <- function(model,
                        prompt,
                        callback = NULL,
                        system = NULL,
                        temperature = 0.7,
                        max_tokens = NULL,
                        tools = NULL,
                        max_steps = 1,
                        skills = NULL,
                        session = NULL,
                        hooks = NULL,
                        registry = NULL,
                        ...) {
  model <- resolve_model(model, registry, type = "language")

  # Handle skills parameter
  skill_registry <- NULL
  if (!is.null(skills)) {
    if (is.character(skills)) {
      # skills is a path, scan for skills
      skill_registry <- create_skill_registry(skills)
    } else if (inherits(skills, "SkillRegistry")) {
      skill_registry <- skills
    } else {
      rlang::abort("skills must be a path string or SkillRegistry object.")
    }
    
    # Inject skill summaries into system prompt
    skill_prompt <- skill_registry$generate_prompt_section()
    if (nzchar(skill_prompt)) {
      system <- if (is.null(system)) skill_prompt else paste(system, "\n\n", skill_prompt, sep = "")
    }
    
    # Add skill tools to the tools list
    skill_tools <- create_skill_tools(skill_registry)
    tools <- if (is.null(tools)) skill_tools else c(tools, skill_tools)
  }

  # Trigger on_generation_start
  if (!is.null(hooks)) {
    hooks$trigger_generation_start(model, prompt, tools)
  }

  messages <- build_messages(prompt, system)

  # Build base params
  base_params <- list(
    temperature = temperature,
    max_tokens = max_tokens,
    tools = tools,
    ...
  )
  
  all_tool_calls <- list()
  step <- 0
  result <- NULL

  renderer <- create_stream_renderer()
  
  # ReAct loop for streaming
  tryCatch({
    while (step < max_steps) {
      step <- step + 1

      # Build params with current messages
      params <- c(list(messages = messages), base_params)

      # Call the model via do_stream
      if (interactive()) renderer$start_thinking()
      
      result <- model$do_stream(params, function(chunk, done) {
        if (interactive()) {
          if (!is.null(callback)) {
            renderer$stop_thinking()
          } else {
            renderer$process_chunk(chunk, done)
          }
        }
        if (!is.null(callback)) callback(chunk, done)
      })

      # Check if there are tool calls to process
      if (!is.null(result$tool_calls) && length(result$tool_calls) > 0 && !is.null(tools)) {
        # Store tool calls
        all_tool_calls <- c(all_tool_calls, result$tool_calls)

        # If we've reached max_steps, return without executing
        if (step >= max_steps) {
          warning(sprintf("Maximum generation steps (%d) reached. Tool execution stopped.", max_steps))
          break
        }

        # Execute tools (with hooks and optional session environment)
        tool_envir <- if (!is.null(session)) session$get_envir() else NULL
        
        # Log tool calls
        if (interactive()) {
          for (tc in result$tool_calls) {
            renderer$render_tool_start(tc$name, tc$arguments)
          }
        }

        tool_results <- execute_tool_calls(result$tool_calls, tools, hooks, envir = tool_envir)

        # Log tool results
        if (interactive()) {
          for (tr in tool_results) {
            renderer$render_tool_result(tr$name, tr$result)
          }
        }

        # Append assistant message with tool_calls to history
        assistant_message <- list(role = "assistant", content = result$text %||% "")
        
        history_format <- model$get_history_format()
        
        # Provider-specific tool call formatting (copied from generate_text)
        if (history_format == "openai") {
          assistant_message$tool_calls <- lapply(result$tool_calls, function(tc) {
            list(
              id = tc$id,
              type = "function",
              `function` = list(
                name = tc$name,
                arguments = safe_to_json(tc$arguments, auto_unbox = TRUE)
              )
            )
          })
        } else if (history_format == "anthropic") {
          assistant_message$content <- result$raw_response$content
        }
        
        messages <- c(messages, list(assistant_message))

        # Append tool results to history
        for (tr in tool_results) {
          tool_result_msg <- model$format_tool_result(tr$id, tr$name, tr$result)
          messages <- c(messages, list(tool_result_msg))
        }

        # Reset renderer state for next step
        if (interactive()) {
          renderer$reset_for_new_step()
        }

        # Continue loop - next iteration will stream the response to the tool output
      } else {
        # No tool calls, we're done
        break
      }
    }
  }, error = handle_network_error)

  # Add step info
  if (max_steps > 1) {
    if (is.null(result)) {
       result <- list() # fallback
    }
    result$steps <- step
    result$all_tool_calls <- all_tool_calls
  }

  # Trigger on_generation_end
  if (!is.null(hooks)) {
    hooks$trigger_generation_end(result)
  }

  result
}

#' @title Create Embeddings
#' @description
#' Generate embeddings for text using an embedding model.
#'
#' @param model Either an EmbeddingModelV1 object, or a string ID like "openai:text-embedding-3-small".
#' @param value A character string or vector to embed.
#' @param registry Optional ProviderRegistry to use.
#' @return A list with embeddings and usage information.
#' @export
#' @examples
#' \dontrun{
#' model <- create_openai()$embedding_model("text-embedding-3-small")
#' result <- create_embeddings(model, "Hello, world!")
#' print(length(result$embeddings[[1]]))
#' }
create_embeddings <- function(model, value, registry = NULL) {
  model <- resolve_model(model, registry, type = "embedding")
  model$do_embed(value)
}

# --- Internal Helper Functions ---

#' @keywords internal
handle_network_error <- function(e) {
  # Check for common network error patterns
  msg <- conditionMessage(e)
  is_network_error <- any(sapply(c(
    "cannot open the connection",
    "Failed to perform HTTP request",
    "timeout",
    "operation timed out",
    "Connection reset",
    "host unreachable"
  ), function(p) grepl(p, msg, ignore.case = TRUE)))
  
  if (is_network_error) {
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_alert_danger("Network Connection Interrupted")
      cli::cli_alert_info("Don't worry! This process is designed to be resilient.")
      cli::cli_ul()
      cli::cli_li("If you were running a task, it is safe to re-run.")
      cli::cli_li("Option 1: Simply run the last command again.")
      cli::cli_li("Option 2: Ask the agent to 'Continue where you left off'.")
      cli::cli_end()
    } else {
      message("\n!!! Network Connection Interrupted !!!")
      message("Don't worry! It is safe to re-run this task.")
      message("Option 1: Simply run the last command again.")
      message("Option 2: Ask the agent to 'Continue where you left off'.\n")
    }
  }
  
  # Re-throw to allow programmatic handling if needed
  rlang::cnd_signal(e)
}

#' @keywords internal
resolve_model <- function(model, registry = NULL, type = c("language", "embedding")) {
  type <- match.arg(type)

  if (is.character(model)) {
    # Model is a string ID, resolve from registry
    reg <- registry %||% get_default_registry()
    if (type == "language") {
      model <- reg$language_model(model)
    } else {
      model <- reg$embedding_model(model)
    }
  }

  # Validate model type
  expected_class <- if (type == "language") "LanguageModelV1" else "EmbeddingModelV1"
  if (!inherits(model, expected_class)) {
    rlang::abort(paste0("Expected a ", expected_class, " object."))
  }

  model
}

#' @keywords internal
build_messages <- function(prompt, system = NULL) {
  if (is.list(prompt) && all(sapply(prompt, function(x) is.list(x) && "role" %in% names(x)))) {
    # prompt is already a list of messages
    messages <- prompt
    if (!is.null(system)) {
      messages <- c(list(list(role = "system", content = system)), messages)
    }
  } else if (is.character(prompt)) {
    # prompt is a string
    messages <- list()
    if (!is.null(system)) {
      messages <- c(messages, list(list(role = "system", content = system)))
    }
    messages <- c(messages, list(list(role = "user", content = prompt)))
  } else {
    rlang::abort("prompt must be a character string or a list of message objects.")
  }

  messages
}

# Null-coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

#' @keywords internal
print_tool_execution <- function(name, arguments) {
  args_str <- tryCatch(
    safe_to_json(arguments, auto_unbox = TRUE),
    error = function(e) "..."
  )
  
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_alert_info("Calling tool {.fn {name}} with args {.code {args_str}}")
  } else {
    message(sprintf("\u2139 Calling tool %s(%s)", name, args_str))
  }
}

#' @keywords internal
print_tool_result <- function(name, result) {
  if (is.null(result)) {
    res_str <- "NULL"
  } else {
    res_str <- if (is.character(result)) result else safe_to_json(result, auto_unbox = TRUE)
    if (length(res_str) == 0) {
      res_str <- "NULL"
    }
  }
  # Truncate if too long (e.g. > 200 chars) for display
  if (nchar(res_str) > 200) {
    res_str <- paste0(substr(res_str, 1, 197), "...")
  }
  
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_alert_success("Tool {.fn {name}} returned: {.val {res_str}}")
  } else {
    message(sprintf("\u2714 Tool %s returned: %s", name, res_str))
  }
}
