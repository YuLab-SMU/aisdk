#' @title Core API: High-Level Functions
#' @description
#' User-facing high-level API functions for interacting with AI models.
#' @name core_api
NULL

#' @keywords internal
parse_tool_call_blocks <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(list(tool_calls = NULL, text = text))
  }

  matches <- gregexpr("(?s)<tool_call>\\s*.*?\\s*</tool_call>", text, perl = TRUE)[[1]]
  if (length(matches) == 1L && identical(matches[[1]], -1L)) {
    return(list(tool_calls = NULL, text = text))
  }

  blocks <- regmatches(text, list(matches))[[1]]
  tool_calls <- list()

  for (i in seq_along(blocks)) {
    inner <- sub("(?s)^\\s*<tool_call>\\s*", "", blocks[[i]], perl = TRUE)
    inner <- sub("(?s)\\s*</tool_call>\\s*$", "", inner, perl = TRUE)
    inner <- trimws(inner)
    if (!nzchar(inner)) {
      next
    }

    parsed <- tryCatch(
      jsonlite::fromJSON(inner, simplifyVector = FALSE),
      error = function(e) {
        repaired <- repair_json_string(inner)
        tryCatch(
          jsonlite::fromJSON(repaired, simplifyVector = FALSE),
          error = function(e2) NULL
        )
      }
    )

    if (is.null(parsed) || !is.list(parsed) || !nzchar(parsed$name %||% "")) {
      next
    }

    tool_calls[[length(tool_calls) + 1L]] <- list(
      id = parsed$id %||% sprintf("text_tool_call_%02d", i),
      name = parsed$name,
      arguments = parse_tool_arguments(parsed$arguments %||% list(), tool_name = parsed$name)
    )
  }

  if (length(tool_calls) == 0) {
    return(list(tool_calls = NULL, text = text))
  }

  cleaned_text <- text
  regmatches(cleaned_text, list(matches)) <- list(rep("", length(blocks)))
  cleaned_text <- trimws(cleaned_text)

  list(tool_calls = tool_calls, text = cleaned_text)
}

#' @keywords internal
recover_text_tool_calls <- function(result) {
  if (!is.null(result$tool_calls) && length(result$tool_calls) > 0) {
    return(result)
  }

  parsed <- parse_tool_call_blocks(result$text %||% "")
  if (is.null(parsed$tool_calls) || length(parsed$tool_calls) == 0) {
    return(result)
  }

  result$tool_calls <- parsed$tool_calls
  result$text <- parsed$text
  if (is.null(result$finish_reason) || !nzchar(result$finish_reason %||% "")) {
    result$finish_reason <- "tool_calls"
  }
  result
}

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
#' @param sandbox Logical. If TRUE, enables R-native programmatic sandbox mode.
#'   All tools are bound into an isolated R environment and replaced by a single
#'   `execute_r_code` meta-tool. The LLM writes R code to batch-invoke tools,
#'   filter data with dplyr/purrr, and return only summary results, dramatically
#'   reducing token usage and latency. Default FALSE.
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
#' \donttest{
#' if (interactive()) {
#'   # Using hooks
#'   my_hooks <- create_hooks(
#'     on_generation_start = function(model, prompt, tools) message("Starting..."),
#'     on_tool_start = function(tool, args) message("Calling tool ", tool$name)
#'   )
#'   result <- generate_text(model, "...", hooks = my_hooks)
#' }
#' }
generate_text <- function(model = NULL,
                          prompt,
                          system = NULL,
                          temperature = 0.7,
                          max_tokens = NULL,
                          tools = NULL,
                          max_steps = 1,
                          sandbox = FALSE,
                          skills = NULL,
                          session = NULL,
                          hooks = NULL,
                          registry = NULL,
                          ...) {
  default_call_options <- if (is.null(model) && is.null(session)) {
    get_default_model_runtime_options()$call_options %||% list()
  } else {
    list()
  }

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

  tools <- filter_tools_for_model_capabilities(tools, model)

  # Handle sandbox mode: bind tools into SandboxManager, replace with meta-tool
  if (isTRUE(sandbox) && !is.null(tools) && length(tools) > 0) {
    parent_env <- if (!is.null(session)) session$get_envir() else NULL
    sandbox_mgr <- SandboxManager$new(
      tools = tools,
      parent_env = parent_env
    )
    # Inject sandbox usage instructions into system prompt
    sandbox_prompt <- create_sandbox_system_prompt(sandbox_mgr)
    system <- if (is.null(system)) sandbox_prompt else paste(system, "\n\n", sandbox_prompt, sep = "")
    # Replace all tools with the single execute_r_code meta-tool
    tools <- list(create_r_code_tool(sandbox_mgr))
  }

  # Trigger on_generation_start
  if (!is.null(hooks)) {
    hooks$trigger_generation_start(model, prompt, tools)
  }

  # Build initial messages
  messages <- build_messages(prompt, system)
  validate_model_messages(model, messages)

  # Build base params (tools stay constant across steps)
  base_params <- merge_call_options(
    default_call_options,
    list(
      temperature = temperature,
      max_tokens = max_tokens,
      tools = tools,
      ...
    )
  )

  # Track all tool calls for debugging/logging
  all_tool_calls <- list()
  all_tool_results <- list()
  step <- 0
  result <- NULL

  # Circuit breaker state
  breaker_state <- new.env(parent = emptyenv())
  breaker_state$consecutive_identical_calls <- 0
  breaker_state$consecutive_tool_errors <- 0
  breaker_state$last_tool_signature <- NULL
  max_identical_calls <- 3 # Threshold for repeating identical tool calls
  max_tool_errors <- 3 # Threshold for consecutive tool execution errors

  # ReAct loop
  tryCatch(
    {
      while (step < max_steps) {
        step <- step + 1

        # Build params with current messages
        params <- c(list(messages = messages), base_params)

        # Call the model
        result <- model$do_generate(params)
        result <- recover_text_tool_calls(result)

        if (isTRUE(getOption("aisdk.debug", FALSE))) {
          message("[DEBUG] generate_text step ", step, " | finish_reason: ", result$finish_reason)
          raw_text <- result$text %||% ""
          message("[DEBUG] response text (", nchar(raw_text), " chars): ",
                  substr(raw_text, 1, min(500, nchar(raw_text))),
                  if (nchar(raw_text) > 500) "... [truncated]" else "")
          if (!is.null(result$usage)) {
            message("[DEBUG] usage: prompt=", result$usage$prompt_tokens,
                    " completion=", result$usage$completion_tokens,
                    " total=", result$usage$total_tokens)
          }
        }

        # Check if there are tool calls to process
        if (!is.null(result$tool_calls) && length(result$tool_calls) > 0 && !is.null(tools)) {
          # Store tool calls
          all_tool_calls <- c(all_tool_calls, result$tool_calls)

          # If we've reached max_steps, return without executing
          if (step >= max_steps) {
            warning(sprintf("Maximum generation steps (%d) reached. Tool execution stopped.", max_steps))
            break
          }

          # --- Circuit Breaker: Detect repeated identical tool calls ---
          current_signature <- paste(
            vapply(result$tool_calls, function(tc) {
              paste0(tc$name, ":", safe_to_json(tc$arguments, auto_unbox = TRUE))
            }, character(1)),
            collapse = "|"
          )
          if (identical(current_signature, breaker_state$last_tool_signature)) {
            breaker_state$consecutive_identical_calls <- breaker_state$consecutive_identical_calls + 1
          } else {
            breaker_state$consecutive_identical_calls <- 0
            breaker_state$last_tool_signature <- current_signature
          }
          if (breaker_state$consecutive_identical_calls >= max_identical_calls) {
            warning(
              "Circuit breaker triggered: model repeated identical tool calls ",
              breaker_state$consecutive_identical_calls, " times."
            )
            result$finish_reason <- "tool_failure"
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

          # --- Circuit Breaker: Detect consecutive tool execution errors ---
          tool_results <- tryCatch(
            {
              res <- execute_tool_calls(result$tool_calls, tools, hooks, envir = tool_envir)
              breaker_state$consecutive_tool_errors <- 0 # Reset on success
              res
            },
            error = function(e) {
              breaker_state$consecutive_tool_errors <- breaker_state$consecutive_tool_errors + 1
              if (breaker_state$consecutive_tool_errors >= max_tool_errors) {
                warning(
                  "Circuit breaker triggered: ", breaker_state$consecutive_tool_errors,
                  " consecutive tool execution failures. Last error: ",
                  conditionMessage(e)
                )
                NULL
              } else {
                # Return error message as tool result so model can self-correct
                lapply(result$tool_calls, function(tc) {
                  list(
                    id = tc$id,
                    name = tc$name,
                    result = paste0("Error executing tool: ", conditionMessage(e))
                  )
                })
              }
            }
          )

          if (is.null(tool_results)) {
            result$finish_reason <- "tool_failure"
            break
          }
          all_tool_results <- c(all_tool_results, tool_results)

          # Log tool results (matching one-to-one with calls usually, but execute_tool_calls returns list)
          if (interactive()) {
            for (tr in tool_results) {
              print_tool_result(
                tr$name,
                tr$result,
                success = !isTRUE(tr$is_error),
                raw_result = tr$raw_result %||% tr$result
              )
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
            if (isTRUE(model$capabilities$preserve_reasoning_content) &&
                !is.null(result$reasoning) &&
                nzchar(result$reasoning)) {
              assistant_message$reasoning_content <- result$reasoning
            }
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
    },
    error = handle_network_error
  )

  # Add step information to result for debugging
  if (max_steps > 1) {
    if (is.null(result)) {
      # If result is NULL here (e.g. error in first loop), initialize it loosely so we return something
      result <- list()
    }
    result$steps <- step
    result$all_tool_calls <- all_tool_calls
    result$all_tool_results <- all_tool_results
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
#' @param sandbox Logical. If TRUE, enables R-native programmatic sandbox mode.
#'   See \code{generate_text} for details. Default FALSE.
#' @param skills Optional path to skills directory, or a SkillRegistry object.
#' @param session Optional ChatSession object for shared state.
#' @param hooks Optional HookHandler object.
#' @param registry Optional ProviderRegistry to use.
#' @param ... Additional arguments passed to the model.
#' @return A GenerateResult object (accumulated from the stream).
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   model <- create_openai()$language_model("gpt-4o")
#'   stream_text(model, "Tell me a story", callback = function(text, done) {
#'     if (!done) cat(text)
#'   })
#' }
#' }
stream_text <- function(model = NULL,
                        prompt,
                        callback = NULL,
                        system = NULL,
                        temperature = 0.7,
                        max_tokens = NULL,
                        tools = NULL,
                        max_steps = 1,
                        sandbox = FALSE,
                        skills = NULL,
                        session = NULL,
                        hooks = NULL,
                        registry = NULL,
                        ...) {
  default_call_options <- if (is.null(model) && is.null(session)) {
    get_default_model_runtime_options()$call_options %||% list()
  } else {
    list()
  }

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

  tools <- filter_tools_for_model_capabilities(tools, model)

  # Handle sandbox mode: bind tools into SandboxManager, replace with meta-tool
  if (isTRUE(sandbox) && !is.null(tools) && length(tools) > 0) {
    parent_env <- if (!is.null(session)) session$get_envir() else NULL
    sandbox_mgr <- SandboxManager$new(
      tools = tools,
      parent_env = parent_env
    )
    # Inject sandbox usage instructions into system prompt
    sandbox_prompt <- create_sandbox_system_prompt(sandbox_mgr)
    system <- if (is.null(system)) sandbox_prompt else paste(system, "\n\n", sandbox_prompt, sep = "")
    # Replace all tools with the single execute_r_code meta-tool
    tools <- list(create_r_code_tool(sandbox_mgr))
  }

  # Trigger on_generation_start
  if (!is.null(hooks)) {
    hooks$trigger_generation_start(model, prompt, tools)
  }

  messages <- build_messages(prompt, system)
  validate_model_messages(model, messages)

  # Build base params
  base_params <- merge_call_options(
    default_call_options,
    list(
      temperature = temperature,
      max_tokens = max_tokens,
      tools = tools,
      ...
    )
  )

  all_tool_calls <- list()
  all_tool_results <- list()
  step <- 0
  result <- NULL

  renderer <- create_stream_renderer()

  # Circuit breaker state
  breaker_state <- new.env(parent = emptyenv())
  breaker_state$consecutive_identical_calls <- 0
  breaker_state$consecutive_tool_errors <- 0
  breaker_state$last_tool_signature <- NULL
  max_identical_calls <- 3
  max_tool_errors <- 3

  # ReAct loop for streaming
  tryCatch(
    {
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
        result <- recover_text_tool_calls(result)

        # Check if there are tool calls to process
        if (!is.null(result$tool_calls) && length(result$tool_calls) > 0 && !is.null(tools)) {
          # Store tool calls
          all_tool_calls <- c(all_tool_calls, result$tool_calls)

          # If we've reached max_steps, return without executing
          if (step >= max_steps) {
            warning(sprintf("Maximum generation steps (%d) reached. Tool execution stopped.", max_steps))
            break
          }

          # --- Circuit Breaker: Detect repeated identical tool calls ---
          current_signature <- paste(
            vapply(result$tool_calls, function(tc) {
              paste0(tc$name, ":", safe_to_json(tc$arguments, auto_unbox = TRUE))
            }, character(1)),
            collapse = "|"
          )
          if (identical(current_signature, breaker_state$last_tool_signature)) {
            breaker_state$consecutive_identical_calls <- breaker_state$consecutive_identical_calls + 1
          } else {
            breaker_state$consecutive_identical_calls <- 0
            breaker_state$last_tool_signature <- current_signature
          }
          if (breaker_state$consecutive_identical_calls >= max_identical_calls) {
            warning(
              "Circuit breaker triggered: model repeated identical tool calls ",
              breaker_state$consecutive_identical_calls, " times."
            )
            result$finish_reason <- "tool_failure"
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

          # --- Circuit Breaker: Detect consecutive tool execution errors ---
          tool_results <- tryCatch(
            {
              res <- execute_tool_calls(result$tool_calls, tools, hooks, envir = tool_envir)
              breaker_state$consecutive_tool_errors <- 0
              res
            },
            error = function(e) {
              breaker_state$consecutive_tool_errors <- breaker_state$consecutive_tool_errors + 1
              if (breaker_state$consecutive_tool_errors >= max_tool_errors) {
                warning(
                  "Circuit breaker triggered: ", breaker_state$consecutive_tool_errors,
                  " consecutive tool execution failures. Last error: ",
                  conditionMessage(e)
                )
                NULL
              } else {
                lapply(result$tool_calls, function(tc) {
                  list(
                    id = tc$id,
                    name = tc$name,
                    result = paste0("Error executing tool: ", conditionMessage(e))
                  )
                })
              }
            }
          )

          if (is.null(tool_results)) {
            result$finish_reason <- "tool_failure"
            break
          }
          all_tool_results <- c(all_tool_results, tool_results)

          # Log tool results
          if (interactive()) {
            for (tr in tool_results) {
              renderer$render_tool_result(
                tr$name,
                tr$result,
                success = !isTRUE(tr$is_error),
                raw_result = tr$raw_result %||% tr$result
              )
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
            if (isTRUE(model$capabilities$preserve_reasoning_content) &&
                !is.null(result$reasoning) &&
                nzchar(result$reasoning)) {
              assistant_message$reasoning_content <- result$reasoning
            }
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
    },
    error = handle_network_error
  )

  # Add step info
  if (max_steps > 1) {
    if (is.null(result)) {
      result <- list() # fallback
    }
    result$steps <- step
    result$all_tool_calls <- all_tool_calls
    result$all_tool_results <- all_tool_results
    # Return messages added during tool execution for session history sync
    # Calculate which messages were added (everything after the initial prompt)
    initial_len <- length(build_messages(prompt, system))
    if (length(messages) > initial_len) {
      result$messages_added <- messages[(initial_len + 1):length(messages)]
    }
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
#' \donttest{
#' if (interactive()) {
#'   model <- create_openai()$embedding_model("text-embedding-3-small")
#'   result <- create_embeddings(model, "Hello, world!")
#'   print(length(result$embeddings[[1]]))
#' }
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
resolve_model <- function(model, registry = NULL, type = c("language", "embedding", "image")) {
  type <- match.arg(type)

  if (is.null(model)) {
    if (type == "language") {
      model <- get_model()
    } else if (type == "image") {
      rlang::abort("No image model configured. Please supply `model` explicitly.")
    } else {
      rlang::abort("No embedding model configured. Please supply `model` explicitly.")
    }
  }

  if (is.character(model)) {
    # Model is a string ID, resolve from registry
    reg <- registry %||% get_default_registry()
    if (type == "language") {
      model <- reg$language_model(model)
    } else if (type == "image") {
      model <- reg$image_model(model)
    } else {
      model <- reg$embedding_model(model)
    }
  }

  # Validate model type
  expected_class <- switch(type,
    language = "LanguageModelV1",
    embedding = "EmbeddingModelV1",
    image = "ImageModelV1"
  )
  if (!inherits(model, expected_class)) {
    rlang::abort(paste0("Expected a ", expected_class, " object."))
  }

  if (identical(type, "language")) {
    model <- enrich_language_model_capabilities(model)
  }

  model
}

#' @keywords internal
enrich_language_model_capabilities <- function(model) {
  if (!inherits(model, "LanguageModelV1")) {
    return(model)
  }

  provider <- model$provider %||% NULL
  model_id <- model$model_id %||% NULL
  if (is.null(provider) || is.null(model_id) || !nzchar(provider) || !nzchar(model_id)) {
    return(model)
  }

  info <- tryCatch(
    get_model_info(provider, model_id),
    error = function(e) NULL
  )
  config_caps <- info$capabilities %||% list()
  if (length(config_caps) == 0) {
    return(model)
  }

  model$capabilities <- utils::modifyList(
    config_caps,
    model$capabilities %||% list(),
    keep.null = TRUE
  )
  model
}

#' @keywords internal
model_capability_value <- function(model, capability, registry = NULL) {
  if (inherits(model, "LanguageModelV1")) {
    model <- enrich_language_model_capabilities(model)
    caps <- model$capabilities %||% list()
    return(caps[[capability]] %||% NULL)
  }

  if (!is.character(model) || length(model) == 0 || !nzchar(model[[1]])) {
    return(NULL)
  }

  model_id <- model[[1]]
  sep_pos <- regexpr(":", model_id, fixed = TRUE)
  if (sep_pos < 1) {
    return(NULL)
  }

  provider <- substr(model_id, 1, sep_pos - 1)
  provider_model <- substr(model_id, sep_pos + 1, nchar(model_id))
  info <- tryCatch(
    get_model_info(provider, provider_model),
    error = function(e) NULL
  )
  caps <- info$capabilities %||% list()
  caps[[capability]] %||% NULL
}

#' @keywords internal
model_capability_explicitly_unavailable <- function(model, capability, registry = NULL) {
  identical(model_capability_value(model, capability, registry = registry), FALSE)
}

#' @keywords internal
tool_required_model_capabilities <- function(tool_obj) {
  if (is.null(tool_obj) || is.null(tool_obj$meta) || !is.list(tool_obj$meta)) {
    return(character(0))
  }

  req <- tool_obj$meta$required_model_capabilities %||%
    tool_obj$meta$requires_model_capabilities %||%
    character(0)
  unique(as.character(req))
}

#' @keywords internal
filter_tools_for_model_capabilities <- function(tools, model) {
  if (is.null(tools) || length(tools) == 0) {
    return(tools)
  }

  filtered <- Filter(function(tool_obj) {
    req <- tool_required_model_capabilities(tool_obj)
    if (length(req) == 0) {
      return(TRUE)
    }

    !any(vapply(
      req,
      function(capability) model_capability_explicitly_unavailable(model, capability),
      logical(1)
    ))
  }, tools)

  if (length(filtered) == 0) {
    return(list())
  }
  filtered
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
  cli_tool_start(name, arguments)
}

#' @keywords internal
print_tool_result <- function(name, result, success = TRUE, raw_result = result) {
  cli_tool_result(name, result, success = success, raw_result = raw_result)
}
