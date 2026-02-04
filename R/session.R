#' @title Session Management: Stateful Chat Sessions
#' @description
#' ChatSession R6 class for managing stateful conversations with LLMs.
#' Provides automatic history management, persistence, and model switching.
#' @name session
NULL

#' @title ChatSession Class
#' @description
#' R6 class representing a stateful chat session. Automatically manages
#' conversation history, supports tool execution, and provides persistence.
#' @export
ChatSession <- R6::R6Class(
  "ChatSession",
  public = list(
    #' @description Initialize a new ChatSession.
    #' @param model A LanguageModelV1 object or model string ID (e.g., "openai:gpt-4o").
    #' @param system_prompt Optional system prompt for the conversation.
    #' @param tools Optional list of Tool objects for function calling.
    #' @param hooks Optional HookHandler object for event hooks.
    #' @param history Optional initial message history (list of message objects).
    #' @param max_steps Maximum steps for tool execution loops. Default 10.
    #' @param registry Optional ProviderRegistry for model resolution.
    #' @param memory Optional initial shared memory (list). For multi-agent state sharing.
    #' @param envir Optional shared R environment. For multi-agent data sharing.
    #' @param agent Optional Agent object. If provided, the session inherits the agent's
    #'   tools and system prompt.
    initialize = function(model = NULL,
                          system_prompt = NULL,
                          tools = NULL,
                          hooks = NULL,
                          history = NULL,
                          max_steps = 10,
                          registry = NULL,
                          memory = NULL,
                          envir = NULL,
                          agent = NULL) {
      private$.model_id <- if (is.character(model)) model else NULL
      private$.model <- if (!is.null(model) && !is.character(model)) model else NULL

      # Handle agent if provided
      agent_system <- NULL
      agent_tools <- list()

      if (!is.null(agent)) {
        if (!inherits(agent, "Agent")) {
          rlang::abort("Argument 'agent' must be an Agent object.")
        }
        agent_system <- agent$system_prompt
        agent_tools <- agent$tools
      }

      # Merge system prompts (agent's prompt + session specific prompt)
      if (!is.null(agent_system) && !is.null(system_prompt)) {
        private$.system_prompt <- paste(agent_system, "\n\n", system_prompt, sep = "")
      } else {
        private$.system_prompt <- system_prompt %||% agent_system
      }

      # Merge tools (session tools + agent tools)
      # Tools provided directly to session take precedence if names collide?
      # For now, we just concatenate them.
      private$.tools <- c(tools %||% list(), agent_tools)
      private$.hooks <- hooks
      private$.history <- history %||% list()
      private$.max_steps <- max_steps
      private$.registry <- registry
      # Multi-agent support: shared memory and environment
      private$.memory <- if (is.null(memory)) list() else memory
      private$.envir <- if (is.null(envir)) new.env(parent = globalenv()) else envir
      private$.stats <- list(
        total_prompt_tokens = 0,
        total_completion_tokens = 0,
        total_tokens = 0,
        messages_sent = 0,
        tool_calls_made = 0
      )
    },

    #' @description Send a message and get a response.
    #' @param prompt The user message to send.
    #' @param ... Additional arguments passed to generate_text.
    #' @return The GenerateResult object from the model.
    send = function(prompt, ...) {
      # Resolve model if needed
      model <- private$get_model()

      # Append user message to history
      self$append_message("user", prompt)

      # Build messages for API call (system prompt + history)
      messages <- private$.history

      # Call generate_text with full history
      result <- generate_text(
        model = model,
        prompt = messages,
        system = private$.system_prompt,
        tools = private$.tools,
        max_steps = private$.max_steps,
        hooks = private$.hooks,
        registry = private$.registry,
        ...
      )

      # Append assistant response to history
      if (!is.null(result$text) && nzchar(result$text)) {
        self$append_message("assistant", result$text)
      }

      # Update stats
      private$update_stats(result)

      result
    },

    #' @description Send a message with streaming output.
    #' @param prompt The user message to send.
    #' @param callback Function called for each chunk: callback(text, done).
    #' @param ... Additional arguments passed to stream_text.
    #' @return Invisible NULL (output is via callback).
    send_stream = function(prompt, callback, ...) {
      model <- private$get_model()

      # Append user message
      self$append_message("user", prompt)

      # Build messages from current history
      messages <- private$.history

      # Let stream_text handle the entire tool execution loop
      # Pass max_steps to enable automatic tool execution
      result <- stream_text(
        model = model,
        prompt = messages,
        callback = callback,
        system = private$.system_prompt,
        registry = private$.registry,
        tools = private$.tools,
        max_steps = private$.max_steps,
        session = self, # Pass session for shared environment
        hooks = private$.hooks,
        ...
      )

      # Sync messages added during tool execution to session history
      # This includes all intermediate assistant (with tool_calls) and tool result messages
      if (!is.null(result$messages_added) && length(result$messages_added) > 0) {
        for (msg in result$messages_added) {
          private$.history <- c(private$.history, list(msg))
        }
      } else if (!is.null(result$text) && nzchar(result$text)) {
        # No tool calls were made, just append the final response
        self$append_message("assistant", result$text)
      }

      # Update stats
      private$update_stats(result)

      invisible(NULL)
    },

    #' @description Append a message to the history.
    #' @param role Message role: "user", "assistant", "system", or "tool".
    #' @param content Message content.
    append_message = function(role, content) {
      msg <- list(role = role, content = content)
      private$.history <- c(private$.history, list(msg))
      invisible(self)
    },

    #' @description Get the conversation history.
    #' @return A list of message objects.
    get_history = function() {
      private$.history
    },

    #' @description Get the last response from the assistant.
    #' @return The content of the last assistant message, or NULL.
    get_last_response = function() {
      for (i in rev(seq_along(private$.history))) {
        if (private$.history[[i]]$role == "assistant") {
          return(private$.history[[i]]$content)
        }
      }
      NULL
    },

    #' @description Clear the conversation history.
    #' @param keep_system If TRUE, keeps the system prompt. Default TRUE.
    clear_history = function(keep_system = TRUE) {
      private$.history <- list()
      invisible(self)
    },

    #' @description Switch to a different model.
    #' @param model A LanguageModelV1 object or model string ID.
    switch_model = function(model) {
      if (is.character(model)) {
        private$.model_id <- model
        private$.model <- NULL
      } else if (inherits(model, "LanguageModelV1")) {
        private$.model <- model
        private$.model_id <- paste0(model$provider, ":", model$model_id)
      } else {
        rlang::abort("model must be a string ID or LanguageModelV1 object")
      }
      invisible(self)
    },

    #' @description Get current model identifier.
    #' @return Model ID string.
    get_model_id = function() {
      if (!is.null(private$.model_id)) {
        return(private$.model_id)
      }
      if (!is.null(private$.model)) {
        return(paste0(private$.model$provider, ":", private$.model$model_id))
      }
      NULL
    },

    #' @description Get token usage statistics.
    #' @return A list with token counts and message stats.
    stats = function() {
      private$.stats
    },

    #' @description Save session to a file.
    #' @param path File path (supports .rds or .json extension).
    #' @param format Optional format override: "rds" or "json". Auto-detected from path.
    save = function(path, format = NULL) {
      # Detect format from extension if not specified
      if (is.null(format)) {
        format <- if (grepl("\\.json$", path, ignore.case = TRUE)) "json" else "rds"
      }

      data <- self$as_list()

      if (format == "json") {
        json_str <- jsonlite::toJSON(data, auto_unbox = TRUE, pretty = TRUE)
        writeLines(json_str, path)
      } else {
        saveRDS(data, path)
      }

      invisible(self)
    },

    #' @description Export session state as a list (for serialization).
    #' @return A list containing session state.
    as_list = function() {
      list(
        version = "0.9.0",
        model_id = self$get_model_id(),
        system_prompt = private$.system_prompt,
        history = private$.history,
        stats = private$.stats,
        max_steps = private$.max_steps,
        # Note: tools and hooks are not serialized (must be re-provided on load)
        tool_names = if (length(private$.tools) > 0) {
          sapply(private$.tools, function(t) t$name)
        } else {
          character(0)
        }
      )
    },

    #' @description Restore session state from a list.
    #' @param data A list exported by as_list().
    restore_from_list = function(data) {
      if (!is.null(data$model_id)) {
        private$.model_id <- data$model_id
        private$.model <- NULL
      }
      if (!is.null(data$system_prompt)) {
        private$.system_prompt <- data$system_prompt
      }
      if (!is.null(data$history)) {
        private$.history <- data$history
      }
      if (!is.null(data$stats)) {
        private$.stats <- data$stats
      }
      if (!is.null(data$max_steps)) {
        private$.max_steps <- data$max_steps
      }
      invisible(self)
    },

    #' @description Print method for ChatSession.
    print = function() {
      cat("<ChatSession>\n")
      cat("  Model:", self$get_model_id() %||% "(not set)", "\n")
      cat("  History:", length(private$.history), "messages\n")
      cat("  Tools:", length(private$.tools), "tools\n")
      cat("  Memory:", length(private$.memory), "keys\n")
      cat("  Envir:", length(ls(private$.envir)), "objects\n")
      cat("  Stats:\n")
      cat("    Total tokens:", private$.stats$total_tokens, "\n")
      cat("    Messages sent:", private$.stats$messages_sent, "\n")
      invisible(self)
    },

    # =========================================================================
    # Multi-Agent Support: Memory and Environment
    # =========================================================================

    #' @description Get a value from shared memory.
    #' @param key The key to retrieve.
    #' @param default Default value if key not found. Default NULL.
    #' @return The stored value or default.
    get_memory = function(key, default = NULL) {
      if (key %in% names(private$.memory)) {
        private$.memory[[key]]
      } else {
        default
      }
    },

    #' @description Set a value in shared memory.
    #' @param key The key to store.
    #' @param value The value to store.
    #' @return Invisible self for chaining.
    set_memory = function(key, value) {
      private$.memory[[key]] <- value
      invisible(self)
    },

    #' @description List all keys in shared memory.
    #' @return Character vector of memory keys.
    list_memory = function() {
      names(private$.memory)
    },

    #' @description Clear shared memory.
    #' @param keys Optional specific keys to clear. If NULL, clears all.
    #' @return Invisible self for chaining.
    clear_memory = function(keys = NULL) {
      if (is.null(keys)) {
        private$.memory <- list()
      } else {
        private$.memory[keys] <- NULL
      }
      invisible(self)
    },

    #' @description Get the shared R environment.
    #' @details
    #' This environment is shared across all agents using this session.
    #' Agents can store and retrieve data frames, models, and other R objects.
    #' @return An environment object.
    get_envir = function() {
      private$.envir
    },

    #' @description Evaluate an expression in the session environment.
    #' @param expr An expression to evaluate.
    #' @return The result of the evaluation.
    eval_in_session = function(expr) {
      eval(expr, envir = private$.envir)
    },

    #' @description List objects in the session environment.
    #' @return Character vector of object names.
    list_envir = function() {
      ls(private$.envir)
    }
  ),
  private = list(
    .model = NULL,
    .model_id = NULL,
    .system_prompt = NULL,
    .tools = NULL,
    .hooks = NULL,
    .history = NULL,
    .max_steps = 10,
    .registry = NULL,
    .stats = NULL,
    # Multi-agent support
    .memory = NULL,
    .envir = NULL,
    get_model = function() {
      if (!is.null(private$.model)) {
        return(private$.model)
      }
      if (!is.null(private$.model_id)) {
        reg <- private$.registry %||% get_default_registry()
        private$.model <- reg$language_model(private$.model_id)
        return(private$.model)
      }
      rlang::abort("No model configured for ChatSession")
    },
    update_stats = function(result) {
      private$.stats$messages_sent <- private$.stats$messages_sent + 1

      if (!is.null(result$usage)) {
        private$.stats$total_prompt_tokens <- private$.stats$total_prompt_tokens +
          (result$usage$prompt_tokens %||% 0)
        private$.stats$total_completion_tokens <- private$.stats$total_completion_tokens +
          (result$usage$completion_tokens %||% 0)
        private$.stats$total_tokens <- private$.stats$total_tokens +
          (result$usage$total_tokens %||% 0)
      }

      if (!is.null(result$all_tool_calls)) {
        private$.stats$tool_calls_made <- private$.stats$tool_calls_made +
          length(result$all_tool_calls)
      } else if (!is.null(result$tool_calls)) {
        private$.stats$tool_calls_made <- private$.stats$tool_calls_made +
          length(result$tool_calls)
      }
    }
  )
)

#' @title Create a Chat Session
#' @description
#' Factory function to create a new ChatSession object.
#' @param model A LanguageModelV1 object or model string ID.
#' @param system_prompt Optional system prompt.
#' @param tools Optional list of Tool objects.
#' @param hooks Optional HookHandler object.
#' @param max_steps Maximum tool execution steps. Default 10.
#' @param agent Optional Agent object to initialize from.
#' @return A ChatSession object.
#' @export
#' @examples
#' \dontrun{
#' # Create a chat session
#' chat <- create_chat_session(
#'   model = "openai:gpt-4o",
#'   system_prompt = "You are a helpful R programming assistant."
#' )
#'
#' # Create from an existing agent
#' agent <- create_agent("MathAgent", "Does math", system_prompt = "You are a math wizard.")
#' chat <- create_chat_session(model = "openai:gpt-4o", agent = agent)
#'
#' # Send messages
#' response <- chat$send("How do I read a CSV file?")
#' print(response$text)
#'
#' # Continue the conversation (history is maintained)
#' response <- chat$send("What about Excel files?")
#'
#' # Check stats
#' print(chat$stats())
#'
#' # Save session
#' chat$save("my_session.rds")
#' }
create_chat_session <- function(model = NULL,
                                system_prompt = NULL,
                                tools = NULL,
                                hooks = NULL,
                                max_steps = 10,
                                agent = NULL) {
  ChatSession$new(
    model = model,
    system_prompt = system_prompt,
    tools = tools,
    hooks = hooks,
    max_steps = max_steps,
    agent = agent
  )
}

#' @title Load a Chat Session
#' @description
#' Load a previously saved ChatSession from a file.
#' @param path File path to load from (.rds or .json).
#' @param tools Optional list of Tool objects (tools are not saved, must be re-provided).
#' @param hooks Optional HookHandler object.
#' @param registry Optional ProviderRegistry.
#' @return A ChatSession object with restored state.
#' @export
#' @examples
#' \dontrun{
#' # Load a saved session
#' chat <- load_chat_session("my_session.rds", tools = my_tools)
#'
#' # Continue where you left off
#' response <- chat$send("Let's continue our discussion")
#' }
load_chat_session <- function(path, tools = NULL, hooks = NULL, registry = NULL) {
  # Detect format
  if (grepl("\\.json$", path, ignore.case = TRUE)) {
    json_str <- paste(readLines(path, warn = FALSE), collapse = "\n")
    data <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
  } else {
    data <- readRDS(path)
  }

  # Create new session

  session <- ChatSession$new(
    model = data$model_id,
    tools = tools,
    hooks = hooks,
    registry = registry
  )

  # Restore state

  session$restore_from_list(data)

  # Warn if tools were used but not provided
  if (length(data$tool_names) > 0 && (is.null(tools) || length(tools) == 0)) {
    rlang::warn(paste0(
      "Original session used tools: ", paste(data$tool_names, collapse = ", "),
      ". Re-provide these tools to continue using them."
    ))
  }

  session
}

# Null-coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
