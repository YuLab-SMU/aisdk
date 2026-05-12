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
    #' @param metadata Optional session metadata (list). Used for channel/runtime state.
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
                          metadata = NULL,
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
      private$.metadata <- if (is.null(metadata)) list() else metadata
      if (!is.null(agent) && inherits(agent, "Agent") && length(agent$capability_models %||% list()) > 0) {
        private$.metadata$capability_models <- utils::modifyList(
          agent$capability_models,
          private$.metadata$capability_models %||% list(),
          keep.null = TRUE
        )
      }
      if (is.null(model)) {
        private$.metadata <- utils::modifyList(
          model_runtime_session_metadata(get_default_model_runtime_options()),
          private$.metadata,
          keep.null = TRUE
        )
      }
      private$.metadata$context_state <- normalize_context_state(
        private$.metadata$context_state %||% NULL
      )
      private$.envir <- if (is.null(envir)) new.env(parent = globalenv()) else envir
      assign(".capability_models", private$.metadata$capability_models %||% list(), envir = private$.envir)
      if (!exists(".semantic_adapter_registry", envir = private$.envir, inherits = FALSE)) {
        assign(
          ".semantic_adapter_registry",
          create_default_semantic_adapter_registry(),
          envir = private$.envir
        )
      }
      if (!is.null(agent) && inherits(agent, "Agent") && inherits(agent$skill_registry, "SkillRegistry")) {
        assign(".skill_registry", agent$skill_registry, envir = private$.envir)
      }
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
      extra_args <- merge_call_options(self$get_model_call_options(), list(...))
      turn_system_prompt <- extra_args$turn_system_prompt %||% NULL
      extra_args$turn_system_prompt <- NULL
      call_hooks <- extra_args$hooks %||% private$.hooks
      extra_args$hooks <- NULL

      # Resolve model if needed
      model <- private$resolve_model()

      # Append user message to history
      self$append_message("user", prompt)

      prompt_payload <- private$build_prompt_payload(turn_system_prompt = turn_system_prompt)

      # Call generate_text with full history
      result <- do.call(
        generate_text,
        c(
          list(
            model = model,
            prompt = prompt_payload$messages,
            system = prompt_payload$system,
            tools = private$prepare_tools(),
            max_steps = private$.max_steps,
            session = self,
            hooks = call_hooks,
            registry = private$.registry
          ),
          extra_args
        )
      )

      private$sync_generation_messages(result)

      # Update stats
      private$update_stats(result)
      self$refresh_context_state(generation_result = result)

      result
    },

    #' @description Send a message with streaming output.
    #' @param prompt The user message to send.
    #' @param callback Function called for each chunk: callback(text, done).
    #' @param ... Additional arguments passed to stream_text.
    #' @return Invisible NULL (output is via callback).
    send_stream = function(prompt, callback, ...) {
      extra_args <- merge_call_options(self$get_model_call_options(), list(...))
      turn_system_prompt <- extra_args$turn_system_prompt %||% NULL
      extra_args$turn_system_prompt <- NULL
      call_hooks <- extra_args$hooks %||% private$.hooks
      extra_args$hooks <- NULL

      model <- private$resolve_model()

      # Append user message
      self$append_message("user", prompt)

      prompt_payload <- private$build_prompt_payload(turn_system_prompt = turn_system_prompt)

      # Let stream_text handle the entire tool execution loop
      # Pass max_steps to enable automatic tool execution
      result <- do.call(
        stream_text,
        c(
          list(
            model = model,
            prompt = prompt_payload$messages,
            callback = callback,
            system = prompt_payload$system,
            registry = private$.registry,
            tools = private$prepare_tools(),
            max_steps = private$.max_steps,
            session = self,
            hooks = call_hooks
          ),
          extra_args
        )
      )

      private$sync_generation_messages(result)

      # Update stats
      private$update_stats(result)
      self$refresh_context_state(generation_result = result)

      invisible(NULL)
    },

    #' @description Append a message to the history.
    #' @param role Message role: "user", "assistant", "system", or "tool".
    #' @param content Message content.
    #' @param reasoning Optional reasoning text to attach to the message.
    append_message = function(role, content, reasoning = NULL) {
      if (!identical(role, "tool") && is_content_block(content)) {
        content <- normalize_content_blocks(content)
      }
      msg <- list(role = role, content = content)
      if (!is.null(reasoning) && nzchar(reasoning)) {
        msg$reasoning <- reasoning
      }
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

    #' @description Get model runtime options for this session.
    #' @return A list with context overrides and call options.
    get_model_options = function() {
      compact_model_runtime_options(list(
        context_window = private$.metadata$context_window_override %||% NULL,
        max_output_tokens = private$.metadata$max_output_tokens_override %||% NULL,
        call_options = private$.metadata$model_call_options %||% list()
      ))
    },

    #' @description Get generation options applied to every model call.
    #' @return A named list of call options.
    get_model_call_options = function() {
      private$.metadata$model_call_options %||% list()
    },

    #' @description Set runtime options for this session's model.
    #' @param context_window Optional context-window override.
    #' @param max_output_tokens Optional maximum output-token metadata override.
    #' @param max_tokens Optional default generation token limit.
    #' @param thinking Optional default thinking-mode value.
    #' @param thinking_budget Optional default thinking budget.
    #' @param reasoning_effort Optional default reasoning effort.
    #' @param reset Logical. If TRUE, clears all model runtime options first.
    #' @return Invisible self for chaining.
    set_model_options = function(context_window = NULL,
                                 max_output_tokens = NULL,
                                 max_tokens = NULL,
                                 thinking = NULL,
                                 thinking_budget = NULL,
                                 reasoning_effort = NULL,
                                 reset = FALSE) {
      base <- if (isTRUE(reset)) list() else self$get_model_options()
      updates <- Filter(Negate(is.null), list(
        context_window = context_window,
        max_output_tokens = max_output_tokens,
        max_tokens = max_tokens,
        thinking = thinking,
        thinking_budget = thinking_budget,
        reasoning_effort = reasoning_effort
      ))
      merged <- compact_model_runtime_options(utils::modifyList(base, updates, keep.null = TRUE))
      metadata <- model_runtime_session_metadata(merged)

      if (isTRUE(reset) || !is.null(context_window)) {
        private$.metadata$context_window_override <- metadata$context_window_override %||% NULL
      }
      if (isTRUE(reset) || !is.null(max_output_tokens)) {
        private$.metadata$max_output_tokens_override <- metadata$max_output_tokens_override %||% NULL
      }
      if (isTRUE(reset) || any(names(updates) %in% c("max_tokens", "thinking", "thinking_budget", "reasoning_effort"))) {
        private$.metadata$model_call_options <- metadata$model_call_options %||% list()
      }

      invisible(self)
    },

    #' @description Clear model runtime options for this session.
    #' @param keys Optional option names to clear. If NULL, clears all.
    #' @return Invisible self for chaining.
    clear_model_options = function(keys = NULL) {
      if (is.null(keys)) {
        private$.metadata$context_window_override <- NULL
        private$.metadata$max_output_tokens_override <- NULL
        private$.metadata$model_call_options <- NULL
        return(invisible(self))
      }

      call_options <- private$.metadata$model_call_options %||% list()
      for (key in keys) {
        if (key %in% c("context", "context_window", "context_window_override")) {
          private$.metadata$context_window_override <- NULL
        } else if (key %in% c("output", "max_output_tokens", "max_output_tokens_override")) {
          private$.metadata$max_output_tokens_override <- NULL
        } else if (key %in% c("max_tokens", "thinking", "thinking_budget", "reasoning_effort")) {
          call_options[[key]] <- NULL
        }
      }
      private$.metadata$model_call_options <- if (length(call_options) > 0) call_options else NULL
      invisible(self)
    },

    #' @description Set a model route for a session capability.
    #' @param capability Capability route name, such as "vision.inspect".
    #' @param model Model ID string or model object. Passing NULL clears the route.
    #' @param type Model type: "auto", "language", "embedding", or "image".
    #' @param required_model_capabilities Optional required model capability flags.
    #' @return Invisible self for chaining.
    set_capability_model = function(capability,
                                    model,
                                    type = "auto",
                                    required_model_capabilities = NULL) {
      capability <- normalize_capability_name(capability)
      routes <- normalize_capability_model_routes(private$.metadata$capability_models %||% list())

      if (missing(model)) {
        rlang::abort("`model` is required.")
      }
      if (is.null(model)) {
        routes[[capability]] <- NULL
      } else {
        routes[[capability]] <- create_capability_model_route(
          model = model,
          type = type,
          required_model_capabilities = required_model_capabilities
        )
      }

      private$.metadata$capability_models <- routes
      if (!is.null(private$.envir) && is.environment(private$.envir)) {
        assign(".capability_models", routes, envir = private$.envir)
      }
      invisible(self)
    },

    #' @description Get the configured model for a session capability.
    #' @param capability Capability route name.
    #' @param default Value returned when no route is configured.
    #' @return A model ID string, model object, or default.
    get_capability_model = function(capability, default = NULL) {
      capability <- normalize_capability_name(capability)
      routes <- normalize_capability_model_routes(private$.metadata$capability_models %||% list())
      route <- routes[[capability]]
      if (is.null(route)) {
        return(default)
      }
      route$model %||% default
    },

    #' @description List session capability model routes.
    #' @return A data frame of configured session routes.
    list_capability_models = function() {
      routes <- normalize_capability_model_routes(private$.metadata$capability_models %||% list())
      if (length(routes) == 0) {
        return(data.frame(
          capability = character(0),
          model = character(0),
          type = character(0),
          required_model_capabilities = character(0),
          stringsAsFactors = FALSE
        ))
      }

      data.frame(
        capability = names(routes),
        model = vapply(routes, function(route) capability_model_label(route$model), character(1)),
        type = vapply(routes, function(route) route$type %||% "auto", character(1)),
        required_model_capabilities = vapply(
          routes,
          function(route) paste(route$required_model_capabilities %||% character(0), collapse = ", "),
          character(1)
        ),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
    },

    #' @description Clear one or all session capability model routes.
    #' @param capability Optional route name. If NULL, clears all routes.
    #' @return Invisible self for chaining.
    clear_capability_model = function(capability = NULL) {
      if (is.null(capability)) {
        private$.metadata$capability_models <- list()
        if (!is.null(private$.envir) && is.environment(private$.envir)) {
          assign(".capability_models", list(), envir = private$.envir)
        }
        return(invisible(self))
      }

      routes <- normalize_capability_model_routes(private$.metadata$capability_models %||% list())
      for (name in as.character(capability)) {
        routes[[normalize_capability_name(name)]] <- NULL
      }
      private$.metadata$capability_models <- routes
      if (!is.null(private$.envir) && is.environment(private$.envir)) {
        assign(".capability_models", routes, envir = private$.envir)
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
      default_model_id(get_model())
    },

    #' @description Get the resolved language model for this session.
    #' @return A `LanguageModelV1` object.
    get_model = function() {
      private$resolve_model()
    },

    #' @description Get tools configured on this session.
    #' @return A list of `Tool` objects.
    get_tools = function() {
      private$.tools %||% list()
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
      envir_state <- list()
      if (exists(".console_image_artifacts", envir = private$.envir, inherits = FALSE)) {
        envir_state$console_image_artifacts <- get(".console_image_artifacts", envir = private$.envir, inherits = FALSE)
      }
      if (exists(".console_image_artifact_next_id", envir = private$.envir, inherits = FALSE)) {
        envir_state$console_image_artifact_next_id <- get(".console_image_artifact_next_id", envir = private$.envir, inherits = FALSE)
      }

      list(
        version = "1.0.0",
        model_id = self$get_model_id(),
        system_prompt = private$.system_prompt,
        history = private$.history,
        stats = private$.stats,
        max_steps = private$.max_steps,
        metadata = private$.metadata,
        envir_state = envir_state,
        # Note: tools and hooks are not serialized (must be re-provided on load)
        tool_names = if (length(private$.tools) > 0) {
          sapply(private$.tools, function(t) t$name)
        } else {
          character(0)
        }
      )
    },

    #' @description Restore session from a file.
    #' @param path File path (supports .rds or .json extension).
    #' @param format Optional format override: "rds" or "json". Auto-detected from path.
    restore = function(path, format = NULL) {
      if (is.null(format)) {
        format <- if (grepl("\\.json$", path, ignore.case = TRUE)) "json" else "rds"
      }

      data <- if (format == "json") {
        json_str <- paste(readLines(path, warn = FALSE), collapse = "\n")
        jsonlite::fromJSON(json_str, simplifyVector = FALSE)
      } else {
        readRDS(path)
      }

      self$restore_from_list(data)
      invisible(self)
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
      if (!is.null(data$metadata)) {
        private$.metadata <- data$metadata
      }
      if (!is.null(data$envir_state$console_image_artifacts)) {
        assign(".console_image_artifacts", data$envir_state$console_image_artifacts, envir = private$.envir)
      }
      if (!is.null(data$envir_state$console_image_artifact_next_id)) {
        assign(".console_image_artifact_next_id", data$envir_state$console_image_artifact_next_id, envir = private$.envir)
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
      cat("  Metadata:", length(private$.metadata), "keys\n")
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

    #' @description Get a value from session metadata.
    #' @param key The metadata key to retrieve.
    #' @param default Default value if key is not present.
    #' @return The stored metadata value or default.
    get_metadata = function(key, default = NULL) {
      if (key %in% names(private$.metadata)) {
        private$.metadata[[key]]
      } else {
        default
      }
    },

    #' @description Set a value in session metadata.
    #' @param key The metadata key to set.
    #' @param value The value to store.
    #' @return Invisible self for chaining.
    set_metadata = function(key, value) {
      private$.metadata[[key]] <- value
      invisible(self)
    },

    #' @description Merge a named list into session metadata.
    #' @param values Named list of metadata values.
    #' @return Invisible self for chaining.
    merge_metadata = function(values) {
      if (is.null(values) || length(values) == 0) {
        return(invisible(self))
      }
      if (is.null(names(values)) || any(!nzchar(names(values)))) {
        rlang::abort("Session metadata updates must be a named list.")
      }
      for (key in names(values)) {
        private$.metadata[[key]] <- values[[key]]
      }
      if ("capability_models" %in% names(values) &&
          !is.null(private$.envir) &&
          is.environment(private$.envir)) {
        assign(".capability_models", private$.metadata$capability_models %||% list(), envir = private$.envir)
      }
      invisible(self)
    },

    #' @description List metadata keys.
    #' @return Character vector of metadata keys.
    list_metadata = function() {
      names(private$.metadata)
    },

    #' @description Get the adaptive context state for this session.
    #' @return A normalized context state list.
    get_context_state = function() {
      normalize_context_state(private$.metadata$context_state %||% NULL)
    },

    #' @description Store adaptive context state for this session.
    #' @param state Context state list.
    #' @return Invisible self for chaining.
    set_context_state = function(state) {
      private$.metadata$context_state <- normalize_context_state(state)
      invisible(self)
    },

    #' @description Clear adaptive context state back to defaults.
    #' @return Invisible self for chaining.
    clear_context_state = function() {
      private$.metadata$context_state <- create_context_state()
      invisible(self)
    },

    #' @description Get the context management mode for this session.
    #' @return One of "off", "basic", or "adaptive".
    get_context_management_mode = function() {
      get_session_context_management_mode(self)
    },

    #' @description Get the full adaptive context-management configuration.
    #' @return A normalized context-management configuration list.
    get_context_management_config = function() {
      get_context_management_config_impl(self)
    },

    #' @description Override the context management mode for this session.
    #' @param mode One of "off", "basic", or "adaptive".
    #' @return Invisible self for chaining.
    set_context_management_mode = function(mode) {
      private$.metadata$context_management_mode <- normalize_context_management_mode(mode)
      invisible(self)
    },

    #' @description Apply adaptive context-management configuration to this session.
    #' @param config Optional config list created by `create_context_management_config()`.
    #' @param ... Optional overrides forwarded to `set_context_management_config()`.
    #' @return Invisible self for chaining.
    set_context_management_config = function(config = NULL, ...) {
      set_context_management_config_impl(self, config = config, ...)
    },

    #' @description Estimate current context metrics for this session.
    #' @param turn_system_prompt Optional turn-specific system prompt to include in the estimate.
    #' @return A list of context metrics, or NULL if no model metadata is available.
    get_context_metrics = function(turn_system_prompt = NULL) {
      get_session_context_metrics(
        session = self,
        system_prompt = private$combine_system_prompt(turn_system_prompt),
        messages = private$.history
      )
    },

    #' @description Build a budget-aware prompt payload from current session history.
    #' @param turn_system_prompt Optional turn-specific system prompt.
    #' @return A list with `messages`, `system`, `metrics`, and `state`.
    assemble_messages = function(turn_system_prompt = NULL) {
      private$build_prompt_payload(turn_system_prompt = turn_system_prompt)
    },

    #' @description Refresh the adaptive context state from current history.
    #' @param generation_result Optional GenerateResult used to update tool/artifact digests.
    #' @param turn_system_prompt Optional turn-specific system prompt for the snapshot.
    #' @return The normalized context state list.
    refresh_context_state = function(generation_result = NULL, turn_system_prompt = NULL) {
      current_state <- self$get_context_state()
      metrics <- self$get_context_metrics(turn_system_prompt = turn_system_prompt)
      synthesized_state <- synthesize_context_state(
        session = self,
        state = current_state,
        generation_result = generation_result,
        metrics = metrics,
        messages = private$.history
      )
      self$set_context_state(synthesized_state)
      assembled <- assemble_session_messages(
        session = self,
        messages = private$.history,
        system = private$combine_system_prompt(turn_system_prompt),
        persist = TRUE
      )
      assembled$state
    },

    #' @description List compact context handles available to this session.
    #' @return A list of context handle records.
    list_context_handles = function() {
      list_context_handles(self)
    },

    #' @description Create context query tools bound to this session.
    #' @return A list of `Tool` objects.
    create_context_query_tools = function() {
      create_context_query_tools(self)
    },

    #' @description Run a bounded child session for a focused query.
    #' @param ... Arguments passed to `sub_session_query()`.
    #' @return A compact sub-session result list.
    sub_session_query = function(...) {
      sub_session_query(self, ...)
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
    },

    #' @description Save a memory snapshot to a file (checkpoint for Mission resume).
    #' @param path File path (.rds). If NULL, uses a temp file and returns the path.
    #' @return Invisible file path.
    checkpoint = function(path = NULL) {
      if (is.null(path)) {
        path <- tempfile(pattern = "session_checkpoint_", fileext = ".rds")
      }
      saveRDS(list(
        model_id      = self$get_model_id(),
        system_prompt = private$.system_prompt,
        memory        = private$.memory,
        metadata      = private$.metadata,
        history       = private$.history,
        stats         = private$.stats
      ), path)
      invisible(path)
    },

    #' @description Restore memory and history from a checkpoint file.
    #' @param path File path to a checkpoint created by checkpoint().
    #' @return Invisible self for chaining.
    restore_checkpoint = function(path) {
      data <- readRDS(path)
      if (!is.null(data$memory))  private$.memory  <- data$memory
      if (!is.null(data$metadata)) private$.metadata <- data$metadata
      if (!is.null(data$history)) private$.history <- data$history
      if (!is.null(data$stats))   private$.stats   <- data$stats
      invisible(self)
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
    .metadata = NULL,
    .envir = NULL,
    combine_system_prompt = function(turn_system_prompt = NULL) {
      if (is.null(turn_system_prompt) || !nzchar(turn_system_prompt)) {
        return(private$.system_prompt)
      }
      if (is.null(private$.system_prompt) || !nzchar(private$.system_prompt)) {
        return(turn_system_prompt)
      }
      paste(private$.system_prompt, "\n\n", turn_system_prompt, sep = "")
    },
    build_prompt_payload = function(turn_system_prompt = NULL) {
      combined_system <- private$combine_system_prompt(turn_system_prompt)
      if (identical(get_session_context_management_mode(self), "off")) {
        state <- self$get_context_state()
        metrics <- get_session_context_metrics(
          session = self,
          system_prompt = combined_system,
          messages = private$.history
        )
        if (!is.null(metrics)) {
          state$occupancy <- list(
            estimated_prompt_tokens = metrics$used_tokens %||% NA_real_,
            context_window = metrics$context_window %||% NA_real_,
            ratio = metrics$ratio %||% NA_real_,
            status = metrics$regime %||% "unknown"
          )
          self$set_context_state(state)
        }
        return(list(
          messages = private$.history,
          system = combined_system,
          metrics = metrics,
          state = state
        ))
      }

      assemble_session_messages(
        session = self,
        messages = private$.history,
        system = combined_system,
        persist = TRUE
      )
    },
    sync_generation_messages = function(result) {
      if (!is.null(result$messages_added) && length(result$messages_added) > 0) {
        for (msg in result$messages_added) {
          private$.history <- c(private$.history, list(msg))
        }
        return(invisible())
      }

      if (!is.null(result$text) && nzchar(result$text)) {
        msg <- list(role = "assistant", content = result$text)
        if (!is.null(result$reasoning) && nzchar(result$reasoning)) {
          msg$reasoning <- result$reasoning
        }
        private$.history <- c(private$.history, list(msg))
      }

      invisible()
    },
    prepare_tools = function() {
      tools <- private$.tools %||% list()
      if (identical(get_session_context_management_mode(self), "adaptive")) {
        tools <- append_unique_tools(tools, create_context_query_tools(self))
      }
      tools
    },
    resolve_model = function() {
      if (!is.null(private$.model)) {
        return(private$.model)
      }
      model_ref <- private$.model_id %||% get_model()

      if (inherits(model_ref, "LanguageModelV1")) {
        private$.model <- model_ref
        private$.model_id <- default_model_id(model_ref)
        return(private$.model)
      }

      if (is.character(model_ref)) {
        private$.model_id <- model_ref
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
#' @param metadata Optional session metadata (list).
#' @param agent Optional Agent object to initialize from.
#' @return A ChatSession object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # Create a chat session
#'   chat <- create_chat_session(
#'     model = "openai:gpt-4o",
#'     system_prompt = "You are a helpful R programming assistant."
#'   )
#'
#'   # Create from an existing agent
#'   agent <- create_agent("MathAgent", "Does math", system_prompt = "You are a math wizard.")
#'   chat <- create_chat_session(model = "openai:gpt-4o", agent = agent)
#'
#'   # Send messages
#'   response <- chat$send("How do I read a CSV file?")
#'   print(response$text)
#'
#'   # Continue the conversation (history is maintained)
#'   response <- chat$send("What about Excel files?")
#'
#'   # Check stats
#'   print(chat$stats())
#'
#'   # Save session
#'   chat$save("my_session.rds")
#' }
#' }
create_chat_session <- function(model = NULL,
                                system_prompt = NULL,
                                tools = NULL,
                                hooks = NULL,
                                max_steps = 10,
                                metadata = NULL,
                                agent = NULL) {
  ChatSession$new(
    model = model,
    system_prompt = system_prompt,
    tools = tools,
    hooks = hooks,
    max_steps = max_steps,
    metadata = metadata,
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
#' \donttest{
#' if (interactive()) {
#'   # Load a saved session
#'   chat <- load_chat_session("my_session.rds", tools = my_tools)
#'
#'   # Continue where you left off
#'   response <- chat$send("Let's continue our discussion")
#' }
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
