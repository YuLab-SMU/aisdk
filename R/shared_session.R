#' @title SharedSession: Enhanced Multi-Agent Session Management
#' @description
#' SharedSession R6 class providing enhanced environment management, execution
#' context tracking, and safety guardrails for multi-agent orchestration.
#' @name shared_session
NULL

#' @title SharedSession Class
#' @description
#' R6 class representing an enhanced session for multi-agent systems.
#' Extends ChatSession with:
#' - Execution context tracking (call stack, delegation history)
#' - Sandboxed code execution with safety guardrails
#' - Variable scoping and access control
#' - Comprehensive tracing and observability
#' @export
SharedSession <- R6::R6Class(
  "SharedSession",
  inherit = ChatSession,

  public = list(
    #' @description Initialize a new SharedSession.
    #' @param model A LanguageModelV1 object or model string ID.
    #' @param system_prompt Optional system prompt for the conversation.
    #' @param tools Optional list of Tool objects.
    #' @param hooks Optional HookHandler object.
    #' @param max_steps Maximum steps for tool execution loops. Default 10.
    #' @param registry Optional ProviderRegistry for model resolution.
    #' @param sandbox_mode Sandbox mode: "strict", "permissive", or "none". Default "strict".
    #' @param trace_enabled Enable execution tracing. Default TRUE.
    initialize = function(model = NULL,
                          system_prompt = NULL,
                          tools = NULL,
                          hooks = NULL,
                          max_steps = 10,
                          registry = NULL,
                          sandbox_mode = "strict",
                          trace_enabled = TRUE) {
      # Call parent constructor
      super$initialize(
        model = model,
        system_prompt = system_prompt,
        tools = tools,
        hooks = hooks,
        max_steps = max_steps,
        registry = registry
      )

      # Enhanced session state
      private$.sandbox_mode <- sandbox_mode
      private$.trace_enabled <- trace_enabled
      private$.execution_trace <- list()
      private$.delegation_stack <- list()
      private$.variable_scopes <- list(global = new.env(parent = baseenv()))

      private$.access_control <- list()
      private$.execution_context <- list(
        current_agent = NULL,
        depth = 0,
        start_time = NULL,
        global_task = NULL
      )
    },

    # =========================================================================
    # Execution Context Management
    # =========================================================================

    #' @description Push an agent onto the execution stack.
    #' @param agent_name Name of the agent being activated.
    #' @param task The task being delegated.
    #' @param parent_agent Name of the delegating agent (or NULL for root).
    #' @return Invisible self for chaining.
    push_context = function(agent_name, task, parent_agent = NULL) {
      context <- list(
        agent = agent_name,
        task = task,
        parent = parent_agent,
        started_at = Sys.time(),
        depth = length(private$.delegation_stack) + 1
      )

      private$.delegation_stack <- c(private$.delegation_stack, list(context))
      private$.execution_context$current_agent <- agent_name
      private$.execution_context$depth <- length(private$.delegation_stack)

      if (private$.trace_enabled) {
        self$trace_event("context_push", list(
          agent = agent_name,
          task = task,
          parent = parent_agent,
          depth = context$depth
        ))
      }

      invisible(self)
    },

    #' @description Pop the current agent from the execution stack.
    #' @param result Optional result from the completed agent.
    #' @return The popped context, or NULL if stack was empty.
    pop_context = function(result = NULL) {
      if (length(private$.delegation_stack) == 0) {
        return(NULL)
      }

      # Pop the last context
      idx <- length(private$.delegation_stack)
      context <- private$.delegation_stack[[idx]]
      context$ended_at <- Sys.time()
      context$duration_ms <- as.numeric(difftime(
        context$ended_at, context$started_at, units = "secs"
      )) * 1000
      context$result_summary <- if (!is.null(result)) {
        substr(as.character(result), 1, 200)
      } else {
        NULL
      }

      private$.delegation_stack <- private$.delegation_stack[-idx]

      # Update current context
      if (length(private$.delegation_stack) > 0) {
        private$.execution_context$current_agent <-
          private$.delegation_stack[[length(private$.delegation_stack)]]$agent
      } else {
        private$.execution_context$current_agent <- NULL
      }
      private$.execution_context$depth <- length(private$.delegation_stack)

      if (private$.trace_enabled) {
        self$trace_event("context_pop", list(
          agent = context$agent,
          duration_ms = context$duration_ms,
          depth = context$depth
        ))
      }

      context
    },

    #' @description Get the current execution context.
    #' @return A list with current_agent, depth, and delegation_stack.
    get_context = function() {
      list(
        current_agent = private$.execution_context$current_agent,
        depth = private$.execution_context$depth,
        global_task = private$.execution_context$global_task,
        stack = lapply(private$.delegation_stack, function(ctx) {
          list(agent = ctx$agent, task = ctx$task, depth = ctx$depth)
        })
      )
    },

    #' @description Set the global task (user's original request).
    #' @param task The global task description.
    #' @return Invisible self for chaining.
    set_global_task = function(task) {
      private$.execution_context$global_task <- task
      private$.execution_context$start_time <- Sys.time()
      invisible(self)
    },

    # =========================================================================
    # Sandboxed Code Execution
    # =========================================================================

    #' @description Execute R code in a sandboxed environment.
    #' @param code R code to execute (character string).
    #' @param scope Variable scope: "global", "agent", or a custom scope name.
    #' @param timeout_ms Execution timeout in milliseconds. Default 30000.
    #' @param capture_output Capture stdout/stderr. Default TRUE.
    #' @return A list with result, output, error, and duration_ms.
    execute_code = function(code,
                            scope = "global",
                            timeout_ms = 30000,
                            capture_output = TRUE) {
      start_time <- Sys.time()

      # Get or create the scope environment
      env <- private$get_scope_env(scope)

      # Check sandbox restrictions
      if (private$.sandbox_mode != "none") {
        violation <- private$check_sandbox_violation(code)
        if (!is.null(violation)) {
          return(list(
            result = NULL,
            output = NULL,
            error = paste0("Sandbox violation: ", violation),
            duration_ms = 0,
            success = FALSE
          ))
        }
      }

      # Execute with error handling and output capture
      result <- tryCatch({
        output <- if (capture_output) {
          utils::capture.output({
            value <- eval(parse(text = code), envir = env)
          })
        } else {
          value <- eval(parse(text = code), envir = env)
          NULL
        }

        list(
          result = value,
          output = output,
          error = NULL,
          success = TRUE
        )
      }, error = function(e) {
        list(
          result = NULL,
          output = NULL,
          error = conditionMessage(e),
          success = FALSE
        )
      })

      result$duration_ms <- as.numeric(difftime(
        Sys.time(), start_time, units = "secs"
      )) * 1000

      if (private$.trace_enabled) {
        self$trace_event("code_execution", list(
          scope = scope,
          code_length = nchar(code),
          success = result$success,
          duration_ms = result$duration_ms,
          error = result$error
        ))
      }

      result
    },

    # =========================================================================
    # Variable Scoping
    # =========================================================================

    #' @description Get a variable from a specific scope.
    #' @param name Variable name.
    #' @param scope Scope name. Default "global".
    #' @param default Default value if not found.
    #' @return The variable value or default.
    get_var = function(name, scope = "global", default = NULL) {
      env <- private$get_scope_env(scope)
      if (exists(name, envir = env, inherits = FALSE)) {
        get(name, envir = env)
      } else {
        default
      }
    },

    #' @description Set a variable in a specific scope.
    #' @param name Variable name.
    #' @param value Variable value.
    #' @param scope Scope name. Default "global".
    #' @return Invisible self for chaining.
    set_var = function(name, value, scope = "global") {
      env <- private$get_scope_env(scope)
      assign(name, value, envir = env)

      if (private$.trace_enabled) {
        self$trace_event("var_set", list(
          name = name,
          scope = scope,
          type = class(value)[1]
        ))
      }

      invisible(self)
    },

    #' @description List variables in a scope.
    #' @param scope Scope name. Default "global".
    #' @param pattern Optional pattern to filter names.
    #' @return Character vector of variable names.
    list_vars = function(scope = "global", pattern = NULL) {
      env <- private$get_scope_env(scope)
      ls(env, pattern = pattern)
    },

    #' @description Get a summary of all variables in a scope.
    #' @param scope Scope name. Default "global".
    #' @return A data frame with name, type, and size information.
    summarize_vars = function(scope = "global") {
      env <- private$get_scope_env(scope)
      vars <- ls(env)

      if (length(vars) == 0) {
        return(data.frame(
          name = character(0),
          type = character(0),
          size = character(0),
          stringsAsFactors = FALSE
        ))
      }

      summaries <- lapply(vars, function(v) {
        obj <- get(v, envir = env)
        type <- paste(class(obj), collapse = ", ")
        size <- if (is.data.frame(obj)) {
          paste0(nrow(obj), " x ", ncol(obj))
        } else if (is.vector(obj) && !is.list(obj)) {
          paste0("length ", length(obj))
        } else if (is.list(obj)) {
          paste0("list of ", length(obj))
        } else {
          format(utils::object.size(obj), units = "auto")

        }
        data.frame(name = v, type = type, size = size, stringsAsFactors = FALSE)
      })

      do.call(rbind, summaries)
    },

    #' @description Create a new variable scope.
    #' @param scope_name Name for the new scope.
    #' @param parent_scope Parent scope name. Default "global".
    #' @return Invisible self for chaining.
    create_scope = function(scope_name, parent_scope = "global") {
      if (scope_name %in% names(private$.variable_scopes)) {
        rlang::warn(paste0("Scope '", scope_name, "' already exists. Skipping."))
        return(invisible(self))
      }

      parent_env <- private$get_scope_env(parent_scope)
      private$.variable_scopes[[scope_name]] <- new.env(parent = parent_env)

      invisible(self)
    },

    #' @description Delete a variable scope.
    #' @param scope_name Name of the scope to delete.
    #' @return Invisible self for chaining.
    delete_scope = function(scope_name) {
      if (scope_name == "global") {
        rlang::abort("Cannot delete the global scope.")
      }
      private$.variable_scopes[[scope_name]] <- NULL
      invisible(self)
    },

    # =========================================================================
    # Tracing and Observability
    # =========================================================================

    #' @description Record a trace event.
    #' @param event_type Type of event (e.g., "context_push", "code_execution").
    #' @param data Event data as a list.
    #' @return Invisible self for chaining.
    trace_event = function(event_type, data = list()) {
      if (!private$.trace_enabled) {
        return(invisible(self))
      }

      event <- list(
        timestamp = Sys.time(),
        type = event_type,
        agent = private$.execution_context$current_agent,
        depth = private$.execution_context$depth,
        data = data
      )

      private$.execution_trace <- c(private$.execution_trace, list(event))
      invisible(self)
    },

    #' @description Get the execution trace.
    #' @param event_types Optional filter by event types.
    #' @param agent Optional filter by agent name.
    #' @return A list of trace events.
    get_trace = function(event_types = NULL, agent = NULL) {
      trace <- private$.execution_trace

      if (!is.null(event_types)) {
        trace <- Filter(function(e) e$type %in% event_types, trace)
      }

      if (!is.null(agent)) {
        trace <- Filter(function(e) identical(e$agent, agent), trace)
      }

      trace
    },

    #' @description Clear the execution trace.
    #' @return Invisible self for chaining.
    clear_trace = function() {
      private$.execution_trace <- list()
      invisible(self)
    },

    #' @description Get trace summary statistics.
    #' @return A list with event counts, agent activity, and timing info.
    trace_summary = function() {
      trace <- private$.execution_trace

      if (length(trace) == 0) {
        return(list(
          total_events = 0,
          event_counts = list(),
          agent_activity = list(),
          duration_ms = 0
        ))
      }

      # Count events by type
      event_types <- sapply(trace, function(e) e$type)
      event_counts <- as.list(table(event_types))

      # Count events by agent
      agents <- sapply(trace, function(e) e$agent %||% "(none)")
      agent_activity <- as.list(table(agents))

      # Calculate total duration
      timestamps <- sapply(trace, function(e) as.numeric(e$timestamp))
      duration_ms <- if (length(timestamps) > 1) {
        (max(timestamps) - min(timestamps)) * 1000
      } else {
        0
      }

      list(
        total_events = length(trace),
        event_counts = event_counts,
        agent_activity = agent_activity,
        duration_ms = duration_ms
      )
    },

    # =========================================================================
    # Access Control
    # =========================================================================

    #' @description Set access control for an agent.
    #' @param agent_name Agent name.
    #' @param permissions List of permissions (read_scopes, write_scopes, tools).
    #' @return Invisible self for chaining.
    set_access_control = function(agent_name, permissions) {
      private$.access_control[[agent_name]] <- permissions
      invisible(self)
    },

    #' @description Check if an agent has permission for an action.
    #' @param agent_name Agent name.
    #' @param action Action type: "read", "write", or "tool".
    #' @param target Target scope or tool name.
    #' @return TRUE if permitted, FALSE otherwise.
    check_permission = function(agent_name, action, target) {
      # If no access control defined, allow all
      if (!agent_name %in% names(private$.access_control)) {
        return(TRUE)
      }

      perms <- private$.access_control[[agent_name]]

      switch(action,
        "read" = target %in% (perms$read_scopes %||% character(0)),
        "write" = target %in% (perms$write_scopes %||% character(0)),
        "tool" = target %in% (perms$tools %||% character(0)),
        TRUE  # Unknown action, allow by default
      )
    },

    # =========================================================================
    # Utility Methods
    # =========================================================================

    #' @description Get sandbox mode.
    #' @return The current sandbox mode.
    get_sandbox_mode = function() {
      private$.sandbox_mode
    },

    #' @description Set sandbox mode.
    #' @param mode Sandbox mode: "strict", "permissive", or "none".
    #' @return Invisible self for chaining.
    set_sandbox_mode = function(mode) {
      if (!mode %in% c("strict", "permissive", "none")) {
        rlang::abort("Sandbox mode must be 'strict', 'permissive', or 'none'.")
      }
      private$.sandbox_mode <- mode
      invisible(self)
    },

    #' @description Print method for SharedSession.
    print = function() {
      cat("<SharedSession>\n")
      cat("  Model:", self$get_model_id() %||% "(not set)", "\n")
      cat("  History:", length(private$.history), "messages\n")
      cat("  Tools:", length(private$.tools), "tools\n")
      cat("  Sandbox mode:", private$.sandbox_mode, "\n")
      cat("  Tracing:", if (private$.trace_enabled) "enabled" else "disabled", "\n")
      cat("  Execution context:\n")
      cat("    Current agent:", private$.execution_context$current_agent %||% "(none)", "\n")
      cat("    Stack depth:", private$.execution_context$depth, "\n")
      cat("  Variable scopes:", paste(names(private$.variable_scopes), collapse = ", "), "\n")
      cat("  Trace events:", length(private$.execution_trace), "\n")
      invisible(self)
    }
  ),

  private = list(
    .sandbox_mode = "strict",
    .trace_enabled = TRUE,
    .execution_trace = NULL,
    .delegation_stack = NULL,
    .variable_scopes = NULL,
    .access_control = NULL,
    .execution_context = NULL,

    # Get or create a scope environment
    get_scope_env = function(scope) {
      # Global scope should always exist from initialization
      if (scope == "global") {
        return(private$.variable_scopes$global)
      }

      if (!scope %in% names(private$.variable_scopes)) {
        # Auto-create scope with global as parent
        private$.variable_scopes[[scope]] <- new.env(
          parent = private$.variable_scopes$global
        )
      }
      private$.variable_scopes[[scope]]
    },

    # Check for sandbox violations
    check_sandbox_violation = function(code) {
      # Patterns that are always blocked in strict mode
      strict_blocked <- c(
        "system\\s*\\(",
        "system2\\s*\\(",
        "shell\\s*\\(",
        "Sys\\.setenv",
        "file\\.remove\\s*\\(",
        "unlink\\s*\\(",
        "file\\.rename\\s*\\(",
        "download\\.file\\s*\\(",
        "source\\s*\\(",
        "eval\\s*\\(\\s*parse",
        "\\bq\\s*\\(\\s*\\)",
        "\\bquit\\s*\\(",
        "\\bstop\\s*\\(",
        "rm\\s*\\(\\s*list\\s*=",
        "assign\\s*\\([^,]+,\\s*[^,]+,\\s*envir\\s*=\\s*\\.GlobalEnv",
        "<<-"
      )

      # Additional patterns blocked only in strict mode
      permissive_allowed <- c(
        "writeLines\\s*\\(",
        "write\\.csv\\s*\\(",
        "write\\.table\\s*\\(",
        "saveRDS\\s*\\(",
        "save\\s*\\(",
        "readLines\\s*\\(",
        "read\\.csv\\s*\\(",
        "readRDS\\s*\\("
      )

      patterns_to_check <- if (private$.sandbox_mode == "strict") {
        c(strict_blocked, permissive_allowed)
      } else {
        strict_blocked
      }

      for (pattern in patterns_to_check) {
        if (grepl(pattern, code, ignore.case = TRUE)) {
          return(paste0("Blocked pattern detected: ", pattern))
        }
      }

      NULL
    }
  )
)

#' @title Create a Shared Session
#' @description
#' Factory function to create a new SharedSession object.
#' @param model A LanguageModelV1 object or model string ID.
#' @param system_prompt Optional system prompt.
#' @param tools Optional list of Tool objects.
#' @param hooks Optional HookHandler object.
#' @param max_steps Maximum tool execution steps. Default 10.
#' @param sandbox_mode Sandbox mode: "strict", "permissive", or "none". Default "strict".
#' @param trace_enabled Enable execution tracing. Default TRUE.
#' @return A SharedSession object.
#' @export
#' @examples
#' \dontrun{
#' # Create a shared session for multi-agent use
#' session <- create_shared_session(
#'   model = "openai:gpt-4o",
#'   sandbox_mode = "strict",
#'   trace_enabled = TRUE
#' )
#'
#' # Execute code safely
#' result <- session$execute_code("x <- 1:10; mean(x)")
#'
#' # Check trace
#' print(session$trace_summary())
#' }
create_shared_session <- function(model = NULL,
                                   system_prompt = NULL,
                                   tools = NULL,
                                   hooks = NULL,
                                   max_steps = 10,
                                   sandbox_mode = "strict",
                                   trace_enabled = TRUE) {
  SharedSession$new(
    model = model,
    system_prompt = system_prompt,
    tools = tools,
    hooks = hooks,
    max_steps = max_steps,
    sandbox_mode = sandbox_mode,
    trace_enabled = trace_enabled
  )
}

# Null-coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
