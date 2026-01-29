#' @title Flow: Multi-Agent Orchestration
#' @description
#' Flow R6 class for managing the execution stack and context switching
#' between agents in a multi-agent system.
#' @name flow
NULL

#' @title Flow Class
#' @description
#' R6 class representing the orchestration layer for multi-agent systems.
#' Manages the call stack, context switching, and delegation between agents.
#'
#' The Flow acts as the "conductor" - it suspends the calling agent,
#' activates the delegate agent, and resumes the caller with the result.
#' @export
Flow <- R6::R6Class(
  "Flow",

  public = list(
    #' @description Initialize a new Flow.
    #' @param session A ChatSession object for shared state between agents.
    #' @param model The default model ID to use (e.g., "openai:gpt-4o").
    #' @param registry Optional AgentRegistry for agent lookup.
    #' @param max_depth Maximum delegation depth (prevents infinite loops). Default 5.
    #' @param max_steps_per_agent Maximum ReAct steps per agent. Default 10.
    initialize = function(session,
                          model,
                          registry = NULL,
                          max_depth = 5,
                          max_steps_per_agent = 10) {
      if (!inherits(session, "ChatSession")) {
        rlang::abort("Flow requires a ChatSession object.")
      }

      private$.session <- session
      private$.model <- model
      private$.registry <- registry
      private$.max_depth <- max_depth
      private$.max_steps <- max_steps_per_agent
      private$.stack <- list()  # Stack of suspended agent contexts
      private$.current_agent <- NULL
      private$.global_context <- NULL
    },

    #' @description Get the current call stack depth.
    #' @return Integer depth.
    depth = function() {
      length(private$.stack)
    },

    #' @description Get the current active agent.
    #' @return The currently active Agent, or NULL.
    current = function() {
      private$.current_agent
    },

    #' @description Get the shared session.
    #' @return The ChatSession object.
    session = function() {
      private$.session
    },

    #' @description Set the global context (the user's original goal).
    #' @param context Character string describing the overall goal.
    #' @return Invisible self for chaining.
    set_global_context = function(context) {
      private$.global_context <- context
      invisible(self)
    },

    #' @description Get the global context.
    #' @return The global context string.
    global_context = function() {
      private$.global_context
    },

    #' @description Delegate a task to another agent.
    #' @details
    #' This is the core orchestration method. It:
    #' 1. Checks depth limit
    #' 2. Pushes current agent to stack (if any)
    #' 3. Builds recursive context
    #' 4. Executes the delegate agent
    #' 5. Pops and returns result
    #'
    #' @param agent The Agent to delegate to.
    #' @param task The task instruction.
    #' @param context Optional additional context.
    #' @return The text result from the delegate agent.
    delegate = function(agent, task, context = NULL) {
      # Check depth limit
      if (self$depth() >= private$.max_depth) {
        return(paste0(
          "[DELEGATION ERROR] Maximum delegation depth (",
          private$.max_depth,
          ") reached. Cannot delegate further. Please complete the task directly."
        ))
      }

      # Build recursive context
      recursive_context <- private$build_recursive_context(task, context)

      # Push current agent to stack (if any)
      if (!is.null(private$.current_agent)) {
        private$.stack <- c(private$.stack, list(list(
          agent = private$.current_agent,
          suspended_at = Sys.time()
        )))
      }

      # Set current agent to delegate
      private$.current_agent <- agent

      # Log delegation for observability
      private$.session$set_memory(
        paste0("flow_delegate_", self$depth()),
        list(
          from = if (length(private$.stack) > 0)
            private$.stack[[length(private$.stack)]]$agent$name
          else
            "User",
          to = agent$name,
          task = task,
          timestamp = Sys.time()
        )
      )

      # Execute the delegate agent
      result <- tryCatch({
        agent$run(
          task = task,
          session = private$.session,
          context = recursive_context,
          model = private$.model,
          max_steps = private$.max_steps
        )
      }, error = function(e) {
        list(text = paste0("[AGENT ERROR] ", agent$name, " failed: ", conditionMessage(e)))
      })

      # Pop the stack (restore previous agent)
      if (length(private$.stack) > 0) {
        restored <- private$.stack[[length(private$.stack)]]
        private$.current_agent <- restored$agent
        private$.stack <- private$.stack[-length(private$.stack)]
      } else {
        private$.current_agent <- NULL
      }

      # Return text result
      result$text %||% "[Agent completed but returned no text]"
    },

    #' @description Run a primary agent (the Manager).
    #' @details
    #' Entry point for a multi-agent flow. The primary agent is run with
    #' delegation tools automatically injected from the registry.
    #'
    #' @param agent The primary/manager Agent to run.
    #' @param task The user's task/input.
    #' @return The final result from the primary agent.
    run = function(agent, task) {
      # Set global context
      self$set_global_context(task)

      # Set current agent
      private$.current_agent <- agent

      # Generate delegate tools from registry
      delegate_tools <- list()
      if (!is.null(private$.registry)) {
        delegate_tools <- private$.registry$generate_delegate_tools(
          flow = self,
          session = private$.session,
          model = private$.model
        )
      }

      # Combine agent's own tools with delegate tools
      all_tools <- c(agent$tools, delegate_tools)

      # Run the primary agent
      result <- generate_text(
        model = private$.model,
        prompt = task,
        system = private$build_manager_system_prompt(agent),
        tools = all_tools,
        max_steps = private$.max_steps
      )

      # Clear current agent
      private$.current_agent <- NULL

      result
    },

    #' @description Print method for Flow.
    print = function() {
      cat("<Flow>\n")
      cat("  Model:", private$.model, "\n")
      cat("  Stack depth:", self$depth(), "\n")
      cat("  Max depth:", private$.max_depth, "\n")
      cat("  Current agent:",
          if (!is.null(private$.current_agent)) private$.current_agent$name else "(none)",
          "\n")
      if (!is.null(private$.registry)) {
        cat("  Registry agents:", paste(private$.registry$list_agents(), collapse = ", "), "\n")
      }
      invisible(self)
    }
  ),

  private = list(
    .session = NULL,
    .model = NULL,
    .registry = NULL,
    .stack = NULL,
    .current_agent = NULL,
    .global_context = NULL,
    .max_depth = 5,
    .max_steps = 10,

    # Build the recursive context injection for subagents
    build_recursive_context = function(task, additional_context = NULL) {
      parts <- character(0)

      # Caller info
      if (length(private$.stack) > 0) {
        caller <- private$.stack[[length(private$.stack)]]$agent
        parts <- c(parts, paste0("You are assisting agent '", caller$name, "'."))
      } else {
        parts <- c(parts, "You are assisting the primary Manager agent.")
      }

      # Global goal
      if (!is.null(private$.global_context)) {
        parts <- c(parts, paste0("Global Goal: \"", private$.global_context, "\""))
      }

      # Current task
      parts <- c(parts, paste0("Your Sub-task: \"", task, "\""))

      # Depth warning
      if (self$depth() >= private$.max_depth - 1) {
        parts <- c(parts, "[WARNING] You are near the maximum delegation depth. Complete this task directly without further delegation.")
      }

      # Additional context
      if (!is.null(additional_context) && nzchar(additional_context)) {
        parts <- c(parts, paste0("Additional Context: ", additional_context))
      }

      paste(parts, collapse = "\n")
    },

    # Build the system prompt for the Manager agent
    build_manager_system_prompt = function(agent) {
      parts <- character(0)

      # Agent's own system prompt
      if (!is.null(agent$system_prompt)) {
        parts <- c(parts, agent$system_prompt)
      }

      # Registry prompt section
      if (!is.null(private$.registry)) {
        registry_section <- private$.registry$generate_prompt_section()
        if (nzchar(registry_section)) {
          parts <- c(parts, "", registry_section)
        }
      }

      if (length(parts) == 0) {
        return(NULL)
      }

      paste(parts, collapse = "\n")
    }
  )
)

#' @title Create a Flow
#' @description
#' Factory function to create a new Flow object for multi-agent orchestration.
#' @param session A ChatSession object for shared state.
#' @param model The default model ID to use (e.g., "openai:gpt-4o").
#' @param registry Optional AgentRegistry for agent lookup and delegation.
#' @param max_depth Maximum delegation depth. Default 5.
#' @param max_steps_per_agent Maximum ReAct steps per agent. Default 10.
#' @return A Flow object.
#' @export
#' @examples
#' \dontrun{
#' # Create a multi-agent flow
#' session <- create_chat_session()
#' cleaner <- create_agent("Cleaner", "Cleans data")
#' plotter <- create_agent("Plotter", "Creates plots")
#' registry <- create_agent_registry(list(cleaner, plotter))
#'
#' manager <- create_agent("Manager", "Coordinates data analysis")
#'
#' flow <- create_flow(
#'   session = session,
#'   model = "openai:gpt-4o",
#'   registry = registry
#' )
#'
#' # Run the manager with auto-delegation
#' result <- flow$run(manager, "Load data and create a visualization")
#' }
create_flow <- function(session,
                        model,
                        registry = NULL,
                        max_depth = 5,
                        max_steps_per_agent = 10) {
  Flow$new(
    session = session,
    model = model,
    registry = registry,
    max_depth = max_depth,
    max_steps_per_agent = max_steps_per_agent
  )
}

# Null-coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
