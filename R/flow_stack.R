#' @title FlowStack: Enhanced Multi-Agent Orchestration
#' @description
#' FlowStack R6 class providing enhanced orchestration with delegation tracing,
#' guardrails, and automatic delegate_task tool generation.
#' @name flow_stack
NULL

#' @title FlowStack Class
#' @description
#' R6 class representing an enhanced orchestration layer for multi-agent systems.
#' Extends Flow with:
#' - Comprehensive delegation tracing
#' - Automatic delegate_task tool generation
#' - Depth and context limits with guardrails
#' - Result aggregation and summarization
#' @export
FlowStack <- R6::R6Class(

  "FlowStack",
  inherit = Flow,

  public = list(
    #' @description Initialize a new FlowStack.
    #' @param session A SharedSession or ChatSession object.
    #' @param model The default model ID to use.
    #' @param registry Optional AgentRegistry for agent lookup.
    #' @param max_depth Maximum delegation depth. Default 5.
    #' @param max_steps_per_agent Maximum ReAct steps per agent. Default 10.
    #' @param max_context_tokens Maximum context tokens per delegation. Default 4000.
    #' @param enable_guardrails Enable safety guardrails. Default TRUE.
    initialize = function(session,
                          model,
                          registry = NULL,
                          max_depth = 5,
                          max_steps_per_agent = 10,
                          max_context_tokens = 4000,
                          enable_guardrails = TRUE) {
      # Call parent constructor
      super$initialize(
        session = session,
        model = model,
        registry = registry,
        max_depth = max_depth,
        max_steps_per_agent = max_steps_per_agent
      )

      # Enhanced state
      private$.max_context_tokens <- max_context_tokens
      private$.enable_guardrails <- enable_guardrails
      private$.delegation_history <- list()
      private$.result_cache <- list()
      private$.active_delegations <- 0
    },

    # =========================================================================
    # Enhanced Delegation
    # =========================================================================

    #' @description Delegate a task to another agent with enhanced tracking.
    #' @param agent The Agent to delegate to.
    #' @param task The task instruction.
    #' @param context Optional additional context.
    #' @param priority Task priority: "high", "normal", "low". Default "normal".
    #' @return The text result from the delegate agent.
    delegate = function(agent, task, context = NULL, priority = "normal") {
      delegation_id <- private$generate_delegation_id()
      start_time <- Sys.time()

      # Check guardrails
      if (private$.enable_guardrails) {
        guardrail_result <- private$check_guardrails(agent, task, context)
        if (!guardrail_result$allowed) {
          return(paste0("[GUARDRAIL] ", guardrail_result$reason))
        }
      }

      # Check depth limit
      if (self$depth() >= private$.max_depth) {
        private$record_delegation(delegation_id, agent$name, task, start_time,
          error = "Max depth exceeded"
        )
        return(paste0(
          "[DELEGATION ERROR] Maximum delegation depth (",
          private$.max_depth,
          ") reached. Cannot delegate further. Please complete the task directly."
        ))
      }

      # Update session context if SharedSession
      if (inherits(private$.session, "SharedSession")) {
        private$.session$push_context(
          agent_name = agent$name,
          task = task,
          parent_agent = if (!is.null(private$.current_agent)) {
            private$.current_agent$name
          } else {
            NULL
          }
        )
      }

      # Build recursive context with enhanced information
      recursive_context <- private$build_enhanced_context(task, context, priority)

      # Push current agent to stack
      if (!is.null(private$.current_agent)) {
        private$.stack <- c(private$.stack, list(list(
          agent = private$.current_agent,
          suspended_at = Sys.time(),
          delegation_id = delegation_id
        )))
      }

      # Set current agent to delegate
      private$.current_agent <- agent
      private$.active_delegations <- private$.active_delegations + 1

      # Log delegation
      private$.session$set_memory(
        paste0("delegation_", delegation_id),
        list(
          from = if (length(private$.stack) > 0) {
            private$.stack[[length(private$.stack)]]$agent$name
          } else {
            "Manager"
          },
          to = agent$name,
          task = task,
          priority = priority,
          depth = self$depth(),
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

      # Pop the stack
      if (length(private$.stack) > 0) {
        restored <- private$.stack[[length(private$.stack)]]
        private$.current_agent <- restored$agent
        private$.stack <- private$.stack[-length(private$.stack)]
      } else {
        private$.current_agent <- NULL
      }

      # Update session context if SharedSession
      if (inherits(private$.session, "SharedSession")) {
        private$.session$pop_context(result$text)
      }

      private$.active_delegations <- private$.active_delegations - 1

      # Record delegation history
      private$record_delegation(
        delegation_id, agent$name, task, start_time,
        result = result$text
      )

      # Cache result for potential reuse
      cache_key <- digest::digest(list(agent = agent$name, task = task))
      private$.result_cache[[cache_key]] <- list(
        result = result$text,
        timestamp = Sys.time()
      )

      result$text %||% "[Agent completed but returned no text]"
    },

    #' @description Generate the delegate_task tool for manager agents.
    #' @details
    #' Creates a single unified tool that can delegate to any registered agent.
    #' This is more efficient than generating separate tools per agent.
    #' @return A Tool object for delegation.
    generate_delegate_tool = function() {
      flow_ref <- self
      registry_ref <- private$.registry

      if (is.null(registry_ref)) {
        rlang::abort("Cannot generate delegate_task tool without a registry.")
      }

      agent_names <- registry_ref$list_agents()
      agent_descriptions <- sapply(agent_names, function(name) {
        agent <- registry_ref$get(name)
        paste0("- ", name, ": ", agent$description)
      })

      Tool$new(
        name = "delegate_task",
        description = paste0(
          "Delegate a task to a specialized agent. Available agents:\n",
          paste(agent_descriptions, collapse = "\n"),
          "\n\nChoose the most appropriate agent for the task."
        ),
        parameters = z_object(
          agent_name = z_string(paste0(
            "Name of the agent to delegate to. Must be one of: ",
            paste(agent_names, collapse = ", ")
          )),
          task = z_string("The specific task to delegate. Be clear and detailed."),
          context = z_string("Optional additional context or constraints."),
          priority = z_enum(
            c("high", "normal", "low"),
            description = "Task priority. High priority tasks are executed first."
          ),
          .required = c("agent_name", "task")
        ),
        execute = function(args) {
          agent_name <- args$agent_name
          task <- args$task
          context <- args$context
          priority <- args$priority %||% "normal"

          # Validate agent exists
          if (!registry_ref$has(agent_name)) {
            return(paste0(
              "[ERROR] Agent '", agent_name, "' not found. ",
              "Available agents: ", paste(registry_ref$list_agents(), collapse = ", ")
            ))
          }

          agent <- registry_ref$get(agent_name)
          flow_ref$delegate(agent, task, context, priority)
        }
      )
    },

    # =========================================================================
    # Enhanced Run
    # =========================================================================

    #' @description Run a primary agent with enhanced orchestration.
    #' @param agent The primary/manager Agent to run.
    #' @param task The user's task/input.
    #' @param use_unified_delegate Use single delegate_task tool. Default TRUE.
    #' @return The final result from the primary agent.
    run = function(agent, task, use_unified_delegate = TRUE) {
      # Set global context
      self$set_global_context(task)

      # Set global task in session if SharedSession
      if (inherits(private$.session, "SharedSession")) {
        private$.session$set_global_task(task)
      }

      # Set current agent
      private$.current_agent <- agent

      # Generate delegation tools
      delegate_tools <- list()
      if (!is.null(private$.registry)) {
        if (use_unified_delegate) {
          delegate_tools <- list(self$generate_delegate_tool())
        } else {
          delegate_tools <- private$.registry$generate_delegate_tools(
            flow = self,
            session = private$.session,
            model = private$.model
          )
        }
      }

      # Combine agent's own tools with delegate tools
      all_tools <- c(agent$tools, delegate_tools)

      # Run the primary agent
      result <- generate_text(
        model = private$.model,
        prompt = task,
        system = private$build_enhanced_manager_prompt(agent),
        tools = all_tools,
        max_steps = private$.max_steps,
        session = private$.session
      )

      # Clear current agent
      private$.current_agent <- NULL

      result
    },

    # =========================================================================
    # Delegation History and Analytics
    # =========================================================================

    #' @description Get delegation history.
    #' @param agent_name Optional filter by agent name.
    #' @param limit Maximum number of records to return.
    #' @return A list of delegation records.
    get_delegation_history = function(agent_name = NULL, limit = NULL) {
      history <- private$.delegation_history

      if (!is.null(agent_name)) {
        history <- Filter(function(h) h$agent == agent_name, history)
      }

      if (!is.null(limit) && length(history) > limit) {
        history <- history[(length(history) - limit + 1):length(history)]
      }

      history
    },

    #' @description Get delegation statistics.
    #' @return A list with counts, timing, and success rates.
    delegation_stats = function() {
      history <- private$.delegation_history

      if (length(history) == 0) {
        return(list(
          total_delegations = 0,
          by_agent = list(),
          avg_duration_ms = 0,
          success_rate = 0
        ))
      }

      # Count by agent
      agents <- sapply(history, function(h) h$agent)
      by_agent <- as.list(table(agents))

      # Calculate average duration
      durations <- sapply(history, function(h) h$duration_ms %||% 0)
      avg_duration <- mean(durations)

      # Calculate success rate
      successes <- sum(sapply(history, function(h) is.null(h$error)))
      success_rate <- successes / length(history)

      list(
        total_delegations = length(history),
        by_agent = by_agent,
        avg_duration_ms = avg_duration,
        success_rate = success_rate,
        max_depth_reached = max(sapply(history, function(h) h$depth %||% 0))
      )
    },

    #' @description Clear delegation history.
    #' @return Invisible self for chaining.
    clear_history = function() {
      private$.delegation_history <- list()
      private$.result_cache <- list()
      invisible(self)
    },

    #' @description Print method for FlowStack.
    print = function() {
      cat("<FlowStack>\n")
      cat("  Model:", private$.model, "\n")
      cat("  Stack depth:", self$depth(), "/", private$.max_depth, "\n")
      cat("  Active delegations:", private$.active_delegations, "\n")
      cat("  Guardrails:", if (private$.enable_guardrails) "enabled" else "disabled", "\n")
      cat("  Current agent:",
          if (!is.null(private$.current_agent)) private$.current_agent$name else "(none)",
          "\n")
      if (!is.null(private$.registry)) {
        cat("  Registry agents:", paste(private$.registry$list_agents(), collapse = ", "), "\n")
      }
      cat("  Delegation history:", length(private$.delegation_history), "records\n")
      invisible(self)
    }
  ),

  private = list(
    .max_context_tokens = 4000,
    .enable_guardrails = TRUE,
    .delegation_history = NULL,
    .result_cache = NULL,
    .active_delegations = 0,

    # Generate unique delegation ID
    generate_delegation_id = function() {
      paste0("del_", format(Sys.time(), "%Y%m%d%H%M%S"), "_",
             sample(1000:9999, 1))
    },

    # Record delegation in history
    record_delegation = function(id, agent, task, start_time, result = NULL, error = NULL) {
      record <- list(
        id = id,
        agent = agent,
        task = substr(task, 1, 200),
        depth = self$depth(),
        start_time = start_time,
        end_time = Sys.time(),
        duration_ms = as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000,
        result_preview = if (!is.null(result)) substr(result, 1, 100) else NULL,
        error = error
      )
      private$.delegation_history <- c(private$.delegation_history, list(record))
    },

    # Check guardrails before delegation
    check_guardrails = function(agent, task, context) {
      # Check for recursive delegation patterns
      recent <- tail(private$.delegation_history, 5)
      if (length(recent) >= 3) {
        recent_agents <- sapply(recent, function(h) h$agent)
        if (agent$name %in% recent_agents) {
          same_agent_count <- sum(recent_agents == agent$name)
          if (same_agent_count >= 3) {
            return(list(
              allowed = FALSE,
              reason = paste0(
                "Potential infinite loop detected: Agent '", agent$name,
                "' has been called ", same_agent_count, " times recently."
              )
            ))
          }
        }
      }

      # Check task similarity (prevent duplicate work)
      task_hash <- digest::digest(task)
      for (h in recent) {
        if (!is.null(h$task) && digest::digest(h$task) == task_hash) {
          return(list(
            allowed = FALSE,
            reason = "Duplicate task detected. This exact task was recently delegated."
          ))
        }
      }

      list(allowed = TRUE, reason = NULL)
    },

    # Build enhanced context for subagents
    build_enhanced_context = function(task, additional_context = NULL, priority = "normal") {
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

      # Current task with priority
      parts <- c(parts, paste0("Your Sub-task [", toupper(priority), " priority]: \"", task, "\""))

      # Depth warning
      remaining_depth <- private$.max_depth - self$depth()
      if (remaining_depth <= 2) {
        parts <- c(parts, paste0(
          "[WARNING] Delegation depth: ", self$depth(), "/", private$.max_depth,
          ". Only ", remaining_depth, " level(s) remaining. ",
          "Complete this task directly if possible."
        ))
      }

      # Session context summary if SharedSession
      if (inherits(private$.session, "SharedSession")) {
        vars <- private$.session$summarize_vars("global")
        if (nrow(vars) > 0) {
          var_summary <- paste(
            apply(vars, 1, function(row) paste0("- ", row["name"], " (", row["type"], ")")),
            collapse = "\n"
          )
          parts <- c(parts, paste0("\nAvailable data in session:\n", var_summary))
        }
      }

      # Additional context
      if (!is.null(additional_context) && nzchar(additional_context)) {
        parts <- c(parts, paste0("\nAdditional Context: ", additional_context))
      }

      paste(parts, collapse = "\n")
    },

    # Build enhanced manager system prompt
    build_enhanced_manager_prompt = function(agent) {
      parts <- character(0)

      # Agent's own system prompt
      if (!is.null(agent$system_prompt)) {
        parts <- c(parts, agent$system_prompt)
      }

      # Orchestration instructions
      parts <- c(parts, "", "[ORCHESTRATION GUIDELINES]")
      parts <- c(parts, "You are a manager agent coordinating specialized sub-agents.")
      parts <- c(parts, "- Delegate tasks to the most appropriate specialist agent")
      parts <- c(parts, "- Provide clear, specific task descriptions")
      parts <- c(parts, "- Synthesize results from multiple agents when needed")
      parts <- c(parts, "- Handle errors gracefully and retry with different approaches")
      parts <- c(parts, paste0("- Maximum delegation depth: ", private$.max_depth))

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

#' @title Create a FlowStack
#' @description
#' Factory function to create a new FlowStack object for enhanced multi-agent orchestration.
#' @param session A SharedSession or ChatSession object.
#' @param model The default model ID to use.
#' @param registry Optional AgentRegistry for agent lookup.
#' @param max_depth Maximum delegation depth. Default 5.
#' @param max_steps_per_agent Maximum ReAct steps per agent. Default 10.
#' @param enable_guardrails Enable safety guardrails. Default TRUE.
#' @return A FlowStack object.
#' @export
#' @examples
#' \dontrun{
#' # Create enhanced multi-agent flow
#' session <- create_shared_session(model = "openai:gpt-4o")
#' registry <- create_agent_registry(list(
#'   create_data_agent(),
#'   create_file_agent(),
#'   create_coder_agent()
#' ))
#'
#' flow <- create_flow_stack(
#'   session = session,
#'   model = "openai:gpt-4o",
#'   registry = registry,
#'   enable_guardrails = TRUE
#' )
#'
#' # Run with unified delegate_task tool
#' manager <- create_agent("Manager", "Coordinates data analysis tasks")
#' result <- flow$run(manager, "Load sales.csv and create a summary report")
#'
#' # Check delegation stats
#' print(flow$delegation_stats())
#' }
create_flow_stack <- function(session,
                               model,
                               registry = NULL,
                               max_depth = 5,
                               max_steps_per_agent = 10,
                               enable_guardrails = TRUE) {
  FlowStack$new(
    session = session,
    model = model,
    registry = registry,
    max_depth = max_depth,
    max_steps_per_agent = max_steps_per_agent,
    enable_guardrails = enable_guardrails
  )
}

# Null-coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
