#' @title Agent Registry: Agent Storage and Lookup
#' @description
#' AgentRegistry R6 class for storing and retrieving Agent instances.
#' Used by the Flow system for agent delegation.
#' @name agent_registry
NULL

#' @title AgentRegistry Class
#' @description
#' R6 class for managing a collection of Agent objects. Provides storage,
#' lookup, and automatic delegation tool generation for multi-agent systems.
#' @export
AgentRegistry <- R6::R6Class(
  "AgentRegistry",

  public = list(
    #' @description Initialize a new AgentRegistry.
    #' @param agents Optional list of Agent objects to register immediately.
    initialize = function(agents = NULL) {
      private$.agents <- list()
      if (!is.null(agents)) {
        for (agent in agents) {
          self$register(agent)
        }
      }
    },

    #' @description Register an agent.
    #' @param agent An Agent object to register.
    #' @return Invisible self for chaining.
    register = function(agent) {
      if (!inherits(agent, "Agent")) {
        rlang::abort("Can only register Agent objects.")
      }
      if (agent$name %in% names(private$.agents)) {
        rlang::warn(paste0("Agent '", agent$name, "' already registered. Replacing."))
      }
      private$.agents[[agent$name]] <- agent
      invisible(self)
    },

    #' @description Get an agent by name.
    #' @param name The agent name.
    #' @return The Agent object, or NULL if not found.
    get = function(name) {
      private$.agents[[name]]
    },

    #' @description Check if an agent is registered.
    #' @param name The agent name.
    #' @return TRUE if registered, FALSE otherwise.
    has = function(name) {
      name %in% names(private$.agents)
    },

    #' @description List all registered agent names.
    #' @return Character vector of agent names.
    list_agents = function() {
      names(private$.agents)
    },

    #' @description Get all registered agents.
    #' @return List of Agent objects.
    get_all = function() {
      private$.agents
    },

    #' @description Unregister an agent.
    #' @param name The agent name to remove.
    #' @return Invisible self for chaining.
    unregister = function(name) {
      private$.agents[[name]] <- NULL
      invisible(self)
    },

    #' @description Generate delegation tools for all registered agents.
    #' @details
    #' Creates a list of Tool objects that wrap each agent's run() method.
    #' These tools can be given to a Manager agent for semantic routing.
    #' @param flow Optional Flow object for context-aware execution.
    #' @param session Optional ChatSession for shared state.
    #' @param model Optional model ID for agent execution.
    #' @return A list of Tool objects.
    generate_delegate_tools = function(flow = NULL, session = NULL, model = NULL) {
      lapply(private$.agents, function(agent) {
        create_delegate_tool(agent, flow = flow, session = session, model = model)
      })
    },

    #' @description Generate a prompt section describing available agents.
    #' @details
    #' Creates a formatted string listing all agents and their descriptions.
    #' Useful for injecting into a Manager's system prompt.
    #' @return A character string.
    generate_prompt_section = function() {
      if (length(private$.agents) == 0) {
        return("")
      }

      lines <- c(
        "[AVAILABLE AGENTS]",
        "You can delegate tasks to the following specialized agents:",
        ""
      )

      for (agent in private$.agents) {
        lines <- c(lines, paste0("- **", agent$name, "**: ", agent$description))
      }

      lines <- c(lines, "", "Use the delegate_to_<AgentName> tools to assign tasks.")

      paste(lines, collapse = "\n")
    },

    #' @description Print method for AgentRegistry.
    print = function() {
      cat("<AgentRegistry>\n")
      cat("  Registered agents:", length(private$.agents), "\n")
      for (name in names(private$.agents)) {
        cat("    -", name, "\n")
      }
      invisible(self)
    }
  ),

  private = list(
    .agents = NULL
  )
)

#' @title Create an Agent Registry
#' @description
#' Factory function to create a new AgentRegistry.
#' @param agents Optional list of Agent objects to register immediately.
#' @return An AgentRegistry object.
#' @export
#' @examples
#' \dontrun{
#' # Create registry with agents
#' cleaner <- create_agent("Cleaner", "Cleans data")
#' plotter <- create_agent("Plotter", "Creates visualizations")
#'
#' registry <- create_agent_registry(list(cleaner, plotter))
#' print(registry$list_agents())  # "Cleaner", "Plotter"
#' }
create_agent_registry <- function(agents = NULL) {
  AgentRegistry$new(agents = agents)
}

#' @title Create a Delegate Tool for an Agent
#' @description
#' Internal function to create a Tool that delegates to an Agent.
#' @param agent The Agent to wrap.
#' @param flow Optional Flow object for context tracking.
#' @param session Optional ChatSession for shared state.
#' @param model Optional model ID for execution.
#' @return A Tool object.
#' @keywords internal
create_delegate_tool <- function(agent, flow = NULL, session = NULL, model = NULL) {
  agent_ref <- agent
  flow_ref <- flow
  session_ref <- session
  model_ref <- model

  Tool$new(
    name = paste0("delegate_to_", agent$name),
    description = paste0(
      "Delegate a task to the ", agent$name, " agent. ",
      agent$description
    ),
    parameters = z_object(
      task = z_string("The specific task to delegate to this agent."),
      context = z_string("Optional additional context for the agent.")
    ),
    execute = function(args) {
      task <- args$task
      context <- args$context

      # If we have a Flow, use it for proper context switching
      if (!is.null(flow_ref)) {
        # Push the current agent onto the stack (handled by Flow internally)
        result <- flow_ref$delegate(
          agent = agent_ref,
          task = task,
          context = context
        )
        return(result)
      }

      # Fallback: Direct execution without Flow
      result <- agent_ref$run(
        task = task,
        session = session_ref,
        context = context,
        model = model_ref
      )

      # Return the text result for the Manager to process
      result$text %||% "Agent completed task but returned no text."
    }
  )
}

# Null-coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
