#' @title Agent Team: Automated Multi-Agent Orchestration
#' @description
#' AgentTeam class for managing a group of agents and automating their orchestration.
#' It implements a "Manager-Worker" pattern where a synthesized Manager agent
#' delegates tasks to registered worker agents based on their descriptions.
#' @name team
NULL

#' @title AgentTeam Class
#' @description
#' R6 class representing a team of agents.
#' @export
AgentTeam <- R6::R6Class(
  "AgentTeam",
  
  public = list(
    #' @field name Name of the team.
    name = NULL,
    
    #' @field members List of registered agents (workers).
    members = NULL,
    
    #' @field manager The manager agent (created automatically).
    manager = NULL,

    #' @field default_model Default model ID for the team (optional).
    default_model = NULL,

    #' @field session Optional shared ChatSession for the team.
    session = NULL,
    
    #' @description Initialize a new AgentTeam.
    #' @param name Name of the team.
    #' @param model Optional default model for the team.
    #' @param session Optional shared ChatSession (or SharedSession).
    #' @return A new AgentTeam object.
    initialize = function(name = "AgentTeam", model = NULL, session = NULL) {
      self$name <- name
      self$members <- list()
      self$default_model <- model

      if (!is.null(session) && !inherits(session, "ChatSession")) {
        rlang::abort("session must be a ChatSession or SharedSession object")
      }
      if (is.null(session)) {
        session <- create_shared_session(model = model)
      }
      self$session <- session

      if (is.null(self$default_model) && !is.null(self$session)) {
        self$default_model <- self$session$get_model_id()
      }
    },
    
    #' @description Register an agent to the team.
    #' @param name Name of the agent.
    #' @param description Description of the agent's capabilities.
    #' @param skills Character vector of skills to load for this agent.
    #' @param tools List of explicit Tool objects.
    #' @param system_prompt Optional system prompt override.
    #' @param model Optional default model for this agent (overrides team default).
    #' @return Self (for chaining).
    register_agent = function(name,
                              description,
                              skills = NULL,
                              tools = NULL,
                              system_prompt = NULL,
                              model = NULL) {
      # Resolve default model for this agent
      if (is.null(model)) {
        model <- self$default_model
      }
      if (is.null(model) && !is.null(self$session)) {
        model <- self$session$get_model_id()
      }

      # Create the agent immediately (could be lazy, but eager is simpler/safer for now)
      agent <- create_agent(
        name = name,
        description = description,
        skills = skills,
        tools = tools,
        system_prompt = system_prompt,
        model = model
      )
      
      self$members[[name]] <- agent
      invisible(self)
    },
    
    #' @description Run the team on a task.
    #' @param task The task instruction.
    #' @param model Model ID to use for the Manager.
    #' @param session Optional shared ChatSession (or SharedSession).
    #' @return The result from the Manager agent.
    run = function(task, model = NULL, session = NULL) {
      if (length(self$members) == 0) {
        rlang::abort("Cannot run team: No agents registered.")
      }

      # Resolve shared session
      active_session <- session %||% self$session
      if (!is.null(active_session) && !inherits(active_session, "ChatSession")) {
        rlang::abort("session must be a ChatSession or SharedSession object")
      }

      # Resolve effective model
      effective_model <- model %||% self$default_model
      effective_model_id <- NULL
      if (is.character(effective_model)) {
        effective_model_id <- effective_model
      } else if (!is.null(effective_model) && inherits(effective_model, "LanguageModelV1")) {
        effective_model_id <- paste0(effective_model$provider, ":", effective_model$model_id)
      }

      if (!is.null(active_session)) {
        session_model_id <- active_session$get_model_id()
        if (is.null(effective_model)) {
          effective_model <- session_model_id
          effective_model_id <- session_model_id
        } else if (!is.null(effective_model_id) &&
                   (is.null(session_model_id) || session_model_id != effective_model_id)) {
          active_session$switch_model(effective_model)
        }
      }

      if (is.null(active_session)) {
        if (is.null(effective_model)) {
          rlang::abort("No model specified. Provide 'model' or use a session with a model.")
        }
        # Create a fresh shared session for this run to enable multi-agent context
        active_session <- create_shared_session(model = effective_model)
      } else if (is.null(effective_model)) {
        rlang::abort("No model specified. Provide 'model' or use a session with a model.")
      }

      # Persist resolved defaults for future registrations
      self$default_model <- effective_model
      self$session <- active_session

      # Ensure all members have a default model
      for (agent in self$members) {
        if (is.null(agent$model)) {
          agent$model <- effective_model
        }
      }

      # 1. Create the Manager Agent dynamically
      self$manager <- private$create_manager_agent(
        model = effective_model,
        session = active_session
      )
      
      # 2. Run the Manager
      # The manager will delegate to members as needed
      self$manager$run(task, session = active_session, model = effective_model)
    },
    
    #' @description Print team info.
    print = function() {
      cat("<AgentTeam: ", self$name, ">\n", sep = "")
      cat("  Members:", length(self$members), "\n")
      for (name in names(self$members)) {
        cat("    - ", name, ": ", substr(self$members[[name]]$description, 1, 50), "...\n", sep = "")
      }
      invisible(self)
    }
  ),
  
  private = list(
    create_manager_agent = function(model = NULL, session = NULL) {
      # 1. Generate Manager System Prompt
      files_info <- paste(sapply(names(self$members), function(n) {
        agent <- self$members[[n]]
        paste0("- **", n, "**: ", agent$description)
      }), collapse = "\n")
      
      system_prompt <- paste0(
        "You are the **Manager** of the '", self$name, "'.\n",
        "Your goal is to coordinate the following agents to complete the user's task:\n\n",
        files_info, "\n\n",
        "**Instructions**:\n",
        "1. Analyze the user's task.\n",
        "2. Delegate sub-tasks to the appropriate agents using the `delegate_task` tool.\n",
        "3. You can call multiple agents if needed.\n",
        "4. Synthesize their responses into a final answer for the user.\n",
        "5. If an agent fails, try to fix the instructions or ask another agent.\n",
        "6. Do NOT try to do the work yourself if an agent is better suited."
      )
      
      # 2. Create the Delegate Tool
      # We need to capture 'self' to access members
      team_self <- self
      
      delegate_tool <- Tool$new(
        name = "delegate_task",
        description = "Delegate a task to a specific agent.",
        parameters = z_object(
          agent_name = z_string("Name of the agent to delegate to"),
          task = z_string("Detailed instruction for the agent"),
          context = z_string("Optional additional context for the agent."),
          .required = c("agent_name", "task")
        ),
        execute = function(args) {
          target_name <- args$agent_name
          sub_task <- args$task
          context <- args$context
          
          agent <- team_self$members[[target_name]]
          
          if (is.null(agent)) {
             # Fuzzy match or list available
             available <- paste(names(team_self$members), collapse = ", ")
             return(paste0("Error: Agent '", target_name, "' not found. Available agents: ", available))
          }
          
          # Resolve model for the sub-agent
          effective_model <- model
          if (is.null(effective_model) && !is.null(session)) {
            effective_model <- session$get_model_id()
          }
          if (is.null(effective_model)) {
            effective_model <- agent$model
          }
          if (is.null(effective_model)) {
            return("Error: No model specified for sub-agent. Provide a model to AgentTeam$run() or set a default model.")
          }

          # Run the sub-agent with shared session for context
          result <- agent$run(sub_task, session = session, context = context, model = effective_model)
          
          paste0("Agent '", target_name, "' output:\n", result$text)
        }
      )
      
      # 3. Create Manager Agent
      Agent$new(
        name = "Manager",
        description = "Team Manager",
        system_prompt = system_prompt,
        tools = list(delegate_tool)
      )
    }
  )
)

#' @title Create an Agent Team
#' @description
#' Helper to create an AgentTeam.
#' @param name Team name.
#' @return An AgentTeam object.
#' @export
create_team <- function(name = "AgentTeam", model = NULL, session = NULL) {
  AgentTeam$new(name = name, model = model, session = session)
}
