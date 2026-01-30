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
    
    #' @description Initialize a new AgentTeam.
    #' @param name Name of the team.
    #' @return A new AgentTeam object.
    initialize = function(name = "AgentTeam") {
      self$name <- name
      self$members <- list()
    },
    
    #' @description Register an agent to the team.
    #' @param name Name of the agent.
    #' @param description Description of the agent's capabilities.
    #' @param skills Character vector of skills to load for this agent.
    #' @param tools List of explicit Tool objects.
    #' @param system_prompt Optional system prompt override.
    #' @return Self (for chaining).
    register_agent = function(name, description, skills = NULL, tools = NULL, system_prompt = NULL) {
      # Create the agent immediately (could be lazy, but eager is simpler/safer for now)
      agent <- create_agent(
        name = name,
        description = description,
        skills = skills,
        tools = tools,
        system_prompt = system_prompt
      )
      
      self$members[[name]] <- agent
      invisible(self)
    },
    
    #' @description Run the team on a task.
    #' @param task The task instruction.
    #' @param model Model ID to use for the Manager.
    #' @return The result from the Manager agent.
    run = function(task, model = NULL) {
      if (length(self$members) == 0) {
        rlang::abort("Cannot run team: No agents registered.")
      }
      
      # 1. Create the Manager Agent dynamically
      self$manager <- private$create_manager_agent()
      
      # 2. Run the Manager
      # The manager will delegate to members as needed
      self$manager$run(task, model = model)
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
    create_manager_agent = function() {
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
          task = z_string("Detailed instruction for the agent")
        ),
        execute = function(args) {
          target_name <- args$agent_name
          sub_task <- args$task
          
          agent <- team_self$members[[target_name]]
          
          if (is.null(agent)) {
             # Fuzzy match or list available
             available <- paste(names(team_self$members), collapse = ", ")
             return(paste0("Error: Agent '", target_name, "' not found. Available agents: ", available))
          }
          
          # Run the sub-agent
          # Note: We share the session context if we had one, but here we run stateless for simplicity first
          # Ideally we pass 'session' if AgentTeam$run received one.
          # For now, simplistic stateless run:
          result <- agent$run(sub_task)
          
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
create_team <- function(name = "AgentTeam") {
  AgentTeam$new(name = name)
}
