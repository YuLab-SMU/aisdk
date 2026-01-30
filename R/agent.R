#' @title Agent System: Multi-Agent Orchestration
#' @description
#' Agent R6 class for building multi-agent systems. An Agent is a stateless
#' worker unit that holds capability (tools, persona) while the Session holds
#' conversation state.
#' @name agent
NULL

#' @title Agent Class
#' @description
#' R6 class representing an AI agent. Agents are the worker units in the

#' multi-agent architecture. Each agent has a name, description (for semantic
#' routing), system prompt (persona), and a set of tools it can use.
#'
#' Key design principle: Agents are stateless regarding conversation history.
#' The ChatSession holds the shared state (history, memory, environment).
#' @export
Agent <- R6::R6Class(
  "Agent",

  public = list(
    #' @field name Unique identifier for this agent.
    name = NULL,

    #' @field description Description of the agent's capability.
    #'   This is the "API" that the LLM Manager uses for semantic routing.
    description = NULL,

    #' @field system_prompt The agent's persona/instructions.
    system_prompt = NULL,

    #' @field tools List of Tool objects this agent can use.
    tools = NULL,

    #' @description Initialize a new Agent.
    #' @param name Unique name for this agent (e.g., "DataCleaner", "Visualizer").
    #' @param description A clear description of what this agent does.
    #'   This is used by the Manager LLM to decide which agent to delegate to.
    #' @param system_prompt Optional system prompt defining the agent's persona.
    #' @param tools Optional list of Tool objects the agent can use.
    #' @param skills Optional character vector of skill paths or "auto" to discover skills.
    #'   When provided, this automatically loads skills, creates tools, and updates the system prompt.
    #' @return An Agent object.
    initialize = function(name,
                          description,
                          system_prompt = NULL,
                          tools = NULL,
                          skills = NULL) {
      if (missing(name) || !is.character(name) || nchar(name) == 0) {
        rlang::abort("Agent 'name' is required and must be a non-empty string.")
      }
      if (missing(description) || !is.character(description) || nchar(description) == 0) {
        rlang::abort("Agent 'description' is required and must be a non-empty string.")
      }

      self$name <- name
      self$description <- description
      
      # Handle skills
      skill_prompt <- NULL
      skill_tools <- list()
      
      if (!is.null(skills)) {
        # Resolve skill paths
        if (identical(skills, "auto")) {
           # Check standard locations
           candidates <- c(
             file.path(Sys.getenv("HOME"), "aisdk", "skills"),
             file.path(getwd(), "aisdk", "skills"),
             file.path(getwd(), "skills"),
             file.path(getwd(), "inst", "skills")
           )
           skills <- candidates[dir.exists(candidates)]
           if (length(skills) == 0) {
             warning("skills='auto' specified but no skill directories found in standard locations.")
           }
        }
        
        # Load skills from paths
        if (length(skills) > 0) {
           # Create a merged registry for all paths
           registry <- SkillRegistry$new()
           for (path in skills) {
             tryCatch({
               registry$scan_skills(path, recursive = TRUE)
             }, error = function(e) {
               warning(paste("Failed to scan skills at", path, ":", conditionMessage(e)))
             })
           }
           
           if (registry$count() > 0) {
             # Generate prompt section
             skill_prompt <- registry$generate_prompt_section()
             
             # Create tools
             skill_tools <- create_skill_tools(registry)
           }
        }
      }

      # Combine system prompt
      if (!is.null(skill_prompt) && nzchar(skill_prompt)) {
        system_prompt <- if (is.null(system_prompt)) {
          skill_prompt 
        } else {
          paste(system_prompt, "\n\n", skill_prompt, sep = "")
        }
      }
      
      self$system_prompt <- system_prompt
      self$tools <- c(tools %||% list(), skill_tools)
    },

    #' @description Run the agent with a given task.
    #' @param task The task instruction (natural language).
    #' @param session Optional ChatSession for shared state. If NULL, a temporary
    #'   session is created.
    #' @param context Optional additional context to inject (e.g., from parent agent).
    #' @param model Optional model override. Uses session's model if not provided.
    #' @param max_steps Maximum ReAct loop iterations. Default 10.
    #' @param ... Additional arguments passed to generate_text.
    #' @return A GenerateResult object from generate_text.
    run = function(task,
                   session = NULL,
                   context = NULL,
                   model = NULL,
                   max_steps = 10,
                   ...) {
      # Build the effective system prompt with context injection
      effective_system <- private$build_system_prompt(context, session)

      # Determine the model to use
      if (is.null(model)) {
        if (!is.null(session)) {
          # Try to get model from session
          model <- session$get_model_id()
        }
        if (is.null(model)) {
          rlang::abort("No model specified. Provide 'model' argument or use a session with a model.")
        }
      }

      # Execute the ReAct loop using the core API
      # Pass session so tools can access/modify the shared environment
      result <- generate_text(
        model = model,
        prompt = task,
        system = effective_system,
        tools = self$tools,
        max_steps = max_steps,
        session = session,
        ...
      )

      # If we have a session, record this interaction
      if (!is.null(session)) {
        # Store the task and result in session memory
        session$set_memory(
          paste0("agent_", self$name, "_last_task"),
          task
        )
        session$set_memory(
          paste0("agent_", self$name, "_last_result"),
          result$text
        )
      }

      result
    },

    #' @description Run the agent with streaming output.
    #' @param task The task instruction (natural language).
    #' @param callback Function to handle streaming chunks: callback(text, done).
    #' @param session Optional ChatSession for shared state.
    #' @param context Optional additional context to inject.
    #' @param model Optional model override.
    #' @param max_steps Maximum ReAct loop iterations. Default 10.
    #' @param ... Additional arguments passed to stream_text.
    #' @return A GenerateResult object (accumulated).
    stream = function(task,
                      callback = NULL,
                      session = NULL,
                      context = NULL,
                      model = NULL,
                      max_steps = 10,
                      ...) {
      # Build the effective system prompt with context injection
      effective_system <- private$build_system_prompt(context, session)

      # Determine the model to use
      if (is.null(model)) {
        if (!is.null(session)) {
          model <- session$get_model_id()
        }
        if (is.null(model)) {
          rlang::abort("No model specified. Provide 'model' argument or use a session with a model.")
        }
      }

      # Execute the streaming ReAct loop
      result <- stream_text(
        model = model,
        prompt = task,
        callback = callback,
        system = effective_system,
        tools = self$tools,
        max_steps = max_steps,
        session = session,
        ...
      )

      # If we have a session, record this interaction
      if (!is.null(session)) {
        # Store the task and result in session memory
        session$set_memory(
          paste0("agent_", self$name, "_last_task"),
          task
        )
        session$set_memory(
          paste0("agent_", self$name, "_last_result"),
          result$text
        )
      }

      invisible(result)
    },

    #' @description Convert this agent to a Tool.
    #' @details
    #' This allows the agent to be used as a delegate target by a Manager agent.
    #' The tool wraps the agent's run() method and uses the agent's description
    #' for semantic routing.
    #' @return A Tool object that wraps this agent.
    as_tool = function() {
      agent_ref <- self
      Tool$new(
        name = paste0("delegate_to_", self$name),
        description = paste0(
          "Delegate a task to the ", self$name, " agent. ",
          self$description
        ),
        parameters = z_object(
          task = z_string("The task to delegate to this agent."),
          context = z_string("Optional additional context for the agent.")
        ),
        execute = function(task, context = NULL) {
          result <- agent_ref$run(task = task, context = context)
          # Return the text result for the Manager to process
          result$text
        }
      )
    },

    #' @description Create a stateful ChatSession from this agent.
    #' @param model Optional model override.
    #' @param ... Additional arguments passed to ChatSession$new.
    #' @return A ChatSession object initialized with this agent's config.
    create_session = function(model = NULL, ...) {
      ChatSession$new(
        model = model,
        agent = self,
        ...
      )
    },

    #' @description Print method for Agent.
    print = function() {
      cat("<Agent>\n")
      cat("  Name:", self$name, "\n")
      cat("  Description:", substr(self$description, 1, 60), "...\n")
      cat("  System Prompt:", if (!is.null(self$system_prompt)) "defined" else "none", "\n")
      cat("  Tools:", length(self$tools), "tools\n")
      invisible(self)
    }
  ),

  private = list(
    # Build the effective system prompt with context injection.
    build_system_prompt = function(context = NULL, session = NULL) {
      parts <- character(0)

      # Add base system prompt if defined
      if (!is.null(self$system_prompt)) {
        parts <- c(parts, self$system_prompt)
      }

      # Add context injection if provided
      if (!is.null(context)) {
        context_block <- paste0(
          "\n\n[CURRENT CONTEXT]\n",
          context
        )
        parts <- c(parts, context_block)
      }

      # Add session environment summary if provided
      if (!is.null(session)) {
        session_info <- character(0)
        
        # Summarize shared environment
        env_vars <- session$list_envir()
        if (length(env_vars) > 0) {
          env_summary <- paste(sapply(env_vars, function(v) {
            obj <- session$get_envir()[[v]]
            paste0("- ", v, " (", paste(class(obj), collapse = ", "), ")")
          }), collapse = "\n")
          session_info <- c(session_info, "Objects in shared environment:", env_summary)
        }
        
        # Summarize shared memory
        mem_keys <- session$list_memory()
        if (length(mem_keys) > 0) {
           # Filter out internal keys like 'agent_..._last_task' to reduce noise, if desired.
           # For now, we list everything to be transparent.
           mem_summary <- paste0("- ", mem_keys, collapse = "\n")
           session_info <- c(session_info, "Keys in shared memory:", mem_summary)
        }

        if (length(session_info) > 0) {
          session_block <- paste0(
            "\n\n[SHARED SESSION CONTEXT]\n",
            "The following objects and memory are available in the shared session. You can use available tools to access them.\n",
            paste(session_info, collapse = "\n")
          )
          parts <- c(parts, session_block)
        }
      }

      if (length(parts) == 0) {
        return(NULL)
      }

      paste(parts, collapse = "\n")
    }
  )
)

#' @title Create an Agent
#' @description
#' Factory function to create a new Agent object.
#' @param name Unique name for this agent.
#' @param description A clear description of what this agent does.
#' @param system_prompt Optional system prompt defining the agent's persona.
#' @param tools Optional list of Tool objects the agent can use.
#' @param skills Optional character vector of skill paths or "auto".
#' @return An Agent object.
#' @export
#' @examples
#' \dontrun{
#' # Create a simple math agent
#' math_agent <- create_agent(
#'   name = "MathAgent",
#'   description = "Performs arithmetic calculations",
#'   system_prompt = "You are a math assistant. Return only numerical results."
#' )
#'
#' # Run the agent
#' result <- math_agent$run("Calculate 2 + 2", model = "openai:gpt-4o")
#' 
#' # Create an agent with skills
#' stock_agent <- create_agent(
#'   name = "StockAnalyst",
#'   description = "Stock analysis agent",
#'   skills = "auto"
#' )
#' }
create_agent <- function(name, description, system_prompt = NULL, tools = NULL, skills = NULL) {
  Agent$new(
    name = name,
    description = description,
    system_prompt = system_prompt,
    tools = tools,
    skills = skills
  )
}

# Null-coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
