#' Genesis: Zero-Configuration Agent Execution
#'
#' Genesis provides a one-line interface for executing tasks with automatic
#' agent discovery and team assembly. It scans available skills, uses an
#' Architect agent to select the optimal team, and executes the task.
#'
#' @export

# Global cache for team compositions
.genesis_cache <- new.env(parent = emptyenv())

#' Execute a task with automatic agent discovery and team assembly
#'
#' @param task Character string describing the task to accomplish
#' @param skill_paths Character vector of paths to scan for skills, or "auto" for default locations
#' @param model Model to use for agents (default: claude-3-5-sonnet-20241022)
#' @param cache Logical, whether to cache team composition for similar tasks (default: TRUE)
#' @param verbose Logical, whether to print orchestration details (default: FALSE)
#' @param architect_model Model to use for Architect agent (default: same as model)
#' @return The result from the team execution
#' @export
#' @examples
#' \dontrun{
#' # Simple one-line execution
#' result <- genesis("Analyze the iris dataset and create a scatter plot")
#'
#' # With custom skill paths
#' result <- genesis(
#'   "Perform differential expression analysis",
#'   skill_paths = "~/bioinformatics/skills"
#' )
#'
#' # With verbose output to see orchestration
#' result <- genesis(
#'   "Check if my package passes CRAN checks",
#'   verbose = TRUE
#' )
#' }
genesis <- function(task,
                    skill_paths = "auto",
                    model = "claude-3-5-sonnet-20241022",
                    cache = TRUE,
                    verbose = FALSE,
                    architect_model = NULL) {

  if (is.null(architect_model)) {
    architect_model <- model
  }

  # Step 1: Discover available agents from skills
  if (verbose) {
    cat("=== Genesis: Automatic Team Assembly ===\n\n")
    cat("Step 1: Discovering agents from skills...\n")
  }

  library <- AgentLibrary$new()
  library$scan_from_skills(skill_paths, recursive = TRUE)

  capabilities <- library$get_capabilities_summary()

  if (nrow(capabilities) == 0) {
    stop(paste0(
      "No agents discovered. Please ensure:\n",
      "1. Skills exist in the specified paths\n",
      "2. SKILL.md files contain 'agent' section in YAML frontmatter\n",
      "3. yaml package is installed"
    ))
  }

  if (verbose) {
    cat(sprintf("  Discovered %d agent(s):\n", nrow(capabilities)))
    for (i in seq_len(nrow(capabilities))) {
      cat(sprintf("    - %s: %s\n", capabilities$role[i], capabilities$description[i]))
    }
    cat("\n")
  }

  # Step 2: Check cache for similar tasks
  cache_key <- NULL
  if (cache) {
    cache_key <- digest::digest(list(task = task, roles = library$list_roles()))

    if (exists(cache_key, envir = .genesis_cache)) {
      cached_plan <- get(cache_key, envir = .genesis_cache)

      if (verbose) {
        cat("Step 2: Using cached team composition\n")
        cat(sprintf("  Selected agents: %s\n\n", paste(cached_plan$selected_agents, collapse = ", ")))
      }

      # Skip to execution with cached plan
      return(execute_with_plan(task, library, cached_plan, model, verbose))
    }
  }

  # Step 3: Invoke Architect to select agents
  if (verbose) {
    cat("Step 2: Consulting Architect for team composition...\n")
  }

  architect <- create_architect_agent(library, architect_model)
  architect_session <- ChatSession$new(model = architect_model, agent = architect)

  architect_prompt <- sprintf(
    "Analyze this task and select the optimal agents:\n\nTask: %s",
    task
  )

  architect_response <- architect_session$chat(architect_prompt)

  plan <- parse_architect_response(architect_response)

  if (verbose) {
    cat("  Architect's analysis:\n")
    cat(sprintf("    Task requires: %s\n", plan$task_analysis))
    cat(sprintf("    Selected agents: %s\n", paste(plan$selected_agents, collapse = ", ")))
    cat(sprintf("    Reasoning: %s\n", plan$reasoning))
    cat("\n")
  }

  # Cache the plan
  if (cache && !is.null(cache_key)) {
    assign(cache_key, plan, envir = .genesis_cache)
  }

  # Step 4: Execute with the plan
  execute_with_plan(task, library, plan, model, verbose)
}

#' Execute task with a given plan
#' @keywords internal
execute_with_plan <- function(task, library, plan, model, verbose) {
  # Handle case where no agents are needed
  if (length(plan$selected_agents) == 0) {
    if (verbose) {
      cat("Step 3: No agents needed for this task\n")
      cat("  Executing with base model...\n\n")
    }

    # Just use a simple chat session
    session <- ChatSession$new(model = model)
    result <- session$chat(task)
    return(result)
  }

  # Step 4: Instantiate selected agents
  if (verbose) {
    cat("Step 3: Instantiating agents...\n")
  }

  agents <- library$instantiate_agents(plan$selected_agents, model)

  if (length(agents) == 0) {
    stop("Failed to instantiate any agents. Check that agent roles match discovered agents.")
  }

  if (verbose) {
    cat(sprintf("  Created %d agent(s)\n\n", length(agents)))
  }

  # Step 5: Create team and execute
  if (verbose) {
    cat("Step 4: Assembling team and executing...\n\n")
  }

  team <- AgentTeam$new(name = "GenesisTeam")

  for (agent_name in names(agents)) {
    agent <- agents[[agent_name]]
    team$register_agent(
      name = agent_name,
      description = agent$description,
      skills = NULL,  # Skills already loaded in agent
      tools = agent$tools,
      system_prompt = agent$system_prompt
    )
  }

  # Execute the task
  result <- team$run(task, model = model)

  if (verbose) {
    cat("\n=== Execution Complete ===\n")
  }

  result
}

#' Set default skill library path
#'
#' @param path Character string path to skill library directory
#' @export
#' @examples
#' \dontrun{
#' set_skill_library("~/my_skills")
#' }
set_skill_library <- function(path) {
  options(aisdk.skill_library = path)
  invisible(path)
}

#' Get current skill library path
#'
#' @return Character string path, or "auto" if using default discovery
#' @export
get_skill_library <- function() {
  getOption("aisdk.skill_library", default = "auto")
}

#' Clear team composition cache
#'
#' Clears the cached team compositions. Useful when skills have been
#' updated or you want to force re-analysis by the Architect.
#'
#' @export
#' @examples
#' \dontrun{
#' clear_genesis_cache()
#' }
clear_genesis_cache <- function() {
  rm(list = ls(envir = .genesis_cache), envir = .genesis_cache)
  invisible(NULL)
}

#' Get cache statistics
#'
#' @return List with cache size and keys
#' @export
genesis_cache_info <- function() {
  keys <- ls(envir = .genesis_cache)
  list(
    size = length(keys),
    keys = keys
  )
}
