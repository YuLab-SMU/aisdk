#' Genesis: Zero-Configuration Direct Execution
#'
#' Genesis provides a one-line interface for executing tasks with automatic
#' skill discovery and direct execution. It loads skills on demand and runs a
#' single manager agent that uses tools as needed.
#'
#' @export

# Global cache for team compositions
.genesis_cache <- new.env(parent = emptyenv())

# Direct execution system prompt (skill tools mode)
GENESIS_DIRECT_PROMPT <- paste(
  "You are Genesis, a direct execution agent.",
  "Work iteratively until the task is complete.",
  "Use available tools and skill scripts when helpful (load_skill, execute_skill_script, list_skill_scripts).",
  "If a tool fails or returns an error, do NOT stop - try an alternative approach.",
  "You can write and run R code using execute_r_code as a fallback.",
  "Prefer executing code or scripts to validate results rather than guessing.",
  "After tool execution, ALWAYS synthesize a clear, structured report (Summary, Statistics, Visualizations, Insights).",
  "Do not claim file outputs (PDF/images/scripts) unless explicitly requested.",
  "Avoid using or installing extra packages unless required; prefer base R and ggplot2.",
  "Persist key outputs to disk using save_text_artifact, save_plot_artifact, and save_data_artifact.",
  "Prefer saving a report.Rmd via save_rmd_artifact; render via render_rmd_artifact if available.",
  "When using Rmd, update only the affected chunk via update_rmd_chunk; do NOT rewrite the whole file.",
  "If a chunk errors, use run_rmd_chunk to isolate and debug; then patch that chunk only.",
  "Use list_artifacts to show what was saved and reference saved paths in your response.",
  "Be concise and return the final answer once done.",
  sep = "\n"
)

# Computer tools system prompt (hierarchical action space mode)
GENESIS_COMPUTER_PROMPT <- paste(
  "You are Genesis, a direct execution agent with computer access.",
  "Work iteratively until the task is complete.",
  "",
  "## Available Tools",
  "You have access to atomic computer tools:",
  "- bash: Execute shell commands, run CLIs, scripts, or utilities",
  "- read_file: Read file contents",
  "- write_file: Write content to files",
  "- execute_r_code: Run R code in isolated process",
  "",
  "## Skills",
  "Skills are available in the inst/skills/ directory. Each skill has:",
  "- SKILL.md: Instructions and documentation",
  "- scripts/: Executable R scripts",
  "- references/: Reference materials",
  "",
  "To use a skill:",
  "1. Read inst/skills/<skill_name>/SKILL.md to understand it",
  "2. Execute scripts via: bash('Rscript inst/skills/<skill_name>/scripts/<script>.R')",
  "3. Read references if needed: read_file('inst/skills/<skill_name>/references/<file>')",
  "",
  "## Execution Strategy",
  "- Use bash for running scripts, CLIs, and shell utilities",
  "- Use execute_r_code for data analysis and computations",
  "- Use read_file/write_file for file operations",
  "- If a command fails, try an alternative approach",
  "- Prefer executing code to validate results rather than guessing",
  "",
  "## Output",
  "- Synthesize clear, structured reports (Summary, Statistics, Visualizations, Insights)",
  "- Persist key outputs using artifact tools",
  "- Be concise and return the final answer once done",
  sep = "\n"
)

#' Execute a task with automatic agent discovery and team assembly
#'
#' @param task Character string describing the task to accomplish
#' @param skill_paths Character vector of paths to scan for skills, or "auto" for default locations
#' @param model Model to use for agents (default: claude-3-5-sonnet-20241022)
#' @param cache Logical, whether to cache team composition for similar tasks (default: TRUE)
#' @param verbose Logical, whether to print orchestration details (default: FALSE)
#' @param architect_model Model to use for Architect agent (default: same as model)
#' @param max_steps Maximum tool execution steps (default: 10)
#' @param mode Execution mode: "plan" (structured plan-script-execute) or "direct" (single agent)
#' @param use_computer_tools Logical, whether to use computer abstraction layer (default: FALSE).
#'   When TRUE, uses atomic tools (bash, read_file, write_file, execute_r_code) instead of
#'   loading all skill tools into context. This reduces context window usage by 30-50%.
#' @return The result from the team execution
#' @export
#' @examples
#' \dontrun{
#' # Simple one-line execution
#' result <- genesis("Analyze the iris dataset and create a scatter plot")
#'
#' # With computer tools (reduced context usage)
#' result <- genesis(
#'   "Analyze the iris dataset",
#'   use_computer_tools = TRUE
#' )
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
                    model = "anthropic:claude-3-5-sonnet-20241022",
                    cache = TRUE,
                    verbose = FALSE,
                    architect_model = NULL,
                    max_steps = 10,
                    mode = c("plan", "direct"),
                    use_computer_tools = FALSE) {
  
  mode <- match.arg(mode)

  if (mode == "plan") {
    return(genesis_do(task, model = model, verbose = verbose))
  }

  if (verbose) {
    cat("=== Genesis: Direct Execution ===\n\n")
    if (use_computer_tools) {
      cat("Using computer abstraction layer (reduced context mode)\n\n")
    } else {
      cat("Running in direct mode (no pre-assembly)\n\n")
    }
  }

  # Create artifact directory and tools
  artifact_dir <- create_artifact_dir()
  artifact_tools <- create_artifact_tools(artifact_dir)

  # Choose execution mode
  if (use_computer_tools) {
    # Computer abstraction mode: atomic tools + bash/filesystem access
    computer_tools <- create_computer_tools(working_dir = artifact_dir)

    agent <- Agent$new(
      name = "Genesis",
      description = "Direct execution agent with computer access",
      system_prompt = GENESIS_COMPUTER_PROMPT,
      tools = c(computer_tools, artifact_tools),
      model = model
    )
  } else {
    # Traditional mode: skill tools + code execution
    coder_tools <- create_coder_agent()$tools

    agent <- Agent$new(
      name = "Genesis",
      description = "Direct execution agent",
      system_prompt = GENESIS_DIRECT_PROMPT,
      skills = skill_paths,
      tools = c(coder_tools, artifact_tools),
      model = model
    )
  }

  # Shared session for tool execution and cross-step context
  session <- create_shared_session(model = model)
  assign(".artifact_dir", artifact_dir, envir = session$get_envir())

  # Execute the task directly
  result <- agent$run(
    task = task,
    session = session,
    model = model,
    max_steps = max_steps
  )

  if (verbose) {
    cat("\n=== Execution Complete ===\n")
    cat("Artifacts directory: ", artifact_dir, "\n", sep = "")
    if (use_computer_tools) {
      cat("Context savings: ~30-50% (computer tools mode)\n")
    }
  }

  result
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
    result <- session$send(task)
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

  team <- AgentTeam$new(name = "GenesisTeam", model = model)

  for (agent_name in names(agents)) {
    agent <- agents[[agent_name]]
    team$register_agent(
      name = agent_name,
      description = agent$description,
      skills = NULL,  # Skills already loaded in agent
      tools = agent$tools,
      system_prompt = agent$system_prompt,
      model = model
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
