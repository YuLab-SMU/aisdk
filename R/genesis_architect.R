#' Architect Agent for Team Composition
#'
#' The Architect is a meta-agent that analyzes user tasks and selects
#' the optimal combination of agents to accomplish the task. It acts as
#' an intelligent "HR system" for automatic team assembly.
#'
#' @export

# Architect system prompt template
ARCHITECT_PROMPT_TEMPLATE <- "
You are the Architect, a meta-agent responsible for intelligent team composition.

Your role:
1. Analyze the user's task to understand required capabilities
2. Review available agent roles and their capabilities
3. Select the MINIMAL set of agents needed to accomplish the task
4. Provide clear reasoning for your selections

Available Agents:
%s

Response Format (JSON):
{
  \"task_analysis\": \"Brief analysis of what the task requires\",
  \"selected_agents\": [\"AgentRole1\", \"AgentRole2\"],
  \"reasoning\": \"Why these specific agents were selected\",
  \"delegation_strategy\": \"How the Manager should coordinate them\"
}

Guidelines:
- Prefer fewer agents over more (avoid over-engineering)
- Select based on capability match, not just keyword matching
- If task is simple enough for one agent, select only one
- If no suitable agents exist, return empty selected_agents array
- Consider agent synergies (e.g., DataAnalyst + Visualizer for analysis tasks)
- Avoid redundant agents with overlapping capabilities

Examples:
Task: \"Analyze iris dataset and create a scatter plot\"
-> Select: [\"DataAnalyst\", \"Visualizer\"]
-> Reasoning: Need data analysis capabilities + visualization

Task: \"Check if my R package passes CRAN checks\"
-> Select: [\"CRANTechLead\"]
-> Reasoning: Single specialized agent sufficient

Task: \"Hello, how are you?\"
-> Select: []
-> Reasoning: No technical task, no agents needed
"

#' Create Architect agent for team composition
#'
#' @param agent_library AgentLibrary instance with discovered agents
#' @param model Model to use for Architect (default: sonnet)
#' @return Agent object configured as Architect
#' @export
create_architect_agent <- function(agent_library,
                                   model = "claude-3-5-sonnet-20241022") {

  # Get capabilities table
  capabilities_df <- agent_library$get_capabilities_summary()

  if (nrow(capabilities_df) == 0) {
    stop("No agents found in library. Please scan skills first with agent_library$scan_from_skills()")
  }

  # Format as readable table
  capabilities_text <- format_capabilities_table(capabilities_df)

  # Create system prompt
  system_prompt <- sprintf(ARCHITECT_PROMPT_TEMPLATE, capabilities_text)

  # Create Architect agent
  Agent$new(
    name = "Architect",
    description = "Meta-agent for intelligent team composition and task analysis",
    system_prompt = system_prompt,
    tools = list(),  # No tools needed, pure reasoning
    model = model
  )
}

#' Format capabilities table for Architect prompt
#'
#' @param df Data frame with role, description, capabilities, skills columns
#' @return Formatted markdown table as string
#' @keywords internal
format_capabilities_table <- function(df) {
  lines <- c("| Role | Description | Capabilities | Skills |")
  lines <- c(lines, "|------|-------------|--------------|--------|")

  for (i in seq_len(nrow(df))) {
    line <- sprintf("| %s | %s | %s | %s |",
                    df$role[i],
                    df$description[i],
                    df$capabilities[i],
                    df$skills[i])
    lines <- c(lines, line)
  }

  paste(lines, collapse = "\n")
}

#' Parse Architect's JSON response
#'
#' @param response Character string from Architect
#' @return List with task_analysis, selected_agents, reasoning, delegation_strategy
#' @export
parse_architect_response <- function(response) {
  tryCatch({
    # Try to extract JSON from response (may be wrapped in markdown code blocks)
    # Look for JSON object pattern
    json_pattern <- "\\{[^{}]*(?:\\{[^{}]*\\}[^{}]*)*\\}"
    json_matches <- gregexpr(json_pattern, response, perl = TRUE)
    json_text <- regmatches(response, json_matches)[[1]]

    if (length(json_text) == 0) {
      stop("No JSON found in Architect response")
    }

    # Try each match until one parses successfully
    plan <- NULL
    for (json_str in json_text) {
      try_plan <- tryCatch({
        jsonlite::fromJSON(json_str, simplifyVector = FALSE)
      }, error = function(e) NULL)

      if (!is.null(try_plan)) {
        plan <- try_plan
        break
      }
    }

    if (is.null(plan)) {
      stop("Could not parse JSON from response")
    }

    # Validate required fields
    required_fields <- c("task_analysis", "selected_agents", "reasoning", "delegation_strategy")
    missing <- setdiff(required_fields, names(plan))
    if (length(missing) > 0) {
      stop(sprintf("Missing required fields: %s", paste(missing, collapse = ", ")))
    }

    # Ensure selected_agents is a character vector
    if (!is.null(plan$selected_agents)) {
      plan$selected_agents <- unlist(plan$selected_agents)
    }

    plan
  }, error = function(e) {
    stop(sprintf("Failed to parse Architect response: %s\n\nResponse:\n%s",
                 e$message, response))
  })
}
