#' Architect V2: Enhanced Meta-Agent with Success Criteria
#'
#' Architect V2 extends the original Architect with the ability to define
#' success criteria for tasks, enabling the Refine phase of the PER cycle.
#'
#' @export

# Enhanced Architect system prompt with success criteria
ARCHITECT_V2_PROMPT_TEMPLATE <- "
You are the Architect V2, an enhanced meta-agent responsible for intelligent team composition.

Your role:
1. Analyze the user's task to understand required capabilities
2. Review available agent roles and their capabilities
3. Select the MINIMAL set of agents needed to accomplish the task
4. Define clear SUCCESS CRITERIA for evaluating the result
5. Provide clear reasoning and delegation strategy

Available Agents:
%s

%s

Response Format (JSON):
{
  \"task_analysis\": \"Brief analysis of what the task requires\",
  \"selected_agents\": [\"AgentRole1\", \"AgentRole2\"],
  \"reasoning\": \"Why these specific agents were selected\",
  \"delegation_strategy\": \"How the Manager should coordinate them\",
  \"success_criteria\": {
    \"must_have\": [
      \"Specific requirement 1\",
      \"Specific requirement 2\"
    ],
    \"quality_checks\": [
      \"Quality check 1\",
      \"Quality check 2\"
    ],
    \"expected_outputs\": [
      \"Expected output 1\",
      \"Expected output 2\"
    ]
  }
}

Guidelines:
- Prefer fewer agents over more (avoid over-engineering)
- Select based on capability match, not just keyword matching
- Define SPECIFIC, MEASURABLE success criteria
- Success criteria should be verifiable from the output
- Consider what would make this task \"done well\" vs \"done poorly\"
- If task is simple enough for one agent, select only one
- If no suitable agents exist, return empty selected_agents array

Examples:

Task: \"Analyze iris dataset and create a scatter plot\"
-> Select: [\"DataAnalyst\", \"Visualizer\"]
-> Success Criteria:
  - must_have: [\"Statistical summary of iris data\", \"Scatter plot visualization\"]
  - quality_checks: [\"All numeric columns analyzed\", \"Plot has clear labels and legend\"]
  - expected_outputs: [\"Summary statistics\", \"ggplot object or plot file\"]

Task: \"Check if my R package passes CRAN checks\"
-> Select: [\"CRANTechLead\"]
-> Success Criteria:
  - must_have: [\"R CMD check results\", \"List of errors/warnings/notes\"]
  - quality_checks: [\"All check categories covered\", \"Clear explanation of issues\"]
  - expected_outputs: [\"Check summary\", \"Actionable recommendations\"]
"

#' Create Architect V2 agent
#'
#' @param agent_library AgentLibrary instance with discovered agents
#' @param model Model to use for Architect (default: sonnet)
#' @return ArchitectV2 R6 object
#' @export
create_architect_v2 <- function(agent_library,
                                 model = "claude-3-5-sonnet-20241022") {

  # Get capabilities table
  capabilities_df <- agent_library$get_capabilities_summary()

  if (nrow(capabilities_df) == 0) {
    stop("No agents found in library. Please scan skills first with agent_library$scan_from_skills()")
  }

  # Format as readable table
  capabilities_text <- format_capabilities_table(capabilities_df)

  ArchitectV2$new(capabilities_text, model)
}

#' Architect V2 R6 Class
#'
#' @export
ArchitectV2 <- R6::R6Class("ArchitectV2",
  public = list(
    #' @field agent The underlying Agent object
    agent = NULL,

    #' @description Initialize Architect V2
    #' @param capabilities_text Formatted capabilities table
    #' @param model Model to use
    initialize = function(capabilities_text, model) {
      # Create system prompt with capabilities
      system_prompt <- sprintf(ARCHITECT_V2_PROMPT_TEMPLATE, capabilities_text, "")

      self$agent <- Agent$new(
        name = "ArchitectV2",
        description = "Enhanced meta-agent with success criteria definition",
        system_prompt = system_prompt,
        model = model
      )
    },

    #' @description Plan a task with success criteria
    #' @param task Task description
    #' @param execution_history List of previous execution attempts
    #' @return List with plan including success criteria
    plan = function(task, execution_history = list()) {
      # Build prompt
      prompt <- sprintf("Analyze this task and create a comprehensive plan:\n\nTask: %s", task)

      # Add execution history if available
      if (length(execution_history) > 0) {
        prompt <- paste0(prompt, "\n\n=== Previous Attempts ===\n")
        for (i in seq_along(execution_history)) {
          attempt <- execution_history[[i]]
          prompt <- paste0(prompt, sprintf(
            "\nAttempt %d:\n  Agents used: %s\n  Quality score: %d/100\n  Issues: %s\n",
            i,
            paste(attempt$plan$selected_agents, collapse = ", "),
            attempt$evaluation$score %||% 0,
            paste(attempt$evaluation$errors %||% "Unknown", collapse = ", ")
          ))
        }
        prompt <- paste0(prompt, "\n[WARNING] Consider different agents or strategies to address these issues.\n")
      }

      # Create session and get response
      session <- ChatSession$new(model = self$agent$model, agent = self$agent)
      response <- session$send(prompt)

      # Parse response
      parse_architect_v2_response(response$text)
    }
  )
)

#' Parse Architect V2's JSON response
#'
#' @param response Character string from Architect V2
#' @return List with task_analysis, selected_agents, reasoning, delegation_strategy, success_criteria
#' @export
parse_architect_v2_response <- function(response) {
  tryCatch({
    # Extract JSON from response
    json_pattern <- "\\{[^{}]*(?:\\{[^{}]*\\}[^{}]*)*\\}"
    json_matches <- gregexpr(json_pattern, response, perl = TRUE)
    json_text <- regmatches(response, json_matches)[[1]]

    if (length(json_text) == 0) {
      stop("No JSON found in Architect V2 response")
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
    required_fields <- c("task_analysis", "selected_agents", "reasoning",
                        "delegation_strategy", "success_criteria")
    missing <- setdiff(required_fields, names(plan))
    if (length(missing) > 0) {
      stop(sprintf("Missing required fields: %s", paste(missing, collapse = ", ")))
    }

    # Validate success_criteria structure
    if (!is.list(plan$success_criteria)) {
      stop("success_criteria must be an object")
    }

    required_criteria <- c("must_have", "quality_checks", "expected_outputs")
    missing_criteria <- setdiff(required_criteria, names(plan$success_criteria))
    if (length(missing_criteria) > 0) {
      warning(sprintf("Missing success criteria fields: %s", paste(missing_criteria, collapse = ", ")))
      # Add empty arrays for missing fields
      for (field in missing_criteria) {
        plan$success_criteria[[field]] <- list()
      }
    }

    # Ensure selected_agents is a character vector
    if (!is.null(plan$selected_agents)) {
      plan$selected_agents <- unlist(plan$selected_agents)
    }

    # Ensure success_criteria arrays are character vectors
    plan$success_criteria$must_have <- unlist(plan$success_criteria$must_have)
    plan$success_criteria$quality_checks <- unlist(plan$success_criteria$quality_checks)
    plan$success_criteria$expected_outputs <- unlist(plan$success_criteria$expected_outputs)

    plan
  }, error = function(e) {
    stop(sprintf("Failed to parse Architect V2 response: %s\n\nResponse:\n%s",
                 e$message, response))
  })
}
