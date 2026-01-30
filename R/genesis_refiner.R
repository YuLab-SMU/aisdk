#' Refiner Agent: Failure Analysis and Improvement Strategy
#'
#' The Refiner Agent analyzes failed executions and generates improvement
#' strategies. It decides whether to retry, replan, or abort.
#'
#' @export

# Refiner system prompt
REFINER_PROMPT <- "
You are the Refiner, responsible for analyzing failures and generating improvement strategies.

Your role:
1. Analyze why the execution failed to meet success criteria
2. Identify the root cause of the failure
3. Determine the best action: retry, replan, or abort
4. Provide specific improvements to make
5. Suggest a new strategy if replanning

Available Agents:
%s

Decision Guidelines:

**RETRY** - Use when:
- The agents selected were correct
- The failure was due to execution issues (not selection)
- Minor adjustments to the approach could fix it
- Example: Agent didn't use all available tools

**REPLAN** - Use when:
- Wrong agents were selected
- Missing critical capabilities
- Need different agent combination
- Example: Forgot to include Visualizer for plotting task

**ABORT** - Use when:
- Task is impossible with available agents
- Fundamental misunderstanding of requirements
- No viable path forward
- Example: Task requires capabilities no agent has

Response Format (JSON):
{
  \"root_cause\": \"Detailed analysis of why the execution failed\",
  \"action\": \"retry\" | \"replan\" | \"abort\",
  \"reasoning\": \"Why this action is the best choice\",
  \"improvements\": [
    \"Specific improvement 1\",
    \"Specific improvement 2\"
  ],
  \"new_agents\": [\"Agent1\", \"Agent2\"],
  \"new_strategy\": \"If replan: new delegation strategy. Otherwise: null\"
}

Examples:

Failure: \"Analysis complete but no visualization\"
-> Action: replan
-> Root Cause: Architect forgot to include Visualizer agent
-> New Agents: [\"DataAnalyst\", \"Visualizer\"]
-> Strategy: \"DataAnalyst analyzes first, then Visualizer creates plots from results\"

Failure: \"Code execution error in analysis\"
-> Action: retry
-> Root Cause: Agent didn't inspect data structure before analysis
-> Improvements: [\"Use inspect_variable tool first\", \"Handle missing values\"]
-> New Agents: null (keep same agents)

Failure: \"Task requires GPU computation but no GPU agent available\"
-> Action: abort
-> Root Cause: Required capability not available in agent library
-> Reasoning: \"No agents have GPU computation capabilities\"
"

#' Create Refiner agent
#'
#' @param agent_library AgentLibrary instance
#' @param model Model to use for Refiner (default: sonnet)
#' @return Refiner R6 object
#' @export
create_refiner_agent <- function(agent_library,
                                  model = "claude-3-5-sonnet-20241022") {

  # Get capabilities table
  capabilities_df <- agent_library$get_capabilities_summary()
  capabilities_text <- format_capabilities_table(capabilities_df)

  Refiner$new(capabilities_text, agent_library, model)
}

#' Refiner R6 Class
#'
#' @export
Refiner <- R6::R6Class("Refiner",
  public = list(
    #' @field agent The underlying Agent object
    agent = NULL,

    #' @field library AgentLibrary reference
    library = NULL,

    #' @description Initialize Refiner
    #' @param capabilities_text Formatted capabilities table
    #' @param library AgentLibrary instance
    #' @param model Model to use
    initialize = function(capabilities_text, library, model) {
      self$library <- library

      # Create system prompt with capabilities
      system_prompt <- sprintf(REFINER_PROMPT, capabilities_text)

      self$agent <- Agent$new(
        name = "Refiner",
        description = "Analyzes failures and suggests improvements",
        system_prompt = system_prompt,
        model = model
      )
    },

    #' @description Analyze failure and generate improvement strategy
    #' @param task Original task description
    #' @param plan Execution plan that was used
    #' @param result Execution result
    #' @param evaluation Evaluation from Evaluator
    #' @return List with refinement strategy
    refine = function(task, plan, result, evaluation) {
      # Build refinement prompt
      prompt <- sprintf(
        "Analyze this failed execution and suggest improvements:\n\n=== TASK ===\n%s\n\n=== PLAN ===\n%s\n\n=== RESULT ===\n%s\n\n=== EVALUATION ===\n%s\n\nProvide a detailed analysis and improvement strategy.",
        task,
        format_plan(plan),
        format_result(result),
        format_evaluation(evaluation)
      )

      # Create session and get response
      session <- ChatSession$new(model = self$agent$model, agent = self$agent)
      response <- session$chat(prompt)

      # Parse response
      parse_refiner_response(response)
    }
  )
)

#' Format plan for display
#' @keywords internal
format_plan <- function(plan) {
  lines <- c()

  lines <- c(lines, sprintf("Selected Agents: %s", paste(plan$selected_agents, collapse = ", ")))
  lines <- c(lines, sprintf("\nReasoning: %s", plan$reasoning))
  lines <- c(lines, sprintf("\nDelegation Strategy: %s", plan$delegation_strategy))

  if (!is.null(plan$success_criteria)) {
    lines <- c(lines, "\nSuccess Criteria:")
    lines <- c(lines, format_success_criteria(plan$success_criteria))
  }

  paste(lines, collapse = "\n")
}

#' Format evaluation for display
#' @keywords internal
format_evaluation <- function(evaluation) {
  lines <- c()

  lines <- c(lines, sprintf("Score: %d/100", evaluation$score))
  lines <- c(lines, sprintf("Passed: %s", evaluation$passed))

  if (!is.null(evaluation$completeness)) {
    lines <- c(lines, sprintf("\nCompleteness: %s",
                             evaluation$completeness$assessment %||% "Unknown"))
  }

  if (!is.null(evaluation$quality)) {
    lines <- c(lines, sprintf("Quality: %s",
                             evaluation$quality$assessment %||% "Unknown"))
  }

  if (length(evaluation$errors) > 0) {
    lines <- c(lines, "\nErrors:")
    for (error in evaluation$errors) {
      lines <- c(lines, sprintf("  - %s", error))
    }
  }

  if (!is.null(evaluation$feedback)) {
    lines <- c(lines, sprintf("\nFeedback: %s", evaluation$feedback))
  }

  paste(lines, collapse = "\n")
}

#' Parse Refiner's JSON response
#'
#' @param response Character string from Refiner
#' @return List with refinement strategy
#' @export
parse_refiner_response <- function(response) {
  tryCatch({
    # Extract JSON from response
    json_pattern <- "\\{[^{}]*(?:\\{[^{}]*\\}[^{}]*)*\\}"
    json_matches <- gregexpr(json_pattern, response, perl = TRUE)
    json_text <- regmatches(response, json_matches)[[1]]

    if (length(json_text) == 0) {
      stop("No JSON found in Refiner response")
    }

    # Try each match until one parses successfully
    refinement <- NULL
    for (json_str in json_text) {
      try_ref <- tryCatch({
        jsonlite::fromJSON(json_str, simplifyVector = FALSE)
      }, error = function(e) NULL)

      if (!is.null(try_ref)) {
        refinement <- try_ref
        break
      }
    }

    if (is.null(refinement)) {
      stop("Could not parse JSON from response")
    }

    # Validate required fields
    required_fields <- c("root_cause", "action", "reasoning", "improvements")
    missing <- setdiff(required_fields, names(refinement))
    if (length(missing) > 0) {
      warning(sprintf("Missing refinement fields: %s", paste(missing, collapse = ", ")))
      # Add defaults
      if (!"root_cause" %in% names(refinement)) refinement$root_cause <- "Unknown"
      if (!"action" %in% names(refinement)) refinement$action <- "abort"
      if (!"reasoning" %in% names(refinement)) refinement$reasoning <- "Unknown"
      if (!"improvements" %in% names(refinement)) refinement$improvements <- list()
    }

    # Validate action
    valid_actions <- c("retry", "replan", "abort")
    if (!refinement$action %in% valid_actions) {
      warning(sprintf("Invalid action '%s', defaulting to 'abort'", refinement$action))
      refinement$action <- "abort"
    }

    # Ensure improvements is a vector
    if (!is.null(refinement$improvements)) {
      refinement$improvements <- unlist(refinement$improvements)
    }

    # Ensure new_agents is a vector (if present)
    if (!is.null(refinement$new_agents)) {
      refinement$new_agents <- unlist(refinement$new_agents)
    }

    refinement
  }, error = function(e) {
    stop(sprintf("Failed to parse Refiner response: %s\n\nResponse:\n%s",
                 e$message, response))
  })
}
