#' Evaluator Agent: Result Quality Assessment
#'
#' The Evaluator Agent assesses the quality of task execution results
#' against predefined success criteria. This is the core of the Refine phase.
#'
#' @export

# Evaluator system prompt
EVALUATOR_PROMPT <- "
You are the Evaluator, responsible for assessing task execution results.

Your role:
1. Compare the execution result against the success criteria
2. Evaluate completeness, quality, and correctness
3. Identify any errors or missing elements
4. Provide a quality score (0-100)
5. Give specific, actionable feedback

Evaluation Guidelines:
- Be objective and thorough
- Check each success criterion individually
- Consider both what was delivered and what was missing
- Identify specific issues, not vague complaints
- Provide constructive feedback for improvement
- Score fairly: 90-100 (excellent), 70-89 (good), 50-69 (acceptable), <50 (poor)

Response Format (JSON):
{
  \"score\": 0-100,
  \"passed\": true/false,
  \"completeness\": {
    \"must_have_met\": [\"criterion1\", \"criterion2\"],
    \"must_have_missing\": [\"criterion3\"],
    \"assessment\": \"Overall completeness assessment\"
  },
  \"quality\": {
    \"strengths\": [\"strength1\", \"strength2\"],
    \"weaknesses\": [\"weakness1\", \"weakness2\"],
    \"assessment\": \"Overall quality assessment\"
  },
  \"errors\": [
    \"Specific error or issue 1\",
    \"Specific error or issue 2\"
  ],
  \"feedback\": \"Detailed feedback with specific suggestions for improvement\"
}

Scoring Rubric:
- 100: Perfect execution, exceeds all criteria
- 90-99: Excellent, meets all criteria with minor room for improvement
- 80-89: Good, meets all must-have criteria, some quality issues
- 70-79: Acceptable, meets most criteria, noticeable quality issues
- 60-69: Below expectations, missing some must-have criteria
- 50-59: Poor, missing multiple must-have criteria
- <50: Unacceptable, major failures or missing most criteria
"

#' Create Evaluator agent
#'
#' @param model Model to use for Evaluator (default: sonnet)
#' @return Evaluator R6 object
#' @export
create_evaluator_agent <- function(model = "claude-3-5-sonnet-20241022") {
  Evaluator$new(model)
}

#' Evaluator R6 Class
#'
#' @export
Evaluator <- R6::R6Class("Evaluator",
  public = list(
    #' @field agent The underlying Agent object
    agent = NULL,

    #' @description Initialize Evaluator
    #' @param model Model to use
    initialize = function(model) {
      self$agent <- Agent$new(
        name = "Evaluator",
        description = "Evaluates task execution results against success criteria",
        system_prompt = EVALUATOR_PROMPT,
        model = model
      )
    },

    #' @description Evaluate a task result
    #' @param task Original task description
    #' @param success_criteria Success criteria from Architect
    #' @param result Execution result to evaluate
    #' @return List with evaluation details
    evaluate = function(task, success_criteria, result) {
      # Build evaluation prompt
      prompt <- sprintf(
        "Evaluate this task execution result:\n\n=== TASK ===\n%s\n\n=== SUCCESS CRITERIA ===\n%s\n\n=== RESULT ===\n%s\n\nProvide a thorough evaluation.",
        task,
        format_success_criteria(success_criteria),
        format_result(result)
      )

      # Create session and get response
      session <- ChatSession$new(model = self$agent$model, agent = self$agent)
      response <- session$send(prompt)

      # Parse response
      parse_evaluator_response(response$text)
    }
  )
)

#' Format success criteria for display
#' @keywords internal
format_success_criteria <- function(criteria) {
  lines <- c()

  if (length(criteria$must_have) > 0) {
    lines <- c(lines, "Must Have:")
    for (item in criteria$must_have) {
      lines <- c(lines, sprintf("  - %s", item))
    }
  }

  if (length(criteria$quality_checks) > 0) {
    lines <- c(lines, "\nQuality Checks:")
    for (item in criteria$quality_checks) {
      lines <- c(lines, sprintf("  - %s", item))
    }
  }

  if (length(criteria$expected_outputs) > 0) {
    lines <- c(lines, "\nExpected Outputs:")
    for (item in criteria$expected_outputs) {
      lines <- c(lines, sprintf("  - %s", item))
    }
  }

  paste(lines, collapse = "\n")
}

#' Format result for display
#' @keywords internal
format_result <- function(result) {
  if (is.character(result)) {
    # Truncate if too long
    if (nchar(result) > 2000) {
      result <- paste0(substr(result, 1, 2000), "\n\n... (truncated)")
    }
    return(result)
  }

  # For other types, convert to string
  result_str <- utils::capture.output(print(result))
  result_text <- paste(result_str, collapse = "\n")

  if (nchar(result_text) > 2000) {
    result_text <- paste0(substr(result_text, 1, 2000), "\n\n... (truncated)")
  }

  result_text
}

#' Parse Evaluator's JSON response
#'
#' @param response Character string from Evaluator
#' @return List with evaluation details
#' @export
parse_evaluator_response <- function(response) {
  tryCatch({
    # Extract JSON from response
    json_pattern <- "\\{[^{}]*(?:\\{[^{}]*\\}[^{}]*)*\\}"
    json_matches <- gregexpr(json_pattern, response, perl = TRUE)
    json_text <- regmatches(response, json_matches)[[1]]

    if (length(json_text) == 0) {
      stop("No JSON found in Evaluator response")
    }

    # Try each match until one parses successfully
    evaluation <- NULL
    for (json_str in json_text) {
      try_eval <- tryCatch({
        jsonlite::fromJSON(json_str, simplifyVector = FALSE)
      }, error = function(e) NULL)

      if (!is.null(try_eval)) {
        evaluation <- try_eval
        break
      }
    }

    if (is.null(evaluation)) {
      stop("Could not parse JSON from response")
    }

    # Validate required fields
    required_fields <- c("score", "passed", "completeness", "quality", "errors", "feedback")
    missing <- setdiff(required_fields, names(evaluation))
    if (length(missing) > 0) {
      warning(sprintf("Missing evaluation fields: %s", paste(missing, collapse = ", ")))
      # Add defaults for missing fields
      if (!"score" %in% names(evaluation)) evaluation$score <- 0
      if (!"passed" %in% names(evaluation)) evaluation$passed <- FALSE
      if (!"completeness" %in% names(evaluation)) evaluation$completeness <- list(assessment = "Unknown")
      if (!"quality" %in% names(evaluation)) evaluation$quality <- list(assessment = "Unknown")
      if (!"errors" %in% names(evaluation)) evaluation$errors <- list()
      if (!"feedback" %in% names(evaluation)) evaluation$feedback <- "No feedback provided"
    }

    # Ensure score is numeric
    evaluation$score <- as.numeric(evaluation$score)

    # Ensure passed is logical
    evaluation$passed <- isTRUE(evaluation$passed)

    # Ensure errors is a vector
    if (!is.null(evaluation$errors)) {
      evaluation$errors <- unlist(evaluation$errors)
    }

    evaluation
  }, error = function(e) {
    stop(sprintf("Failed to parse Evaluator response: %s\n\nResponse:\n%s",
                 e$message, response))
  })
}
