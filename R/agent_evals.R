#' @title Performance & Benchmarking: Agent Evals
#' @description
#' Testing infrastructure for LLM-powered code. Provides testthat integration
#' with custom expectations for evaluating AI agent performance, tool accuracy,
#' and hallucination rates.
#' @importFrom stats median
#' @name agent_evals
NULL

#' @title Expect LLM Pass
#' @description
#' Custom testthat expectation that evaluates whether an LLM response
#' meets specified criteria. Uses an LLM judge to assess the response.
#' @param response The LLM response to evaluate (text or GenerateResult object).
#' @param criteria Character string describing what constitutes a passing response.
#' @param model Model to use for judging (default: same as response or gpt-4o).
#' @param threshold Minimum score (0-1) to pass (default: 0.7).
#' @param info Additional information to include in failure message.
#' @return Invisibly returns the evaluation result.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   test_that("agent answers math questions correctly", {
#'     result <- generate_text(
#'       model = "openai:gpt-4o",
#'       prompt = "What is 2 + 2?"
#'     )
#'     expect_llm_pass(result, "The response should contain the number 4")
#'   })
#' }
#' }
expect_llm_pass <- function(response,
                            criteria,
                            model = NULL,
                            threshold = 0.7,
                            info = NULL) {
  # Extract text from response
  response_text <- if (is.list(response) && !is.null(response$text)) {
    response$text
  } else if (is.character(response)) {
    response
  } else {
    as.character(response)
  }

  # Determine judge model
  judge_model <- model %||%
    getOption("aisdk.eval_model", "openai:gpt-4o")

  # Build evaluation prompt
  eval_prompt <- sprintf(
    'Evaluate the following response against the given criteria.

Response to evaluate:
"""%s"""

Criteria for passing:
%s

Score the response from 0.0 to 1.0 where:
- 0.0 = Completely fails the criteria
- 0.5 = Partially meets the criteria
- 1.0 = Fully meets the criteria

Respond with ONLY a JSON object in this exact format:
{"score": <number>, "reasoning": "<brief explanation>"}',
    response_text,
    criteria
  )

  # Get evaluation
  eval_result <- tryCatch(
    {
      result <- generate_text(
        model = judge_model,
        prompt = eval_prompt,
        system = "You are an objective evaluator. Respond only with the requested JSON format."
      )

      # Parse JSON response
      json_match <- regmatches(
        result$text,
        regexpr("\\{[^}]+\\}", result$text)
      )

      if (length(json_match) > 0) {
        jsonlite::fromJSON(json_match)
      } else {
        list(score = 0, reasoning = "Failed to parse evaluation response")
      }
    },
    error = function(e) {
      list(score = 0, reasoning = paste("Evaluation error:", conditionMessage(e)))
    }
  )

  # Build expectation
  passed <- eval_result$score >= threshold

  # Use testthat if available
  if (requireNamespace("testthat", quietly = TRUE)) {
    testthat::expect(
      passed,
      sprintf(
        "LLM response did not meet criteria.\nScore: %.2f (threshold: %.2f)\nReasoning: %s%s",
        eval_result$score,
        threshold,
        eval_result$reasoning,
        if (!is.null(info)) paste0("\nInfo: ", info) else ""
      )
    )
  } else {
    if (!passed) {
      stop(sprintf(
        "LLM response did not meet criteria. Score: %.2f, Reasoning: %s",
        eval_result$score,
        eval_result$reasoning
      ))
    }
  }

  invisible(eval_result)
}

#' @title Expect Tool Selection
#' @description
#' Test that an agent selects the correct tool(s) for a given task.
#' @param result A GenerateResult object from generate_text with tools.
#' @param expected_tools Character vector of expected tool names.
#' @param exact If TRUE, require exactly these tools (no more, no less).
#' @param info Additional information for failure message.
#' @export
expect_tool_selection <- function(result,
                                  expected_tools,
                                  exact = FALSE,
                                  info = NULL) {
  # Extract tool calls from result
  tool_calls <- if (!is.null(result$tool_calls)) {
    sapply(result$tool_calls, function(tc) tc$name)
  } else if (!is.null(result$steps)) {
    unlist(lapply(result$steps, function(step) {
      if (!is.null(step$tool_calls)) {
        sapply(step$tool_calls, function(tc) tc$name)
      }
    }))
  } else {
    character(0)
  }

  tool_calls <- unique(tool_calls)

  if (exact) {
    passed <- setequal(tool_calls, expected_tools)
    message <- sprintf(
      "Tool selection mismatch.\nExpected: %s\nActual: %s%s",
      paste(expected_tools, collapse = ", "),
      paste(tool_calls, collapse = ", "),
      if (!is.null(info)) paste0("\nInfo: ", info) else ""
    )
  } else {
    passed <- all(expected_tools %in% tool_calls)
    message <- sprintf(
      "Expected tools not selected.\nExpected: %s\nActual: %s%s",
      paste(expected_tools, collapse = ", "),
      paste(tool_calls, collapse = ", "),
      if (!is.null(info)) paste0("\nInfo: ", info) else ""
    )
  }

  if (requireNamespace("testthat", quietly = TRUE)) {
    testthat::expect(passed, message)
  } else {
    if (!passed) stop(message)
  }

  invisible(list(
    passed = passed,
    expected = expected_tools,
    actual = tool_calls
  ))
}

#' @title Expect No Hallucination
#' @description
#' Test that an LLM response does not contain hallucinated information
#' when compared against ground truth.
#' @param response The LLM response to check.
#' @param ground_truth The factual information to check against.
#' @param model Model to use for checking.
#' @param tolerance Allowed deviation (0 = strict, 1 = lenient).
#' @param info Additional information for failure message.
#' @export
expect_no_hallucination <- function(response,
                                    ground_truth,
                                    model = NULL,
                                    tolerance = 0.1,
                                    info = NULL) {
  response_text <- if (is.list(response) && !is.null(response$text)) {
    response$text
  } else {
    as.character(response)
  }

  judge_model <- model %||% getOption("aisdk.eval_model", "openai:gpt-4o")

  check_prompt <- sprintf(
    'Compare the response against the ground truth and identify any hallucinations (false or unsupported claims).

Response:
"""%s"""

Ground Truth:
"""%s"""

Identify any statements in the response that:
1. Contradict the ground truth
2. Add information not supported by the ground truth
3. Misrepresent facts from the ground truth

Respond with ONLY a JSON object:
{"hallucination_score": <0.0-1.0>, "hallucinations": ["list of hallucinated claims"], "reasoning": "<explanation>"}

Where hallucination_score is:
- 0.0 = No hallucinations
- 0.5 = Minor unsupported claims
- 1.0 = Major false information',
    response_text,
    ground_truth
  )

  result <- tryCatch(
    {
      eval_response <- generate_text(
        model = judge_model,
        prompt = check_prompt,
        system = "You are a fact-checker. Be thorough but fair."
      )

      json_match <- regmatches(
        eval_response$text,
        regexpr("\\{[^}]*\\}", eval_response$text, perl = TRUE)
      )

      if (length(json_match) > 0) {
        jsonlite::fromJSON(json_match)
      } else {
        list(hallucination_score = 1, hallucinations = list(), reasoning = "Parse error")
      }
    },
    error = function(e) {
      list(hallucination_score = 1, hallucinations = list(), reasoning = conditionMessage(e))
    }
  )

  passed <- result$hallucination_score <= tolerance

  message <- sprintf(
    "Hallucination detected.\nScore: %.2f (tolerance: %.2f)\nHallucinations: %s\nReasoning: %s%s",
    result$hallucination_score,
    tolerance,
    paste(unlist(result$hallucinations), collapse = "; "),
    result$reasoning,
    if (!is.null(info)) paste0("\nInfo: ", info) else ""
  )

  if (requireNamespace("testthat", quietly = TRUE)) {
    testthat::expect(passed, message)
  } else {
    if (!passed) stop(message)
  }

  invisible(result)
}

# Null-coalescing operator
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
