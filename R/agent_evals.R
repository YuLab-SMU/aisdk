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
#' \dontrun{
#' test_that("agent answers math questions correctly", {
#'   result <- generate_text(
#'     model = "openai:gpt-4o",
#'     prompt = "What is 2 + 2?"
#'   )
#'   expect_llm_pass(result, "The response should contain the number 4")
#' })
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
  eval_result <- tryCatch({
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
  }, error = function(e) {
    list(score = 0, reasoning = paste("Evaluation error:", conditionMessage(e)))
  })

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

  result <- tryCatch({
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
  }, error = function(e) {
    list(hallucination_score = 1, hallucinations = list(), reasoning = conditionMessage(e))
  })

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

#' @title Benchmark Agent
#' @description
#' Run a benchmark suite against an agent and collect performance metrics.
#' @param agent An Agent object or model string.
#' @param tasks A list of benchmark tasks (see details).
#' @param tools Optional list of tools for the agent.
#' @param verbose Print progress.
#' @return A benchmark result object with metrics.
#' @details
#' Each task in the tasks list should have:
#' - prompt: The task prompt
#' - expected: Expected output or criteria
#' - category: Optional category for grouping
#' - ground_truth: Optional ground truth for hallucination checking
#' @export
benchmark_agent <- function(agent,
                            tasks,
                            tools = NULL,
                            verbose = TRUE) {
  # Determine model
  model <- if (inherits(agent, "Agent")) {
    NULL  # Agent will use its own model
  } else if (is.character(agent)) {
    agent
  } else {
    rlang::abort("agent must be an Agent object or model string")
  }

  results <- list()
  start_time <- Sys.time()

  for (i in seq_along(tasks)) {
    task <- tasks[[i]]

    if (verbose) {
      message(sprintf("[%d/%d] Running: %s",
                      i, length(tasks),
                      substr(task$prompt, 1, 50)))
    }

    task_start <- Sys.time()

    # Run the task
    response <- tryCatch({
      if (inherits(agent, "Agent")) {
        agent$run(task$prompt)
      } else {
        generate_text(
          model = model,
          prompt = task$prompt,
          tools = tools
        )
      }
    }, error = function(e) {
      list(text = "", error = conditionMessage(e))
    })

    task_time <- as.numeric(difftime(Sys.time(), task_start, units = "secs"))

    # Evaluate response
    eval_result <- tryCatch({
      if (!is.null(task$expected)) {
        expect_llm_pass(response, task$expected, threshold = 0)
      } else {
        list(score = NA, reasoning = "No criteria provided")
      }
    }, error = function(e) {
      list(score = 0, reasoning = conditionMessage(e))
    })

    # Check hallucination if ground truth provided
    hallucination_result <- if (!is.null(task$ground_truth)) {
      tryCatch({
        expect_no_hallucination(response, task$ground_truth, tolerance = 1)
      }, error = function(e) {
        list(hallucination_score = NA, reasoning = conditionMessage(e))
      })
    } else {
      list(hallucination_score = NA)
    }

    # Collect tool usage
    tool_calls <- if (!is.null(response$tool_calls)) {
      sapply(response$tool_calls, function(tc) tc$name)
    } else {
      character(0)
    }

    results[[i]] <- list(
      task_id = i,
      prompt = task$prompt,
      category = task$category %||% "general",
      response = response$text %||% "",
      score = eval_result$score,
      reasoning = eval_result$reasoning,
      hallucination_score = hallucination_result$hallucination_score,
      tool_calls = tool_calls,
      time_seconds = task_time,
      error = response$error %||% NULL
    )
  }

  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Calculate aggregate metrics
  scores <- sapply(results, function(r) r$score)
  scores <- scores[!is.na(scores)]

  hallucination_scores <- sapply(results, function(r) r$hallucination_score)
  hallucination_scores <- hallucination_scores[!is.na(hallucination_scores)]

  times <- sapply(results, function(r) r$time_seconds)

  metrics <- list(
    total_tasks = length(tasks),
    completed = sum(sapply(results, function(r) is.null(r$error))),
    mean_score = if (length(scores) > 0) mean(scores) else NA,
    median_score = if (length(scores) > 0) median(scores) else NA,
    pass_rate = if (length(scores) > 0) mean(scores >= 0.7) else NA,
    mean_hallucination = if (length(hallucination_scores) > 0) mean(hallucination_scores) else NA,
    mean_time = mean(times),
    total_time = total_time,
    tool_accuracy = private_calculate_tool_accuracy(results, tasks)
  )

  # Group by category
  categories <- unique(sapply(results, function(r) r$category))
  by_category <- lapply(categories, function(cat) {
    cat_results <- results[sapply(results, function(r) r$category == cat)]
    cat_scores <- sapply(cat_results, function(r) r$score)
    cat_scores <- cat_scores[!is.na(cat_scores)]
    list(
      category = cat,
      count = length(cat_results),
      mean_score = if (length(cat_scores) > 0) mean(cat_scores) else NA
    )
  })

  structure(
    list(
      results = results,
      metrics = metrics,
      by_category = by_category,
      agent = if (inherits(agent, "Agent")) agent$name else agent
    ),
    class = "benchmark_result"
  )
}

#' @title Calculate Tool Accuracy
#' @keywords internal
private_calculate_tool_accuracy <- function(results, tasks) {
  correct <- 0
  total <- 0

  for (i in seq_along(results)) {
    if (!is.null(tasks[[i]]$expected_tools)) {
      total <- total + 1
      actual <- results[[i]]$tool_calls
      expected <- tasks[[i]]$expected_tools
      if (all(expected %in% actual)) {
        correct <- correct + 1
      }
    }
  }

  if (total == 0) return(NA)
  correct / total
}

#' @title Print Benchmark Result
#' @param x A benchmark result object.
#' @param ... Additional arguments (not used).
#' @export
print.benchmark_result <- function(x, ...) {
  cat("<Benchmark Result>\n")
  cat("  Agent:", x$agent, "\n")
  cat("  Tasks:", x$metrics$total_tasks, "\n")
  cat("  Completed:", x$metrics$completed, "\n")
  cat("\nMetrics:\n")
  cat("  Mean Score:", sprintf("%.2f", x$metrics$mean_score), "\n")
  cat("  Pass Rate:", sprintf("%.1f%%", x$metrics$pass_rate * 100), "\n")
  cat("  Mean Hallucination:", sprintf("%.2f", x$metrics$mean_hallucination), "\n")
  cat("  Tool Accuracy:", sprintf("%.1f%%", x$metrics$tool_accuracy * 100), "\n")
  cat("  Mean Time:", sprintf("%.2fs", x$metrics$mean_time), "\n")
  cat("  Total Time:", sprintf("%.2fs", x$metrics$total_time), "\n")

  if (length(x$by_category) > 1) {
    cat("\nBy Category:\n")
    for (cat in x$by_category) {
      cat(sprintf("  %s: %.2f (%d tasks)\n",
                  cat$category, cat$mean_score, cat$count))
    }
  }

  invisible(x)
}

#' @title Create R Data Tasks Benchmark
#' @description
#' Create a standard benchmark suite for R data science tasks.
#' @param difficulty Difficulty level: "easy", "medium", "hard".
#' @return A list of benchmark tasks.
#' @export
r_data_tasks <- function(difficulty = "medium") {
  tasks <- list()

  # Easy tasks
  if (difficulty %in% c("easy", "medium", "hard")) {
    tasks <- c(tasks, list(
      list(
        prompt = "Calculate the mean of the numbers 1, 2, 3, 4, 5",
        expected = "The response should contain the value 3",
        category = "basic_stats"
      ),
      list(
        prompt = "Create a vector containing the numbers 1 through 10",
        expected = "The response should show R code using c() or 1:10 or seq()",
        category = "basic_r"
      )
    ))
  }

  # Medium tasks
  if (difficulty %in% c("medium", "hard")) {
    tasks <- c(tasks, list(
      list(
        prompt = "Write dplyr code to filter a dataframe 'df' for rows where column 'x' is greater than 5 and select columns 'x' and 'y'",
        expected = "The response should contain filter(), x > 5, and select() with correct syntax",
        category = "dplyr"
      ),
      list(
        prompt = "Create a ggplot2 scatter plot of mpg vs hp from the mtcars dataset",
        expected = "The response should contain ggplot(), aes(), geom_point(), mpg, and hp",
        category = "ggplot2"
      )
    ))
  }

  # Hard tasks
  if (difficulty == "hard") {
    tasks <- c(tasks, list(
      list(
        prompt = "Write a function that performs k-fold cross-validation for a linear regression model",
        expected = "The response should include a function definition, fold creation, model fitting in a loop, and metric calculation",
        category = "ml"
      ),
      list(
        prompt = "Create a Shiny app with a slider input and a reactive plot that updates based on the slider value",
        expected = "The response should contain ui, server, sliderInput, renderPlot, and reactive elements",
        category = "shiny"
      )
    ))
  }

  tasks
}

# Null-coalescing operator
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
