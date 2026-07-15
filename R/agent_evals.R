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
  # Names of every tool called across the whole trajectory. The multi-step
  # runtime accumulates calls in result$all_tool_calls and leaves
  # result$tool_calls NULL on a final text turn, so reading tool_calls alone
  # missed the trajectory; use the accumulated list first.
  tool_calls <- unique(eval_result_tool_call_names(result))

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

#' @keywords internal
# The full trajectory of tool calls a (possibly multi-step) run made, as a
# list of {id, name, arguments}. Prefers the runtime's accumulated
# all_tool_calls; falls back to a single-turn result$tool_calls.
eval_result_tool_calls <- function(result) {
  calls <- result$all_tool_calls
  if (is.null(calls) || length(calls) == 0) {
    calls <- result$tool_calls
  }
  calls %||% list()
}

#' @keywords internal
eval_result_tool_call_names <- function(result) {
  calls <- eval_result_tool_calls(result)
  if (length(calls) == 0) {
    return(character(0))
  }
  vapply(calls, function(tc) tc$name %||% "", character(1))
}

#' @keywords internal
eval_expect <- function(passed, message) {
  if (requireNamespace("testthat", quietly = TRUE)) {
    testthat::expect(isTRUE(passed), message)
  } else if (!isTRUE(passed)) {
    stop(message)
  }
  invisible(isTRUE(passed))
}

#' @title Expect a Tool-Call Trajectory
#' @description
#' Assert on the sequence of tool calls a (possibly multi-step) agent made —
#' the trajectory, not just the final text. Reads the runtime's accumulated
#' `result$all_tool_calls`, so it works for multi-step agent runs.
#' @param result A generation result from `generate_text()`.
#' @param names Character vector of expected tool names.
#' @param exact If TRUE, the called tool-name set must equal `names` exactly.
#' @param ordered If TRUE, `names` must appear as an ordered subsequence of the
#'   actual call order (checks the agent called tools in the right order).
#' @param info Optional context for the failure message.
#' @return Invisibly, a list with `passed` and the `actual` call names.
#' @export
expect_tool_trajectory <- function(result, names, exact = FALSE, ordered = FALSE, info = NULL) {
  actual <- eval_result_tool_call_names(result)
  passed <- if (isTRUE(ordered)) {
    # `names` is an ordered subsequence of `actual`.
    i <- 1L
    for (nm in actual) {
      if (i <= length(names) && identical(nm, names[[i]])) i <- i + 1L
    }
    i > length(names)
  } else if (isTRUE(exact)) {
    setequal(unique(actual), names)
  } else {
    all(names %in% actual)
  }
  message <- sprintf(
    "Tool trajectory mismatch (%s).\nExpected: %s\nActual: %s%s",
    if (ordered) "ordered" else if (exact) "exact set" else "subset",
    paste(names, collapse = ", "),
    paste(actual, collapse = ", "),
    if (!is.null(info)) paste0("\nInfo: ", info) else ""
  )
  eval_expect(passed, message)
  invisible(list(passed = passed, actual = actual))
}

#' @title Expect Tool-Call Arguments
#' @description
#' Assert that at least one call to `tool` in the trajectory was made with
#' arguments matching `args` (a named subset — each named value must equal the
#' corresponding argument in some call to that tool).
#' @param result A generation result from `generate_text()`.
#' @param tool The tool name to check.
#' @param args A named list of argument key/values that must all match in one call.
#' @param info Optional context for the failure message.
#' @return Invisibly, a list with `passed` and the matched call (or NULL).
#' @export
expect_tool_args <- function(result, tool, args, info = NULL) {
  calls <- Filter(function(tc) identical(tc$name %||% "", tool), eval_result_tool_calls(result))
  matched <- NULL
  for (tc in calls) {
    call_args <- tc$arguments %||% list()
    if (all(vapply(names(args), function(k) {
      !is.null(call_args[[k]]) && isTRUE(all.equal(call_args[[k]], args[[k]]))
    }, logical(1)))) {
      matched <- tc
      break
    }
  }
  passed <- !is.null(matched)
  message <- sprintf(
    "No call to '%s' matched the expected arguments.\nExpected args: %s\nCalls seen: %d%s",
    tool,
    paste(names(args), unlist(lapply(args, as.character)), sep = "=", collapse = ", "),
    length(calls),
    if (!is.null(info)) paste0("\nInfo: ", info) else ""
  )
  eval_expect(passed, message)
  invisible(list(passed = passed, matched = matched))
}

#' @title Expect a Run Within Resource Budgets
#' @description
#' Regression assertion on a run's aggregated cost signals — token usage, step
#' count, and tool-call count — using the fields the runtime already sums onto
#' the result. Use it to catch regressions (e.g. an agent that suddenly takes
#' twice as many steps or tokens) on a static task.
#' @param result A generation result from `generate_text()`.
#' @param max_tokens Optional cap on `result$usage$total_tokens`.
#' @param max_steps Optional cap on `result$steps`.
#' @param max_tool_calls Optional cap on the number of tool calls made.
#' @param info Optional context for the failure message.
#' @return Invisibly, a list of the observed metrics.
#' @export
expect_run_within <- function(result, max_tokens = NULL, max_steps = NULL,
                              max_tool_calls = NULL, info = NULL) {
  observed <- list(
    total_tokens = result$usage$total_tokens %||% NA_real_,
    steps = result$steps %||% NA_real_,
    tool_calls = length(eval_result_tool_calls(result))
  )
  failures <- character(0)
  if (!is.null(max_tokens) && !is.na(observed$total_tokens) && observed$total_tokens > max_tokens) {
    failures <- c(failures, sprintf("total_tokens %g > %g", observed$total_tokens, max_tokens))
  }
  if (!is.null(max_steps) && !is.na(observed$steps) && observed$steps > max_steps) {
    failures <- c(failures, sprintf("steps %g > %g", observed$steps, max_steps))
  }
  if (!is.null(max_tool_calls) && observed$tool_calls > max_tool_calls) {
    failures <- c(failures, sprintf("tool_calls %d > %d", observed$tool_calls, max_tool_calls))
  }
  passed <- length(failures) == 0
  message <- sprintf(
    "Run exceeded its budget: %s%s",
    paste(failures, collapse = "; "),
    if (!is.null(info)) paste0("\nInfo: ", info) else ""
  )
  eval_expect(passed, message)
  invisible(observed)
}

#' @title Define an Evaluation Task
#' @description
#' A single eval task: an input prompt plus a `check` that grades the run. The
#' check receives the `generate_text()` result and returns `TRUE`/`FALSE` (or
#' uses the `expect_*` assertions, whose failure is caught as a fail).
#' @param name A short task name.
#' @param prompt The input prompt.
#' @param check A function `function(result)` returning TRUE on success.
#' @param system Optional per-task system prompt.
#' @param ... Extra arguments passed to `generate_text()` for this task
#'   (e.g. `tools`, `max_steps`).
#' @return An `aisdk_eval_task` list.
#' @export
eval_task <- function(name, prompt, check, system = NULL, ...) {
  if (!is.function(check)) {
    rlang::abort("`check` must be a function(result) returning TRUE/FALSE.")
  }
  structure(
    list(name = name, prompt = prompt, check = check, system = system, args = list(...)),
    class = "aisdk_eval_task"
  )
}

#' @keywords internal
eval_model_id <- function(model) {
  if (is.character(model)) {
    return(model)
  }
  if (inherits(model, "LanguageModelV1")) {
    return(paste0(model$provider %||% "", ":", model$model_id %||% ""))
  }
  ""
}

#' @title Run an Evaluation Suite
#' @description
#' Run a bank of [eval_task()]s against a model, grade each with its `check`,
#' and aggregate the results — pass rate plus the token and cost regression
#' signals the runtime already captures. This is the harness the `expect_*`
#' assertions plug into for regression testing across many cases.
#' @param tasks A list of [eval_task()]s.
#' @param model A model object or "provider:model" string used for every task.
#' @param registry Optional ProviderRegistry.
#' @param ... Extra arguments passed to every `generate_text()` call.
#' @return An `aisdk_eval_suite_result`: a list with `tasks` (per-task
#'   name/passed/total_tokens/cost_usd/steps), `pass_rate`, `n`, `passed`,
#'   `total_tokens`, and `total_cost`.
#' @export
run_eval_suite <- function(tasks, model = NULL, registry = NULL, ...) {
  if (!is.list(tasks) || length(tasks) == 0) {
    rlang::abort("`tasks` must be a non-empty list of eval_task() objects.")
  }
  resolved <- resolve_model(model, registry, type = "language")
  model_id <- eval_model_id(resolved)
  extra <- list(...)

  rows <- lapply(seq_along(tasks), function(i) {
    task <- tasks[[i]]
    name <- task$name %||% paste0("task_", i)
    call_args <- c(
      list(model = resolved, prompt = task$prompt, system = task$system, registry = registry),
      task$args %||% list(), extra
    )
    result <- tryCatch(do.call(generate_text, call_args), error = function(e) e)
    if (inherits(result, "error")) {
      return(list(name = name, passed = FALSE, error = conditionMessage(result),
                  total_tokens = NA_real_, cost_usd = NA_real_, steps = NA_real_))
    }
    passed <- isTRUE(tryCatch(task$check(result), error = function(e) FALSE))
    list(
      name = name,
      passed = passed,
      error = NA_character_,
      total_tokens = result$usage$total_tokens %||% NA_real_,
      cost_usd = estimate_cost(result$usage, model_id),
      steps = result$steps %||% NA_real_
    )
  })

  passed_vec <- vapply(rows, function(r) isTRUE(r$passed), logical(1))
  total_tokens <- sum(vapply(rows, function(r) r$total_tokens %||% NA_real_, numeric(1)), na.rm = TRUE)
  costs <- vapply(rows, function(r) r$cost_usd %||% NA_real_, numeric(1))
  structure(
    list(
      tasks = rows,
      n = length(rows),
      passed = sum(passed_vec),
      pass_rate = mean(passed_vec),
      total_tokens = total_tokens,
      total_cost = if (all(is.na(costs))) NA_real_ else sum(costs, na.rm = TRUE)
    ),
    class = "aisdk_eval_suite_result"
  )
}

#' @title Print an Eval Suite Result
#' @param x An `aisdk_eval_suite_result`.
#' @param ... Ignored.
#' @export
print.aisdk_eval_suite_result <- function(x, ...) {
  cat(sprintf("=== Eval suite: %d/%d passed (%.0f%%) ===\n", x$passed, x$n, 100 * x$pass_rate))
  for (r in x$tasks) {
    mark <- if (isTRUE(r$passed)) "PASS" else "FAIL"
    cat(sprintf("  [%s] %s", mark, r$name))
    if (!is.na(r$total_tokens)) cat(sprintf("  (%g tok)", r$total_tokens))
    if (!is.null(r$cost_usd) && !is.na(r$cost_usd)) cat(sprintf("  $%.4f", r$cost_usd))
    cat("\n")
  }
  if (!is.na(x$total_cost)) cat(sprintf("Total: %g tokens, $%.4f\n", x$total_tokens, x$total_cost))
  invisible(x)
}

# Null-coalescing operator
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
