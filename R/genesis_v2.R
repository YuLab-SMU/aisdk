#' Genesis V2: Direct Execute-Refine Architecture
#'
#' Genesis V2 implements a direct Execute-Refine loop with automatic
#' quality assessment and iterative improvement. It runs a single
#' direct agent with skills and refines based on evaluator feedback.
#'
#' Execute a task with Direct Execute-Refine cycle
#'
#' @param task Character string describing the task to accomplish
#' @param skill_paths Character vector of paths to scan for skills, or "auto" for default locations
#' @param model Model to use for agents (default: claude-3-5-sonnet-20241022)
#' @param max_iterations Maximum number of iterations (default: 3)
#' @param auto_refine Logical, whether to enable automatic refinement (default: TRUE)
#' @param quality_threshold Minimum quality score to accept (0-100, default: 70)
#' @param cache Logical, whether to cache team composition for similar tasks (unused in direct mode)
#' @param verbose Logical, whether to print orchestration details (default: FALSE)
#' @param architect_model Model to use for criteria generation (default: same as model)
#' @param evaluator_model Model to use for Evaluator agent (default: same as model)
#' @param refiner_model Model to use for Refiner agent (unused in direct mode)
#' @param max_steps Maximum tool execution steps (default: 10)
#' @return List with result, iterations, evaluation, and history
#' @export
#' @examples
#' \dontrun{
#' result <- genesis_v2(
#'   "Analyze the iris dataset and create a comprehensive report with visualizations",
#'   max_iterations = 3,
#'   quality_threshold = 80,
#'   auto_refine = TRUE,
#'   verbose = TRUE
#' )
#' }
genesis_v2 <- function(task,
                       skill_paths = "auto",
                       model = "claude-3-5-sonnet-20241022",
                       max_iterations = 3,
                       auto_refine = TRUE,
                       quality_threshold = 70,
                       cache = TRUE,
                       verbose = FALSE,
                       architect_model = NULL,
                       evaluator_model = NULL,
                       refiner_model = NULL,
                       max_steps = 10) {

  if (is.null(architect_model)) architect_model <- model
  if (is.null(evaluator_model)) evaluator_model <- model
  if (is.null(refiner_model)) refiner_model <- model

  if (max_iterations < 1) {
    stop("max_iterations must be at least 1")
  }
  if (quality_threshold < 0 || quality_threshold > 100) {
    stop("quality_threshold must be between 0 and 100")
  }

  if (verbose) {
    cat("[PLAN] Phase: Generating success criteria...\n")
  }
  success_criteria <- generate_success_criteria(task, architect_model)

  plan <- list(
    task_analysis = "Direct execution (no multi-agent planning)",
    selected_agents = character(0),
    reasoning = "Direct mode: single agent with skill tools",
    delegation_strategy = "Direct execution with tool calls",
    success_criteria = success_criteria
  )

  coder_tools <- create_coder_agent()$tools
  artifact_dir <- create_artifact_dir()
  artifact_tools <- create_artifact_tools(artifact_dir)
  agent <- Agent$new(
    name = "Genesis",
    description = "Direct execution agent",
    system_prompt = GENESIS_DIRECT_PROMPT,
    skills = skill_paths,
    tools = c(coder_tools, artifact_tools),
    model = model
  )
  session <- create_shared_session(model = model)
  assign(".artifact_dir", artifact_dir, envir = session$get_envir())

  iteration <- 1
  execution_history <- list()

  while (iteration <= max_iterations) {
    if (verbose) {
      cat(sprintf("\n%s Iteration %d/%d %s\n\n",
                  strrep("=", 20), iteration, max_iterations, strrep("=", 20)))
      cat("[EXECUTE] Phase: Running direct agent...\n")
    }

    result <- agent$run(
      task = task,
      session = session,
      model = model,
      max_steps = max_steps
    )

    execution_history[[iteration]] <- list(
      plan = plan,
      result = result,
      timestamp = Sys.time(),
      evaluation = NULL
    )

    if (!auto_refine) {
      if (verbose) {
        cat("\n[DONE] Execution complete (auto-refine disabled)\n")
      }
      return(list(
        result = result,
        iterations = iteration,
        evaluation = NULL,
        history = execution_history,
        converged = TRUE
      ))
    }

    if (verbose) {
      cat("\n[REFINE] Phase: Evaluating result quality...\n")
    }

    evaluator <- create_evaluator_agent(evaluator_model)
    evaluation <- evaluator$evaluate(task, success_criteria, result)
    execution_history[[iteration]]$evaluation <- evaluation

    if (verbose) {
      cat(sprintf("  Quality score: %d/100\n", evaluation$score))
      cat(sprintf("  Passed: %s\n", evaluation$passed))
      if (length(evaluation$errors) > 0) {
        cat(sprintf("  Errors: %s\n", paste(evaluation$errors, collapse = ", ")))
      }
    }

    if (evaluation$passed && evaluation$score >= quality_threshold) {
      if (verbose) {
        cat(sprintf("\n[SUCCESS] Result meets quality standards (score: %d >= %d)\n",
                    evaluation$score, quality_threshold))
      }
      return(list(
        result = result,
        iterations = iteration,
        evaluation = evaluation,
        history = execution_history,
        converged = TRUE
      ))
    }

    if (iteration >= max_iterations) {
      if (verbose) {
        cat(sprintf("\n[WARNING] Max iterations reached (%d). Returning best result.\n",
                    max_iterations))
      }
      break
    }

    refinement <- build_refinement_context(evaluation)
    task <- paste(task, "\n\n[REVISION REQUIRED]\n", refinement)
    iteration <- iteration + 1
  }

  best_result <- find_best_result(execution_history)

  list(
    result = best_result$result,
    iterations = length(execution_history),
    evaluation = best_result$evaluation,
    history = execution_history,
    converged = FALSE
  )
}

#' Generate success criteria for a task
#' @keywords internal
generate_success_criteria <- function(task, model) {
  prompt <- paste(
    "Generate success criteria for the task below.",
    "Return JSON with keys: must_have, quality_checks, expected_outputs.",
    "Keep items specific and verifiable.",
    "Do NOT require file outputs (PDF, images, scripts) unless explicitly requested in the task.",
    "Assume in-memory analysis and textual report output are acceptable.",
    "Avoid requiring extra packages unless the task explicitly asks for them.",
    "",
    paste0("Task: ", task),
    sep = "\n"
  )

  agent <- Agent$new(
    name = "Criteria",
    description = "Generates success criteria",
    system_prompt = "You generate concise, testable success criteria as JSON.",
    model = model
  )
  session <- ChatSession$new(model = model, agent = agent)
  response <- session$send(prompt)

  parse_success_criteria(response$text)
}

#' Parse success criteria JSON
#' @keywords internal
parse_success_criteria <- function(response) {
  tryCatch({
    json_pattern <- "\\{[^{}]*(?:\\{[^{}]*\\}[^{}]*)*\\}"
    json_matches <- gregexpr(json_pattern, response, perl = TRUE)
    json_text <- regmatches(response, json_matches)[[1]]

    if (length(json_text) == 0) {
      stop("No JSON found in criteria response")
    }

    criteria <- NULL
    for (json_str in json_text) {
      try_criteria <- tryCatch({
        jsonlite::fromJSON(json_str, simplifyVector = FALSE)
      }, error = function(e) NULL)
      if (!is.null(try_criteria)) {
        criteria <- try_criteria
        break
      }
    }

    if (is.null(criteria)) {
      stop("Could not parse JSON from criteria response")
    }

    required_fields <- c("must_have", "quality_checks", "expected_outputs")
    missing <- setdiff(required_fields, names(criteria))
    if (length(missing) > 0) {
      for (field in missing) criteria[[field]] <- list()
    }

    criteria$must_have <- unlist(criteria$must_have)
    criteria$quality_checks <- unlist(criteria$quality_checks)
    criteria$expected_outputs <- unlist(criteria$expected_outputs)

    criteria
  }, error = function(e) {
    list(
      must_have = c("Complete the task as described"),
      quality_checks = c("Clear, structured output"),
      expected_outputs = c("Final answer")
    )
  })
}

#' Build refinement context from evaluation
#' @keywords internal
build_refinement_context <- function(evaluation) {
  lines <- c()

  if (!is.null(evaluation$completeness)) {
    missing <- evaluation$completeness$must_have_missing %||% list()
    if (length(missing) > 0) {
      lines <- c(lines, "Missing requirements:")
      lines <- c(lines, paste0("- ", missing))
    }
  }

  if (!is.null(evaluation$quality)) {
    weaknesses <- evaluation$quality$weaknesses %||% list()
    if (length(weaknesses) > 0) {
      lines <- c(lines, "Quality issues:")
      lines <- c(lines, paste0("- ", weaknesses))
    }
  }

  if (length(evaluation$errors) > 0) {
    lines <- c(lines, "Errors:")
    lines <- c(lines, paste0("- ", evaluation$errors))
  }

  if (!is.null(evaluation$feedback) && nzchar(evaluation$feedback)) {
    lines <- c(lines, "Evaluator feedback:")
    lines <- c(lines, evaluation$feedback)
  }

  paste(lines, collapse = "\n")
}

#' Find best result from execution history
#' @keywords internal
find_best_result <- function(execution_history) {
  if (length(execution_history) == 0) {
    stop("No execution history available")
  }

  best_idx <- 1
  best_score <- -1

  for (i in seq_along(execution_history)) {
    attempt <- execution_history[[i]]
    score <- 0
    if (!is.null(attempt$evaluation)) {
      score <- attempt$evaluation$score %||% 0
    }
    if (score > best_score) {
      best_score <- score
      best_idx <- i
    }
  }

  execution_history[[best_idx]]
}

#' Print Genesis V2 result summary
#'
#' @param result Result object from genesis_v2()
#' @export
print_genesis_v2_result <- function(result) {
  cat("\n=== Genesis V2 Result Summary ===\n\n")

  cat(sprintf("Iterations: %d\n", result$iterations))
  cat(sprintf("Converged: %s\n", result$converged))

  if (!is.null(result$evaluation)) {
    cat(sprintf("\nFinal Quality Score: %d/100\n", result$evaluation$score))
    cat(sprintf("Passed: %s\n", result$evaluation$passed))

    if (!is.null(result$evaluation$completeness)) {
      cat(sprintf("\nCompleteness: %s\n", result$evaluation$completeness$assessment %||% "Unknown"))
    }

    if (!is.null(result$evaluation$quality)) {
      cat(sprintf("Quality: %s\n", result$evaluation$quality$assessment %||% "Unknown"))
    }

    if (length(result$evaluation$errors) > 0) {
      cat("\nErrors:\n")
      for (error in result$evaluation$errors) {
        cat(sprintf("  - %s\n", error))
      }
    }

    if (!is.null(result$evaluation$feedback)) {
      cat(sprintf("\nFeedback: %s\n", result$evaluation$feedback))
    }
  }

  cat("\n=== Iteration History ===\n\n")
  for (i in seq_along(result$history)) {
    attempt <- result$history[[i]]
    cat(sprintf("Iteration %d:\n", i))
    cat("  Mode: direct\n")
    if (!is.null(attempt$evaluation)) {
      cat(sprintf("  Score: %d/100\n", attempt$evaluation$score))
    }
    cat(sprintf("  Timestamp: %s\n", attempt$timestamp))
    cat("\n")
  }

  invisible(result)
}
