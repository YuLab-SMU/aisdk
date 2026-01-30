#' Genesis V2: Plan-Execute-Refine Architecture
#'
#' Genesis V2 implements a complete Plan-Execute-Refine cycle with automatic
#' quality assessment and iterative improvement. It extends Genesis V1 with
#' the ability to evaluate results and automatically refine failed executions.
#'
#' Execute a task with Plan-Execute-Refine cycle
#'
#' @param task Character string describing the task to accomplish
#' @param skill_paths Character vector of paths to scan for skills, or "auto" for default locations
#' @param model Model to use for agents (default: claude-3-5-sonnet-20241022)
#' @param max_iterations Maximum number of PER iterations (default: 3)
#' @param auto_refine Logical, whether to enable automatic refinement (default: TRUE)
#' @param quality_threshold Minimum quality score to accept (0-100, default: 70)
#' @param cache Logical, whether to cache team composition (default: TRUE)
#' @param verbose Logical, whether to print orchestration details (default: FALSE)
#' @param architect_model Model to use for Architect agent (default: same as model)
#' @param evaluator_model Model to use for Evaluator agent (default: same as model)
#' @param refiner_model Model to use for Refiner agent (default: same as model)
#' @return List with result, iterations, evaluation, and history
#' @export
#' @examples
#' \dontrun{
#' # Basic usage with auto-refine
#' result <- genesis_v2(
#'   "Analyze the iris dataset and create a comprehensive report with visualizations"
#' )
#'
#' # With custom settings
#' result <- genesis_v2(
#'   "Analyze iris and create plots",
#'   max_iterations = 5,
#'   quality_threshold = 85,
#'   verbose = TRUE
#' )
#'
#' # Disable auto-refine (behaves like V1)
#' result <- genesis_v2(
#'   "Simple task",
#'   auto_refine = FALSE
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
                       refiner_model = NULL) {

  # Set default models
  if (is.null(architect_model)) architect_model <- model
  if (is.null(evaluator_model)) evaluator_model <- model
  if (is.null(refiner_model)) refiner_model <- model

  # Validate parameters
  if (max_iterations < 1) {
    stop("max_iterations must be at least 1")
  }
  if (quality_threshold < 0 || quality_threshold > 100) {
    stop("quality_threshold must be between 0 and 100")
  }

  # Initialize state
  iteration <- 1
  plan <- NULL
  execution_history <- list()
  library <- NULL

  # Main PER loop
  while (iteration <= max_iterations) {

    if (verbose) {
      cat(sprintf("\n%s Iteration %d/%d %s\n\n",
                  strrep("=", 20), iteration, max_iterations, strrep("=", 20)))
    }

    # ============================================================
    # PHASE 1: PLAN
    # ============================================================
    if (is.null(plan) || iteration > 1) {
      if (verbose) {
        cat("[PLAN] Phase: Analyzing task and selecting agents...\n")
      }

      # Discover agents (only once)
      if (is.null(library)) {
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
      }

      # Check cache (only on first iteration)
      if (cache && iteration == 1) {
        cache_key <- digest::digest(list(task = task, roles = library$list_roles()))

        if (exists(cache_key, envir = .genesis_cache)) {
          cached_plan <- get(cache_key, envir = .genesis_cache)

          if (verbose) {
            cat("  Using cached team composition\n")
            cat(sprintf("  Selected agents: %s\n\n", paste(cached_plan$selected_agents, collapse = ", ")))
          }

          plan <- cached_plan
        }
      }

      # Consult Architect V2 (with execution history for learning)
      if (is.null(plan)) {
        architect <- create_architect_v2(library, architect_model)
        plan <- architect$plan(task, execution_history)

        if (verbose) {
          cat(sprintf("  Selected agents: %s\n", paste(plan$selected_agents, collapse = ", ")))
          cat(sprintf("  Reasoning: %s\n", plan$reasoning))
          if (length(plan$success_criteria$must_have) > 0) {
            cat(sprintf("  Success criteria: %s\n",
                       paste(plan$success_criteria$must_have, collapse = ", ")))
          }
        }

        # Cache the plan (only on first iteration)
        if (cache && iteration == 1) {
          assign(cache_key, plan, envir = .genesis_cache)
        }
      }
    }

    # ============================================================
    # PHASE 2: EXECUTE
    # ============================================================
    if (verbose) {
      cat("\n[EXECUTE] Phase: Running agents...\n")
    }

    result <- execute_with_plan(task, library, plan, model, verbose)

    # Record execution
    execution_history[[iteration]] <- list(
      plan = plan,
      result = result,
      timestamp = Sys.time(),
      evaluation = NULL  # Will be filled in Refine phase
    )

    # ============================================================
    # PHASE 3: REFINE
    # ============================================================
    if (auto_refine) {
      if (verbose) {
        cat("\n[REFINE] Phase: Evaluating result quality...\n")
      }

      # Evaluate result
      evaluator <- create_evaluator_agent(evaluator_model)
      evaluation <- evaluator$evaluate(task, plan$success_criteria, result)

      # Store evaluation in history
      execution_history[[iteration]]$evaluation <- evaluation

      if (verbose) {
        cat(sprintf("  Quality score: %d/100\n", evaluation$score))
        cat(sprintf("  Passed: %s\n", evaluation$passed))
        if (length(evaluation$errors) > 0) {
          cat(sprintf("  Errors: %s\n", paste(evaluation$errors, collapse = ", ")))
        }
      }

      # Check if result meets quality standards
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

      # If not converged and iterations remain
      if (iteration < max_iterations) {
        if (verbose) {
          cat(sprintf("\n[WARNING] Quality below threshold (%d < %d)\n",
                     evaluation$score, quality_threshold))
          cat("  Consulting Refiner for improvements...\n")
        }

        # Consult Refiner
        refiner <- create_refiner_agent(library, refiner_model)
        refinement <- refiner$refine(task, plan, result, evaluation)

        if (verbose) {
          cat(sprintf("  Root cause: %s\n", refinement$root_cause))
          cat(sprintf("  Action: %s\n", refinement$action))
          cat(sprintf("  Reasoning: %s\n", refinement$reasoning))
          if (length(refinement$improvements) > 0) {
            cat(sprintf("  Improvements: %s\n", paste(refinement$improvements, collapse = ", ")))
          }
        }

        # Take action based on Refiner's recommendation
        if (refinement$action == "replan") {
          plan <- NULL  # Trigger replanning in next iteration
          if (verbose) {
            cat("  -> Will replan with different agents\n")
          }
        } else if (refinement$action == "abort") {
          if (verbose) {
            cat("\n[ABORT] Refiner suggests aborting. Returning best result.\n")
          }
          break
        } else if (refinement$action == "retry") {
          # Keep current plan, will retry in next iteration
          if (verbose) {
            cat("  -> Will retry with same agents\n")
          }
        }

      } else {
        if (verbose) {
          cat(sprintf("\n[WARNING] Max iterations reached (%d). Returning best result.\n",
                     max_iterations))
        }
        break
      }

    } else {
      # Auto-refine disabled, return immediately
      if (verbose) {
        cat("\n[DONE] Execution complete (auto-refine disabled)\n")
      }

      return(list(
        result = result,
        iterations = 1,
        evaluation = NULL,
        history = execution_history,
        converged = FALSE
      ))
    }

    iteration <- iteration + 1
  }

  # Return best result from all iterations
  best_result <- find_best_result(execution_history)

  list(
    result = best_result$result,
    iterations = length(execution_history),
    evaluation = best_result$evaluation,
    history = execution_history,
    converged = FALSE
  )
}

#' Find best result from execution history
#' @keywords internal
find_best_result <- function(execution_history) {
  if (length(execution_history) == 0) {
    stop("No execution history available")
  }

  # Find iteration with highest score
  best_idx <- 1
  best_score <- -1

  for (i in seq_along(execution_history)) {
    attempt <- execution_history[[i]]
    score <- attempt$evaluation$score %||% 0

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
    cat(sprintf("  Agents: %s\n", paste(attempt$plan$selected_agents, collapse = ", ")))
    if (!is.null(attempt$evaluation)) {
      cat(sprintf("  Score: %d/100\n", attempt$evaluation$score))
    }
    cat(sprintf("  Timestamp: %s\n", attempt$timestamp))
    cat("\n")
  }

  invisible(result)
}
