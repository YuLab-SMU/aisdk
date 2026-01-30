#' Genesis V2 Helper Functions
#'
#' Utility functions to support Genesis V2 PER cycle
#'
#' @keywords internal

#' Compare two Genesis V2 results
#'
#' @param result1 First result from genesis_v2()
#' @param result2 Second result from genesis_v2()
#' @export
compare_genesis_results <- function(result1, result2) {
  cat("\n=== Genesis V2 Result Comparison ===\n\n")

  cat("Result 1:\n")
  cat(sprintf("  Iterations: %d\n", result1$iterations))
  cat(sprintf("  Score: %d/100\n", result1$evaluation$score %||% 0))
  cat(sprintf("  Converged: %s\n", result1$converged))

  cat("\nResult 2:\n")
  cat(sprintf("  Iterations: %d\n", result2$iterations))
  cat(sprintf("  Score: %d/100\n", result2$evaluation$score %||% 0))
  cat(sprintf("  Converged: %s\n", result2$converged))

  score1 <- result1$evaluation$score %||% 0
  score2 <- result2$evaluation$score %||% 0

  cat("\nComparison:\n")
  if (score1 > score2) {
    cat(sprintf("  Result 1 is better (+%d points)\n", score1 - score2))
  } else if (score2 > score1) {
    cat(sprintf("  Result 2 is better (+%d points)\n", score2 - score1))
  } else {
    cat("  Results are equal\n")
  }

  invisible(list(result1 = result1, result2 = result2))
}

#' Extract insights from Genesis V2 execution history
#'
#' @param result Result from genesis_v2()
#' @export
analyze_genesis_history <- function(result) {
  cat("\n=== Genesis V2 Execution Analysis ===\n\n")

  if (length(result$history) == 0) {
    cat("No execution history available\n")
    return(invisible(NULL))
  }

  # Score progression
  scores <- sapply(result$history, function(h) h$evaluation$score %||% 0)

  cat("Score Progression:\n")
  for (i in seq_along(scores)) {
    change <- if (i > 1) sprintf(" (%+d)", scores[i] - scores[i-1]) else ""
    cat(sprintf("  Iteration %d: %d/100%s\n", i, scores[i], change))
  }

  # Agent usage
  cat("\nAgent Usage:\n")
  agent_counts <- table(unlist(lapply(result$history, function(h) h$plan$selected_agents)))
  for (agent in names(agent_counts)) {
    cat(sprintf("  %s: %d time(s)\n", agent, agent_counts[agent]))
  }

  # Common errors
  all_errors <- unlist(lapply(result$history, function(h) h$evaluation$errors))
  if (length(all_errors) > 0) {
    cat("\nCommon Issues:\n")
    error_counts <- table(all_errors)
    for (error in names(error_counts)) {
      cat(sprintf("  - %s (%d time(s))\n", error, error_counts[error]))
    }
  }

  # Improvement rate
  if (length(scores) > 1) {
    improvement <- scores[length(scores)] - scores[1]
    cat(sprintf("\nOverall Improvement: %+d points\n", improvement))
    cat(sprintf("Average per iteration: %+.1f points\n", improvement / (length(scores) - 1)))
  }

  invisible(result)
}

#' Get Genesis V2 statistics
#'
#' @param results List of results from multiple genesis_v2() calls
#' @importFrom stats sd
#' @export
genesis_v2_stats <- function(results) {
  if (length(results) == 0) {
    stop("No results provided")
  }

  cat("\n=== Genesis V2 Statistics ===\n\n")

  # Overall stats
  total_runs <- length(results)
  converged <- sum(sapply(results, function(r) r$converged))

  cat(sprintf("Total runs: %d\n", total_runs))
  cat(sprintf("Converged: %d (%.1f%%)\n", converged, 100 * converged / total_runs))

  # Score statistics
  scores <- sapply(results, function(r) r$evaluation$score %||% 0)
  cat(sprintf("\nQuality Scores:\n"))
  cat(sprintf("  Mean: %.1f\n", mean(scores)))
  cat(sprintf("  Median: %.1f\n", median(scores)))
  cat(sprintf("  Min: %d\n", min(scores)))
  cat(sprintf("  Max: %d\n", max(scores)))
  cat(sprintf("  SD: %.1f\n", sd(scores)))

  # Iteration statistics
  iterations <- sapply(results, function(r) r$iterations)
  cat(sprintf("\nIterations:\n"))
  cat(sprintf("  Mean: %.1f\n", mean(iterations)))
  cat(sprintf("  Median: %.1f\n", median(iterations)))
  cat(sprintf("  Min: %d\n", min(iterations)))
  cat(sprintf("  Max: %d\n", max(iterations)))

  invisible(list(
    total_runs = total_runs,
    converged = converged,
    scores = scores,
    iterations = iterations
  ))
}

#' Export Genesis V2 result to JSON
#'
#' @param result Result from genesis_v2()
#' @param file Output file path
#' @export
export_genesis_result <- function(result, file) {
  # Prepare data for export
  export_data <- list(
    iterations = result$iterations,
    converged = result$converged,
    evaluation = result$evaluation,
    history = lapply(result$history, function(h) {
      list(
        plan = list(
          selected_agents = h$plan$selected_agents,
          reasoning = h$plan$reasoning,
          success_criteria = h$plan$success_criteria
        ),
        evaluation = h$evaluation,
        timestamp = as.character(h$timestamp)
      )
    })
  )

  # Write to file
  jsonlite::write_json(export_data, file, pretty = TRUE, auto_unbox = TRUE)

  cat(sprintf("Result exported to: %s\n", file))
  invisible(file)
}

#' Import Genesis V2 result from JSON
#'
#' @param file Input file path
#' @export
import_genesis_result <- function(file) {
  if (!file.exists(file)) {
    stop(sprintf("File not found: %s", file))
  }

  data <- jsonlite::read_json(file, simplifyVector = FALSE)

  cat(sprintf("Result imported from: %s\n", file))
  cat(sprintf("  Iterations: %d\n", data$iterations))
  cat(sprintf("  Converged: %s\n", data$converged))

  invisible(data)
}

#' Create a Genesis V2 benchmark suite
#'
#' @param tasks Character vector of tasks to benchmark
#' @param skill_paths Skill paths to use
#' @param model Model to use
#' @param max_iterations Maximum iterations per task
#' @param quality_threshold Quality threshold
#' @export
benchmark_genesis_v2 <- function(tasks,
                                  skill_paths = "auto",
                                  model = "claude-3-5-sonnet-20241022",
                                  max_iterations = 3,
                                  quality_threshold = 70) {

  cat(sprintf("\n=== Genesis V2 Benchmark ===\n"))
  cat(sprintf("Tasks: %d\n", length(tasks)))
  cat(sprintf("Max iterations: %d\n", max_iterations))
  cat(sprintf("Quality threshold: %d\n\n", quality_threshold))

  results <- list()

  for (i in seq_along(tasks)) {
    task <- tasks[i]
    cat(sprintf("[%d/%d] Running: %s\n", i, length(tasks), substr(task, 1, 50)))

    start_time <- Sys.time()

    result <- tryCatch({
      genesis_v2(
        task = task,
        skill_paths = skill_paths,
        model = model,
        max_iterations = max_iterations,
        quality_threshold = quality_threshold,
        verbose = FALSE
      )
    }, error = function(e) {
      list(
        error = e$message,
        iterations = 0,
        converged = FALSE,
        evaluation = list(score = 0)
      )
    })

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    result$task <- task
    result$elapsed_seconds <- elapsed

    results[[i]] <- result

    cat(sprintf("  Score: %d/100, Iterations: %d, Time: %.1fs\n\n",
               result$evaluation$score %||% 0,
               result$iterations,
               elapsed))
  }

  # Summary
  cat("\n=== Benchmark Summary ===\n\n")
  genesis_v2_stats(results)

  invisible(results)
}

#' Visualize Genesis V2 score progression
#'
#' @param result Result from genesis_v2()
#' @export
plot_genesis_progression <- function(result) {
  # Global variable definitions for R CMD check
  iteration <- score <- NULL
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required for plotting")
  }

  # Extract scores
  scores <- sapply(result$history, function(h) h$evaluation$score %||% 0)
  iterations <- seq_along(scores)

  # Create data frame
  df <- data.frame(
    iteration = iterations,
    score = scores
  )

  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = iteration, y = score)) +
    ggplot2::geom_line(color = "steelblue", size = 1) +
    ggplot2::geom_point(color = "steelblue", size = 3) +
    ggplot2::geom_hline(yintercept = 70, linetype = "dashed", color = "red", alpha = 0.5) +
    ggplot2::labs(
      title = "Genesis V2 Quality Score Progression",
      x = "Iteration",
      y = "Quality Score",
      subtitle = sprintf("Final score: %d/100 after %d iteration(s)",
                        scores[length(scores)], length(scores))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::ylim(0, 100)

  print(p)
  invisible(p)
}
