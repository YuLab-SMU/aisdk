#' Console Utility Functions
#'
#' Internal utility functions for console failure detection and user interaction.
#'
#' @name console_utils
#' @keywords internal
NULL

#' Analyze Tool Failures
#'
#' Analyzes a list of tool results and counts failures per tool.
#'
#' @param tool_results List of tool result objects from a generation
#' @return Named integer vector with failure counts per tool name
#' @keywords internal
analyze_tool_failures <- function(tool_results) {
  if (is.null(tool_results) || length(tool_results) == 0) {
    return(integer(0))
  }

  # Count failures per tool
  failure_counts <- list()

  for (tr in tool_results) {
    if (isTRUE(tr$is_validation_error)) {
      next
    }

    tool_name <- tr$name %||% "unknown"

    # Check if this tool result represents a failure
    is_failure <- FALSE

    # Method 1: Check is_error flag
    if (isTRUE(tr$is_error)) {
      is_failure <- TRUE
    }

    # Method 2: Check if result starts with "Error"
    if (!is_failure && !is.null(tr$result)) {
      result_str <- as.character(tr$result)
      if (length(result_str) > 0 && grepl("^Error", result_str[1], ignore.case = FALSE)) {
        is_failure <- TRUE
      }
    }

    # Method 3: Check for common error patterns
    if (!is_failure && !is.null(tr$result)) {
      result_str <- as.character(tr$result)
      if (length(result_str) > 0) {
        error_patterns <- c(
          "^Error:",
          "^Error executing",
          "execution failed",
          "threw an error"
        )
        if (any(sapply(error_patterns, function(p) grepl(p, result_str[1], ignore.case = TRUE)))) {
          is_failure <- TRUE
        }
      }
    }

    if (is_failure) {
      if (is.null(failure_counts[[tool_name]])) {
        failure_counts[[tool_name]] <- 0L
      }
      failure_counts[[tool_name]] <- failure_counts[[tool_name]] + 1L
    }
  }

  # Convert to named integer vector
  if (length(failure_counts) == 0) {
    return(integer(0))
  }

  result <- unlist(failure_counts)
  names(result) <- names(failure_counts)
  result
}

#' Get Last Error for Tool
#'
#' Retrieves the error message from the last failure of a specific tool.
#'
#' @param tool_results List of tool result objects
#' @param tool_name Name of the tool to find the last error for
#' @return Character string with the error message, or NULL if not found
#' @keywords internal
get_last_error_for_tool <- function(tool_results, tool_name) {
  if (is.null(tool_results) || length(tool_results) == 0) {
    return(NULL)
  }

  # Find all results for this tool (in reverse order to get the last one first)
  for (i in rev(seq_along(tool_results))) {
    tr <- tool_results[[i]]
    if (!is.null(tr$name) && tr$name == tool_name) {
      # Check if this is an error result
      if (isTRUE(tr$is_error) || (!is.null(tr$result) && grepl("^Error", as.character(tr$result)[1]))) {
        return(as.character(tr$result)[1])
      }
    }
  }

  return(NULL)
}

# Define %||% if not already defined
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
