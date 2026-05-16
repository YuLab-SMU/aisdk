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

#' Read Line with Options
#'
#' Displays options to the user and reads their choice.
#'
#' @param prompt The prompt to display
#' @param options Named character vector where names are keys and values are descriptions
#' @return The key corresponding to the user's choice
#' @keywords internal
readline_with_options <- function(prompt, options) {
  if (!interactive()) {
    # In non-interactive mode, return the first option
    return(names(options)[1])
  }

  # Display options
  cat("\n")
  for (key in names(options)) {
    cat(sprintf("  %s: %s\n", key, options[[key]]))
  }
  cat("\n")

  # Read user input
  repeat {
    response <- readline(prompt = paste0(prompt, " "))
    response <- trimws(response)

    # Check if response is valid
    if (response %in% names(options)) {
      return(response)
    }

    # If not valid, show error and try again
    cat("Invalid choice. Please enter one of: ", paste(names(options), collapse = ", "), "\n", sep = "")
  }
}

#' Handle User Choice for Tool Failure
#'
#' Handles the user's choice when a tool has failed repeatedly.
#'
#' @param choice The user's choice (1, 2, 3, or 4)
#' @param tool_name Name of the failing tool
#' @param session The ChatSession object
#' @param last_error Optional last error message for the failing tool.
#' @return A list describing the selected action.
#' @keywords internal
handle_user_choice <- function(choice, tool_name, session, last_error = NULL) {
  choice <- trimws(choice)

  if (choice == "1") {
    cat("Continuing with a recovery prompt...\n")
    return(list(
      action = "continue",
      guidance = paste(
        sprintf("Continue from the previous `%s` tool failure.", tool_name),
        "Use the previous tool error as evidence.",
        "Do not repeat the exact same failing call unless you make a concrete change.",
        "Switch to a safer diagnostic or ask the user if more input is needed.",
        if (!is.null(last_error) && nzchar(last_error)) paste("Last error:", last_error) else NULL
      ),
      prompt = paste(
        sprintf("Continue from the previous `%s` tool failure.", tool_name),
        "Use the previous tool error as evidence.",
        "Do not repeat the exact same failing call unless you make a concrete change.",
        "Switch to a safer diagnostic or ask the user if more input is needed.",
        if (!is.null(last_error) && nzchar(last_error)) paste("Last error:", last_error) else NULL
      )
    ))
  } else if (choice == "2") {
    cat("Continuing with guidance to avoid this tool.\n")
    return(list(
      action = "avoid_tool",
      guidance = sprintf(
        "The tool `%s` has failed multiple times. Stop using that tool for this task. Explain the failure briefly and try a different approach.",
        tool_name
      ),
      prompt = sprintf(
        "The tool `%s` has failed multiple times. Stop using that tool for this task. Explain the failure briefly and try a different approach.",
        tool_name
      )
    ))
  } else if (choice == "3") {
    cat("Asking agent to explain the problem...\n")
    return(list(
      action = "explain",
      guidance = sprintf(
        "Please explain why the tool `%s` keeps failing and what alternative approaches we could try. Do not call the failing tool again in this response.",
        tool_name
      ),
      prompt = sprintf(
        "Please explain why the tool `%s` keeps failing and what alternative approaches we could try. Do not call the failing tool again in this response.",
        tool_name
      )
    ))
  } else if (choice == "4") {
    # Manual - inform user they can now intervene
    cat("\nYou can now manually fix the issue. When ready, continue the conversation.\n")
    return(list(action = "manual", guidance = NULL))
  } else {
    cat("Unknown choice, continuing...\n")
    return(list(action = "none", guidance = NULL))
  }
}

# Define %||% if not already defined
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
