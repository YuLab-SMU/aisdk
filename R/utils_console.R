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
#' @return Invisible NULL
#' @keywords internal
handle_user_choice <- function(choice, tool_name, session) {
  choice <- trimws(choice)

  if (choice == "1") {
    # Continue - do nothing, let the agent keep trying
    cat("Continuing...\n")
  } else if (choice == "2") {
    # Give up - add a message to the session suggesting to avoid this tool
    if (!is.null(session) && inherits(session, "ChatSession")) {
      session$add_message(list(
        role = "user",
        content = sprintf(
          "The tool '%s' has failed multiple times. Please try a different approach without using this tool.",
          tool_name
        )
      ))
    }
    cat("Added guidance to avoid this tool.\n")
  } else if (choice == "3") {
    # Explain - add a message asking the agent to explain
    if (!is.null(session) && inherits(session, "ChatSession")) {
      session$add_message(list(
        role = "user",
        content = sprintf(
          "Please explain why the tool '%s' keeps failing and what alternative approaches we could try.",
          tool_name
        )
      ))
    }
    cat("Asking agent to explain the problem...\n")
  } else if (choice == "4") {
    # Manual - inform user they can now intervene
    cat("\nYou can now manually fix the issue. When ready, continue the conversation.\n")
  } else {
    cat("Unknown choice, continuing...\n")
  }

  invisible(NULL)
}

# Define %||% if not already defined
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
