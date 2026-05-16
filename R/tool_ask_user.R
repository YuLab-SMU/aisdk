#' Create an ask_user Tool
#'
#' Creates a tool that allows an agent to pause execution and ask the user
#' for help, clarification, or guidance. This is useful when the agent
#' encounters repeated failures, uncertainty, or needs additional information
#' that only the user can provide.
#'
#' @return A Tool object that can be used with agents
#'
#' @details
#' The ask_user tool enables intelligent failure handling by allowing agents
#' to recognize when they're stuck and proactively seek user input rather than
#' blindly retrying failed operations.
#'
#' The tool accepts:
#' - `question`: The main question or explanation to show the user (required)
#' - `context`: Optional context about why the agent is asking (what was tried, what failed)
#' - `options`: Optional list of suggested responses for the user to choose from
#'
#' When options are provided, the user can either select a numbered option or
#' type their own custom response.
#'
#' @examples
#' \dontrun{
#' # Create the tool
#' ask_tool <- create_ask_user_tool()
#'
#' # Use in an agent
#' agent <- Agent$new(
#'   name = "helper",
#'   tools = list(ask_tool, other_tools...)
#' )
#'
#' # The agent can then use it when stuck:
#' # ask_user(
#' #   question = "I've tried 3 different approaches to parse this file and all failed. How should I proceed?",
#' #   context = "Attempts: 1) JSON parse - syntax error, 2) CSV parse - wrong delimiter, 3) XML parse - not valid XML",
#' #   options = ["Try a different file format", "Show me the raw file content", "Skip this file"]
#' # )
#' }
#'
#' @export
create_ask_user_tool <- function() {
  Tool$new(
    name = "ask_user",
    description = paste(
      "Ask the user for help, clarification, or guidance.",
      "Use this tool when you're stuck, uncertain, or need user input to proceed.",
      "The user will see your question and can provide guidance.",
      "\n\nWhen to use this tool:",
      "- After 2+ failed attempts with the same tool or approach",
      "- When you're uncertain about how to proceed",
      "- When you need clarification or additional information",
      "- When the user might have insights you don't have",
      "\n\nDo NOT use this for routine questions - only when you genuinely need help."
    ),
    parameters = list(
      question = list(
        type = "string",
        description = "The question or explanation to show the user. Be specific about what you need help with."
      ),
      context = list(
        type = "string",
        description = "Optional context about why you're asking. Include what you tried and what failed."
      ),
      options = list(
        type = "array",
        description = "Optional list of suggested responses for the user to choose from",
        items = list(type = "string")
      )
    ),
    required = c("question"),
    handler = function(question, context = NULL, options = NULL) {
      # Check if we're in an interactive session
      if (!interactive()) {
        # In non-interactive mode, just log the question
        message("\n[AGENT QUESTION] ", question)
        if (!is.null(context)) {
          message("[CONTEXT] ", context)
        }
        return("[Non-interactive mode: user input not available]")
      }

      # Display the question with formatting
      cat("\n")
      if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_rule("Agent Question", line = 2)
      } else {
        cat("=== Agent Question ===\n")
      }
      cat("\n", question, "\n", sep = "")

      # Display context if provided
      if (!is.null(context) && nzchar(context)) {
        cat("\n")
        if (requireNamespace("cli", quietly = TRUE)) {
          cli::cli_text("{.emph Context:} {context}")
        } else {
          cat("Context: ", context, "\n", sep = "")
        }
      }

      # Display options if provided
      if (!is.null(options) && length(options) > 0) {
        cat("\n")
        if (requireNamespace("cli", quietly = TRUE)) {
          cli::cli_text("{.strong Suggested responses:}")
        } else {
          cat("Suggested responses:\n")
        }
        for (i in seq_along(options)) {
          cat(sprintf("  %d. %s\n", i, options[[i]]))
        }
        cat("\n")
        response <- readline(prompt = "Your choice (number or custom text): ")

        # If user entered a number, convert to the corresponding option
        if (grepl("^\\d+$", trimws(response))) {
          idx <- as.integer(trimws(response))
          if (idx >= 1 && idx <= length(options)) {
            response <- options[[idx]]
            cat("Selected: ", response, "\n", sep = "")
          }
        }
      } else {
        cat("\n")
        response <- readline(prompt = "Your response: ")
      }

      # Close the question section
      if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_rule(line = 2)
      } else {
        cat("=====================\n")
      }
      cat("\n")

      # Return the user's response
      return(trimws(response))
    }
  )
}
