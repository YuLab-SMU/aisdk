#' @title Capture R Console Output
#' @description
#' Internal helpers to capture printed output, messages, and warnings from
#' evaluated R expressions so tool execution can be rendered cleanly in the
#' console UI.
#' @name utils_capture
NULL

#' Capture the output, messages, and value of evaluating an expression
#'
#' Part of the companion-package extension API (used by \pkg{aisdk.datatools}).
#' @param expr Expression to evaluate.
#' @param envir Environment in which to evaluate `expr`.
#' @param auto_print_value Whether to auto-print the resulting value.
#' @return A list describing the captured execution (output, value, status).
#' @keywords internal
#' @export
capture_r_execution <- function(expr, envir = parent.frame(), auto_print_value = FALSE) {
  expr <- substitute(expr)

  state <- new.env(parent = emptyenv())
  state$messages <- character(0)
  state$warnings <- character(0)
  state$value <- NULL
  state$visible <- FALSE

  result <- tryCatch(
    {
      output <- utils::capture.output(
        withCallingHandlers(
          {
            evaluated <- withVisible(eval(expr, envir = envir))
            state$value <- evaluated$value
            state$visible <- isTRUE(evaluated$visible)
          },
          message = function(m) {
            state$messages <- c(state$messages, trimws(conditionMessage(m)))
            invokeRestart("muffleMessage")
          },
          warning = function(w) {
            state$warnings <- c(state$warnings, trimws(conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        ),
        type = "output"
      )

      if (isTRUE(auto_print_value) && isTRUE(state$visible) && length(output) == 0) {
        output <- utils::capture.output(print(state$value))
      }

      list(
        ok = TRUE,
        value = state$value,
        visible = state$visible,
        output = output,
        messages = state$messages,
        warnings = state$warnings
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        error = conditionMessage(e),
        output = character(0),
        messages = state$messages,
        warnings = state$warnings
      )
    }
  )

  result
}

#' Format a captured execution result as text
#'
#' Part of the companion-package extension API (used by \pkg{aisdk.datatools}).
#' @param captured A list produced by [capture_r_execution()].
#' @param empty_message Message to use when there was no printed output.
#' @return A character string describing the captured execution.
#' @keywords internal
#' @export
format_captured_execution <- function(captured,
                                      empty_message = "(Code executed successfully with no printed output)") {
  if (!isTRUE(captured$ok)) {
    lines <- c(
      if (length(captured$output)) captured$output else character(0),
      if (length(captured$messages)) paste0("Message: ", captured$messages) else character(0),
      if (length(captured$warnings)) paste0("Warning: ", captured$warnings) else character(0)
    )

    if (!is.null(captured$error) && nzchar(captured$error)) {
      lines <- c(lines, paste("Error:", captured$error))
    }

    return(paste(lines, collapse = "\n"))
  }

  lines <- c(
    if (length(captured$output)) captured$output else character(0),
    if (length(captured$messages)) paste0("Message: ", captured$messages) else character(0),
    if (length(captured$warnings)) paste0("Warning: ", captured$warnings) else character(0)
  )

  if (length(lines) == 0) {
    return(empty_message)
  }

  paste(lines, collapse = "\n")
}
