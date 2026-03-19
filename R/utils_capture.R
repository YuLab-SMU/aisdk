#' @title Capture R Console Output
#' @description
#' Internal helpers to capture printed output, messages, and warnings from
#' evaluated R expressions so tool execution can be rendered cleanly in the
#' console UI.
#' @name utils_capture
NULL

#' @keywords internal
capture_r_execution <- function(expr, envir = parent.frame(), auto_print_value = FALSE) {
  expr <- substitute(expr)

  messages <- character(0)
  warnings <- character(0)
  value <- NULL
  visible <- FALSE

  result <- tryCatch(
    {
      output <- utils::capture.output(
        withCallingHandlers(
          {
            evaluated <- withVisible(eval(expr, envir = envir))
            value <<- evaluated$value
            visible <<- isTRUE(evaluated$visible)
          },
          message = function(m) {
            messages <<- c(messages, trimws(conditionMessage(m)))
            invokeRestart("muffleMessage")
          },
          warning = function(w) {
            warnings <<- c(warnings, trimws(conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        ),
        type = "output"
      )

      if (isTRUE(auto_print_value) && isTRUE(visible) && length(output) == 0) {
        output <- utils::capture.output(print(value))
      }

      list(
        ok = TRUE,
        value = value,
        visible = visible,
        output = output,
        messages = messages,
        warnings = warnings
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        error = conditionMessage(e),
        output = character(0),
        messages = messages,
        warnings = warnings
      )
    }
  )

  result
}

#' @keywords internal
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
