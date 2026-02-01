#' @title JSON Utilities
#' @description
#' Provides robust utilities for parsing potentially truncated or malformed
#' JSON strings, commonly encountered in streaming LLM outputs.
#' @name utils_json

#' @title Repair Truncated JSON
#' @description
#' A robust utility that uses a finite state machine to close open brackets,
#' braces, and quotes to make a truncated JSON string valid for parsing.
#'
#' @param json_str A potentially truncated JSON string.
#' @return A repaired JSON string.
#' @export
#' @examples
#' fix_json('{"name": "Gene...')
#' fix_json('[1, 2, {"a":')
fix_json <- function(json_str) {
  if (is.null(json_str) || nchar(json_str) == 0) {
    return("{}")
  }

  # State machine variables
  in_string <- FALSE
  escape <- FALSE
  stack <- character() # Stack for unmatched opening brackets/braces

  chars <- strsplit(json_str, "")[[1]]

  for (char in chars) {
    if (in_string) {
      if (char == "\\" && !escape) {
        escape <- TRUE
      } else if (char == '"' && !escape) {
        in_string <- FALSE
      } else {
        escape <- FALSE
      }
    } else {
      if (char == '"') {
        in_string <- TRUE
      } else if (char == "{") {
        stack <- c(stack, "}")
      } else if (char == "[") {
        stack <- c(stack, "]")
      } else if (char == "}" || char == "]") {
        # Pop stack if it matches the expected closing character
        if (length(stack) > 0 && tail(stack, 1) == char) {
          stack <- head(stack, -1)
        }
      }
    }
  }

  # Repair logic
  result <- json_str

  # 1. If still inside a string, close the quote
  if (in_string) {
    result <- paste0(result, '"')
  }

  # 2. Close any unclosed brackets/braces from the stack (in reverse order)
  if (length(stack) > 0) {
    result <- paste0(result, paste(rev(stack), collapse = ""))
  }

  return(result)
}

#' @title Safe JSON Parser
#' @description
#' Parses a JSON string, attempting to repair it using `fix_json` if
#' the initial parse fails.
#'
#' @param text A JSON string.
#' @return A parsed R object (list, vector, etc.) or NULL if parsing fails
#'   even after repair.
#' @export
#' @examples
#' safe_parse_json('{"a": 1}')
#' safe_parse_json('{"a": 1,')
safe_parse_json <- function(text) {
  if (is.null(text) || nchar(trimws(text)) == 0) {
    return(NULL)
  }
  
  tryCatch({
    jsonlite::fromJSON(text, simplifyVector = TRUE)
  }, error = function(e) {
    # If parsing fails, attempt to repair and re-parse
    fixed_text <- fix_json(text)
    tryCatch({
      jsonlite::fromJSON(fixed_text, simplifyVector = TRUE)
    }, error = function(e2) {
      return(NULL) # Return NULL if repair also fails
    })
  })
}
