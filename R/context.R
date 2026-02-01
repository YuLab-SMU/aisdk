#' @title Context Management
#' @description Utilities for capturing and summarizing R objects for LLM context.
#' @name context
NULL

#' @title Get R Context
#' @description
#' Generates a text summary of R objects to be used as context for the LLM.
#' @param vars Character vector of variable names to include.
#' @param envir The environment to look for variables in. Default is parent.frame().
#' @return A single string containing the summaries of the requested variables.
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:10, y = rnorm(10))
#' context <- get_r_context("df")
#' cat(context)
#' }
get_r_context <- function(vars, envir = parent.frame()) {
  if (is.null(vars) || length(vars) == 0) {
    return("")
  }

  summaries <- lapply(vars, function(var) {
    if (!exists(var, envir = envir, inherits = FALSE)) {
      return(paste0("Variable `", var, "`: Not found in current environment.\n"))
    }
    
    obj <- get(var, envir = envir)
    summary_text <- summarize_object(obj, name = var)
    paste0("### Variable: `", var, "`\n\n", summary_text, "\n")
  })
  
  paste(unlist(summaries), collapse = "\n")
}

#' @title Summarize Object
#' @description
#' Creates a concise summary of an R object for LLM consumption.
#' Handles different object types appropriately.
#' @param obj The object to summarize.
#' @param name The name of the object (for display).
#' @return A string summary suitable for LLM context.
#' @keywords internal
summarize_object <- function(obj, name) {
  # Handle NULL
  if (is.null(obj)) {
    return("Value: NULL")
  }
  
  # Data frames and tibbles
  if (is.data.frame(obj)) {
    return(summarize_dataframe(obj, name))
  }
  
  # Matrices
  if (is.matrix(obj)) {
    return(summarize_matrix(obj, name))
  }
  
  # Lists (including named lists)
  if (is.list(obj) && !is.data.frame(obj)) {
    return(summarize_list(obj, name))
  }
  
  # Vectors (atomic)
  if (is.atomic(obj) && length(obj) > 1) {
    return(summarize_vector(obj, name))
  }
  
  # Functions
  if (is.function(obj)) {
    return(summarize_function(obj, name))
  }
  
  # Default: use str()
  summarize_default(obj, name)
}

#' @keywords internal
summarize_dataframe <- function(obj, name) {
  dims <- paste0(nrow(obj), " rows x ", ncol(obj), " columns")
  col_info <- vapply(names(obj), function(col) {
    paste0("  - `", col, "`: ", class(obj[[col]])[1])
  }, character(1))
  
  # Get first few rows
  n_preview <- min(5, nrow(obj))
  
  if (nrow(obj) > 0) {
    head_out <- capture.output(print(head(obj, n_preview)))
    preview <- paste(head_out, collapse = "\n")
  } else {
    preview <- "(empty data frame)"
  }
  
  paste0(
    "**Data Frame** (", dims, ")\n\n",
    "**Columns:**\n", paste(col_info, collapse = "\n"), "\n\n",
    "**Preview (first ", n_preview, " rows):**\n```\n", preview, "\n```"
  )
}

#' @keywords internal
summarize_matrix <- function(obj, name) {
  dims <- paste0(nrow(obj), " x ", ncol(obj))
  
  # Preview
  n_rows <- min(5, nrow(obj))
  n_cols <- min(5, ncol(obj))
  preview_out <- capture.output(print(obj[1:n_rows, 1:n_cols, drop = FALSE]))
  preview <- paste(preview_out, collapse = "\n")
  
  paste0(
    "**Matrix** (", dims, ", ", typeof(obj), ")\n\n",
    "**Preview:**\n```\n", preview, "\n```"
  )
}

#' @keywords internal
summarize_vector <- function(obj, name) {
  len <- length(obj)
  type <- typeof(obj)
  
  # Preview
  n_preview <- min(10, len)
  if (is.character(obj)) {
    preview <- paste0('"', head(obj, n_preview), '"', collapse = ", ")
  } else {
    preview <- paste(head(obj, n_preview), collapse = ", ")
  }
  if (len > n_preview) {
    preview <- paste0(preview, ", ...")
  }
  
  # Summary stats for numeric
  stats <- ""
  if (is.numeric(obj)) {
    stats <- paste0(
      "\n**Stats:** min=", round(min(obj, na.rm = TRUE), 3),
      ", max=", round(max(obj, na.rm = TRUE), 3),
      ", mean=", round(mean(obj, na.rm = TRUE), 3),
      ", NAs=", sum(is.na(obj))
    )
  }
  
  paste0(
    "**Vector** (length ", len, ", ", type, ")\n",
    "**Values:** [", preview, "]", stats
  )
}

#' @keywords internal
summarize_list <- function(obj, name) {
  len <- length(obj)
  elem_names <- names(obj)
  
  if (is.null(elem_names)) {
    elem_info <- paste0("  - [[", seq_along(obj), "]]: ", vapply(obj, function(x) class(x)[1], character(1)))
  } else {
    elem_info <- vapply(seq_along(obj), function(i) {
      n <- if (nzchar(elem_names[i])) paste0("$", elem_names[i]) else paste0("[[", i, "]]")
      paste0("  - ", n, ": ", class(obj[[i]])[1])
    }, character(1))
  }
  
  # Limit display
  if (length(elem_info) > 10) {
    elem_info <- c(elem_info[1:10], paste0("  ... and ", length(elem_info) - 10, " more elements"))
  }
  
  paste0(
    "**List** (", len, " elements)\n\n",
    paste(elem_info, collapse = "\n")
  )
}

#' @keywords internal
summarize_function <- function(obj, name) {
  args <- names(formals(obj))
  if (length(args) == 0) {
    args_str <- "(no arguments)"
  } else {
    args_str <- paste(args, collapse = ", ")
  }
  
  paste0("**Function** with arguments: ", args_str)
}

#' @keywords internal
summarize_default <- function(obj, name) {
  type_info <- paste0("Type: ", typeof(obj), ", Class: ", paste(class(obj), collapse = ", "))
  
  # Capture str() output, limited
  str_out <- tryCatch({
    capture.output(str(obj, max.level = 2, list.len = 5))
  }, error = function(e) {
    "(unable to display structure)"
  })
  
  paste0(
    type_info, "\n\n",
    "**Structure:**\n```r\n", paste(str_out, collapse = "\n"), "\n```"
  )
}
