#' @title Knitr Engine for AI
#' @description
#' Implements a custom knitr engine `{ai}` that allows using LLMs to generate
#' and execute R code within RMarkdown/Quarto documents.
#' @name knitr_engine
NULL

# Private cache for storing active sessions during a knit process
# This environment persists across chunks within a single knit() call
.engine_env <- new.env(parent = emptyenv())

#' @title Register AI Engine
#' @description
#' Registers the `{ai}` engine with knitr. Call this function once before
#' knitting a document that uses `{ai}` chunks.
#' @return Invisible NULL.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   library(aisdk)
#'   register_ai_engine()
#'   # Now you can use ```{ai} chunks in your RMarkdown
#' }
#' }
register_ai_engine <- function() {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    rlang::abort("Package 'knitr' is required to register the AI engine.")
  }

  knitr::knit_engines$set(ai = eng_ai)
  rlang::inform("AI engine registered. You can now use {ai} chunks in your documents.")
  invisible(NULL)
}

#' @title AI Engine Function
#' @description
#' The core engine function for `{ai}` knitr chunks.
#' @param options A list of chunk options provided by knitr.
#' @return A character string suitable for knitr output.
#' @keywords internal
eng_ai <- function(options) {
  # Validate knitr is available (should be, since we're called by knitr)
  if (!requireNamespace("knitr", quietly = TRUE)) {
    return("Error: knitr is not available")
  }

  # --- 1. Setup ---
  envir <- options$envir %||% knitr::knit_global()
  user_prompt <- paste(options$code, collapse = "\n")

  # Skip empty chunks
  if (!nzchar(trimws(user_prompt))) {
    return("")
  }

  # --- 2. Get or Create Session ---
  session <- get_or_create_session(options)

  # --- 3. Build Context ---
  context_str <- build_context(user_prompt, options$context, envir)

  # --- 4. Construct Full Prompt ---
  full_prompt <- construct_prompt(user_prompt, context_str)

  # --- 5. Call LLM ---
  response <- tryCatch(
    {
      session$send(full_prompt)
    },
    error = function(e) {
      return(list(text = paste0("**Error calling LLM:** ", e$message)))
    }
  )

  response_text <- response$text %||% ""

  # --- 6. Extract and Execute Code with Retry ---
  max_retries <- options$max_retries %||% 2
  extracted_code <- extract_r_code(response_text)

  # Format output
  explanation <- remove_r_code_blocks(response_text)
  out_parts <- character(0)
  if (nzchar(explanation)) {
    out_parts <- c(out_parts, explanation)
  }

  if (isTRUE(options$eval) && nzchar(extracted_code)) {
    extracted_code <- sanitize_r_code(extracted_code)

    # Use knit_child to evaluate the code and capture all output natively
    code_chunk <- c("```{r, error=TRUE, echo=TRUE}", strsplit(extracted_code, "\n")[[1]], "```")
    # If echo is FALSE in the parent chunk, we pass it down to hide the code
    if (isFALSE(options$echo)) {
      code_chunk[1] <- "```{r, error=TRUE, echo=FALSE}"
    }

    knit_out <- knitr::knit_child(text = code_chunk, envir = envir, quiet = TRUE)

    # Retry if error detected in output
    retry_count <- 0
    while (grepl("## Error", knit_out) && retry_count < max_retries) {
      retry_count <- retry_count + 1

      # Extract error message for the prompt
      error_lines <- grep("## Error", strsplit(knit_out, "\n")[[1]], value = TRUE)
      error_msg <- paste(error_lines, collapse = "\n")

      retry_prompt <- paste0(
        "The previous code failed with this error:\n\n```\n",
        error_msg, "\n```\n\n",
        "Please fix the code and try again."
      )

      response <- tryCatch(
        {
          session$send(retry_prompt)
        },
        error = function(e) {
          break
        }
      )

      new_response <- response$text %||% ""
      new_explanation <- remove_r_code_blocks(new_response)
      if (nzchar(new_explanation)) {
        out_parts <- c(out_parts, "\n\n---\n\n**Retry:**\n\n", new_explanation)
      }

      extracted_code <- extract_r_code(new_response)

      if (nzchar(extracted_code)) {
        extracted_code <- sanitize_r_code(extracted_code)
        code_chunk <- c("```{r, error=TRUE, echo=TRUE}", strsplit(extracted_code, "\n")[[1]], "```")
        if (isFALSE(options$echo)) {
          code_chunk[1] <- "```{r, error=TRUE, echo=FALSE}"
        }
        knit_out <- knitr::knit_child(text = code_chunk, envir = envir, quiet = TRUE)
      }
    }

    if (nzchar(knit_out)) {
      out_parts <- c(out_parts, knit_out)
    }
  } else if (isTRUE(options$echo) && nzchar(extracted_code)) {
    # If not evaluating but echo is TRUE, just show the code
    out_parts <- c(out_parts, paste0("```r\n", extracted_code, "\n```"))
  }

  # --- 7. Return Final String ---
  # We must tell knitr to render this result as 'asis' so it doesn't get double encoded
  # while allowing knitr::engine_output to echo the user's prompt code
  options$results <- "asis"
  knitr::engine_output(options, code = options$code, out = paste(out_parts, collapse = "\n\n"))
}

#' @title Get or Create Session
#' @description
#' Retrieves the current chat session from the cache, or creates a new one.
#' Sessions persist across chunks within a single knit process.
#' @param options Chunk options containing potential `model` specification.
#' @return A ChatSession object.
#' @keywords internal
get_or_create_session <- function(options) {
  model <- options$model %||% getOption("aisdk.default_model", "openai:gpt-4o")
  session_name <- options$session %||% "default"

  # Check if we need to reset (e.g., new document)
  if (isTRUE(options$new_session)) {
    .engine_env[[session_name]] <- NULL
  }

  # Get or create session

  if (is.null(.engine_env[[session_name]])) {
    system_prompt <- options$system %||% get_default_system_prompt()
    .engine_env[[session_name]] <- create_chat_session(
      model = model,
      system_prompt = system_prompt
    )
  } else {
    # Switch model if specified and different
    current_session <- .engine_env[[session_name]]
    if (!is.null(options$model) && options$model != current_session$get_model_id()) {
      tryCatch(
        {
          current_session$switch_model(options$model)
        },
        error = function(e) {
          rlang::warn(paste("Failed to switch model:", e$message))
        }
      )
    }
  }

  .engine_env[[session_name]]
}

#' @title Get Default System Prompt
#' @description Returns the default system prompt for the AI engine.
#' @return A character string.
#' @keywords internal
get_default_system_prompt <- function() {
  paste0(
    "You are an expert R programmer and data analyst. ",
    "When the user asks you to perform an analysis or create a visualization, ",
    "you MUST respond with EXECUTABLE R code wrapped in ```r ... ``` blocks. ",
    "DO NOT just explain what to do - write the actual working code. ",
    "The code will be executed immediately in the user's R environment. ",
    "Always include explicit library() calls at the top of your code for any package you use ",
    "(e.g., library(dplyr), library(ggplot2)). ",
    "Use the base R pipe |> instead of %>% when possible, ",
    "or include library(magrittr) / library(dplyr) before using %>%. ",
    "Keep explanations brief and focus on generating correct, executable code."
  )
}

#' @title Build Context
#' @description
#' Builds context string from R objects in the environment.
#' @param prompt The user's prompt.
#' @param context_spec NULL (auto-detect), FALSE (skip), or character vector of var names.
#' @param envir The environment to look for variables.
#' @return A character string with context information.
#' @keywords internal
build_context <- function(prompt, context_spec, envir) {
  # If explicitly disabled
  if (isFALSE(context_spec)) {
    return("")
  }

  # Determine which variables to include
  if (is.character(context_spec)) {
    vars <- context_spec
  } else {
    # Auto-detect: find tokens in prompt that exist in environment
    vars <- auto_detect_vars(prompt, envir)
  }

  if (length(vars) == 0) {
    return("")
  }

  # Limit context to avoid token explosion
  max_vars <- getOption("aisdk.max_context_vars", 5)
  if (length(vars) > max_vars) {
    vars <- vars[1:max_vars]
    rlang::warn(paste("Context limited to", max_vars, "variables"))
  }

  get_r_context(vars, envir = envir)
}

#' @title Auto-detect Variables
#' @description
#' Detects variable names mentioned in the prompt that exist in the environment.
#' @param prompt The user's prompt.
#' @param envir The environment to check.
#' @return A character vector of variable names.
#' @keywords internal
auto_detect_vars <- function(prompt, envir) {
  # Extract potential variable names (simple identifiers)
  pattern <- "\\b([a-zA-Z][a-zA-Z0-9_.]*|\\.[a-zA-Z][a-zA-Z0-9_.]*)\\b"
  tokens <- unique(unlist(regmatches(prompt, gregexpr(pattern, prompt, perl = TRUE))))

  # Filter to those that exist and are not common keywords
  r_keywords <- c(
    "if", "else", "for", "while", "function", "in", "next", "break",
    "TRUE", "FALSE", "NULL", "NA", "NaN", "Inf", "library", "require",
    "return", "print", "plot", "summary", "head", "str", "the", "a", "an"
  )

  tokens <- setdiff(tokens, r_keywords)

  # Check existence in environment
  existing <- tokens[vapply(tokens, function(v) {
    exists(v, envir = envir, inherits = FALSE)
  }, logical(1))]

  existing
}

#' @title Construct Prompt
#' @description
#' Combines user prompt with context.
#' @param user_prompt The user's original prompt.
#' @param context_str The context string (may be empty).
#' @return The full prompt to send to the LLM.
#' @keywords internal
construct_prompt <- function(user_prompt, context_str) {
  if (nzchar(context_str)) {
    paste0(
      "## Available Data Context\n\n", context_str, "\n\n",
      "## User Request\n\n", user_prompt
    )
  } else {
    user_prompt
  }
}

#' @title Extract R Code
#' @description
#' Extracts R code from markdown code blocks in the LLM response.
#' @param text The LLM response text.
#' @return A character string containing all extracted R code.
#' @keywords internal
extract_r_code <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return("")
  }

  # Match ```r or ```{r} or ```{r ...} code blocks
  # (?s) makes . match newlines
  # Pattern captures content between fences
  pattern <- "(?s)```\\s*(?:\\{r[^}]*\\}|r)\\s*\\n(.*?)\\n?```"

  # Use regmatches to get all matches
  matches <- gregexpr(pattern, text, perl = TRUE, ignore.case = TRUE)
  raw_blocks <- regmatches(text, matches)[[1]]

  if (length(raw_blocks) == 0) {
    return("")
  }

  # Extract just the code content from each block
  code_blocks <- vapply(raw_blocks, function(block) {
    # Remove the opening fence and language identifier
    content <- sub("(?s)^```\\s*(?:\\{r[^}]*\\}|r)\\s*\\n", "", block, perl = TRUE, ignore.case = TRUE)
    # Remove the closing fence
    content <- sub("\\n?```$", "", content, perl = TRUE)
    trimws(content)
  }, character(1), USE.NAMES = FALSE)

  paste(code_blocks, collapse = "\n\n")
}

#' @title Sanitize R Code
#' @description
#' Patches common issues in LLM-generated R code before execution.
#' Currently handles: missing library() calls for %>% and other common operators.
#' @param code The R code string to sanitize.
#' @return The sanitized code string.
#' @keywords internal
sanitize_r_code <- function(code) {
  prepend <- character(0)

  # If %>% is used but not available on the search path, inject a library() call.
  # existsMethod() / exists() searches the current search path from globalenv.
  pipe_available <- tryCatch(
    is.function(get("%>%", envir = globalenv(), inherits = TRUE)),
    error = function(e) FALSE
  )
  if (grepl("%>%", code, fixed = TRUE) &&
    !grepl("library\\s*\\(\\s*(magrittr|dplyr|tidyverse)", code, perl = TRUE) &&
    !pipe_available) {
    prepend <- c(prepend, "library(magrittr)")
  }

  if (length(prepend) > 0) {
    code <- paste(c(prepend, code), collapse = "\n")
  }
  code
}


#' @description
#' Removes R code blocks from text to get just the explanation.
#' @param text The text to process.
#' @return Text without R code blocks.
#' @keywords internal
remove_r_code_blocks <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return("")
  }
  # (?s) makes . match newlines
  pattern <- "(?s)```\\s*(?:\\{r[^}]*\\}|r)\\s*\\n.*?\\n?```"
  result <- gsub(pattern, "", text, perl = TRUE, ignore.case = TRUE)
  # Clean up extra whitespace
  gsub("\\n{3,}", "\n\n", trimws(result))
}


#' @title Clear AI Engine Session
#' @description
#' Clears the cached session(s) for the AI engine.
#' Useful for resetting state between documents.
#' @param session_name Optional name of specific session to clear. If NULL, clears all.
#' @return Invisible NULL.
#' @export
clear_ai_session <- function(session_name = NULL) {
  if (is.null(session_name)) {
    rm(list = ls(envir = .engine_env), envir = .engine_env)
  } else if (exists(session_name, envir = .engine_env)) {
    rm(list = session_name, envir = .engine_env)
  }
  invisible(NULL)
}

#' @title Get AI Engine Session
#' @description
#' Gets the current AI engine session for inspection or manual interaction.
#' @param session_name Name of the session. Default is "default".
#' @return A ChatSession object or NULL if not initialized.
#' @export
get_ai_session <- function(session_name = "default") {
  .engine_env[[session_name]]
}
