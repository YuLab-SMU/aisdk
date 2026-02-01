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
#' \dontrun{
#' library(aisdk)
#' register_ai_engine()
#' # Now you can use ```{ai} chunks in your RMarkdown
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
  response <- tryCatch({
    session$send(full_prompt)
  }, error = function(e) {
    return(list(text = paste0("**Error calling LLM:** ", e$message)))
  })
  
  response_text <- response$text %||% ""
  
  # --- 6. Extract and Execute Code ---
  extracted_code <- extract_r_code(response_text)
  execution_result <- ""
  
  if (isTRUE(options$eval) && nzchar(extracted_code)) {
    execution_result <- execute_code_safely(extracted_code, envir)
  }
  
  # --- 7. Format Output ---
  format_engine_output(options, user_prompt, response_text, extracted_code, execution_result)
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
      tryCatch({
        current_session$switch_model(options$model)
      }, error = function(e) {
        rlang::warn(paste("Failed to switch model:", e$message))
      })
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
    "respond with executable R code wrapped in ```r ... ``` blocks. ",
    "Provide brief explanations outside the code blocks. ",
    "The code will be executed in the user's R environment."
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
  r_keywords <- c("if", "else", "for", "while", "function", "in", "next", "break",
                  "TRUE", "FALSE", "NULL", "NA", "NaN", "Inf", "library", "require",
                  "return", "print", "plot", "summary", "head", "str", "the", "a", "an")
  
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

#' @title Remove R Code Blocks
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

#' @title Execute Code Safely
#' @description
#' Executes R code in the given environment and captures output.
#' @param code The R code to execute.
#' @param envir The environment in which to execute.
#' @return A character string with the output or error message.
#' @keywords internal
execute_code_safely <- function(code, envir) {
  if (!nzchar(trimws(code))) {
    return("")
  }
  
  # Use evaluate package if available for better output capture
  if (requireNamespace("evaluate", quietly = TRUE)) {
    result <- tryCatch({
      evaluated <- evaluate::evaluate(code, envir = envir, new_device = FALSE)
      
      # Format the evaluation results
      output_parts <- lapply(evaluated, function(x) {
        if (inherits(x, "source")) {
          NULL  # Skip source echo, we handle it separately
        } else if (inherits(x, "character")) {
          x
        } else if (inherits(x, "message")) {
          paste0("Message: ", conditionMessage(x))
        } else if (inherits(x, "warning")) {
          paste0("Warning: ", conditionMessage(x))
        } else if (inherits(x, "error")) {
          paste0("Error: ", conditionMessage(x))
        } else if (inherits(x, "recordedplot")) {
          # Plots are handled by knitr's graphics device
          "[Plot generated]"
        } else {
          NULL
        }
      })
      
      paste(unlist(output_parts[!vapply(output_parts, is.null, logical(1))]), collapse = "\n")
    }, error = function(e) {
      paste0("Error executing code: ", e$message)
    })
    
    return(result)
  }
  
  # Fallback: simple eval with output capture
  tryCatch({
    output <- capture.output({
      result <- eval(parse(text = code), envir = envir)
      if (!is.null(result) && !inherits(result, "gg")) {
        print(result)
      }
    })
    paste(output, collapse = "\n")
  }, error = function(e) {
    paste0("Error: ", e$message)
  }, warning = function(w) {
    paste0("Warning: ", w$message)
  })
}

#' @title Format Engine Output
#' @description
#' Formats the final output for the knitr engine.
#' @param options Chunk options.
#' @param user_prompt The original user prompt.
#' @param response_text The full LLM response.
#' @param code The extracted R code.
#' @param execution_result The result of code execution.
#' @return Formatted output string.
#' @keywords internal
format_engine_output <- function(options, user_prompt, response_text, code, execution_result) {
  parts <- character(0)
  
  # Get explanation text (response without code blocks)
  explanation <- remove_r_code_blocks(response_text)
  
  # Add explanation if present
  if (nzchar(explanation)) {
    parts <- c(parts, explanation)
  }
  
  # Add code block if echo is TRUE and we have code
  if (isTRUE(options$echo) && nzchar(code)) {
    # Format as a proper R code block for the output document
    code_block <- paste0("```r\n", code, "\n```")
    parts <- c(parts, code_block)
  }
  
  # Add execution result if we have any output
  if (nzchar(execution_result)) {
    # Format as output block
    result_block <- paste0("```\n", execution_result, "\n```")
    parts <- c(parts, result_block)
  }
  
  output <- paste(parts, collapse = "\n\n")
  
  # Use knitr's engine_output for proper formatting
  knitr::engine_output(options, code = options$code, out = output)
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
