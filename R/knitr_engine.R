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

  envir <- options$envir %||% knitr::knit_global()
  user_prompt <- paste(options$code, collapse = "\n")

  # Skip empty chunks
  if (!nzchar(trimws(user_prompt))) {
    return("")
  }

  session <- get_or_create_session(options)
  context_str <- build_context(user_prompt, options$context, envir)
  full_prompt <- construct_prompt(user_prompt, context_str)
  response <- tryCatch(
    {
      session$send(full_prompt)
    },
    error = function(e) {
      return(list(text = paste0("**Error calling LLM:** ", e$message)))
    }
  )

  response_text <- response$text %||% ""
  extracted_code <- extract_r_code(response_text)
  if (nzchar(extracted_code)) {
    extracted_code <- sanitize_r_code(extracted_code)
  }

  out <- response_text
  if (isTRUE(options$eval) && nzchar(extracted_code)) {
    execution_output <- evaluate_ai_review_code(extracted_code, options, envir)
    narrative <- trimws(remove_r_code_blocks(response_text))
    parts <- Filter(nzchar, c(narrative, execution_output))
    out <- if (length(parts) > 0) paste(parts, collapse = "\n\n") else execution_output
  }

  options$results <- "asis"
  knitr::engine_output(options, code = character(), out = out)
}

#' @keywords internal
resolve_ai_review_input_path <- function(input_file = NULL) {
  if (is.null(input_file) || !nzchar(input_file)) {
    return("unknown.Rmd")
  }

  normalized <- normalizePath(input_file, winslash = "/", mustWork = FALSE)

  # Quarto hands knitr an intermediate `.rmarkdown` path for `.qmd` sources.
  # When a sibling `.qmd` exists, persist review records against the source file
  # so `get_pending_reviews("...qmd")` works predictably.
  if (grepl("\\.rmarkdown$", input_file, ignore.case = TRUE)) {
    qmd_candidate <- sub("\\.rmarkdown$", ".qmd", input_file, ignore.case = TRUE)
    if (file.exists(qmd_candidate)) {
      return(normalizePath(qmd_candidate, winslash = "/", mustWork = FALSE))
    }
  }

  normalized
}

#' @keywords internal
derive_ai_review_card_state <- function(review_status = NULL, execution_status = NULL) {
  if (identical(review_status, "approved")) {
    return("frozen")
  }

  if (identical(review_status, "rejected")) {
    return("rejected")
  }

  if (identical(execution_status, "error")) {
    return("error")
  }

  if (identical(execution_status, "completed")) {
    return("ran")
  }

  "draft"
}

#' @keywords internal
build_ai_review_artifact_record <- function(state,
                                            prompt,
                                            response_text,
                                            draft_response,
                                            final_code,
                                            execution_status,
                                            execution_output = "",
                                            error_message = NULL,
                                            transcript = list(),
                                            retries = list(),
                                            model_id = NULL,
                                            session_id = NULL,
                                            review_mode = "none",
                                            runtime_mode = "static",
                                            defer_eval = FALSE,
                                            embed_session = "none",
                                            runtime_manifest = NULL) {
  list(
    state = state,
    prompt = prompt,
    response_text = response_text,
    draft_response = draft_response,
    final_code = final_code,
    execution = list(
      status = execution_status,
      output = execution_output,
      error = error_message
    ),
    transcript = transcript %||% list(),
    retries = retries %||% list(),
    model = model_id,
    session_id = session_id,
    review_mode = review_mode,
    runtime_mode = runtime_mode,
    defer_eval = defer_eval,
    embed_session = embed_session,
    runtime_manifest = runtime_manifest
  )
}

#' @keywords internal
persist_ai_review_runtime_record <- function(memory, review, artifact = NULL) {
  memory$store_review(
    chunk_id = review$chunk_id,
    file_path = review$file_path,
    chunk_label = review$chunk_label,
    prompt = review$prompt,
    response = review$response,
    status = review$status %||% "pending",
    ai_agent = review$ai_agent %||% NULL,
    uncertainty = review$uncertainty %||% NULL,
    session_id = review$session_id %||% NULL,
    review_mode = review$review_mode %||% NULL,
    runtime_mode = review$runtime_mode %||% NULL,
    execution_status = review$execution_status %||% NULL,
    execution_output = review$execution_output %||% NULL,
    final_code = review$final_code %||% NULL,
    error_message = review$error_message %||% NULL
  )

  if (!is.null(artifact)) {
    memory$store_review_artifact(
      chunk_id = review$chunk_id,
      artifact = artifact,
      session_id = review$session_id %||% NULL,
      review_mode = review$review_mode %||% NULL,
      runtime_mode = review$runtime_mode %||% NULL
    )
  }

  memory$update_execution_result(
    chunk_id = review$chunk_id,
    execution_status = review$execution_status %||% NULL,
    execution_output = review$execution_output %||% NULL,
    final_code = review$final_code %||% NULL,
    error_message = review$error_message %||% NULL
  )

  invisible(TRUE)
}

#' @keywords internal
ai_review_capture_transcript <- function(session) {
  if (is.null(session) || !is.function(session$get_history)) {
    return(list())
  }

  history <- tryCatch(session$get_history(), error = function(e) list())
  if (!is.list(history)) {
    return(list())
  }

  lapply(history, function(entry) {
    if (!is.list(entry)) {
      return(list(content = as.character(entry)))
    }

    role <- entry$role %||% entry$author %||% NULL
    content <- entry$content %||% entry$text %||% entry$message %||% NULL
    reasoning <- entry$reasoning %||% NULL
    list(
      role = if (!is.null(role)) as.character(role) else NULL,
      content = if (!is.null(content)) as.character(content) else NULL,
      reasoning = if (!is.null(reasoning)) as.character(reasoning) else NULL
    )
  })
}

#' @keywords internal
execute_ai_review_code <- function(initial_code, initial_response, draft_response,
                                   session, options, envir,
                                   existing_transcript = list()) {
  current_code <- initial_code
  current_response <- initial_response
  current_draft <- draft_response
  retries <- list()
  execution_output <- ""
  error_message <- NULL
  max_retries <- options$max_retries %||% 2L
  attempt <- 0L

  repeat {
    execution_output <- evaluate_ai_review_code(current_code, options, envir)
    error_message <- extract_ai_review_execution_error(execution_output)

    if (is.null(error_message)) {
      transcript <- ai_review_capture_transcript(session)
      if (length(transcript) == 0) {
        transcript <- existing_transcript
      }
      return(list(
        final_code = current_code,
        response_text = current_response,
        draft_response = current_draft,
        execution_output = execution_output,
        execution_status = "completed",
        error_message = NULL,
        retries = retries,
        transcript = transcript
      ))
    }

    if (attempt >= max_retries || is.null(session)) {
      transcript <- ai_review_capture_transcript(session)
      if (length(transcript) == 0) {
        transcript <- existing_transcript
      }
      return(list(
        final_code = current_code,
        response_text = current_response,
        draft_response = current_draft,
        execution_output = execution_output,
        execution_status = "error",
        error_message = error_message,
        retries = retries,
        transcript = transcript
      ))
    }

    attempt <- attempt + 1L
    retries[[length(retries) + 1L]] <- list(
      attempt = attempt,
      code = current_code,
      output = execution_output,
      error = error_message
    )

    retry_prompt <- paste0(
      "The previous code failed with this error:\n\n```\n",
      error_message,
      "\n```\n\nPlease fix the code and try again."
    )

    retry_response <- tryCatch(
      session$send(retry_prompt),
      error = function(e) list(text = paste0("**Error calling LLM:** ", e$message))
    )
    current_response <- retry_response$text %||% ""
    retry_explanation <- remove_r_code_blocks(current_response)
    if (nzchar(retry_explanation)) {
      current_draft <- paste(
        c(current_draft, "\n\n---\n\nRetry:\n\n", retry_explanation),
        collapse = ""
      )
    }

    current_code <- extract_r_code(current_response)
    if (!nzchar(current_code)) {
      transcript <- ai_review_capture_transcript(session)
      if (length(transcript) == 0) {
        transcript <- existing_transcript
      }
      return(list(
        final_code = "",
        response_text = current_response,
        draft_response = current_draft,
        execution_output = execution_output,
        execution_status = "no_code",
        error_message = "Retry response did not include executable R code.",
        retries = retries,
        transcript = transcript
      ))
    }

    current_code <- sanitize_r_code(current_code)
  }
}

#' @keywords internal
evaluate_ai_review_code <- function(code, options, envir) {
  code_chunk <- c("```{r, error=TRUE, echo=TRUE}", strsplit(code, "\n", fixed = TRUE)[[1]], "```")
  if (isFALSE(options$echo)) {
    code_chunk[1] <- "```{r, error=TRUE, echo=FALSE}"
  }

  knitr::knit_child(text = code_chunk, envir = envir, quiet = TRUE)
}

#' @keywords internal
extract_ai_review_execution_error <- function(output) {
  if (is.null(output) || !nzchar(output)) {
    return(NULL)
  }

  lines <- strsplit(output, "\n", fixed = TRUE)[[1]]
  error_lines <- grep("(^## Error)|(^Error)", lines, value = TRUE)

  if (length(error_lines) == 0) {
    return(NULL)
  }

  paste(error_lines, collapse = "\n")
}

#' @title Get or Create Session
#' @description
#' Retrieves the current chat session from the cache, or creates a new one.
#' Sessions persist across chunks within a single knit process.
#' @param options Chunk options containing potential `model` specification.
#' @return A ChatSession object.
#' @keywords internal
get_or_create_session <- function(options) {
  model <- options$model %||% get_model()
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


#' @title Remove R Code Blocks
#' @description
#' Removes fenced R code blocks from text, leaving the explanatory prose.
#' @param text The text to process.
#' @return A character string with R code blocks removed.
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
