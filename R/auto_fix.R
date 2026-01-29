#' @title Autonomous Data Science Pipelines
#' @description
#' Self-healing runtime for R code execution. Implements a "Hypothesis-Fix-Verify"
#' loop that feeds error messages, stack traces, and context back to an LLM
#' for automatic error correction.
#' @name auto_fix
NULL

#' @title Auto-Fix Wrapper
#' @description
#' Execute R code with automatic error recovery using LLM assistance.
#' When code fails, the error is analyzed and a fix is attempted automatically.
#' @param expr The R expression to execute.
#' @param model The LLM model to use for error analysis (default: from options).
#' @param max_attempts Maximum number of fix attempts (default: 3).
#' @param context Optional additional context about the code's purpose.
#' @param verbose Print progress messages (default: TRUE).
#' @param memory Optional ProjectMemory object for learning from past fixes.
#' @return The result of successful execution, or an error if all attempts fail.
#' @export
#' @examples
#' \dontrun{
#' # Simple usage - auto-fix a data transformation
#' result <- auto_fix({
#'   df <- read.csv("data.csv")
#'   df %>% filter(value > 100) %>% summarize(mean = mean(value))
#' })
#'
#' # With context for better error understanding
#' result <- auto_fix(
#'   expr = { model <- lm(y ~ x, data = df) },
#'   context = "Fitting a linear regression model to predict sales"
#' )
#' }
auto_fix <- function(expr,
                     model = getOption("aisdk.default_model", "openai:gpt-4o"),
                     max_attempts = 3,
                     context = NULL,
                     verbose = TRUE,
                     memory = NULL) {
  # Capture the expression as text for LLM analysis
  expr_text <- deparse(substitute(expr))
  if (length(expr_text) > 1) {
    expr_text <- paste(expr_text, collapse = "\n")
  }

  # Create execution environment
  exec_env <- new.env(parent = parent.frame())

  # Track attempts
  attempts <- list()
  current_code <- expr_text

  for (attempt in seq_len(max_attempts)) {
    if (verbose) {
      message(sprintf("[auto_fix] Attempt %d/%d", attempt, max_attempts))
    }

    # Try to execute the current code
    result <- tryCatch({
      # Parse and evaluate the current code
      parsed <- parse(text = current_code)
      eval(parsed, envir = exec_env)
    }, error = function(e) {
      structure(
        list(
          error = conditionMessage(e),
          call = deparse(conditionCall(e)),
          traceback = capture_traceback()
        ),
        class = "auto_fix_error"
      )
    }, warning = function(w) {
      # Capture warnings but continue execution
      invokeRestart("muffleWarning")
    })

    # Check if execution succeeded
    if (!inherits(result, "auto_fix_error")) {
      if (verbose) {
        message("[auto_fix] Success!")
      }

      # Store successful fix in memory if available
      if (!is.null(memory) && attempt > 1) {
        memory$store_fix(
          original_code = expr_text,
          error = attempts[[length(attempts)]]$error,
          fixed_code = current_code
        )
      }

      return(result)
    }

    # Record this attempt
    attempts[[attempt]] <- list(
      code = current_code,
      error = result$error,
      call = result$call,
      traceback = result$traceback
    )

    # If this was the last attempt, give up

if (attempt == max_attempts) {
      if (verbose) {
        message("[auto_fix] All attempts exhausted. Returning error.")
      }
      rlang::abort(
        message = paste0("auto_fix failed after ", max_attempts, " attempts. Last error: ", result$error),
        class = "auto_fix_exhausted",
        attempts = attempts
      )
    }

    # Query memory for similar past fixes
    memory_hint <- NULL
    if (!is.null(memory)) {
      similar <- memory$find_similar_fix(result$error)
      if (!is.null(similar)) {
        memory_hint <- paste0(
          "\n\nI found a similar error in my memory:\n",
          "Original error: ", similar$error, "\n",
          "The fix was: ", similar$fix_description
        )
      }
    }

    # Ask LLM to fix the error
    if (verbose) {
      message("[auto_fix] Analyzing error with LLM...")
    }

    fix_prompt <- build_fix_prompt(
      code = current_code,
      error = result$error,
      call = result$call,
      traceback = result$traceback,
      context = context,
      memory_hint = memory_hint,
      attempt = attempt
    )

    # Call the LLM
    llm_response <- tryCatch({
      generate_text(
        model = model,
        prompt = fix_prompt,
        system = AUTO_FIX_SYSTEM_PROMPT
      )
    }, error = function(e) {
      if (verbose) {
        message("[auto_fix] LLM call failed: ", conditionMessage(e))
      }
      NULL
    })

    if (is.null(llm_response)) {
      next
    }

    # Extract the fixed code from LLM response
    fixed_code <- extract_code_from_response(llm_response$text)

    if (is.null(fixed_code) || nchar(trimws(fixed_code)) == 0) {
      if (verbose) {
        message("[auto_fix] Could not extract fixed code from LLM response")
      }
      next
    }

    # Update current code for next attempt
    current_code <- fixed_code

    if (verbose) {
      message("[auto_fix] Trying fixed code...")
    }
  }
}

#' @title System Prompt for Auto-Fix
#' @keywords internal
AUTO_FIX_SYSTEM_PROMPT <- "You are an expert R programmer helping to fix code errors.

Your task is to analyze the error and provide a corrected version of the code.

Guidelines:
1. Analyze the error message and stack trace carefully
2. Identify the root cause of the error
3. Provide a minimal fix that addresses the issue
4. Do NOT add unnecessary changes or improvements
5. Preserve the original intent of the code
6. Return ONLY the fixed code wrapped in ```r ... ``` code blocks
7. If you need to add a package, use library() at the start

Common R errors and fixes:
- 'object not found': Check variable names, ensure data is loaded
- 'could not find function': Load required package with library()
- 'subscript out of bounds': Check array/list indices
- 'non-numeric argument': Ensure numeric operations use numeric data
- 'cannot open connection': Check file paths exist
- 'replacement has X rows': Ensure vector lengths match"

#' @title Build Fix Prompt
#' @keywords internal
build_fix_prompt <- function(code, error, call, traceback, context, memory_hint, attempt) {
  parts <- c(
    "The following R code produced an error:\n",
    "```r",
    code,
    "```\n",
    "Error message:",
    error,
    "\nError occurred in:",
    call
  )

  if (!is.null(traceback) && length(traceback) > 0) {
    parts <- c(parts, "\nStack trace:", paste(traceback, collapse = "\n"))
  }

  if (!is.null(context)) {
    parts <- c(parts, "\nContext about this code:", context)
  }

  if (!is.null(memory_hint)) {
    parts <- c(parts, memory_hint)
  }

  if (attempt > 1) {
    parts <- c(parts, sprintf("\nThis is attempt %d. Previous fixes did not work.", attempt))
  }

  parts <- c(parts, "\nPlease provide the corrected R code.")

  paste(parts, collapse = "\n")
}

#' @title Extract Code from LLM Response
#' @keywords internal
extract_code_from_response <- function(response) {
  # Try to extract code from markdown code blocks
  pattern <- "```(?:r|R)?\\s*\\n([\\s\\S]*?)\\n```"
  matches <- regmatches(response, regexec(pattern, response, perl = TRUE))

  if (length(matches[[1]]) >= 2) {
    return(trimws(matches[[1]][2]))
  }

  # If no code block found, try to use the whole response if it looks like code
  lines <- strsplit(response, "\n")[[1]]
  code_lines <- lines[!grepl("^(Here|The|I |This|Note|Please)", lines)]

  if (length(code_lines) > 0) {
    return(paste(code_lines, collapse = "\n"))
  }

  NULL
}

#' @title Capture Traceback
#' @keywords internal
capture_traceback <- function() {
  tb <- sys.calls()
  if (length(tb) > 0) {
    # Filter out internal calls
    tb_text <- sapply(tb, function(x) paste(deparse(x), collapse = " "))
    tb_text <- tb_text[!grepl("^(tryCatch|doTryCatch|auto_fix)", tb_text)]
    head(tb_text, 10)
  } else {
    character(0)
  }
}

#' @title Safe Eval with Timeout
#' @description
#' Execute R code with a timeout to prevent infinite loops.
#' @param expr Expression to evaluate.
#' @param timeout_seconds Maximum execution time in seconds.
#' @param envir Environment for evaluation.
#' @return The result or an error.
#' @export
safe_eval <- function(expr, timeout_seconds = 30, envir = parent.frame()) {
  expr_text <- deparse(substitute(expr))

  # Use callr for isolated execution with timeout
  tryCatch({
    callr::r(
      function(code, env_data) {
        env <- list2env(env_data, parent = globalenv())
        eval(parse(text = code), envir = env)
      },
      args = list(
        code = paste(expr_text, collapse = "\n"),
        env_data = as.list(envir)
      ),
      timeout = timeout_seconds
    )
  }, error = function(e) {
    if (grepl("timeout", conditionMessage(e), ignore.case = TRUE)) {
      rlang::abort(
        "Execution timed out",
        class = "safe_eval_timeout"
      )
    }
    rlang::abort(conditionMessage(e))
  })
}

#' @title Hypothesis-Fix-Verify Loop
#' @description
#' Advanced self-healing execution that generates hypotheses about errors,
#' attempts fixes, and verifies the results.
#' @param code Character string of R code to execute.
#' @param model LLM model for analysis.
#' @param test_fn Optional function to verify the result is correct.
#' @param max_iterations Maximum fix iterations.
#' @param verbose Print progress.
#' @return List with result, fix history, and verification status.
#' @export
hypothesis_fix_verify <- function(code,
                                  model = getOption("aisdk.default_model"),
                                  test_fn = NULL,
                                  max_iterations = 5,
                                  verbose = TRUE) {
  history <- list()
  current_code <- code

  for (i in seq_len(max_iterations)) {
    if (verbose) message(sprintf("[HFV] Iteration %d", i))

    # Execute
    exec_result <- tryCatch({
      eval(parse(text = current_code), envir = new.env(parent = globalenv()))
    }, error = function(e) {
      structure(list(error = conditionMessage(e)), class = "hfv_error")
    })

    # Check for execution error
    if (inherits(exec_result, "hfv_error")) {
      if (verbose) message("[HFV] Execution error: ", exec_result$error)

      # Generate hypothesis
      hypothesis <- generate_hypothesis(current_code, exec_result$error, model)

      # Generate fix based on hypothesis
      fix <- generate_fix(current_code, hypothesis, model)

      history[[i]] <- list(
        code = current_code,
        error = exec_result$error,
        hypothesis = hypothesis,
        fix = fix$description
      )

      current_code <- fix$code
      next
    }

    # Verify result if test function provided
    if (!is.null(test_fn)) {
      verification <- tryCatch({
        test_fn(exec_result)
      }, error = function(e) {
        FALSE
      })

      if (!verification) {
        if (verbose) message("[HFV] Verification failed")

        # Generate hypothesis about why verification failed
        hypothesis <- generate_verification_hypothesis(
          current_code, exec_result, test_fn, model
        )

        fix <- generate_fix(current_code, hypothesis, model)

        history[[i]] <- list(
          code = current_code,
          result = exec_result,
          verification = "failed",
          hypothesis = hypothesis,
          fix = fix$description
        )

        current_code <- fix$code
        next
      }
    }

    # Success!
    if (verbose) message("[HFV] Success!")

    return(list(
      success = TRUE,
      result = exec_result,
      final_code = current_code,
      iterations = i,
      history = history
    ))
  }

  # Exhausted iterations
  list(
    success = FALSE,
    result = NULL,
    final_code = current_code,
    iterations = max_iterations,
    history = history
  )
}

#' @title Generate Hypothesis
#' @keywords internal
generate_hypothesis <- function(code, error, model) {
  prompt <- paste0(
    "Analyze this R code error and provide a brief hypothesis (1-2 sentences) about the root cause:\n\n",
    "Code:\n```r\n", code, "\n```\n\n",
    "Error: ", error, "\n\n",
    "Hypothesis:"
  )

  result <- generate_text(
    model = model,
    prompt = prompt,
    system = "You are an R debugging expert. Provide concise, technical hypotheses."
  )

  trimws(result$text)
}

#' @title Generate Fix
#' @keywords internal
generate_fix <- function(code, hypothesis, model) {
  prompt <- paste0(
    "Based on this hypothesis, fix the R code:\n\n",
    "Hypothesis: ", hypothesis, "\n\n",
    "Original code:\n```r\n", code, "\n```\n\n",
    "Provide the fixed code in a ```r code block."
  )

  result <- generate_text(
    model = model,
    prompt = prompt,
    system = "You are an R expert. Provide minimal, targeted fixes."
  )

  fixed_code <- extract_code_from_response(result$text)

  list(
    code = fixed_code %||% code,
    description = hypothesis
  )
}

#' @title Generate Verification Hypothesis
#' @keywords internal
generate_verification_hypothesis <- function(code, result, test_fn, model) {
  result_str <- tryCatch(
    paste(capture.output(print(result)), collapse = "\n"),
    error = function(e) "<unable to print result>"
  )

  prompt <- paste0(
    "This R code executed but the result failed verification.\n\n",
    "Code:\n```r\n", code, "\n```\n\n",
    "Result:\n", substr(result_str, 1, 500), "\n\n",
    "Why might the result be incorrect? Provide a brief hypothesis."
  )

  result <- generate_text(
    model = model,
    prompt = prompt,
    system = "You are an R debugging expert."
  )

  trimws(result$text)
}

# Null-coalescing operator
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
