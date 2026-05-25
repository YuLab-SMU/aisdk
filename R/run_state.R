#' Run State Helpers
#'
#' Internal helpers for normalizing generation stop states.
#'
#' @name run_state
NULL

aisdk_run_state_statuses <- c(
  "running",
  "continuing",
  "finalizing",
  "completed",
  "waiting_user",
  "blocked",
  "aborted_safety",
  "cancelled",
  "error"
)

#' @keywords internal
new_run_state <- function(status = "running",
                          stop_reason = NULL,
                          recoverable = FALSE,
                          failure_summary = NULL,
                          pending_action = NULL,
                          last_tool_results = list(),
                          run_id = NULL,
                          details = list()) {
  if (!status %in% aisdk_run_state_statuses) {
    rlang::abort(sprintf("Unknown run state status: %s", status))
  }

  new_task_state(
    status = status,
    phase = stop_reason %||% status,
    run_id = run_id,
    blocker = failure_summary,
    last_tool_results = last_tool_results,
    decision = if (!is.null(pending_action)) {
      decision <- switch(
        pending_action,
        retry = "continue",
        ask_user = "ask_user",
        manual = "ask_user",
        continue = "continue",
        "continue"
      )
      new_agent_decision(decision = decision, reason = stop_reason %||% status)
    } else {
      NULL
    },
    details = utils::modifyList(
      details %||% list(),
      list(
        stop_reason = stop_reason %||% status,
        recoverable = isTRUE(recoverable),
        failure_summary = failure_summary,
        pending_action = pending_action
      ),
      keep.null = TRUE
    )
  )
}

#' @keywords internal
run_state_tool_results_tail <- function(tool_results, n = 5L) {
  tool_results <- tool_results %||% list()
  if (!length(tool_results)) {
    return(list())
  }
  indexes <- seq.int(max(1L, length(tool_results) - as.integer(n) + 1L), length(tool_results))
  lapply(tool_results[indexes], function(tr) {
    result <- tr$result %||% tr$raw_result %||% NULL
    result_text <- tryCatch(
      {
        if (is.null(result)) {
          ""
        } else if (is.character(result)) {
          paste(result, collapse = "\n")
        } else {
          safe_to_json(result, auto_unbox = TRUE)
        }
      },
      error = function(e) as.character(result)[[1]] %||% ""
    )
    list(
      id = tr$id %||% NULL,
      name = tr$name %||% "unknown",
      is_error = isTRUE(tr$is_error) || tool_result_indicates_error(tr$result %||% NULL, tr$raw_result %||% tr$result %||% NULL),
      result = compact_text_preview(result_text, width = 500)
    )
  })
}

#' @keywords internal
run_state_failure_summary <- function(result = NULL, error = NULL) {
  if (!is.null(error)) {
    return(conditionMessage(error))
  }
  tool_results <- result$all_tool_results %||% list()
  if (length(tool_results) > 0) {
    for (tr in rev(tool_results)) {
      if (isTRUE(tr$is_error) || tool_result_indicates_error(tr$result %||% NULL, tr$raw_result %||% tr$result %||% NULL)) {
        return(as.character(tr$result %||% tr$raw_result %||% "Tool failed")[[1]])
      }
    }
  }
  result$text %||% NULL
}

#' @keywords internal
is_network_error_condition <- function(e) {
  msg <- conditionMessage(e)
  any(vapply(c(
    "cannot open the connection",
    "Failed to perform HTTP request",
    "timeout",
    "operation timed out",
    "Connection reset",
    "host unreachable",
    "Could not resolve host",
    "Couldn't connect",
    "SSL connect error"
  ), function(p) grepl(p, msg, ignore.case = TRUE), logical(1)))
}

#' @keywords internal
run_state_from_result <- function(result = NULL,
                                  step = NULL,
                                  max_steps = NULL,
                                  all_tool_results = list(),
                                  run_id = NULL,
                                  default_status = "completed") {
  if (is.null(result)) {
    return(new_run_state(
      status = "error",
      stop_reason = "no_result",
      recoverable = FALSE,
      failure_summary = "Generation stopped before returning a result.",
      last_tool_results = run_state_tool_results_tail(all_tool_results),
      run_id = run_id
    ))
  }

  if (!is.null(result$task_state)) {
    return(result$task_state)
  }

  finish_reason <- result$finish_reason %||% ""
  status <- switch(
    finish_reason,
    waiting_user = "waiting_user",
    blocked = "blocked",
    aborted_safety = "aborted_safety",
    error = "error",
    default_status
  )
  new_run_state(
    status = status,
    stop_reason = finish_reason %||% default_status,
    recoverable = identical(status, "blocked"),
    failure_summary = if (status %in% c("blocked", "error")) run_state_failure_summary(result) else NULL,
    last_tool_results = run_state_tool_results_tail(all_tool_results),
    run_id = run_id
  )
}

#' @keywords internal
attach_run_state <- function(result, run_state) {
  if (is.null(result)) {
    result <- list()
  }
  result$run_state <- run_state
  result
}

#' @keywords internal
blocked_network_result <- function(e, run_id = NULL) {
  state <- new_run_state(
    status = "blocked",
    stop_reason = "network_error",
    recoverable = TRUE,
    failure_summary = conditionMessage(e),
    pending_action = "retry",
    run_id = run_id
  )
  attach_run_state(
    GenerateResult$new(
      text = "",
      finish_reason = "blocked",
      warnings = conditionMessage(e)
    ),
    state
  )
}

#' @keywords internal
normalize_continue_action <- function(action) {
  action <- tolower(trimws(action %||% "continue"))
  aliases <- c(
    c = "continue",
    retry = "continue",
    resume = "continue",
    giveup = "give_up",
    "give-up" = "give_up",
    stop = "give_up",
    avoid = "avoid_tool",
    avoidtool = "avoid_tool",
    "avoid-tool" = "avoid_tool",
    manual_fix = "manual"
  )
  if (action %in% names(aliases)) {
    action <- aliases[[action]]
  }
  if (!action %in% c("continue", "give_up", "avoid_tool", "explain", "manual")) {
    rlang::abort("`action` must be one of continue, give_up, avoid_tool, explain, or manual.")
  }
  action
}

#' @keywords internal
run_state_continuation_prompt <- function(action = "continue",
                                          guidance = NULL,
                                          run_state = NULL) {
  action <- normalize_continue_action(action)
  state_summary <- if (!is.null(run_state)) {
    decision <- run_state$decision %||% list()
    paste(c(
      paste0("Previous run status: ", run_state$status %||% "unknown"),
      paste0("Phase: ", run_state$phase %||% run_state$details$stop_reason %||% "unknown"),
      if (nzchar(decision$reason %||% "")) paste0("Last decision reason: ", decision$reason) else NULL,
      if (nzchar(run_state$blocker %||% "")) paste0("Blocker: ", run_state$blocker) else NULL
    ), collapse = "\n")
  } else {
    "Previous run status: unknown"
  }

  action_text <- switch(
    action,
    continue = paste(
      "Continue the interrupted or recoverable run.",
      "Either call the next appropriate tool immediately, provide a clear final answer, or ask the user for a specific missing input.",
      "Do not repeat the exact same failing tool call unless the arguments or approach have changed."
    ),
    give_up = paste(
      "Stop trying to execute further tools for this run.",
      "Give a concise final explanation of what failed, what was completed, and what the user can do next."
    ),
    avoid_tool = paste(
      "Continue without using the tool that just failed.",
      "Use a different diagnostic or implementation path, or give a final explanation if no alternative is available."
    ),
    explain = paste(
      "Explain why the previous run stopped.",
      "Do not call the failing tool again in this response unless the user explicitly asked for another attempt."
    ),
    manual = paste(
      "The user chose to intervene manually.",
      "Wait for the user's next instruction instead of taking another model action."
    )
  )

  paste(c(
    "[continue_run_begin]",
    paste0("Action: ", action),
    state_summary,
    if (nzchar(guidance %||% "")) paste0("User/operator guidance: ", guidance) else NULL,
    action_text,
    "[continue_run_end]"
  ), collapse = "\n")
}
