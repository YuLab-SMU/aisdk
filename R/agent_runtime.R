# Agent Runtime
#
# Internal task-state driven runtime for tool-using text generation.

aisdk_task_state_statuses <- c(
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

aisdk_agent_decisions <- c(
  "continue",
  "finalize",
  "ask_user",
  "blocked",
  "abort_for_safety"
)

#' @keywords internal
new_task_state <- function(goal = NULL,
                           status = "running",
                           phase = "running",
                           observations = list(),
                           artifacts = list(),
                           failures = list(),
                           open_questions = character(0),
                           risk_gate = list(pending = FALSE, reason = NULL),
                           can_finalize = FALSE,
                           decision = NULL,
                           blocker = NULL,
                           last_tool_results = list(),
                           budget = list(),
                           run_id = NULL,
                           details = list()) {
  if (!status %in% aisdk_task_state_statuses) {
    rlang::abort(sprintf("Unknown task state status: %s", status))
  }

  structure(
    list(
      run_id = run_id %||% paste0("run_", generate_stable_id("task", Sys.time(), stats::runif(1))),
      status = status,
      phase = phase %||% status,
      goal = goal %||% "",
      observations = observations %||% list(),
      artifacts = artifacts %||% list(),
      failures = failures %||% list(),
      open_questions = open_questions %||% character(0),
      risk_gate = risk_gate %||% list(pending = FALSE, reason = NULL),
      can_finalize = isTRUE(can_finalize),
      decision = decision,
      blocker = blocker,
      last_tool_results = last_tool_results %||% list(),
      budget = budget %||% list(),
      details = details %||% list(),
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
    ),
    class = c("aisdk_task_state", "aisdk_run_state")
  )
}

#' @keywords internal
new_run_trace <- function(run_id = NULL) {
  structure(
    list(
      run_id = run_id %||% paste0("run_", generate_stable_id("trace", Sys.time(), stats::runif(1))),
      events = list()
    ),
    class = "aisdk_run_trace"
  )
}

#' @keywords internal
run_trace_add <- function(trace, type, payload = list()) {
  trace <- trace %||% new_run_trace()
  trace$events[[length(trace$events) + 1L]] <- list(
    event_id = paste0("evt_", generate_stable_id(type, Sys.time(), stats::runif(1))),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
    type = type,
    payload = payload %||% list()
  )
  trace
}

#' @keywords internal
new_agent_decision <- function(decision = "continue",
                               reason = NULL,
                               next_instruction = NULL,
                               needs_user_question = NULL,
                               final_answer_hint = NULL) {
  if (!decision %in% aisdk_agent_decisions) {
    rlang::abort(sprintf("Unknown agent decision: %s", decision))
  }
  structure(
    list(
      decision = decision,
      reason = reason %||% decision,
      next_instruction = next_instruction,
      needs_user_question = needs_user_question,
      final_answer_hint = final_answer_hint,
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
    ),
    class = "aisdk_agent_decision"
  )
}

#' @keywords internal
task_state_set_status <- function(task_state, status, phase = NULL, blocker = NULL) {
  if (!status %in% aisdk_task_state_statuses) {
    rlang::abort(sprintf("Unknown task state status: %s", status))
  }
  task_state$status <- status
  task_state$phase <- phase %||% status
  if (!is.null(blocker)) {
    task_state$blocker <- blocker
  }
  task_state$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
  task_state
}

#' @keywords internal
task_state_set_decision <- function(task_state, decision) {
  task_state$decision <- decision
  task_state$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
  task_state
}

#' Width-Limited Display Text for an Arbitrary Value
#'
#' Coerces any value (character, list, or JSON-serializable object) to a
#' single compact display string, truncated to `width` characters. Part of
#' the package-author extension API.
#'
#' @param value The value to render.
#' @param width Maximum number of characters to keep.
#' @return A single string.
#' @keywords internal
#' @export
agent_runtime_text <- function(value, width = 800) {
  text <- tryCatch(
    {
      if (is.null(value)) {
        ""
      } else if (is.character(value)) {
        paste(value, collapse = "\n")
      } else {
        safe_to_json(value, auto_unbox = TRUE)
      }
    },
    error = function(e) as.character(value)[[1]] %||% ""
  )
  compact_text_preview(text, width = width)
}

#' @keywords internal
agent_runtime_goal_from_messages <- function(messages) {
  messages <- messages %||% list()
  user_messages <- Filter(function(msg) identical(msg$role %||% "", "user"), messages)
  if (length(user_messages) == 0) {
    return("")
  }
  content <- user_messages[[length(user_messages)]]$content %||% ""
  agent_runtime_text(content, width = 1000)
}

#' @keywords internal
agent_runtime_tool_observation <- function(tool_result) {
  result <- tool_result$result %||% tool_result$raw_result %||% NULL
  list(
    id = tool_result$id %||% NULL,
    name = tool_result$name %||% "unknown",
    status = if (isTRUE(tool_result$is_error)) "error" else "ok",
    is_error = isTRUE(tool_result$is_error),
    result = agent_runtime_text(result, width = 800)
  )
}

#' @keywords internal
task_state_add_tool_results <- function(task_state, tool_results) {
  tool_results <- tool_results %||% list()
  if (length(tool_results) == 0) {
    return(task_state)
  }

  observations <- lapply(tool_results, agent_runtime_tool_observation)
  task_state$observations <- c(task_state$observations %||% list(), observations)
  task_state$last_tool_results <- run_state_tool_results_tail(tool_results)

  failures <- Filter(function(obs) isTRUE(obs$is_error), observations)
  if (length(failures) > 0) {
    task_state$failures <- c(task_state$failures %||% list(), failures)
  }
  if (any(vapply(observations, function(obs) !isTRUE(obs$is_error), logical(1)))) {
    task_state$can_finalize <- TRUE
  }
  task_state$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
  task_state
}

#' @keywords internal
agent_runtime_tool_signature <- function(tool_calls) {
  if (is.null(tool_calls) || length(tool_calls) == 0) {
    return(NULL)
  }
  paste(
    vapply(tool_calls, function(tc) {
      paste0(tc$name %||% "unknown", ":", safe_to_json(tc$arguments %||% list(), auto_unbox = TRUE))
    }, character(1)),
    collapse = "|"
  )
}

#' @keywords internal
agent_runtime_has_tool_calls <- function(result) {
  length(result$tool_calls %||% list()) > 0
}

#' @keywords internal
agent_runtime_emit_stream_event <- function(callback,
                                            type,
                                            text = NULL,
                                            done = FALSE,
                                            step = NULL,
                                            metadata = list()) {
  if (!is.function(callback)) {
    return(invisible(NULL))
  }
  event <- c(
    list(
      type = type,
      text = text,
      done = isTRUE(done),
      step = step
    ),
    metadata %||% list()
  )
  callback(event)
  invisible(event)
}

#' @keywords internal
agent_runtime_pending_tag_suffix <- function(text, patterns) {
  if (!nzchar(text %||% "")) {
    return("")
  }

  best <- ""
  for (pattern in patterns) {
    max_len <- min(nchar(pattern) - 1L, nchar(text))
    if (max_len <= 0L) {
      next
    }
    for (len in seq.int(max_len, 1L)) {
      suffix <- substr(text, nchar(text) - len + 1L, nchar(text))
      prefix <- substr(pattern, 1L, len)
      if (identical(suffix, prefix) && len > nchar(best)) {
        best <- suffix
      }
    }
  }
  best
}

#' @keywords internal
new_agent_runtime_thinking_markup_filter <- function() {
  state <- new.env(parent = emptyenv())
  state$in_thinking <- FALSE
  state$pending <- ""

  process <- function(text, done = FALSE) {
    text <- paste0(state$pending, text %||% "")
    state$pending <- ""
    if (!nzchar(text)) {
      return(list(visible = "", thinking = ""))
    }

    if (!isTRUE(done)) {
      patterns <- c("<think>", "</think>")
      pending <- agent_runtime_pending_tag_suffix(text, patterns)
      if (nzchar(pending)) {
        state$pending <- pending
        text <- substr(text, 1L, nchar(text) - nchar(pending))
      }
    }

    visible <- character()
    thinking <- character()
    while (nzchar(text)) {
      if (isTRUE(state$in_thinking)) {
        close_pos <- regexpr("</think>", text, fixed = TRUE)[[1]]
        if (close_pos > 0L) {
          close_end <- close_pos + nchar("</think>") - 1L
          thinking <- c(thinking, substr(text, 1L, close_end))
          text <- substr(text, close_end + 1L, nchar(text))
          state$in_thinking <- FALSE
        } else {
          thinking <- c(thinking, text)
          text <- ""
        }
      } else {
        open_pos <- regexpr("<think>", text, fixed = TRUE)[[1]]
        if (open_pos > 0L) {
          if (open_pos > 1L) {
            visible <- c(visible, substr(text, 1L, open_pos - 1L))
          }
          text <- substr(text, open_pos, nchar(text))
          state$in_thinking <- TRUE
        } else {
          visible <- c(visible, text)
          text <- ""
        }
      }
    }

    list(
      visible = paste0(visible, collapse = ""),
      thinking = paste0(thinking, collapse = "")
    )
  }

  list(process = process)
}

#' @keywords internal
agent_runtime_anthropic_content_without_text <- function(content) {
  if (!is.list(content)) {
    return(content)
  }
  kept <- Filter(function(block) {
    !identical(block$type %||% NULL, "text")
  }, content)
  if (length(kept) == 0) {
    return(content)
  }
  kept
}

#' @keywords internal
agent_runtime_tool_results_have_success <- function(tool_results) {
  any(vapply(tool_results %||% list(), function(tr) !isTRUE(tr$is_error), logical(1)))
}

#' @keywords internal
agent_runtime_tool_results_have_error <- function(tool_results) {
  any(vapply(tool_results %||% list(), function(tr) isTRUE(tr$is_error), logical(1)))
}

#' @keywords internal
agent_runtime_policy_fallback <- function(reason,
                                          task_state,
                                          all_tool_results = list(),
                                          budget = list()) {
  if (isTRUE(task_state$risk_gate$pending)) {
    return(new_agent_decision(
      "ask_user",
      reason = task_state$risk_gate$reason %||% "risk_gate_pending",
      needs_user_question = task_state$risk_gate$question %||% "Please confirm how to proceed."
    ))
  }

  if (reason %in% c("repeated_identical_tool_calls", "network_error", "no_progress_budget_exhausted")) {
    return(new_agent_decision(
      "blocked",
      reason = reason,
      final_answer_hint = "Explain what was attempted and why the task cannot safely continue without a changed approach."
    ))
  }

  if (reason %in% c("empty_after_tools", "protocol_exhausted", "tool_result_budget_exhausted")) {
    if (length(all_tool_results %||% list()) > 0) {
      return(new_agent_decision(
        "finalize",
        reason = reason,
        final_answer_hint = "Summarize the tool evidence for the user."
      ))
    }
  }

  if (reason %in% c("window_boundary", "tool_result_errors", "empty_no_progress")) {
    total_steps <- budget$total_steps %||% 0L
    max_total_steps <- budget$max_total_steps %||% Inf
    if (is.finite(max_total_steps) && total_steps >= max_total_steps) {
      if (length(all_tool_results %||% list()) > 0) {
        return(new_agent_decision(
          "finalize",
          reason = "execution_budget_exhausted",
          final_answer_hint = "Give the best answer possible from the observations."
        ))
      }
      return(new_agent_decision("blocked", reason = "execution_budget_exhausted"))
    }
    return(new_agent_decision(
      "continue",
      reason = reason,
      next_instruction = "Open another execution window and continue the task."
    ))
  }

  if (length(all_tool_results %||% list()) > 0) {
    return(new_agent_decision(
      "finalize",
      reason = reason %||% "tool_evidence_available",
      final_answer_hint = "Use the available tool observations to answer."
    ))
  }

  new_agent_decision("continue", reason = reason %||% "default_continue")
}

#' @keywords internal
agent_runtime_parse_policy_decision <- function(text) {
  parsed <- safe_parse_json(text %||% "")
  if (is.null(parsed) || !is.list(parsed)) {
    return(NULL)
  }
  decision <- parsed$decision %||% NULL
  if (is.null(decision) || !decision %in% aisdk_agent_decisions) {
    return(NULL)
  }
  new_agent_decision(
    decision = decision,
    reason = parsed$reason %||% "llm_policy",
    next_instruction = parsed$next_instruction %||% NULL,
    needs_user_question = parsed$needs_user_question %||% NULL,
    final_answer_hint = parsed$final_answer_hint %||% NULL
  )
}

#' @keywords internal
agent_runtime_policy_decide <- function(reason,
                                        task_state,
                                        trace,
                                        model = NULL,
                                        messages = NULL,
                                        base_params = list(),
                                        all_tool_results = list(),
                                        budget = list(),
                                        policy_config = list()) {
  use_llm_policy <- isTRUE(policy_config$use_llm) ||
    isTRUE(getOption("aisdk.agent_runtime.llm_policy", FALSE))

  if (isTRUE(use_llm_policy) && !is.null(model)) {
    policy_messages <- list(
      list(
        role = "system",
        content = paste(
          "You are the hidden policy controller for an agent runtime.",
          "Return only JSON with keys: decision, reason, next_instruction, needs_user_question, final_answer_hint.",
          "decision must be one of: continue, finalize, ask_user, blocked, abort_for_safety.",
          sep = "\n"
        )
      ),
      list(
        role = "user",
        content = safe_to_json(list(
          reason = reason,
          task_state = task_state,
          recent_observations = utils::tail(task_state$observations %||% list(), 6),
          budget = budget
        ), auto_unbox = TRUE)
      )
    )
    policy_params <- base_params
    policy_params$tools <- NULL
    policy_params$messages <- policy_messages
    policy_result <- tryCatch(
      model$do_generate(policy_params),
      error = function(e) NULL
    )
    parsed <- agent_runtime_parse_policy_decision(policy_result$text %||% "")
    if (!is.null(parsed)) {
      return(parsed)
    }
    trace <- run_trace_add(trace, "policy_parse_failure", list(
      reason = reason,
      text = agent_runtime_text(policy_result$text %||% "", width = 500)
    ))
  }

  agent_runtime_policy_fallback(
    reason = reason,
    task_state = task_state,
    all_tool_results = all_tool_results,
    budget = budget
  )
}

#' @keywords internal
agent_runtime_build_final_answer <- function(task_state,
                                             all_tool_results = list(),
                                             blocked = FALSE) {
  observations <- task_state$observations %||% list()
  if (length(observations) == 0) {
    if (isTRUE(blocked)) {
      return("I could not complete the task because the run is blocked and no usable tool observations were produced.")
    }
    return("I completed the run, but there were no tool observations to summarize.")
  }

  tail_obs <- utils::tail(observations, 6)
  lines <- vapply(tail_obs, function(obs) {
    status <- if (isTRUE(obs$is_error)) "error" else "ok"
    sprintf("- %s [%s]: %s", obs$name %||% "unknown", status, obs$result %||% "")
  }, character(1))

  if (isTRUE(blocked)) {
    intro <- "I could not safely continue the task. Here is the latest evidence:"
  } else if (agent_runtime_tool_results_have_error(all_tool_results) &&
             !agent_runtime_tool_results_have_success(all_tool_results)) {
    intro <- "The tool work did not complete successfully. Here is the latest evidence:"
  } else {
    intro <- "I completed the tool work. Here is the result:"
  }

  paste(c(intro, lines), collapse = "\n")
}

#' @keywords internal
agent_runtime_generate_final_answer <- function(model,
                                                messages,
                                                base_params = list(),
                                                task_state,
                                                all_tool_results = list(),
                                                blocked = FALSE,
                                                reason = "finalize") {
  fallback <- agent_runtime_build_final_answer(
    task_state = task_state,
    all_tool_results = all_tool_results,
    blocked = blocked
  )

  if (is.null(model) || length(all_tool_results %||% list()) == 0) {
    return(fallback)
  }

  finalizer_params <- base_params %||% list()
  finalizer_params$tools <- NULL
  finalizer_params$messages <- c(
    messages %||% list(),
    list(list(
      role = "user",
      content = paste(
        "The agent executed tools but did not produce a visible final answer.",
        "Write the final user-visible response now.",
        "Do not call tools. Do not mention hidden policy or runtime internals.",
        "Use the tool observations as evidence. Mention concrete files, paths, results, errors, and next steps when relevant.",
        "If the task is blocked, explain what was attempted and what input or state change is required.",
        "",
        paste0("Blocked: ", if (isTRUE(blocked)) "true" else "false"),
        paste0("Reason: ", reason %||% "finalize"),
        paste0("User goal: ", task_state$goal %||% ""),
        paste0("Tool observations: ", safe_to_json(task_state$observations %||% list(), auto_unbox = TRUE)),
        sep = "\n"
      )
    ))
  )

  finalizer_result <- tryCatch(
    model$do_generate(finalizer_params),
    error = function(e) NULL
  )
  if (is.null(finalizer_result)) {
    return(fallback)
  }

  finalizer_result <- recover_text_final_answer(finalizer_result)
  text <- trimws(finalizer_result$text %||% "")
  if (!nzchar(text)) {
    return(fallback)
  }

  text
}

#' @keywords internal
agent_runtime_append_provider_messages <- function(messages,
                                                   model,
                                                   result,
                                                   tool_results,
                                                   require_post_tool_protocol = FALSE,
                                                   use_text_tool_fallback = FALSE) {
  if (isTRUE(use_text_tool_fallback)) {
    return(list(
      messages = append_text_tool_result_messages(messages, result, tool_results),
      awaiting_post_tool_protocol = TRUE
    ))
  }

  has_tool_calls <- agent_runtime_has_tool_calls(result)
  assistant_message <- list(
    role = "assistant",
    content = if (isTRUE(has_tool_calls)) "" else result$text %||% ""
  )
  history_format <- model$get_history_format()

  if (identical(history_format, "openai")) {
    assistant_message$tool_calls <- lapply(result$tool_calls, function(tc) {
      list(
        id = tc$id,
        type = "function",
        `function` = list(
          name = tc$name,
          arguments = safe_to_json(tc$arguments, auto_unbox = TRUE)
        )
      )
    })
    if (isTRUE(model$capabilities$preserve_reasoning_content) &&
        !is.null(result$reasoning) &&
        nzchar(result$reasoning)) {
      assistant_message$reasoning_content <- result$reasoning
    }
  } else if (identical(history_format, "openai_responses")) {
    assistant_message$tool_calls <- lapply(result$tool_calls, function(tc) {
      list(
        type = "function_call",
        call_id = tc$id,
        name = tc$name,
        arguments = if (is.character(tc$arguments)) {
          tc$arguments
        } else {
          safe_to_json(tc$arguments %||% list(), auto_unbox = TRUE)
        }
      )
    })
  } else if (identical(history_format, "anthropic")) {
    raw_content <- result$raw_response$content
    if (is.list(raw_content) && length(raw_content) > 0) {
      # Non-streaming: the full response object carries content blocks.
      assistant_message$content <- if (isTRUE(has_tool_calls)) {
        agent_runtime_anthropic_content_without_text(raw_content)
      } else {
        raw_content
      }
    } else {
      # Streaming: raw_response is the last SSE event (message_stop) with no
      # content, so the tool_use blocks were dropped and the next turn's
      # tool_result had no matching tool_use (Anthropic 400). Rebuild the
      # content blocks from the aggregated text + tool calls.
      blocks <- list()
      if (!isTRUE(has_tool_calls) && nzchar(result$text %||% "")) {
        blocks <- c(blocks, list(list(type = "text", text = result$text)))
      }
      for (tc in result$tool_calls %||% list()) {
        blocks <- c(blocks, list(list(
          type = "tool_use",
          id = tc$id,
          name = tc$name,
          input = if (is.character(tc$arguments)) {
            parse_tool_arguments(tc$arguments, tool_name = tc$name)
          } else {
            tc$arguments
          }
        )))
      }
      assistant_message$content <- blocks
    }
  } else if (identical(history_format, "gemini")) {
    # Gemini's format_messages rebuilds functionCall parts from
    # `msg$tool_calls` (name + arguments); without this branch the model's
    # tool-call turn was dropped, so the next turn sent a functionResponse
    # with no preceding functionCall and Gemini 400s. Use NULL content for a
    # pure tool-call turn so no empty text part is emitted alongside it.
    assistant_message$tool_calls <- lapply(result$tool_calls, function(tc) {
      list(name = tc$name, arguments = tc$arguments)
    })
    if (isTRUE(has_tool_calls)) {
      assistant_message$content <- NULL
    }
  }

  messages <- c(messages, list(assistant_message))
  for (tr in tool_results) {
    messages <- c(messages, list(model$format_tool_result(tr$id, tr$name, tr$result)))
  }

  awaiting <- FALSE
  if (isTRUE(require_post_tool_protocol)) {
    messages <- append_post_tool_protocol_message(
      messages,
      use_text_tool_fallback = FALSE
    )
    awaiting <- TRUE
  }

  list(messages = messages, awaiting_post_tool_protocol = awaiting)
}

#' @keywords internal
agent_runtime_make_blocked_result <- function(message,
                                              reason = "blocked",
                                              run_id = NULL) {
  GenerateResult$new(
    text = message %||% "",
    finish_reason = "blocked",
    warnings = reason
  )
}

#' @keywords internal
agent_runtime_deliver_final_text <- function(text,
                                             stream = FALSE,
                                             callback = NULL,
                                             renderer = NULL) {
  if (!isTRUE(stream) || !nzchar(text %||% "")) {
    return(invisible(NULL))
  }
  if (interactive() && !is.null(renderer) && is.null(callback)) {
    renderer$process_chunk(text, FALSE)
    renderer$process_chunk(NULL, TRUE)
  }
  if (!is.null(callback)) {
    callback(text, FALSE)
    callback(NULL, TRUE)
  }
  invisible(NULL)
}

# Sum per-step usage across a multi-step run. The runtime overwrote `result`
# each iteration, so ChatSession$stats saw only the final step's tokens; a
# 6-step tool run reported ~1/6 of real spend.
agent_runtime_sum_usage <- function(acc, usage) {
  if (is.null(usage)) {
    return(acc)
  }
  acc <- acc %||% list(prompt_tokens = 0, completion_tokens = 0, total_tokens = 0)
  add <- function(a, b) (a %||% 0) + (b %||% 0)
  acc$prompt_tokens <- add(acc$prompt_tokens, usage$prompt_tokens)
  acc$completion_tokens <- add(acc$completion_tokens, usage$completion_tokens)
  reported_total <- usage$total_tokens %||%
    ((usage$prompt_tokens %||% 0) + (usage$completion_tokens %||% 0))
  acc$total_tokens <- add(acc$total_tokens, reported_total)
  if (!is.null(usage$reasoning_tokens)) {
    acc$reasoning_tokens <- add(acc$reasoning_tokens, usage$reasoning_tokens)
  }
  acc
}

#' @keywords internal
run_agent_runtime <- function(model,
                              messages,
                              base_params = list(),
                              tools = NULL,
                              session = NULL,
                              hooks = NULL,
                              stream = FALSE,
                              callback = NULL,
                              renderer = NULL,
                              run_id = NULL,
                              max_steps = 1,
                              max_tool_result_errors = 2,
                              require_post_tool_protocol = FALSE,
                              use_text_tool_fallback = FALSE,
                              initial_messages_len = length(messages),
                              stream_event_callback = NULL,
                              policy_config = list()) {
  run_id <- run_id %||% paste0("run_", generate_stable_id("agent_runtime", Sys.time(), stats::runif(1)))
  raw_window_size <- max_steps %||% 1L
  window_size <- if (is.numeric(raw_window_size) && length(raw_window_size) == 1L && is.finite(raw_window_size)) {
    max(1L, as.integer(raw_window_size))
  } else {
    20L
  }
  raw_max_total_steps <- policy_config$max_total_steps %||% max(20L, window_size * 8L)
  max_total_steps <- if (is.numeric(raw_max_total_steps) &&
                         length(raw_max_total_steps) == 1L &&
                         is.finite(raw_max_total_steps)) {
    max(1L, as.integer(raw_max_total_steps))
  } else {
    Inf
  }
  repeated_call_limit <- as.integer(policy_config$max_identical_tool_calls %||% 3L)

  task_state <- new_task_state(
    goal = agent_runtime_goal_from_messages(messages),
    status = "running",
    phase = "model_call",
    run_id = run_id,
    budget = list(
      window_steps = window_size,
      max_total_steps = max_total_steps,
      total_steps = 0L,
      execution_windows = 1L,
      max_tool_result_errors = max_tool_result_errors
    )
  )
  trace <- new_run_trace(run_id = run_id)
  decision <- new_agent_decision("continue", reason = "start")

  all_tool_calls <- list()
  accumulated_usage <- NULL
  all_tool_results <- list()
  stream_events <- list()
  result <- NULL
  step <- 0L
  window_step <- 0L
  execution_windows <- 1L
  awaiting_post_tool_protocol <- FALSE
  last_tool_signature <- NULL
  repeated_identical_calls <- 0L

  record_stream_event <- function(type,
                                  text = NULL,
                                  done = FALSE,
                                  metadata = list()) {
    event <- c(
      list(
        type = type,
        text = text,
        done = isTRUE(done),
        step = step
      ),
      metadata %||% list()
    )
    stream_events[[length(stream_events) + 1L]] <<- event
    agent_runtime_emit_stream_event(
      stream_event_callback,
      type = type,
      text = text,
      done = done,
      step = step,
      metadata = metadata
    )
    invisible(event)
  }

  finalize_result <- function(final_text, reason = "finalize", blocked = FALSE) {
    if (is.null(result)) {
      result <<- GenerateResult$new()
    }
    result$text <<- final_text %||% ""
    result$tool_calls <<- NULL
    result$finish_reason <<- if (isTRUE(blocked)) "blocked" else "stop"
    task_state <<- task_state_set_status(
      task_state,
      status = if (isTRUE(blocked)) "blocked" else "completed",
      phase = if (isTRUE(blocked)) "blocked" else "completed",
      blocker = if (isTRUE(blocked)) reason else NULL
    )
    task_state$can_finalize <<- TRUE
    trace <<- run_trace_add(trace, "finalizer_output", list(
      reason = reason,
      blocked = isTRUE(blocked),
      text = agent_runtime_text(final_text, width = 1000)
    ))
    if (isTRUE(stream) && is.function(stream_event_callback)) {
      record_stream_event(
        "final_text",
        text = final_text,
        metadata = list(reason = reason, blocked = isTRUE(blocked))
      )
      record_stream_event("done", done = TRUE)
    } else {
      agent_runtime_deliver_final_text(
        final_text,
        stream = stream,
        callback = callback,
        renderer = renderer
      )
    }
  }

  final_text_from_state <- function(blocked = FALSE, reason = "finalize") {
    finalizer_messages <- utils::head(messages %||% list(), initial_messages_len)
    agent_runtime_generate_final_answer(
      model = model,
      messages = finalizer_messages,
      base_params = base_params,
      task_state = task_state,
      all_tool_results = all_tool_results,
      blocked = blocked,
      reason = reason
    )
  }

  tryCatch(
    {
      repeat {
        if (window_step >= window_size) {
          budget <- list(
            total_steps = step,
            max_total_steps = max_total_steps,
            window_steps = window_size,
            execution_windows = execution_windows
          )
          decision <- agent_runtime_policy_decide(
            reason = "window_boundary",
            task_state = task_state,
            trace = trace,
            model = model,
            messages = messages,
            base_params = base_params,
            all_tool_results = all_tool_results,
            budget = budget,
            policy_config = policy_config
          )
          task_state <- task_state_set_decision(task_state, decision)
          trace <- run_trace_add(trace, "policy_decision", list(
            boundary = "window",
            decision = decision
          ))

          if (identical(decision$decision, "continue")) {
            execution_windows <- execution_windows + 1L
            window_step <- 0L
            task_state <- task_state_set_status(task_state, "continuing", phase = "execution_window")
          } else if (identical(decision$decision, "finalize")) {
            finalize_result(
              final_text_from_state(reason = decision$reason),
              reason = decision$reason
            )
            break
          } else if (identical(decision$decision, "ask_user")) {
            final_text <- decision$needs_user_question %||% "I need your input before I can safely continue."
            finalize_result(final_text, reason = decision$reason)
            task_state <- task_state_set_status(task_state, "waiting_user", phase = "waiting_user")
            result$finish_reason <- "waiting_user"
            break
          } else {
            finalize_result(
              final_text_from_state(blocked = TRUE, reason = decision$reason),
              reason = decision$reason,
              blocked = TRUE
            )
            break
          }
        }

        if (step >= max_total_steps) {
          reason <- if (length(all_tool_results) > 0) "tool_result_budget_exhausted" else "no_progress_budget_exhausted"
          decision <- agent_runtime_policy_decide(
            reason = reason,
            task_state = task_state,
            trace = trace,
            model = model,
            messages = messages,
            base_params = base_params,
            all_tool_results = all_tool_results,
            budget = list(total_steps = step, max_total_steps = max_total_steps),
            policy_config = policy_config
          )
          task_state <- task_state_set_decision(task_state, decision)
          trace <- run_trace_add(trace, "policy_decision", list(
            boundary = "total_budget",
            decision = decision
          ))
          blocked <- identical(decision$decision, "blocked")
          finalize_result(
            final_text_from_state(blocked = blocked, reason = decision$reason),
            reason = decision$reason,
            blocked = blocked
          )
          break
        }

        step <- step + 1L
        window_step <- window_step + 1L
        task_state$budget$total_steps <- step
        task_state$budget$execution_windows <- execution_windows
        task_state <- task_state_set_status(task_state, if (step == 1L) "running" else "continuing", phase = "model_call")

        params <- c(list(messages = messages), base_params)
        filter_protocol_output <- isTRUE(stream) &&
          isTRUE(require_post_tool_protocol) &&
          isTRUE(awaiting_post_tool_protocol)
        protocol_markup_filter <- if (isTRUE(filter_protocol_output)) {
          new_tool_protocol_markup_filter()
        } else {
          NULL
        }

        trace <- run_trace_add(trace, "model_call", list(step = step, window = execution_windows))
        if (isTRUE(stream)) {
          step_stream_chunks <- character()
          step_stream_has_visible_text <- FALSE
          thinking_markup_filter <- if (is.function(stream_event_callback)) {
            new_agent_runtime_thinking_markup_filter()
          } else {
            NULL
          }
          if (interactive() && !is.null(renderer)) {
            renderer$start_thinking()
          }
          result <- model$do_stream(params, function(chunk, done) {
            display_chunk <- chunk
            if (!is.null(protocol_markup_filter)) {
              display_chunk <- protocol_markup_filter$process(chunk, done)
            }

            if (is.function(stream_event_callback)) {
              split_chunk <- thinking_markup_filter$process(display_chunk, done = done)
              if (nzchar(split_chunk$thinking %||% "")) {
                record_stream_event(
                  "thinking_text",
                  text = split_chunk$thinking,
                  metadata = list(reason = "provider_reasoning")
                )
              }
              if (nzchar(split_chunk$visible %||% "")) {
                visible_has_content <- nzchar(trimws(split_chunk$visible))
                if (isTRUE(visible_has_content) || isTRUE(step_stream_has_visible_text)) {
                  step_stream_chunks <<- c(step_stream_chunks, split_chunk$visible)
                  if (isTRUE(visible_has_content)) {
                    step_stream_has_visible_text <<- TRUE
                  }
                  record_stream_event(
                    "text_delta",
                    text = split_chunk$visible,
                    metadata = list(reason = "assistant_text")
                  )
                }
              }
              if (interactive() && !is.null(renderer)) {
                renderer$stop_thinking()
              }
            } else if (interactive() && !is.null(renderer)) {
              if (!is.null(callback)) {
                renderer$stop_thinking()
              } else {
                renderer$process_chunk(display_chunk, done)
              }
            }
            if (!is.function(stream_event_callback) && !is.null(callback)) {
              callback(display_chunk, done)
            }
          })
          if (is.function(stream_event_callback) &&
              !nzchar(result$text %||% "") &&
              length(step_stream_chunks) > 0) {
            result$text <- paste(step_stream_chunks, collapse = "")
          }
        } else {
          result <- model$do_generate(params)
        }

        # Accumulate this step's tokens before any `next` skips ahead; the
        # aggregate is written onto the final result below.
        accumulated_usage <- agent_runtime_sum_usage(accumulated_usage, result$usage)

        result <- recover_text_tool_calls(result)
        result <- recover_text_final_answer(result)
        trace <- run_trace_add(trace, "model_response", list(
          step = step,
          finish_reason = result$finish_reason %||% NULL,
          text = agent_runtime_text(result$text %||% "", width = 800),
          tool_call_count = length(result$tool_calls %||% list())
        ))

        if (text_tool_protocol_missing(result, awaiting_post_tool_protocol)) {
          if (isTRUE(stream) &&
              is.function(stream_event_callback) &&
              nzchar(result$text %||% "")) {
            record_stream_event(
              "intermediate_text",
              text = result$text,
              metadata = list(reason = "protocol_correction")
            )
          }
          messages <- c(messages, list(text_tool_protocol_correction_message(
            result,
            use_text_tool_fallback = use_text_tool_fallback
          )))
          trace <- run_trace_add(trace, "protocol_correction", list(step = step))
          if (interactive() && isTRUE(stream) && !is.null(renderer)) {
            renderer$reset_for_new_step()
          }
          next
        }

        awaiting_post_tool_protocol <- FALSE

        if (agent_runtime_has_tool_calls(result) && length(tools %||% list()) > 0) {
          if (isTRUE(stream) &&
              is.function(stream_event_callback) &&
              nzchar(result$text %||% "")) {
            record_stream_event(
              "intermediate_text",
              text = result$text,
              metadata = list(reason = "tool_call")
            )
          }
          all_tool_calls <- c(all_tool_calls, result$tool_calls)
          task_state <- task_state_set_status(task_state, "running", phase = "tool_execution")
          task_state$budget$tool_calls <- length(all_tool_calls)

          current_signature <- agent_runtime_tool_signature(result$tool_calls)
          if (identical(current_signature, last_tool_signature)) {
            repeated_identical_calls <- repeated_identical_calls + 1L
          } else {
            repeated_identical_calls <- 0L
            last_tool_signature <- current_signature
          }

          if (repeated_identical_calls >= repeated_call_limit) {
            decision <- agent_runtime_policy_decide(
              reason = "repeated_identical_tool_calls",
              task_state = task_state,
              trace = trace,
              model = model,
              messages = messages,
              base_params = base_params,
              all_tool_results = all_tool_results,
              budget = list(total_steps = step, max_total_steps = max_total_steps),
              policy_config = policy_config
            )
            task_state <- task_state_set_decision(task_state, decision)
            trace <- run_trace_add(trace, "policy_decision", list(
              boundary = "repeated_tool_call",
              decision = decision
            ))
            finalize_result(
              final_text_from_state(blocked = TRUE, reason = decision$reason),
              reason = decision$reason,
              blocked = TRUE
            )
            break
          }

          tool_envir <- if (!is.null(session)) session$get_envir() else NULL
          if (interactive()) {
            for (tc in result$tool_calls) {
              if (isTRUE(stream) && !is.null(renderer)) {
                renderer$render_tool_start(tc$name, tc$arguments)
              } else {
                print_tool_execution(tc$name, tc$arguments)
              }
            }
          }

          tool_results <- tryCatch(
            execute_tool_calls(result$tool_calls, tools, hooks, envir = tool_envir),
            error = function(e) {
              lapply(result$tool_calls, function(tc) {
                list(
                  id = tc$id,
                  name = tc$name,
                  result = paste0("Error executing tool: ", conditionMessage(e)),
                  raw_result = NULL,
                  is_error = TRUE
                )
              })
            }
          )
          all_tool_results <- c(all_tool_results, tool_results)
          task_state <- task_state_add_tool_results(task_state, tool_results)
          trace <- run_trace_add(trace, "tool_results", list(
            step = step,
            results = lapply(tool_results, agent_runtime_tool_observation)
          ))

          if (agent_runtime_tool_results_have_error(tool_results)) {
            decision <- agent_runtime_policy_decide(
              reason = "tool_result_errors",
              task_state = task_state,
              trace = trace,
              model = model,
              messages = messages,
              base_params = base_params,
              all_tool_results = all_tool_results,
              budget = list(total_steps = step, max_total_steps = max_total_steps),
              policy_config = policy_config
            )
            task_state <- task_state_set_decision(task_state, decision)
            trace <- run_trace_add(trace, "policy_decision", list(
              boundary = "tool_result_errors",
              decision = decision
            ))
            if (identical(decision$decision, "ask_user")) {
              finalize_result(
                decision$needs_user_question %||% agent_runtime_build_final_answer(task_state, all_tool_results),
                reason = decision$reason
              )
              task_state <- task_state_set_status(task_state, "waiting_user", phase = "waiting_user")
              result$finish_reason <- "waiting_user"
              break
            } else if (identical(decision$decision, "blocked") ||
                       identical(decision$decision, "abort_for_safety")) {
              finalize_result(
                final_text_from_state(blocked = TRUE, reason = decision$reason),
                reason = decision$reason,
                blocked = TRUE
              )
              if (identical(decision$decision, "abort_for_safety")) {
                task_state <- task_state_set_status(task_state, "aborted_safety", phase = "aborted_safety")
                result$finish_reason <- "aborted_safety"
              }
              break
            }
          }

          if (interactive()) {
            for (tr in tool_results) {
              if (isTRUE(stream) && !is.null(renderer)) {
                renderer$render_tool_result(
                  tr$name,
                  tr$result,
                  success = !isTRUE(tr$is_error),
                  raw_result = tr$raw_result %||% tr$result
                )
              } else {
                print_tool_result(
                  tr$name,
                  tr$result,
                  success = !isTRUE(tr$is_error),
                  raw_result = tr$raw_result %||% tr$result
                )
              }
            }
          }

          appended <- agent_runtime_append_provider_messages(
            messages = messages,
            model = model,
            result = result,
            tool_results = tool_results,
            require_post_tool_protocol = require_post_tool_protocol,
            use_text_tool_fallback = use_text_tool_fallback
          )
          messages <- appended$messages
          awaiting_post_tool_protocol <- appended$awaiting_post_tool_protocol

          if (interactive() && isTRUE(stream) && !is.null(renderer)) {
            renderer$reset_for_new_step()
          }
          next
        }

        text <- trimws(result$text %||% "")
        if (!nzchar(text)) {
          empty_reason <- if (length(all_tool_results) > 0) {
            "empty_after_tools"
          } else {
            "empty_no_progress"
          }

          if (identical(empty_reason, "empty_after_tools")) {
            decision <- new_agent_decision(
              "finalize",
              reason = empty_reason,
              final_answer_hint = "Summarize the tool evidence for the user."
            )
            task_state <- task_state_set_decision(task_state, decision)
            trace <- run_trace_add(trace, "policy_decision", list(
              boundary = empty_reason,
              decision = decision
            ))
            finalize_result(
              final_text_from_state(reason = decision$reason),
              reason = decision$reason
            )
            break
          }

          decision <- agent_runtime_policy_decide(
            reason = empty_reason,
            task_state = task_state,
            trace = trace,
            model = model,
            messages = messages,
            base_params = base_params,
            all_tool_results = all_tool_results,
            budget = list(total_steps = step, max_total_steps = max_total_steps),
            policy_config = policy_config
          )
          task_state <- task_state_set_decision(task_state, decision)
          trace <- run_trace_add(trace, "policy_decision", list(
            boundary = empty_reason,
            decision = decision
          ))

          if (identical(decision$decision, "continue")) {
            correction_text <- if (length(all_tool_results) > 0) {
              paste(
                "You executed tools but produced no visible answer.",
                "Continue with the next necessary tool call or provide the final answer now.",
                "Do not return an empty response.",
                sep = "\n"
              )
            } else {
              paste(
                "Your previous response produced no visible answer and made no tool call.",
                "Continue the user's task now: either use the appropriate tool or provide a visible answer.",
                "Do not return only reasoning or an empty response.",
                sep = "\n"
              )
            }
            messages <- c(messages, list(list(
              role = "user",
              content = correction_text
            )))
            if (interactive() && isTRUE(stream) && !is.null(renderer)) {
              renderer$reset_for_new_step()
            }
            next
          }

          blocked <- identical(decision$decision, "blocked")
          finalize_result(
            final_text_from_state(blocked = blocked, reason = decision$reason),
            reason = decision$reason,
            blocked = blocked
          )
          break
        }

        task_state <- task_state_set_status(task_state, "completed", phase = "completed")
        task_state$can_finalize <- TRUE
        decision <- new_agent_decision("finalize", reason = "completed")
        task_state <- task_state_set_decision(task_state, decision)
        if (isTRUE(stream) && is.function(stream_event_callback)) {
          final_already_streamed <- length(step_stream_chunks) > 0 &&
            identical(paste(step_stream_chunks, collapse = ""), result$text %||% "")
          record_stream_event(
            "final_text",
            text = result$text %||% "",
            metadata = list(
              reason = "completed",
              blocked = FALSE,
              already_streamed = isTRUE(final_already_streamed)
            )
          )
          record_stream_event("done", done = TRUE)
        }
        break
      }
    },
    error = function(e) {
      if (is_network_error_condition(e)) {
        handle_network_error(e, rethrow = FALSE)
        decision <<- new_agent_decision("blocked", reason = "network_error")
        task_state <<- task_state_set_decision(task_state, decision)
        task_state <<- task_state_set_status(
          task_state,
          "blocked",
          phase = "blocked",
          blocker = conditionMessage(e)
        )
        result <<- agent_runtime_make_blocked_result(
          message = "",
          reason = conditionMessage(e),
          run_id = run_id
        )
        trace <<- run_trace_add(trace, "network_error", list(message = conditionMessage(e)))
      } else {
        stop(e)
      }
    }
  )

  if (is.null(result)) {
    result <- GenerateResult$new(text = "", finish_reason = "error")
    task_state <- task_state_set_status(task_state, "error", phase = "error", blocker = "runtime_returned_no_result")
    decision <- new_agent_decision("blocked", reason = "runtime_returned_no_result")
    task_state <- task_state_set_decision(task_state, decision)
  }

  result$steps <- step
  result$all_tool_calls <- all_tool_calls
  result$all_tool_results <- all_tool_results
  result$stream_events <- stream_events
  result$messages_added <- build_messages_added(
    messages = messages,
    initial_len = initial_messages_len,
    final_text = result$text %||% NULL,
    final_reasoning = result$reasoning %||% NULL
  )

  # Report tokens summed across every step, not just the last model call.
  if (!is.null(accumulated_usage)) {
    result$usage <- accumulated_usage
  }

  task_state$budget$total_steps <- step
  task_state$budget$execution_windows <- execution_windows
  task_state$budget$tool_calls <- length(all_tool_calls)
  task_state$last_tool_results <- run_state_tool_results_tail(all_tool_results)
  task_state$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")

  result$task_state <- task_state
  result$run_state <- task_state
  result$run_trace <- trace
  result$decision <- decision
  result
}
