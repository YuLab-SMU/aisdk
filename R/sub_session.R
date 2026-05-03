#' @keywords internal
format_child_context_handles <- function(session, context_handles = NULL) {
  ids <- context_handles %||% character(0)
  if (length(ids) == 0) {
    return("")
  }
  resolved <- lapply(as.character(ids), function(id) {
    tryCatch(context_get(session, id, detail = "summary"), error = function(e) NULL)
  })
  resolved <- Filter(Negate(is.null), resolved)
  if (length(resolved) == 0) {
    return("")
  }
  lines <- c("[SELECTED CONTEXT HANDLES]")
  for (handle in resolved) {
    lines <- c(
      lines,
      sprintf("- %s (%s): %s", handle$id %||% "", handle$kind %||% "", handle$title %||% ""),
      if (nzchar(handle$summary %||% "")) paste0("  ", handle$summary) else NULL,
      if (nzchar(handle$accessor %||% "")) paste0("  accessor: ", handle$accessor) else NULL
    )
  }
  paste(lines, collapse = "\n")
}

#' @keywords internal
compact_child_trace <- function(child) {
  history <- child$get_history()
  if (length(history) == 0) {
    return(list())
  }
  lapply(seq_along(history), function(i) {
    msg <- history[[i]]
    list(
      index = i,
      role = msg$role %||% "unknown",
      preview = trim_context_preview(as.character(msg$content %||% ""), max_chars = 220L)
    )
  })
}

#' @keywords internal
record_sub_session_result <- function(parent, result) {
  state <- parent$get_context_state()
  state$sub_sessions <- c(state$sub_sessions %||% list(), list(result))
  state$active_facts <- merge_state_items(
    state$active_facts,
    list(list(
      text = sprintf("Sub-session `%s` completed: %s", result$id %||% "child", trim_context_preview(result$summary %||% "", max_chars = 180L)),
      source = "sub_session",
      timestamp = result$completed_at %||% as.character(Sys.time())
    )),
    max_items = 8L
  )
  state$tool_digest <- c(
    state$tool_digest %||% list(),
    list(list(
      tool = "sub_session_query",
      status = if (isTRUE(result$success)) "ok" else "error",
      summary = result$summary %||% "",
      timestamp = result$completed_at %||% as.character(Sys.time())
    ))
  )
  state <- append_context_event(
    state,
    "sub_session_completed",
    list(
      id = result$id %||% "",
      success = as.character(isTRUE(result$success)),
      turns = as.character(result$turns %||% 0L)
    )
  )
  parent$set_context_state(state)
  invisible(parent)
}

#' Run a Bounded Child Session
#'
#' Creates a scoped child `ChatSession` for a focused task. The child uses an
#' environment whose parent is the parent session environment, so writes stay in
#' the child scope. Only a compact result summary and trace are written back to
#' the parent context state.
#'
#' @param session Parent `ChatSession` or `SharedSession`.
#' @param task Focused child task.
#' @param context_handles Optional context handle IDs to summarize for the child.
#' @param max_turns Maximum child generation/tool turns.
#' @param budget Optional budget metadata recorded in the result.
#' @param timeout Optional timeout in seconds. Currently recorded as metadata.
#' @return A compact sub-session result list.
#' @export
sub_session_query <- function(session,
                              task,
                              context_handles = NULL,
                              max_turns = 3L,
                              budget = NULL,
                              timeout = NULL) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    rlang::abort("`session` must be a ChatSession or SharedSession.")
  }
  task <- as.character(task %||% "")
  if (!nzchar(task)) {
    rlang::abort("`task` must be a non-empty string.")
  }
  max_turns <- max(1L, as.integer(max_turns %||% 3L))

  child_env <- new.env(parent = session$get_envir())
  context_block <- format_child_context_handles(session, context_handles = context_handles)
  child_system <- paste(
    c(
      "You are a bounded child session. Answer the focused task with compact findings only.",
      "You may read parent context through provided handles. Writes stay in the child R scope.",
      if (nzchar(context_block)) context_block else NULL
    ),
    collapse = "\n\n"
  )

  parent_tools <- tryCatch(session$get_tools(), error = function(e) list())
  child_tools <- append_unique_tools(parent_tools, create_context_query_tools(session))
  model <- tryCatch(session$get_model(), error = function(e) session$get_model_id())
  child <- ChatSession$new(
    model = model,
    system_prompt = child_system,
    tools = child_tools,
    max_steps = max_turns,
    registry = session$get_metadata("registry", default = NULL),
    envir = child_env
  )
  child$set_context_management_mode("basic")

  started_at <- as.character(Sys.time())
  response <- tryCatch(
    child$send(task),
    error = function(e) {
      structure(list(text = paste0("Sub-session error: ", conditionMessage(e))), class = "sub_session_error")
    }
  )
  success <- !inherits(response, "sub_session_error")
  summary <- trim_context_preview(response$text %||% "", max_chars = 600L)
  result <- list(
    id = context_handle_id("sub_session", digest::digest(paste(task, started_at))),
    task = trim_context_preview(task, max_chars = 240L),
    success = success,
    summary = summary,
    context_handles = as.character(context_handles %||% character(0)),
    turns = length(child$get_history()),
    max_turns = max_turns,
    budget = budget,
    timeout = timeout,
    started_at = started_at,
    completed_at = as.character(Sys.time()),
    trace = compact_child_trace(child)
  )
  record_sub_session_result(session, result)
  result
}
