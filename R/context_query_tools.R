#' Search Session Context
#'
#' Search compact handles for live objects, memory entries, transcript segments,
#' retrieval hits, and artifacts.
#'
#' @param session A `ChatSession` or `SharedSession`.
#' @param query Search query.
#' @param kinds Optional character vector of handle kinds to include.
#' @param limit Maximum number of handles to return.
#' @return A list of matching context handles.
#' @export
context_search <- function(session, query, kinds = NULL, limit = 5L) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    rlang::abort("`session` must be a ChatSession or SharedSession.")
  }
  query <- as.character(query %||% "")
  limit <- max(1L, as.integer(limit %||% 5L))
  state <- session$get_context_state()
  handles <- list_context_handles(session, state = state, persist = TRUE)
  if (length(handles) == 0) {
    return(list())
  }

  if (!is.null(kinds) && length(kinds) > 0) {
    kinds <- as.character(kinds)
    handles <- Filter(function(handle) (handle$kind %||% "") %in% kinds, handles)
  }
  if (length(handles) == 0) {
    return(list())
  }

  normalized_query <- normalize_retrieval_query(query)
  if (!nzchar(normalized_query)) {
    return(utils::head(handles, limit))
  }

  scored <- lapply(handles, function(handle) {
    haystack <- paste(handle$id %||% "", handle$kind %||% "", handle$title %||% "", handle$summary %||% "", handle$location %||% "", collapse = " ")
    scored_candidate <- score_retrieval_candidate(
      list(provider = handle$kind %||% "context", title = handle$title %||% "", summary = handle$summary %||% "", preview = handle$location %||% ""),
      normalized_query,
      get_context_management_config_impl(session)
    )
    handle$score <- scored_candidate$score %||% 0
    handle$matched <- retrieval_text_matches(haystack, normalized_query, min_hits = 1L)
    handle
  })
  scored <- Filter(function(handle) isTRUE(handle$matched) || (handle$score %||% 0) > 0, scored)
  if (length(scored) == 0) {
    return(list())
  }
  scores <- vapply(scored, function(handle) as.numeric(handle$score %||% 0), numeric(1))
  scored <- scored[order(scores, decreasing = TRUE)]
  utils::head(scored, limit)
}

#' Get Session Context by Handle
#'
#' Resolve a compact context handle to either its summary metadata or a bounded
#' detailed representation.
#'
#' @param session A `ChatSession` or `SharedSession`.
#' @param handle_id Context handle ID.
#' @param detail One of `"summary"` or `"full"`.
#' @return A list with handle metadata and optional bounded content.
#' @export
context_get <- function(session, handle_id, detail = c("summary", "full")) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    rlang::abort("`session` must be a ChatSession or SharedSession.")
  }
  detail <- match.arg(detail)
  found <- find_context_handle(session, handle_id)
  if (is.null(found)) {
    rlang::abort(sprintf("Context handle `%s` was not found.", handle_id))
  }
  handle <- found$handle
  if (identical(detail, "summary")) {
    return(handle)
  }

  ref <- handle$ref %||% list()
  type <- ref$type %||% handle$kind %||% ""
  state <- normalize_context_state(found$state)
  content <- switch(type,
    object = inspect_r_object(ref$name %||% handle$title, session = session, detail = "full"),
    memory = render_context_handle_value(session$get_memory(ref$key), detail = "full"),
    transcript = {
      segment <- (state$transcript_segments %||% list())[[as.integer(ref$index %||% 0L)]]
      segment$summary %||% ""
    },
    retrieval = {
      hit <- (state$retrieval_cache$ranked_hits %||% list())[[as.integer(ref$index %||% 0L)]]
      paste(c(hit$title %||% "", hit$summary %||% "", hit$preview %||% ""), collapse = "\n")
    },
    artifact = {
      path <- ref$path %||% handle$location %||% ""
      if (nzchar(path) && file.exists(path) && !dir.exists(path)) {
        paste(utils::head(readLines(path, warn = FALSE), 200L), collapse = "\n")
      } else {
        handle$summary %||% ""
      }
    },
    handle$summary %||% ""
  )
  list(handle = handle, content = render_context_handle_value(content, detail = "full"))
}

#' Peek at a Live R Object
#'
#' Inspect a live object through the existing semantic inspection adapters.
#'
#' @param session A `ChatSession` or `SharedSession`.
#' @param name Object name in the session environment.
#' @param detail One of `"summary"`, `"structure"`, `"sample"`, or `"full"`.
#' @return A compact inspection string.
#' @export
object_peek <- function(session, name, detail = c("summary", "structure", "sample", "full")) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    rlang::abort("`session` must be a ChatSession or SharedSession.")
  }
  detail <- match.arg(detail)
  switch(detail,
    summary = inspect_r_object(name, session = session, detail = "summary"),
    structure = {
      structured <- inspect_r_object(name, session = session, detail = "structured")
      render_context_handle_value(structured, detail = "full", max_chars = 2500L)
    },
    sample = inspect_r_object(name, session = session, detail = "full", head_rows = 6L),
    full = inspect_r_object(name, session = session, detail = "full")
  )
}

#' @keywords internal
normalize_context_tool_kinds <- function(kinds) {
  if (is.null(kinds) || length(kinds) == 0) {
    return(NULL)
  }
  if (is.character(kinds) && length(kinds) == 1L && grepl(",", kinds, fixed = TRUE)) {
    kinds <- strsplit(kinds, ",", fixed = TRUE)[[1]]
  }
  kinds <- trimws(as.character(kinds))
  kinds[nzchar(kinds)]
}

#' Create Context Query Tools
#'
#' Create ordinary `Tool` objects for searching handles, resolving handles,
#' peeking live R objects, and launching bounded child sessions.
#'
#' @param session Optional `ChatSession` or `SharedSession` captured by the tools.
#' @return A list of `Tool` objects.
#' @export
create_context_query_tools <- function(session = NULL) {
  list(
    tool(
      name = "context_search",
      description = paste(
        "Search compact handles for live objects, memory, transcript segments, retrieval hits, and artifacts.",
        "Use this before requesting detailed context."
      ),
      parameters = z_object(
        query = z_string("Search query"),
        kinds = z_array(z_string("Handle kind such as object, memory, transcript, retrieval, or artifact"), nullable = TRUE),
        limit = z_integer("Maximum number of handles to return", nullable = TRUE),
        .required = c("query")
      ),
      execute = function(args) {
        context_search(
          session = session,
          query = args$query %||% "",
          kinds = normalize_context_tool_kinds(args$kinds %||% NULL),
          limit = args$limit %||% 5L
        )
      }
    ),
    tool(
      name = "context_get",
      description = "Resolve a context handle to summary metadata or a bounded detailed view.",
      parameters = z_object(
        handle_id = z_string("Context handle ID returned by context_search or listed in the context block"),
        detail = z_enum(c("summary", "full"), description = "Detail level", default = "summary"),
        .required = c("handle_id")
      ),
      execute = function(args) {
        context_get(
          session = session,
          handle_id = args$handle_id,
          detail = args$detail %||% "summary"
        )
      }
    ),
    tool(
      name = "object_peek",
      description = paste(
        "Peek at a live R object through semantic inspection adapters.",
        "Prefer summary or structure before full."
      ),
      parameters = z_object(
        name = z_string("Object name in the session environment"),
        detail = z_enum(c("summary", "structure", "sample", "full"), description = "Inspection detail", default = "summary"),
        .required = c("name")
      ),
      execute = function(args) {
        object_peek(
          session = session,
          name = args$name,
          detail = args$detail %||% "summary"
        )
      }
    ),
    tool(
      name = "sub_session_query",
      description = paste(
        "Run a bounded child session for a focused subtask.",
        "Pass only the context handle IDs needed for the subtask."
      ),
      parameters = z_object(
        task = z_string("Focused task for the child session"),
        context_handles = z_array(z_string("Context handle IDs to summarize for the child"), nullable = TRUE),
        max_turns = z_integer("Maximum child generation/tool turns", nullable = TRUE),
        budget = z_any("Optional budget metadata", nullable = TRUE),
        timeout = z_integer("Optional timeout in seconds", nullable = TRUE),
        .required = c("task")
      ),
      execute = function(args) {
        sub_session_query(
          session = session,
          task = args$task,
          context_handles = args$context_handles %||% NULL,
          max_turns = args$max_turns %||% 3L,
          budget = args$budget %||% NULL,
          timeout = args$timeout %||% NULL
        )
      }
    )
  )
}

#' @keywords internal
context_query_tool_names <- function() {
  c("context_search", "context_get", "object_peek", "sub_session_query")
}

#' @keywords internal
append_unique_tools <- function(tools, extra_tools) {
  tools <- tools %||% list()
  extra_tools <- extra_tools %||% list()
  existing <- vapply(tools, function(tool_obj) tool_obj$name %||% "", character(1))
  for (tool_obj in extra_tools) {
    if (!((tool_obj$name %||% "") %in% existing)) {
      tools <- c(tools, list(tool_obj))
      existing <- c(existing, tool_obj$name %||% "")
    }
  }
  tools
}
