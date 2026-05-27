#' @keywords internal
context_handle_id <- function(kind, key) {
  key <- as.character(key %||% "")
  key <- gsub("[^A-Za-z0-9_.:-]+", "_", key, perl = TRUE)
  key <- gsub("_+", "_", key, perl = TRUE)
  key <- sub("^_+", "", sub("_+$", "", key))
  if (!nzchar(key)) {
    key <- substr(digest::digest(paste(kind, Sys.time())), 1L, 12L)
  }
  paste0(kind, ":", key)
}

#' @keywords internal
compact_context_handle <- function(id,
                                   kind,
                                   title,
                                   summary = "",
                                   accessor = NULL,
                                   location = NULL,
                                   updated_at = NULL,
                                   ref = NULL) {
  list(
    id = as.character(id),
    kind = as.character(kind),
    title = trim_context_preview(title %||% id, max_chars = 90L),
    summary = trim_context_preview(summary %||% "", max_chars = 220L),
    accessor = accessor %||% "",
    location = location %||% "",
    updated_at = updated_at %||% as.character(Sys.time()),
    ref = ref %||% list()
  )
}

#' @keywords internal
render_context_handle_value <- function(value, detail = c("summary", "full"), max_chars = 4000L) {
  detail <- match.arg(detail)
  if (is.null(value)) {
    return("")
  }
  preview <- if (identical(detail, "summary")) {
    render_retrieval_value_preview(value, max_chars = 220L)
  } else if (is.character(value)) {
    paste(value, collapse = "\n")
  } else if (is.atomic(value)) {
    paste(as.character(value), collapse = ", ")
  } else if (is.data.frame(value)) {
    paste(utils::capture.output(print(utils::head(value, 20L))), collapse = "\n")
  } else {
    tryCatch(
      safe_to_json(value, auto_unbox = TRUE, pretty = TRUE),
      error = function(e) paste(utils::capture.output(str(value, max.level = 2L, list.len = 20L)), collapse = "\n")
    )
  }
  if (nchar(preview, type = "chars") > max_chars) {
    preview <- paste0(substr(preview, 1L, max_chars - 3L), "...")
  }
  preview
}

#' @keywords internal
build_live_object_context_handles <- function(session, state = NULL) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    return(list())
  }
  state <- normalize_context_state(state %||% session$get_context_state())
  object_cards <- state$object_cards %||% list()
  if (length(object_cards) == 0) {
    object_cards <- build_context_object_cards(session)
  }
  if (length(object_cards) == 0) {
    return(list())
  }

  lapply(unname(object_cards), function(card) {
    name <- card$name %||% ""
    compact_context_handle(
      id = context_handle_id("object", name),
      kind = "object",
      title = name,
      summary = card$summary %||% "",
      accessor = paste(c(
        sprintf("object_peek(name = \"%s\", detail = \"summary\")", name),
        sprintf("object_peek(name = \"%s\", detail = \"structure\")", name)
      ), collapse = "; "),
      location = "session environment",
      updated_at = card$updated_at %||% as.character(Sys.time()),
      ref = list(type = "object", name = name)
    )
  })
}

#' @keywords internal
build_memory_context_handles <- function(session) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    return(list())
  }
  keys <- session$list_memory() %||% character(0)
  if (length(keys) == 0) {
    return(list())
  }
  lapply(keys, function(key) {
    value <- session$get_memory(key)
    compact_context_handle(
      id = context_handle_id("memory", key),
      kind = "memory",
      title = key,
      summary = render_retrieval_value_preview(value, max_chars = 220L),
      accessor = sprintf("context_get(handle_id = \"%s\", detail = \"summary\")", context_handle_id("memory", key)),
      location = "session memory",
      ref = list(type = "memory", key = key)
    )
  })
}

#' @keywords internal
build_transcript_context_handles <- function(state) {
  state <- normalize_context_state(state)
  segments <- state$transcript_segments %||% list()
  if (length(segments) == 0) {
    return(list())
  }
  lapply(seq_along(segments), function(i) {
    segment <- segments[[i]]
    stable_key <- segment$id %||% digest::digest(segment$summary %||% i)
    id <- context_handle_id("transcript", stable_key)
    compact_context_handle(
      id = id,
      kind = "transcript",
      title = sprintf("Compacted transcript %s", i),
      summary = segment$summary %||% "",
      accessor = sprintf("context_get(handle_id = \"%s\", detail = \"summary\")", id),
      location = "context_state$transcript_segments",
      updated_at = segment$created_at %||% state$last_compaction_at %||% as.character(Sys.time()),
      ref = list(type = "transcript", index = i)
    )
  })
}

#' @keywords internal
build_retrieval_context_handles <- function(state) {
  state <- normalize_context_state(state)
  hits <- state$retrieval_cache$ranked_hits %||% list()
  if (length(hits) == 0) {
    return(list())
  }
  lapply(seq_along(hits), function(i) {
    hit <- hits[[i]]
    stable_key <- hit$retrieval_id %||% digest::digest(paste(hit$provider %||% "", hit$title %||% "", hit$summary %||% ""))
    id <- context_handle_id("retrieval", stable_key)
    compact_context_handle(
      id = id,
      kind = "retrieval",
      title = hit$title %||% sprintf("Retrieval hit %s", i),
      summary = hit$summary %||% hit$preview %||% "",
      accessor = sprintf("context_get(handle_id = \"%s\", detail = \"summary\")", id),
      location = hit$provider %||% "retrieval_cache",
      updated_at = hit$timestamp %||% hit$created_at %||% hit$updated_at %||% as.character(Sys.time()),
      ref = list(type = "retrieval", index = i)
    )
  })
}

#' @keywords internal
build_artifact_context_handles <- function(state) {
  state <- normalize_context_state(state)
  artifacts <- state$artifact_cards %||% list()
  if (length(artifacts) == 0) {
    return(list())
  }
  lapply(seq_along(artifacts), function(i) {
    artifact <- artifacts[[i]]
    path <- artifact$path %||% artifact$uri %||% sprintf("artifact-%s", i)
    id <- context_handle_id("artifact", digest::digest(path))
    compact_context_handle(
      id = id,
      kind = "artifact",
      title = basename(path),
      summary = artifact$summary %||% sprintf("Artifact from %s", artifact$tool %||% "tool"),
      accessor = sprintf("context_get(handle_id = \"%s\", detail = \"summary\")", id),
      location = path,
      updated_at = artifact$timestamp %||% as.character(Sys.time()),
      ref = list(type = "artifact", index = i, path = path)
    )
  })
}

#' @keywords internal
deduplicate_context_handles <- function(handles) {
  handles <- handles %||% list()
  if (length(handles) == 0) {
    return(list())
  }
  ids <- vapply(handles, function(handle) handle$id %||% "", character(1))
  handles[!duplicated(ids) & nzchar(ids)]
}

#' List Available Context Handles
#'
#' Builds compact handles for live session context without embedding full object
#' payloads in the prompt.
#'
#' @param session A `ChatSession` or `SharedSession`.
#' @param state Optional normalized context state. Defaults to the session state.
#' @param persist Logical; whether to persist the handle list into the session
#'   context state.
#' @return A list of compact context handle records.
#' @export
list_context_handles <- function(session, state = NULL, persist = TRUE) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    rlang::abort("`session` must be a ChatSession or SharedSession.")
  }
  state <- normalize_context_state(state %||% session$get_context_state())
  handles <- deduplicate_context_handles(c(
    build_live_object_context_handles(session, state = state),
    build_memory_context_handles(session),
    build_transcript_context_handles(state),
    build_retrieval_context_handles(state),
    build_artifact_context_handles(state)
  ))
  if (isTRUE(persist)) {
    state$context_handles <- handles
    session$set_context_state(state)
  }
  handles
}

#' @keywords internal
find_context_handle <- function(session, handle_id, state = NULL) {
  state <- normalize_context_state(state %||% session$get_context_state())
  handles <- state$context_handles %||% list()
  if (length(handles) == 0 || !any(vapply(handles, function(handle) identical(handle$id %||% "", handle_id), logical(1)))) {
    handles <- list_context_handles(session, state = state, persist = TRUE)
    state <- session$get_context_state()
  }
  for (handle in handles) {
    if (identical(handle$id %||% "", handle_id)) {
      return(list(handle = handle, state = state))
    }
  }
  NULL
}
