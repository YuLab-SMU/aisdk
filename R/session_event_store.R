#' Session Event Store
#'
#' JSONL-backed event log and branch tree for interactive chat sessions.
#' Front ends (the aisdk.console package, aisdk.shiny, or custom UIs) use
#' these helpers to persist per-turn events under `.aisdk/sessions/` and to
#' fork/checkout conversation branches.
#'
#' Session metadata keys intentionally keep their historical `console_*`
#' names (`console_session_id`, `console_branch_id`, `console_branch_tree`,
#' `console_session_store_root`) so that session files saved by earlier
#' aisdk releases keep restoring cleanly.
#'
#' @name session_event_store
NULL

#' Stable Store Id for a Chat Session
#'
#' Returns the stable identifier used to name the session's JSONL event log.
#' For a `ChatSession` the id is created once and memoised in the session
#' metadata; without a session a fresh id is generated.
#'
#' @param session A `ChatSession` object, or `NULL`.
#' @return A single string, e.g. `"session_ab12..."`.
#' @keywords internal
#' @export
session_store_id <- function(session = NULL) {
  if (!is.null(session) && inherits(session, "ChatSession")) {
    existing <- session$get_metadata("console_session_id", default = NULL)
    if (is.character(existing) && length(existing) == 1L && nzchar(existing)) {
      return(existing)
    }
    id <- paste0("session_", generate_stable_id("console_session", Sys.time(), stats::runif(1)))
    session$set_metadata("console_session_id", id)
    return(id)
  }
  paste0("session_", generate_stable_id("console_session", Sys.time(), stats::runif(1)))
}

#' @keywords internal
session_store_root <- function(session = NULL, startup_dir = getwd()) {
  root <- if (!is.null(session) && inherits(session, "ChatSession")) {
    session$get_metadata("console_session_store_root", default = NULL)
  } else {
    NULL
  }
  root <- root %||% file.path(startup_dir, ".aisdk", "sessions")
  normalizePath(root, winslash = "/", mustWork = FALSE)
}

#' Path of a Session's JSONL Event Log
#'
#' Resolves (and creates, if needed) the store directory and returns the
#' path of the session's event log file.
#'
#' @param session A `ChatSession` object, or `NULL`.
#' @param startup_dir Directory whose `.aisdk/sessions/` subdirectory holds
#'   session event logs when the session does not carry an explicit store
#'   root. Defaults to `getwd()`.
#' @return The event-log file path (character scalar).
#' @keywords internal
#' @export
session_event_path <- function(session = NULL, startup_dir = getwd()) {
  root <- session_store_root(session, startup_dir = startup_dir)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  file.path(root, paste0(session_store_id(session), ".jsonl"))
}

#' Append an Event to a Session's Event Log
#'
#' Serializes one event as a JSON line and appends it to the session's event
#' log. Events carry a type, the active branch id, a visibility flag, and an
#' arbitrary JSON-serializable payload.
#'
#' @param session A `ChatSession` object.
#' @param type Event type string (e.g. `"message"`, `"branch"`).
#' @param payload Named list payload; non-serializable objects are reduced
#'   to compact placeholders.
#' @param startup_dir Directory used to resolve the event log path.
#' @param visible Logical; marks events that represent user-visible
#'   transcript entries.
#' @param branch_id Branch id to tag the event with; defaults to the
#'   session's active branch.
#' @return Invisibly, the event record (a list).
#' @keywords internal
#' @export
session_append_event <- function(session,
                                 type,
                                 payload = list(),
                                 startup_dir = getwd(),
                                 visible = FALSE,
                                 branch_id = NULL) {
  path <- session_event_path(session, startup_dir = startup_dir)
  branch_id <- branch_id %||% session$get_metadata("console_branch_id", default = "main")
  event <- list(
    event_id = paste0("evt_", generate_stable_id("event", Sys.time(), stats::runif(1))),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
    type = type,
    branch_id = branch_id,
    visible = isTRUE(visible),
    payload = session_event_jsonable(payload %||% list())
  )
  line <- jsonlite::toJSON(event, auto_unbox = TRUE, null = "null")
  cat(line, "\n", file = path, append = TRUE, sep = "")
  invisible(event)
}

#' @keywords internal
session_event_jsonable <- function(x) {
  if (inherits(x, "aisdk_run_state") ||
      inherits(x, "aisdk_task_state") ||
      inherits(x, "aisdk_agent_decision") ||
      inherits(x, "aisdk_run_trace")) {
    x <- unclass(x)
  }
  if (is.list(x)) {
    return(lapply(x, session_event_jsonable))
  }
  if (is.environment(x) || inherits(x, "R6")) {
    return(as.character(x)[[1]] %||% "<object>")
  }
  x
}

#' Read a Session's Event Log
#'
#' Reads and parses the JSONL event log of a session. Unparseable lines are
#' dropped.
#'
#' @param session A `ChatSession` object, or `NULL` when `path` is given.
#' @param path Optional explicit event-log path.
#' @param startup_dir Directory used to resolve the event log path.
#' @return A list of event records (lists).
#' @keywords internal
#' @export
session_read_events <- function(session = NULL, path = NULL, startup_dir = getwd()) {
  path <- path %||% session_event_path(session, startup_dir = startup_dir)
  if (!file.exists(path)) {
    return(list())
  }
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  events <- lapply(lines, function(line) {
    tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
  })
  Filter(Negate(is.null), events)
}

#' Extract Visible Messages from Session Events
#'
#' Filters an event list down to user-visible transcript entries and returns
#' their message payloads.
#'
#' @param events A list of event records, as returned by
#'   [session_read_events()].
#' @return A list of message payloads.
#' @keywords internal
#' @export
session_event_visible_messages <- function(events) {
  visible <- Filter(function(event) {
    identical(event$type %||% "", "message") ||
      identical(event$type %||% "", "custom_message") ||
      isTRUE(event$visible)
  }, events %||% list())

  lapply(visible, function(event) {
    payload <- event$payload %||% list()
    payload$message %||% payload
  })
}

#' Branch Tree of a Chat Session
#'
#' Returns the session's conversation branch tree, initializing it with a
#' single `main` branch on first use.
#'
#' @param session A `ChatSession` object.
#' @return A list with elements `active` (branch id) and `branches`.
#' @keywords internal
#' @export
session_branch_tree <- function(session) {
  tree <- session$get_metadata("console_branch_tree", default = NULL)
  if (is.null(tree)) {
    tree <- list(
      active = "main",
      branches = list(
        main = list(
          id = "main",
          name = "main",
          parent = NULL,
          created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
          summary = NULL
        )
      )
    )
    session$merge_metadata(list(console_branch_id = "main", console_branch_tree = tree))
  }
  tree
}

#' @keywords internal
session_set_branch_tree <- function(session, tree) {
  session$merge_metadata(list(
    console_branch_id = tree$active %||% "main",
    console_branch_tree = tree
  ))
  invisible(tree)
}

#' Fork a New Conversation Branch
#'
#' Creates a new branch off the active branch and makes it active.
#'
#' @param session A `ChatSession` object.
#' @param name Optional human-readable branch name.
#' @return The new branch id (character scalar).
#' @keywords internal
#' @export
session_fork_branch <- function(session, name = NULL) {
  tree <- session_branch_tree(session)
  parent <- tree$active %||% "main"
  id <- paste0("branch_", generate_stable_id("branch", Sys.time(), stats::runif(1)))
  name <- name %||% id
  tree$branches[[id]] <- list(
    id = id,
    name = name,
    parent = parent,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
    summary = NULL
  )
  tree$active <- id
  session_set_branch_tree(session, tree)
  id
}

#' Check Out an Existing Conversation Branch
#'
#' Makes the given branch the session's active branch.
#'
#' @param session A `ChatSession` object.
#' @param branch_id Id of the branch to activate.
#' @return `TRUE` if the branch exists and was activated, else `FALSE`.
#' @keywords internal
#' @export
session_checkout_branch <- function(session, branch_id) {
  tree <- session_branch_tree(session)
  if (is.null(tree$branches[[branch_id]])) {
    return(FALSE)
  }
  tree$active <- branch_id
  session_set_branch_tree(session, tree)
  TRUE
}

#' Set the Summary of the Active Branch
#'
#' Stores a short human-readable summary on the session's active branch.
#'
#' @param session A `ChatSession` object.
#' @param summary Summary text.
#' @return Invisibly, `summary`.
#' @keywords internal
#' @export
session_set_branch_summary <- function(session, summary) {
  tree <- session_branch_tree(session)
  active <- tree$active %||% "main"
  tree$branches[[active]]$summary <- summary
  session_set_branch_tree(session, tree)
  invisible(summary)
}
