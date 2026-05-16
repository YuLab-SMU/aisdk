#' Console Session Event Store
#'
#' JSONL-backed session event helpers for console runs.
#'
#' @name session_event_store
NULL

#' @keywords internal
console_session_id <- function(session = NULL) {
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
console_session_store_root <- function(session = NULL, startup_dir = getwd()) {
  root <- if (!is.null(session) && inherits(session, "ChatSession")) {
    session$get_metadata("console_session_store_root", default = NULL)
  } else {
    NULL
  }
  root <- root %||% file.path(startup_dir, ".aisdk", "sessions")
  normalizePath(root, winslash = "/", mustWork = FALSE)
}

#' @keywords internal
console_session_event_path <- function(session = NULL, startup_dir = getwd()) {
  root <- console_session_store_root(session, startup_dir = startup_dir)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  file.path(root, paste0(console_session_id(session), ".jsonl"))
}

#' @keywords internal
console_append_session_event <- function(session,
                                         type,
                                         payload = list(),
                                         startup_dir = getwd(),
                                         visible = FALSE,
                                         branch_id = NULL) {
  path <- console_session_event_path(session, startup_dir = startup_dir)
  branch_id <- branch_id %||% session$get_metadata("console_branch_id", default = "main")
  event <- list(
    event_id = paste0("evt_", generate_stable_id("event", Sys.time(), stats::runif(1))),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
    type = type,
    branch_id = branch_id,
    visible = isTRUE(visible),
    payload = console_event_jsonable(payload %||% list())
  )
  line <- jsonlite::toJSON(event, auto_unbox = TRUE, null = "null")
  cat(line, "\n", file = path, append = TRUE, sep = "")
  invisible(event)
}

#' @keywords internal
console_event_jsonable <- function(x) {
  if (inherits(x, "aisdk_run_state")) {
    x <- unclass(x)
  }
  if (is.list(x)) {
    return(lapply(x, console_event_jsonable))
  }
  if (is.environment(x) || inherits(x, "R6")) {
    return(as.character(x)[[1]] %||% "<object>")
  }
  x
}

#' @keywords internal
console_read_session_events <- function(session = NULL, path = NULL, startup_dir = getwd()) {
  path <- path %||% console_session_event_path(session, startup_dir = startup_dir)
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

#' @keywords internal
console_event_visible_messages <- function(events) {
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

#' @keywords internal
console_branch_tree <- function(session) {
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
console_set_branch_tree <- function(session, tree) {
  session$merge_metadata(list(
    console_branch_id = tree$active %||% "main",
    console_branch_tree = tree
  ))
  invisible(tree)
}

#' @keywords internal
console_fork_branch <- function(session, name = NULL) {
  tree <- console_branch_tree(session)
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
  console_set_branch_tree(session, tree)
  id
}

#' @keywords internal
console_checkout_branch <- function(session, branch_id) {
  tree <- console_branch_tree(session)
  if (is.null(tree$branches[[branch_id]])) {
    return(FALSE)
  }
  tree$active <- branch_id
  console_set_branch_tree(session, tree)
  TRUE
}

#' @keywords internal
console_set_branch_summary <- function(session, summary) {
  tree <- console_branch_tree(session)
  active <- tree$active %||% "main"
  tree$branches[[active]]$summary <- summary
  console_set_branch_tree(session, tree)
  invisible(summary)
}
