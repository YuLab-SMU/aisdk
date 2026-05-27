#' @title Channel Session Store
#' @description
#' Durable local storage for channel-driven chat sessions and their routing metadata.
#' @name channel_session_store
NULL

channel_write_json_atomic <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = "aisdk_channel_", fileext = ".json", tmpdir = dirname(path))
  json <- jsonlite::toJSON(data, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(json, tmp, useBytes = TRUE)
  ok <- file.rename(tmp, path)
  if (!ok) {
    unlink(tmp)
    channel_runtime_abort(sprintf("Failed to write channel index to '%s'.", path))
  }
  invisible(path)
}

channel_overwrite_record_fields <- function(current, updates = NULL) {
  if (is.null(updates) || length(updates) == 0) {
    return(current)
  }

  next_record <- current
  for (key in names(updates)) {
    next_record[[key]] <- updates[[key]]
  }
  next_record
}

channel_hash_session_key <- function(session_key) {
  digest::digest(enc2utf8(session_key), algo = "sha256")
}

channel_normalize_participants <- function(participants = NULL) {
  if (is.null(participants) || length(participants) == 0) {
    return(list())
  }

  if (!is.list(participants)) {
    return(list())
  }

  out <- list()
  for (entry in participants) {
    if (!is.list(entry)) {
      next
    }
    participant_id <- entry$sender_id %||% entry$id %||% NULL
    if (is.null(participant_id) || !nzchar(participant_id)) {
      next
    }
    out[[participant_id]] <- list(
      sender_id = participant_id,
      sender_name = entry$sender_name %||% entry$name %||% NULL,
      last_seen_at = entry$last_seen_at %||% NULL
    )
  }
  unname(out)
}

#' @title Channel Session Store
#' @description
#' Abstract persistence seam for channel-driven sessions.
#' @export
ChannelSessionStore <- R6::R6Class(
  "ChannelSessionStore",
  public = list(
    #' @description Load a persisted `ChatSession`.
    #' @param session_key Session key.
    #' @param tools Optional tools to reattach.
    #' @param hooks Optional hooks to reattach.
    #' @param registry Optional provider registry.
    #' @return A `ChatSession` or NULL.
    load_session = function(session_key, tools = NULL, hooks = NULL, registry = NULL) {
      channel_runtime_abort(
        "ChannelSessionStore$load_session() must be implemented by subclass.",
        class = "aisdk_channel_not_implemented"
      )
    },
    #' @description Save a `ChatSession` and update store metadata.
    #' @param session_key Session key.
    #' @param session `ChatSession` instance.
    #' @param record Optional record fields to merge.
    #' @return Store-specific save result.
    save_session = function(session_key, session, record = NULL) {
      channel_runtime_abort(
        "ChannelSessionStore$save_session() must be implemented by subclass.",
        class = "aisdk_channel_not_implemented"
      )
    },
    #' @description Update a store record without persisting a session file.
    #' @param session_key Session key.
    #' @param record Record fields to merge.
    #' @return Store-specific update result.
    update_record = function(session_key, record) {
      channel_runtime_abort(
        "ChannelSessionStore$update_record() must be implemented by subclass.",
        class = "aisdk_channel_not_implemented"
      )
    },
    #' @description Retrieve a single session record.
    #' @param session_key Session key.
    #' @return Store-specific record object.
    get_record = function(session_key) {
      channel_runtime_abort(
        "ChannelSessionStore$get_record() must be implemented by subclass.",
        class = "aisdk_channel_not_implemented"
      )
    },
    #' @description List all session records.
    #' @return Store-specific collection of session records.
    list_sessions = function() {
      channel_runtime_abort(
        "ChannelSessionStore$list_sessions() must be implemented by subclass.",
        class = "aisdk_channel_not_implemented"
      )
    },
    #' @description Check whether an event id has already been processed.
    #' @param channel_id Channel identifier.
    #' @param event_id Event identifier.
    #' @return Logical scalar.
    has_processed_event = function(channel_id, event_id) {
      channel_runtime_abort(
        "ChannelSessionStore$has_processed_event() must be implemented by subclass.",
        class = "aisdk_channel_not_implemented"
      )
    },
    #' @description Mark an event id as processed.
    #' @param channel_id Channel identifier.
    #' @param event_id Event identifier.
    #' @param payload Optional event payload to keep in the dedupe index.
    #' @return Store-specific event record.
    mark_processed_event = function(channel_id, event_id, payload = NULL) {
      channel_runtime_abort(
        "ChannelSessionStore$mark_processed_event() must be implemented by subclass.",
        class = "aisdk_channel_not_implemented"
      )
    },
    #' @description Register a child session relationship.
    #' @param parent_session_key Parent session key.
    #' @param child_session_key Child session key.
    #' @return Store-specific link result.
    link_child_session = function(parent_session_key, child_session_key) {
      channel_runtime_abort(
        "ChannelSessionStore$link_child_session() must be implemented by subclass.",
        class = "aisdk_channel_not_implemented"
      )
    }
  )
)

#' @title File Channel Session Store
#' @description
#' File-backed session store for external messaging channels.
#' @export
FileChannelSessionStore <- R6::R6Class(
  "FileChannelSessionStore",
  inherit = ChannelSessionStore,
  public = list(
    #' @field base_dir Base directory for persisted channel sessions.
    base_dir = NULL,

    #' @description Initialize a file-backed channel session store.
    #' @param base_dir Base directory for store files.
    initialize = function(base_dir) {
      if (missing(base_dir) || !is.character(base_dir) || !nzchar(trimws(base_dir))) {
        channel_runtime_abort("FileChannelSessionStore requires a non-empty base_dir.")
      }

      self$base_dir <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
      dir.create(private$get_sessions_dir(), recursive = TRUE, showWarnings = FALSE)
      if (!file.exists(self$get_index_path())) {
        channel_write_json_atomic(
          list(version = "1.0.0", sessions = list(), events = list()),
          self$get_index_path()
        )
      }
    },

    #' @description Get the on-disk session file path for a key.
    #' @param session_key Session key.
    #' @return Absolute file path.
    get_session_path = function(session_key) {
      file.path(private$get_sessions_dir(), paste0(channel_hash_session_key(session_key), ".rds"))
    },

    #' @description Get the channel index path.
    #' @return Absolute file path.
    get_index_path = function() {
      file.path(self$base_dir, "index.json")
    },

    #' @description List all session records.
    #' @return Named list of session records.
    list_sessions = function() {
      private$read_index()$sessions %||% list()
    },

    #' @description Check whether an event id has already been processed.
    #' @param channel_id Channel identifier.
    #' @param event_id Event identifier.
    #' @return Logical scalar.
    has_processed_event = function(channel_id, event_id) {
      if (is.null(event_id) || !nzchar(event_id)) {
        return(FALSE)
      }
      index <- private$read_index()
      bucket <- index$events[[channel_id]] %||% list()
      !is.null(bucket[[event_id]])
    },

    #' @description Mark an event id as processed.
    #' @param channel_id Channel identifier.
    #' @param event_id Event identifier.
    #' @param payload Optional event payload to keep in the dedupe index.
    #' @return Invisible stored event record.
    mark_processed_event = function(channel_id, event_id, payload = NULL) {
      if (is.null(event_id) || !nzchar(event_id)) {
        return(invisible(NULL))
      }
      index <- private$read_index()
      index$events[[channel_id]] <- index$events[[channel_id]] %||% list()
      index$events[[channel_id]][[event_id]] <- list(
        processed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
        payload = payload
      )
      private$write_index(index)
      invisible(index$events[[channel_id]][[event_id]])
    },

    #' @description Get a single session record.
    #' @param session_key Session key.
    #' @return Session record or NULL.
    get_record = function(session_key) {
      self$list_sessions()[[session_key]] %||% NULL
    },

    #' @description Load a persisted `ChatSession`.
    #' @param session_key Session key.
    #' @param tools Optional tools to reattach.
    #' @param hooks Optional hooks to reattach.
    #' @param registry Optional provider registry.
    #' @return A `ChatSession` or NULL if no persisted state exists.
    load_session = function(session_key, tools = NULL, hooks = NULL, registry = NULL) {
      path <- self$get_session_path(session_key)
      if (!file.exists(path)) {
        return(NULL)
      }
      load_chat_session(path, tools = tools, hooks = hooks, registry = registry)
    },

    #' @description Save a `ChatSession` and update the local index.
    #' @param session_key Session key.
    #' @param session `ChatSession` instance.
    #' @param record Optional record fields to merge into the index.
    #' @return Invisible normalized record.
    save_session = function(session_key, session, record = NULL) {
      if (!inherits(session, "ChatSession")) {
        channel_runtime_abort("save_session() requires a ChatSession instance.")
      }

      path <- self$get_session_path(session_key)
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      session$save(path)

      now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
      current <- self$get_record(session_key) %||% list(
        session_key = session_key,
        created_at = now,
        child_session_keys = list()
      )

      next_record <- channel_overwrite_record_fields(current, record %||% list())
      next_record$session_key <- session_key
      next_record$updated_at <- now
      next_record$session_file <- path
      next_record$participants <- channel_normalize_participants(next_record$participants)
      next_record$child_session_keys <- unique(unlist(next_record$child_session_keys %||% list(), use.names = FALSE))

      private$upsert_record(session_key, next_record)
      invisible(next_record)
    },

    #' @description Update a record without saving a session file.
    #' @param session_key Session key.
    #' @param record Record fields to merge.
    #' @return Invisible updated record.
    update_record = function(session_key, record) {
      current <- self$get_record(session_key) %||% list(
        session_key = session_key,
        created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
        child_session_keys = list()
      )

      next_record <- channel_overwrite_record_fields(current, record %||% list())
      next_record$participants <- channel_normalize_participants(next_record$participants)
      next_record$child_session_keys <- unique(unlist(next_record$child_session_keys %||% list(), use.names = FALSE))
      next_record$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

      private$upsert_record(session_key, next_record)
      invisible(next_record)
    },

    #' @description Register a child session relationship.
    #' @param parent_session_key Parent session key.
    #' @param child_session_key Child session key.
    #' @return Invisible updated parent record.
    link_child_session = function(parent_session_key, child_session_key) {
      parent <- self$get_record(parent_session_key) %||% list(
        session_key = parent_session_key,
        created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
        child_session_keys = list()
      )
      parent$child_session_keys <- unique(c(parent$child_session_keys %||% list(), child_session_key))
      self$update_record(parent_session_key, parent)
    }
  ),
  private = list(
    get_sessions_dir = function() {
      file.path(self$base_dir, "sessions")
    },

    read_index = function() {
      path <- self$get_index_path()
      if (!file.exists(path)) {
        return(list(version = "1.0.0", sessions = list(), events = list()))
      }

      data <- jsonlite::fromJSON(
        paste(readLines(path, warn = FALSE), collapse = "\n"),
        simplifyVector = FALSE
      )
      if (is.null(data$sessions) || !is.list(data$sessions)) {
        data$sessions <- list()
      }
      if (is.null(data$events) || !is.list(data$events)) {
        data$events <- list()
      }
      data
    },

    write_index = function(index) {
      channel_write_json_atomic(index, self$get_index_path())
    },

    upsert_record = function(session_key, record) {
      index <- private$read_index()
      index$sessions[[session_key]] <- record
      private$write_index(index)
    }
  )
)

#' @title Create a File Channel Session Store
#' @description
#' Helper for creating a local file-backed channel session store.
#' @param base_dir Base directory for channel session state.
#' @return A `FileChannelSessionStore`.
#' @export
create_file_channel_session_store <- function(base_dir) {
  FileChannelSessionStore$new(base_dir = base_dir)
}
