#' @title Channel Integration Types
#' @description
#' Low-level types and seams for external messaging channels.
#' These abstractions sit above providers and below UI surfaces.
#' @name channel_types
NULL

channel_runtime_abort <- function(message, class = NULL) {
  rlang::abort(message, class = c(class, "aisdk_channel_error"))
}

normalize_channel_request_headers <- function(headers = NULL) {
  if (is.null(headers)) {
    return(list())
  }

  if (is.atomic(headers) && !is.null(names(headers))) {
    header_list <- as.list(headers)
    names(header_list) <- trimws(tolower(names(headers)))
    return(header_list[nzchar(names(header_list))])
  }

  if (is.list(headers)) {
    if (length(headers) == 0) {
      return(list())
    }

    nm <- names(headers) %||% character(length(headers))
    nm <- trimws(tolower(nm))
    names(headers) <- nm
    return(headers[nzchar(nm)])
  }

  channel_runtime_abort("Channel headers must be provided as a named list.")
}

normalize_channel_body <- function(body) {
  if (is.null(body)) {
    return(NULL)
  }

  if (is.character(body) && length(body) == 1) {
    return(jsonlite::fromJSON(body, simplifyVector = FALSE))
  }

  if (is.list(body)) {
    return(body)
  }

  channel_runtime_abort("Channel request body must be a JSON string or a list.")
}

#' @title Channel Adapter
#' @description
#' Base class for transport adapters that translate external messaging
#' events into normalized `aisdk` channel events.
#' @export
ChannelAdapter <- R6::R6Class(
  "ChannelAdapter",
  public = list(
    #' @field id Unique channel identifier.
    id = NULL,

    #' @field config Adapter configuration.
    config = NULL,

    #' @description Initialize a channel adapter.
    #' @param id Channel identifier.
    #' @param config Adapter configuration list.
    initialize = function(id, config = list()) {
      if (missing(id) || !is.character(id) || !nzchar(trimws(id))) {
        channel_runtime_abort("Channel adapter id must be a non-empty string.")
      }

      self$id <- trimws(id)
      self$config <- config %||% list()
    },

    #' @description Parse a raw channel request.
    #' @param headers Request headers as a named list.
    #' @param body Raw body as JSON string or parsed list.
    #' @param ... Optional transport-specific values.
    #' @return A normalized parse result list.
    parse_request = function(headers = NULL, body = NULL, ...) {
      channel_runtime_abort(
        sprintf("Adapter '%s' does not implement parse_request().", self$id),
        class = "aisdk_channel_not_implemented"
      )
    },

    #' @description Resolve a stable session key for an inbound message.
    #' @param message Normalized inbound message list.
    #' @param policy Session policy list.
    #' @return Character scalar session key.
    resolve_session_key = function(message, policy = list()) {
      chat_scope <- message$chat_scope %||% "shared_chat"
      account_id <- message$account_id %||% "default"
      chat_id <- message$chat_id %||% message$sender_id %||% "unknown"
      thread_id <- message$thread_id %||% NULL

      if (identical(chat_scope, "per_sender")) {
        chat_id <- paste(chat_id, message$sender_id %||% "unknown", sep = ":sender:")
      }

      key <- paste(self$id, account_id, message$chat_type %||% "unknown", chat_id, sep = ":")
      if (!is.null(thread_id) && nzchar(thread_id)) {
        key <- paste(key, paste0("thread:", thread_id), sep = ":")
      }
      key
    },

    #' @description Format an inbound prompt for a `ChatSession`.
    #' @param message Normalized inbound message list.
    #' @return Character scalar prompt.
    format_inbound_message = function(message) {
      channel_format_inbound_prompt(message)
    },

    #' @description Prepare an inbound message using session state.
    #' @param session Current `ChatSession`.
    #' @param message Normalized inbound message list.
    #' @return Possibly enriched inbound message list.
    prepare_inbound_message = function(session, message) {
      message
    },

    #' @description Send a final text reply back to the channel.
    #' @param message Original normalized inbound message.
    #' @param text Final outbound text.
    #' @param ... Optional adapter-specific values.
    #' @return Transport-specific response.
    send_text = function(message, text, ...) {
      channel_runtime_abort(
        sprintf("Adapter '%s' does not implement send_text().", self$id),
        class = "aisdk_channel_not_implemented"
      )
    },

    #' @description Optionally send an intermediate status message.
    #' @param message Original normalized inbound message.
    #' @param status Status name such as "thinking", "working", or "error".
    #' @param text Optional status text override.
    #' @param ... Optional adapter-specific values.
    #' @return Adapter-specific status result, or NULL if unsupported.
    send_status = function(message, status = c("thinking", "working", "error"), text = NULL, ...) {
      invisible(NULL)
    },

    #' @description Optionally send a generated local attachment.
    #' @param message Original normalized inbound message.
    #' @param path Absolute local file path.
    #' @param ... Optional adapter-specific values.
    #' @return Adapter-specific attachment result, or NULL if unsupported.
    send_attachment = function(message, path, ...) {
      invisible(NULL)
    }
  )
)

channel_request_result <- function(
  type = c("inbound", "challenge", "ignored"),
  messages = NULL,
  payload = NULL,
  status = 200L
) {
  type <- match.arg(type)
  list(
    type = type,
    messages = messages %||% list(),
    payload = payload,
    status = as.integer(status)
  )
}

channel_inbound_message <- function(
  channel_id,
  account_id = "default",
  event_id = NULL,
  chat_id = NULL,
  chat_type = c("direct", "group"),
  thread_id = NULL,
  sender_id = NULL,
  sender_name = NULL,
  text = "",
  mentions = NULL,
  attachments = NULL,
  raw = NULL,
  metadata = NULL,
  chat_scope = c("shared_chat", "per_sender")
) {
  chat_type <- match.arg(chat_type)
  chat_scope <- match.arg(chat_scope)

  list(
    channel_id = channel_id,
    account_id = account_id,
    event_id = event_id,
    chat_id = chat_id,
    chat_type = chat_type,
    thread_id = thread_id,
    sender_id = sender_id,
    sender_name = sender_name,
    text = text %||% "",
    mentions = mentions %||% list(),
    attachments = attachments %||% list(),
    raw = raw,
    metadata = metadata %||% list(),
    chat_scope = chat_scope
  )
}

channel_format_inbound_prompt <- function(message) {
  text <- trimws(message$text %||% "")
  sender_label <- message$sender_name %||% message$sender_id %||% "unknown"
  sender_id <- message$sender_id %||% "unknown"
  lines <- character()

  if (identical(message$chat_type, "group")) {
    lines <- c(
      lines,
      sprintf("[channel: %s]", message$channel_id %||% "unknown"),
      sprintf("[chat: %s]", message$chat_id %||% "unknown"),
      sprintf("[sender: %s <%s>]", sender_label, sender_id)
    )
    if (!is.null(message$thread_id) && nzchar(message$thread_id)) {
      lines <- c(lines, sprintf("[thread: %s]", message$thread_id))
    }
  }

  if (nzchar(text)) {
    lines <- c(lines, text)
  }

  paste(lines, collapse = "\n")
}
