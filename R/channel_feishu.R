#' @title Feishu Channel Adapter
#' @description
#' Feishu adapter built on top of the generic channel runtime seam.
#' Phase 1 focuses on text events and final text replies.
#' @name channel_feishu
NULL

feishu_raw_body_text <- function(body) {
  if (is.null(body)) {
    return("")
  }
  if (is.character(body) && length(body) == 1) {
    return(body)
  }
  jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
}

feishu_extract_json_text <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    channel_runtime_abort("Feishu decrypted payload is empty.")
  }

  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  start <- match("{", chars)
  end <- tail(which(chars == "}"), 1)
  if (is.na(start) || is.na(end) || end < start) {
    channel_runtime_abort("Feishu decrypted payload did not contain a JSON object.")
  }
  paste(chars[start:end], collapse = "")
}

feishu_compute_signature <- function(timestamp, nonce, encrypt_key, raw_body) {
  digest::digest(
    paste0(timestamp %||% "", nonce %||% "", encrypt_key %||% "", raw_body %||% ""),
    algo = "sha256",
    serialize = FALSE
  )
}

feishu_validate_signature <- function(headers, encrypt_key, raw_body) {
  signature <- headers[["x-lark-signature"]] %||% headers[["x-lark-sign"]] %||% NULL
  timestamp <- headers[["x-lark-request-timestamp"]] %||% NULL
  nonce <- headers[["x-lark-request-nonce"]] %||% NULL

  if (is.null(signature) || is.null(timestamp) || is.null(nonce)) {
    channel_runtime_abort("Feishu callback is missing signature headers.")
  }

  expected <- feishu_compute_signature(timestamp, nonce, encrypt_key, raw_body)
  identical(tolower(signature), tolower(expected))
}

feishu_decrypt_payload <- function(encrypt, encrypt_key) {
  if (is.null(encrypt) || !nzchar(encrypt)) {
    channel_runtime_abort("Feishu encrypted payload is empty.")
  }
  cipher_raw <- base64enc::base64decode(encrypt)
  if (length(cipher_raw) <= 16) {
    channel_runtime_abort("Feishu encrypted payload is too short.")
  }

  iv <- cipher_raw[1:16]
  cipher_text <- cipher_raw[-(1:16)]
  key <- openssl::sha256(charToRaw(enc2utf8(encrypt_key)))
  plain_raw <- openssl::aes_cbc_decrypt(cipher_text, key = key, iv = iv)
  json_text <- feishu_extract_json_text(rawToChar(plain_raw))
  jsonlite::fromJSON(json_text, simplifyVector = FALSE)
}

feishu_parse_text_content <- function(content) {
  if (is.null(content)) {
    return("")
  }

  if (is.list(content)) {
    return(trimws(content$text %||% ""))
  }

  if (is.character(content) && length(content) == 1) {
    parsed <- tryCatch(
      jsonlite::fromJSON(content, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.list(parsed)) {
      return(trimws(parsed$text %||% ""))
    }
    return(trimws(content))
  }

  ""
}

feishu_parse_mentions <- function(mentions) {
  if (is.null(mentions) || !is.list(mentions)) {
    return(list())
  }

  out <- list()
  for (mention in mentions) {
    if (!is.list(mention)) {
      next
    }
    out[[length(out) + 1L]] <- list(
      key = mention$key %||% NULL,
      name = mention$name %||% NULL,
      id = mention$id$open_id %||% mention$id$user_id %||% mention$id$union_id %||% NULL
    )
  }
  out
}

feishu_resolve_sender_id <- function(sender) {
  if (is.null(sender) || !is.list(sender)) {
    return(NULL)
  }
  sender$sender_id$open_id %||% sender$sender_id$user_id %||% sender$sender_id$union_id %||% NULL
}

feishu_resolve_sender_name <- function(sender) {
  if (is.null(sender) || !is.list(sender)) {
    return(NULL)
  }
  sender$sender_type %||% sender$tenant_key %||% feishu_resolve_sender_id(sender)
}

feishu_is_image_path <- function(path) {
  ext <- tolower(tools::file_ext(path))
  ext %in% c("png", "jpg", "jpeg", "webp", "gif", "tif", "tiff", "bmp", "ico")
}

feishu_guess_file_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    mp4 = "mp4",
    pdf = "pdf",
    doc = "doc",
    docx = "doc",
    xls = "xls",
    xlsx = "xls",
    ppt = "ppt",
    pptx = "ppt",
    "stream"
  )
}

feishu_parse_message_content <- function(content) {
  if (is.null(content)) {
    return(list())
  }
  if (is.list(content)) {
    return(content)
  }
  if (is.character(content) && length(content) == 1 && nzchar(content)) {
    parsed <- tryCatch(
      jsonlite::fromJSON(content, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.list(parsed)) {
      return(parsed)
    }
  }
  list()
}

feishu_trim_preview <- function(text, max_chars = 1500) {
  if (is.null(text) || !nzchar(text)) {
    return(NULL)
  }
  cleaned <- gsub("[[:cntrl:]]+", " ", text)
  cleaned <- gsub("\\s+", " ", cleaned)
  cleaned <- trimws(cleaned)
  if (!nzchar(cleaned)) {
    return(NULL)
  }
  substr(cleaned, 1, max_chars)
}

feishu_extract_file_preview <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("txt", "md", "csv", "tsv", "json", "yaml", "yml")) {
    lines <- tryCatch(readLines(path, warn = FALSE, n = 40), error = function(e) character(0))
    return(feishu_trim_preview(paste(lines, collapse = "\n")))
  }

  if (ext == "pdf" && nzchar(Sys.which("strings"))) {
    text <- tryCatch(
      paste(utils::head(system2("strings", c("-n", "8", path), stdout = TRUE, stderr = FALSE), 80), collapse = "\n"),
      error = function(e) NULL
    )
    return(feishu_trim_preview(text))
  }

  NULL
}

#' @title Feishu Channel Adapter
#' @description
#' Transport adapter for Feishu/Lark event callbacks and text replies.
#' @export
FeishuChannelAdapter <- R6::R6Class(
  "FeishuChannelAdapter",
  inherit = ChannelAdapter,
  public = list(
    #' @description Initialize the Feishu adapter.
    #' @param app_id Feishu app id.
    #' @param app_secret Feishu app secret.
    #' @param base_url Feishu API base URL.
    #' @param verification_token Optional callback verification token.
    #' @param encrypt_key Optional event subscription encryption key.
    #' @param verify_signature Whether to validate Feishu callback signatures when applicable.
    #' @param send_text_fn Optional custom send function for tests or overrides.
    #' @param send_status_fn Optional custom status function for tests or overrides.
    #' @param download_resource_fn Optional custom downloader for inbound message resources.
    initialize = function(app_id,
                          app_secret,
                          base_url = "https://open.feishu.cn",
                          verification_token = NULL,
                          encrypt_key = NULL,
                          verify_signature = TRUE,
                          send_text_fn = NULL,
                          send_status_fn = NULL,
                          download_resource_fn = NULL) {
      config <- list(
        app_id = app_id,
        app_secret = app_secret,
        base_url = sub("/+$", "", base_url),
        verification_token = verification_token,
        encrypt_key = encrypt_key,
        verify_signature = isTRUE(verify_signature),
        send_text_fn = send_text_fn,
        send_status_fn = send_status_fn,
        download_resource_fn = download_resource_fn
      )
      super$initialize(id = "feishu", config = config)
    },

    #' @description Parse a Feishu callback request.
    #' @param headers Request headers.
    #' @param body Raw JSON string or parsed list.
    #' @param ... Unused.
    #' @return Channel request result.
    parse_request = function(headers = NULL, body = NULL, ...) {
      body_is_raw <- is.character(body) && length(body) == 1
      headers <- normalize_channel_request_headers(headers)
      raw_body <- feishu_raw_body_text(body)
      payload <- normalize_channel_body(body)

      if (is.null(payload)) {
        return(channel_request_result(type = "ignored", payload = list(reason = "empty_body")))
      }

      has_encrypt <- !is.null(payload$encrypt) && nzchar(payload$encrypt %||% "")
      has_signature_headers <- !is.null(headers[["x-lark-signature"]]) ||
        !is.null(headers[["x-lark-sign"]])
      encrypt_key <- self$config$encrypt_key %||% NULL

      if ((has_encrypt || has_signature_headers) && isTRUE(self$config$verify_signature)) {
        if (!isTRUE(body_is_raw)) {
          channel_runtime_abort(
            "Feishu signature validation requires the original raw request body string."
          )
        }
        if (is.null(encrypt_key) || !nzchar(encrypt_key)) {
          channel_runtime_abort("Feishu signature validation requires encrypt_key to be configured.")
        }
        if (!isTRUE(feishu_validate_signature(headers, encrypt_key, raw_body))) {
          channel_runtime_abort("Feishu callback signature validation failed.")
        }
      }

      if (has_encrypt) {
        if (is.null(encrypt_key) || !nzchar(encrypt_key)) {
          channel_runtime_abort("Feishu encrypted callbacks require encrypt_key to be configured.")
        }
        payload <- feishu_decrypt_payload(payload$encrypt, encrypt_key)
      }

      callback_token <- payload$token %||% payload$header$token %||% NULL
      expected_token <- self$config$verification_token %||% NULL
      if (!is.null(expected_token) && nzchar(expected_token)) {
        if (is.null(callback_token) || !identical(callback_token, expected_token)) {
          channel_runtime_abort("Feishu callback verification token mismatch.")
        }
      }

      if (identical(payload$type %||% "", "url_verification")) {
        return(channel_request_result(
          type = "challenge",
          payload = list(challenge = payload$challenge %||% NULL),
          status = 200L
        ))
      }

      event_type <- payload$header$event_type %||% NULL
      if (!identical(event_type, "im.message.receive_v1")) {
        return(channel_request_result(
          type = "ignored",
          payload = list(reason = "unsupported_event", event_type = event_type)
        ))
      }

      event <- payload$event %||% list()
      message <- event$message %||% list()
      sender <- event$sender %||% list()
      parsed_content <- feishu_parse_message_content(message$content)
      attachments <- private$resolve_inbound_attachments(message, parsed_content)

      normalized <- channel_inbound_message(
        channel_id = self$id,
        account_id = self$config$app_id %||% "default",
        event_id = payload$header$event_id %||% message$message_id %||% NULL,
        chat_id = message$chat_id %||% NULL,
        chat_type = if (identical(message$chat_type %||% "", "p2p")) "direct" else "group",
        thread_id = message$root_id %||% message$parent_id %||% NULL,
        sender_id = feishu_resolve_sender_id(sender),
        sender_name = feishu_resolve_sender_name(sender),
        text = feishu_parse_text_content(message$content),
        mentions = feishu_parse_mentions(event$mentions),
        attachments = attachments,
        raw = payload,
        metadata = list(
          message_id = message$message_id %||% NULL,
          message_type = message$message_type %||% NULL,
          parsed_content = parsed_content,
          chat_type_raw = message$chat_type %||% NULL,
          header_timestamp = payload$header$create_time %||% NULL,
          request_headers = headers
        ),
        chat_scope = if (identical(message$chat_type %||% "", "p2p")) "per_sender" else "shared_chat"
      )

      channel_request_result(type = "inbound", messages = list(normalized), status = 200L)
    },

    #' @description Resolve a stable session key for a Feishu inbound message.
    #' @param message Normalized inbound message.
    #' @param policy Session policy list.
    #' @return Character scalar session key.
    resolve_session_key = function(message, policy = list()) {
      account_id <- message$account_id %||% "default"
      chat_type <- message$chat_type %||% "group"
      thread_mode <- policy$thread %||% "per_thread"
      thread_id <- message$thread_id %||% NULL

      if (identical(chat_type, "direct")) {
        peer_id <- message$sender_id %||% message$chat_id %||% "unknown"
        return(paste(self$id, account_id, "direct", peer_id, sep = ":"))
      }

      key <- paste(self$id, account_id, "group", message$chat_id %||% "unknown", sep = ":")
      if (!is.null(thread_id) && nzchar(thread_id) && identical(thread_mode, "per_thread")) {
        key <- paste(key, paste0("thread:", thread_id), sep = ":")
      }
      key
    },

    #' @description Format a Feishu inbound message for a `ChatSession`.
    #' @param message Normalized inbound message.
    #' @return Character scalar prompt.
    format_inbound_message = function(message) {
      lines <- character()

      if (identical(message$chat_type, "group")) {
        lines <- c(
          lines,
          sprintf("[channel: feishu]"),
          sprintf("[chat_id: %s]", message$chat_id %||% "unknown"),
          sprintf("[sender: %s <%s>]",
                  message$sender_name %||% message$sender_id %||% "unknown",
                  message$sender_id %||% "unknown")
        )
        if (!is.null(message$thread_id) && nzchar(message$thread_id)) {
          lines <- c(lines, sprintf("[thread_id: %s]", message$thread_id))
        }
      }

      text <- trimws(message$text %||% "")
      if (nzchar(text)) {
        lines <- c(lines, text)
      } else if (!is.null(message$metadata$document_context) && nzchar(message$metadata$document_context)) {
        lines <- c(lines, message$metadata$document_context)
      } else if (length(message$attachments %||% list()) > 0) {
        lines <- c(lines, "[收到附件消息]")
        for (attachment in message$attachments) {
          lines <- c(
            lines,
            sprintf("[attachment_type: %s]", attachment$type %||% "unknown"),
            sprintf("[attachment_name: %s]", attachment$file_name %||% "unknown"),
            sprintf("[attachment_path: %s]", attachment$local_path %||% "unknown")
          )
        }
      } else {
        lines <- c(lines, "[non-text feishu message omitted in phase 1]")
      }

      paste(lines, collapse = "\n")
    },

    #' @description Prepare a Feishu inbound message using stored document context.
    #' @param session Current `ChatSession`.
    #' @param message Normalized inbound message.
    #' @return Enriched inbound message.
    prepare_inbound_message = function(session, message) {
      docs <- session$get_metadata("channel_documents", default = list())

      if (length(message$attachments %||% list()) > 0) {
        new_docs <- lapply(message$attachments, channel_build_document_record)
        docs <- c(docs, new_docs)
        session$set_metadata("channel_documents", docs)
        message$metadata$document_ids <- vapply(new_docs, function(doc) doc$document_id, character(1))
        message$metadata$document_context <- channel_format_document_context(new_docs, max_docs = 1, max_chunks = 2)
        return(message)
      }

      text <- trimws(message$text %||% "")
      mentions_document <- nzchar(text) && grepl(
        "刚上传|这个pdf|这篇文献|这个文件|该文献|这份pdf|这份文献|uploaded pdf|this pdf|the paper|document",
        text,
        ignore.case = TRUE
      )

      if (mentions_document && length(docs) > 0) {
        doc_context <- channel_format_document_context(docs, max_docs = 1, max_chunks = 2)
        if (!is.null(doc_context) && nzchar(doc_context)) {
          message$text <- paste(doc_context, "", text, sep = "\n")
          message$metadata$document_ids <- vapply(tail(docs, 1), function(doc) doc$document_id, character(1))
        }
      }

      message
    },

    #' @description Send a final text reply to Feishu.
    #' @param message Original normalized inbound message.
    #' @param text Final outbound text.
    #' @param ... Unused.
    #' @return Parsed API response.
    send_text = function(message, text, ...) {
      custom_send <- self$config$send_text_fn %||% NULL
      if (is.function(custom_send)) {
        return(custom_send(message = message, text = text, ...))
      }

      private$send_text_impl(message = message, text = text)
    },

    #' @description Send an intermediate status message to Feishu.
    #' @param message Original normalized inbound message.
    #' @param status Status name.
    #' @param text Optional status text.
    #' @param ... Unused.
    #' @return Parsed API response or NULL.
    send_status = function(message, status = c("thinking", "working", "error"), text = NULL, ...) {
      status <- match.arg(status)

      custom_status <- self$config$send_status_fn %||% NULL
      if (is.function(custom_status)) {
        return(custom_status(message = message, status = status, text = text, ...))
      }

      status_text <- text %||% switch(
        status,
        thinking = "\U0001F914 正在思考...",
        working = "\U0001F4BB 正在处理...",
        error = "\u26A0\ufe0f 处理时发生错误"
      )

      private$send_text_impl(message = message, text = status_text)
    },

    #' @description Send a generated local attachment to Feishu.
    #' @param message Original normalized inbound message.
    #' @param path Absolute local file path.
    #' @param ... Unused.
    #' @return Parsed API response or NULL.
    send_attachment = function(message, path, ...) {
      if (is.null(path) || !nzchar(path) || !file.exists(path)) {
        channel_runtime_abort("Feishu attachment path does not exist.")
      }

      if (feishu_is_image_path(path)) {
        image_key <- private$upload_image(path)
        return(private$send_media_message(
          message = message,
          msg_type = "image",
          content = list(image_key = image_key)
        ))
      }

      file_key <- private$upload_file(path)
      private$send_media_message(
        message = message,
        msg_type = "file",
        content = list(file_key = file_key)
      )
    }
  ),
  private = list(
    .tenant_access_token = NULL,
    .tenant_access_token_expires_at = NULL,

    send_text_impl = function(message, text) {
      token <- private$get_tenant_access_token()
      receive <- private$resolve_receive_target(message)

      post_to_api(
        url = sprintf(
          "%s/open-apis/im/v1/messages?receive_id_type=%s",
          self$config$base_url,
          receive$receive_id_type
        ),
        headers = list(
          Authorization = paste("Bearer", token)
        ),
        body = list(
          receive_id = receive$receive_id,
          msg_type = "text",
          content = jsonlite::toJSON(list(text = text), auto_unbox = TRUE)
        )
      )
    },

    resolve_receive_target = function(message) {
      receive_id_type <- if (!is.null(message$chat_id) && nzchar(message$chat_id)) {
        "chat_id"
      } else {
        "open_id"
      }
      receive_id <- if (identical(receive_id_type, "chat_id")) {
        message$chat_id
      } else {
        message$sender_id
      }

      if (is.null(receive_id) || !nzchar(receive_id)) {
        channel_runtime_abort("Feishu outbound reply requires a receive_id.")
      }

      list(receive_id_type = receive_id_type, receive_id = receive_id)
    },

    upload_image = function(path) {
      token <- private$get_tenant_access_token()
      req <- httr2::request(sprintf("%s/open-apis/im/v1/images", self$config$base_url))
      req <- httr2::req_headers(req, Authorization = paste("Bearer", token))
      req <- httr2::req_body_multipart(
        req,
        image_type = "message",
        image = curl::form_file(path)
      )
      req <- httr2::req_error(req, is_error = function(resp) FALSE)
      resp <- httr2::req_perform(req)
      status <- httr2::resp_status(resp)
      body <- tryCatch(httr2::resp_body_json(resp), error = function(e) list())

      code_ok <- isTRUE((body$code %||% 0) == 0)
      if (status < 200 || status >= 300 || !code_ok) {
        rlang::abort(c(
          "Feishu image upload failed",
          "i" = paste0("Path: ", path),
          "x" = httr2::resp_body_string(resp)
        ))
      }

      image_key <- body$data$image_key %||% NULL
      if (is.null(image_key) || !nzchar(image_key)) {
        channel_runtime_abort("Feishu image upload did not return image_key.")
      }
      image_key
    },

    upload_file = function(path) {
      token <- private$get_tenant_access_token()
      req <- httr2::request(sprintf("%s/open-apis/im/v1/files", self$config$base_url))
      req <- httr2::req_headers(req, Authorization = paste("Bearer", token))
      req <- httr2::req_body_multipart(
        req,
        file_type = feishu_guess_file_type(path),
        file_name = basename(path),
        file = curl::form_file(path)
      )
      req <- httr2::req_error(req, is_error = function(resp) FALSE)
      resp <- httr2::req_perform(req)
      status <- httr2::resp_status(resp)
      body <- tryCatch(httr2::resp_body_json(resp), error = function(e) list())

      code_ok <- isTRUE((body$code %||% 0) == 0)
      if (status < 200 || status >= 300 || !code_ok) {
        rlang::abort(c(
          "Feishu file upload failed",
          "i" = paste0("Path: ", path),
          "x" = httr2::resp_body_string(resp)
        ))
      }

      file_key <- body$data$file_key %||% NULL
      if (is.null(file_key) || !nzchar(file_key)) {
        channel_runtime_abort("Feishu file upload did not return file_key.")
      }
      file_key
    },

    send_media_message = function(message, msg_type, content) {
      token <- private$get_tenant_access_token()
      receive <- private$resolve_receive_target(message)

      post_to_api(
        url = sprintf(
          "%s/open-apis/im/v1/messages?receive_id_type=%s",
          self$config$base_url,
          receive$receive_id_type
        ),
        headers = list(
          Authorization = paste("Bearer", token)
        ),
        body = list(
          receive_id = receive$receive_id,
          msg_type = msg_type,
          content = jsonlite::toJSON(content, auto_unbox = TRUE)
        )
      )
    },

    resolve_inbound_attachments = function(message, parsed_content) {
      message_type <- message$message_type %||% "unknown"
      if (!message_type %in% c("file", "image", "media")) {
        return(list())
      }

      file_key <- parsed_content$file_key %||% parsed_content$image_key %||% NULL
      if (is.null(file_key) || !nzchar(file_key)) {
        return(list())
      }

      file_name <- parsed_content$file_name %||% basename(file_key)
      local_path <- tryCatch(
        {
          custom_downloader <- self$config$download_resource_fn %||% NULL
          if (is.function(custom_downloader)) {
            custom_downloader(
              message_id = message$message_id %||% NULL,
              file_key = file_key,
              type = message_type,
              file_name = file_name
            )
          } else {
            private$download_message_resource(
              message_id = message$message_id %||% NULL,
              file_key = file_key,
              type = message_type,
              file_name = file_name
            )
          }
        },
        error = function(e) NULL
      )

      list(list(
        type = message_type,
        file_key = file_key,
        file_name = file_name,
        local_path = local_path,
        preview = if (!is.null(local_path)) feishu_extract_file_preview(local_path) else NULL
      ))
    },

    download_message_resource = function(message_id, file_key, type, file_name) {
      if (is.null(message_id) || !nzchar(message_id)) {
        channel_runtime_abort("Feishu resource download requires message_id.")
      }

      token <- private$get_tenant_access_token()
      download_dir <- file.path(tempdir(), "aisdk_feishu_inbound")
      dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
      local_path <- file.path(download_dir, basename(file_name %||% file_key))

      req <- httr2::request(sprintf(
        "%s/open-apis/im/v1/messages/%s/resources/%s",
        self$config$base_url,
        message_id,
        file_key
      ))
      req <- httr2::req_headers(req, Authorization = paste("Bearer", token))
      req <- httr2::req_url_query(req, type = type)
      req <- httr2::req_error(req, is_error = function(resp) FALSE)
      resp <- httr2::req_perform(req)
      status <- httr2::resp_status(resp)
      if (status < 200 || status >= 300) {
        rlang::abort(c(
          "Feishu inbound resource download failed",
          "i" = paste0("message_id: ", message_id),
          "i" = paste0("file_key: ", file_key),
          "x" = httr2::resp_body_string(resp)
        ))
      }

      writeBin(httr2::resp_body_raw(resp), local_path)
      normalizePath(local_path, winslash = "/", mustWork = FALSE)
    },

    get_tenant_access_token = function() {
      now <- as.numeric(Sys.time())
      expires_at <- private$.tenant_access_token_expires_at %||% 0
      if (!is.null(private$.tenant_access_token) && now < (expires_at - 60)) {
        return(private$.tenant_access_token)
      }

      resp <- post_to_api(
        url = sprintf("%s/open-apis/auth/v3/tenant_access_token/internal", self$config$base_url),
        headers = list(),
        body = list(
          app_id = self$config$app_id,
          app_secret = self$config$app_secret
        )
      )

      token <- resp$tenant_access_token %||% NULL
      expire <- resp$expire %||% 7200
      if (is.null(token) || !nzchar(token)) {
        channel_runtime_abort("Failed to obtain Feishu tenant_access_token.")
      }

      private$.tenant_access_token <- token
      private$.tenant_access_token_expires_at <- now + as.numeric(expire)
      token
    }
  )
)

#' @title Create a Feishu Channel Adapter
#' @description
#' Helper for creating a `FeishuChannelAdapter`.
#' @param app_id Feishu app id.
#' @param app_secret Feishu app secret.
#' @param base_url Feishu API base URL.
#' @param verification_token Optional callback verification token.
#' @param encrypt_key Optional event subscription encryption key.
#' @param verify_signature Whether to validate Feishu callback signatures when applicable.
#' @param send_text_fn Optional custom send function for tests or overrides.
#' @param send_status_fn Optional custom status function for tests or overrides.
#' @param download_resource_fn Optional custom downloader for inbound message resources.
#' @return A `FeishuChannelAdapter`.
#' @export
create_feishu_channel_adapter <- function(app_id,
                                          app_secret,
                                          base_url = "https://open.feishu.cn",
                                          verification_token = NULL,
                                          encrypt_key = NULL,
                                          verify_signature = TRUE,
                                          send_text_fn = NULL,
                                          send_status_fn = NULL,
                                          download_resource_fn = NULL) {
  FeishuChannelAdapter$new(
    app_id = app_id,
    app_secret = app_secret,
    base_url = base_url,
    verification_token = verification_token,
    encrypt_key = encrypt_key,
    verify_signature = verify_signature,
    send_text_fn = send_text_fn,
    send_status_fn = send_status_fn,
    download_resource_fn = download_resource_fn
  )
}

feishu_http_json_response <- function(status = 200L, payload = list()) {
  list(
    status = as.integer(status),
    headers = list("Content-Type" = "application/json; charset=utf-8"),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  )
}

feishu_http_read_body <- function(req) {
  if (!is.null(req$body)) {
    return(req$body)
  }

  input <- req$rook.input %||% NULL
  if (!is.null(input) && is.function(input$read)) {
    raw <- input$read()
    if (is.raw(raw)) {
      return(rawToChar(raw))
    }
    if (is.character(raw)) {
      return(paste(raw, collapse = ""))
    }
  }

  ""
}

#' @title Create a Feishu Channel Runtime
#' @description
#' Construct a `ChannelRuntime` and register a Feishu adapter on it.
#' @param session_store Channel session store.
#' @param app_id Feishu app id.
#' @param app_secret Feishu app secret.
#' @param base_url Feishu API base URL.
#' @param verification_token Optional callback verification token.
#' @param encrypt_key Optional event subscription encryption key.
#' @param verify_signature Whether to validate Feishu callback signatures when applicable.
#' @param send_text_fn Optional custom send function for tests or overrides.
#' @param send_status_fn Optional custom status function for tests or overrides.
#' @param download_resource_fn Optional custom downloader for inbound message resources.
#' @param model Optional default model id.
#' @param agent Optional default agent.
#' @param skills Optional skill paths or `"auto"`. Defaults to `"auto"` when `agent` is `NULL`.
#' @param tools Optional default tools.
#' @param hooks Optional session hooks.
#' @param registry Optional provider registry.
#' @param max_steps Maximum tool execution steps.
#' @param session_policy Optional session policy overrides.
#' @return A `ChannelRuntime` with the Feishu adapter registered.
#' @export
create_feishu_channel_runtime <- function(session_store,
                                          app_id,
                                          app_secret,
                                          base_url = "https://open.feishu.cn",
                                          verification_token = NULL,
                                          encrypt_key = NULL,
                                          verify_signature = TRUE,
                                          send_text_fn = NULL,
                                          send_status_fn = NULL,
                                          download_resource_fn = NULL,
                                          model = NULL,
                                          agent = NULL,
                                          skills = "auto",
                                          tools = NULL,
                                          hooks = NULL,
                                          registry = NULL,
                                          max_steps = 10,
                                          session_policy = channel_default_session_policy()) {
  runtime <- create_channel_runtime(
    session_store = session_store,
    model = model,
    agent = agent,
    skills = skills,
    tools = tools,
    hooks = hooks,
    registry = registry,
    max_steps = max_steps,
    session_policy = session_policy
  )
  runtime$register_adapter(
    create_feishu_channel_adapter(
      app_id = app_id,
      app_secret = app_secret,
      base_url = base_url,
      verification_token = verification_token,
      encrypt_key = encrypt_key,
      verify_signature = verify_signature,
      send_text_fn = send_text_fn,
      send_status_fn = send_status_fn,
      download_resource_fn = download_resource_fn
    )
  )
  runtime
}

#' @title Create a Feishu Webhook Handler
#' @description
#' Create a transport-agnostic handler that turns a raw Feishu callback request
#' into a JSON HTTP response payload.
#' @param runtime A `ChannelRuntime` with a Feishu adapter registered.
#' @return A function `(headers, body)` that returns a response list.
#' @export
create_feishu_webhook_handler <- function(runtime) {
  if (!inherits(runtime, "ChannelRuntime")) {
    channel_runtime_abort("create_feishu_webhook_handler() requires a ChannelRuntime.")
  }

  force(runtime)

  function(headers = NULL, body = NULL) {
    result <- runtime$handle_request("feishu", headers = headers, body = body)

    if (identical(result$type, "challenge")) {
      return(feishu_http_json_response(
        status = result$status %||% 200L,
        payload = list(challenge = result$payload$challenge %||% NULL)
      ))
    }

    if (identical(result$type, "ignored")) {
      return(feishu_http_json_response(status = result$status %||% 200L, payload = list(ok = TRUE)))
    }

    feishu_http_json_response(
      status = result$status %||% 200L,
      payload = list(ok = TRUE, results = result$results %||% list())
    )
  }
}

#' @title Start a Feishu Webhook Server
#' @description
#' Start a minimal `httpuv` server exposing a Feishu callback endpoint.
#' @param runtime A `ChannelRuntime` with a Feishu adapter registered.
#' @param host Bind host.
#' @param port Bind port.
#' @param path Callback path.
#' @return An `httpuv` server handle.
#' @export
start_feishu_webhook_server <- function(runtime,
                                        host = "127.0.0.1",
                                        port = 8788,
                                        path = "/feishu/webhook") {
  rlang::check_installed("httpuv")

  handler <- create_feishu_webhook_handler(runtime)
  normalized_path <- paste0("/", gsub("^/+", "", path))

  app <- list(
    call = function(req) {
      req_path <- req$PATH_INFO %||% req$path %||% "/"
      if (!identical(req_path, normalized_path)) {
        return(list(
          status = 404L,
          headers = list("Content-Type" = "text/plain; charset=utf-8"),
          body = "Not found"
        ))
      }

      headers <- req$HEADERS %||% list()
      body <- feishu_http_read_body(req)
      handler(headers = headers, body = body)
    }
  )

  httpuv::startServer(host = host, port = as.integer(port), app = app)
}

#' @title Run a Feishu Webhook Server
#' @description
#' Run a blocking `httpuv` loop for a Feishu callback endpoint.
#' This helper is intended for local demos and manual integration testing.
#' @param runtime A `ChannelRuntime` with a Feishu adapter registered.
#' @param host Bind host.
#' @param port Bind port.
#' @param path Callback path.
#' @param poll_ms Event loop polling interval in milliseconds.
#' @return Invisible server handle. Interrupt the R process to stop it.
#' @export
run_feishu_webhook_server <- function(runtime,
                                      host = "127.0.0.1",
                                      port = 8788,
                                      path = "/feishu/webhook",
                                      poll_ms = 100) {
  rlang::check_installed("httpuv")

  handler <- create_feishu_webhook_handler(runtime)
  normalized_path <- paste0("/", gsub("^/+", "", path))
  app <- list(
    call = function(req) {
      req_path <- req$PATH_INFO %||% req$path %||% "/"
      if (!identical(req_path, normalized_path)) {
        return(list(
          status = 404L,
          headers = list("Content-Type" = "text/plain; charset=utf-8"),
          body = "Not found"
        ))
      }

      headers <- req$HEADERS %||% list()
      body <- feishu_http_read_body(req)
      handler(headers = headers, body = body)
    }
  )

  message(sprintf(
    "Feishu webhook server listening on http://%s:%s%s",
    host,
    as.integer(port),
    normalized_path
  ))
  httpuv::runServer(host = host, port = as.integer(port), app = app)
}

#' @title Create a Feishu Event Processor
#' @description
#' Create a plain event processor for Feishu events that already arrived through
#' an authenticated ingress such as the official long-connection SDK.
#' @param runtime A `ChannelRuntime` with a Feishu adapter registered.
#' @return A function `(payload)` that processes one Feishu event payload.
#' @export
create_feishu_event_processor <- function(runtime) {
  if (!inherits(runtime, "ChannelRuntime")) {
    channel_runtime_abort("create_feishu_event_processor() requires a ChannelRuntime.")
  }

  force(runtime)

  function(payload) {
    result <- runtime$handle_request("feishu", body = payload)
    invisible(result)
  }
}
