#' @title Live AI Review Bridge
#' @description Helpers for serving a rendered review document with optional
#' inline live actions backed by ProjectMemory.
#' @name ai_review_live
NULL

#' @keywords internal
ai_review_live_content_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    html = "text/html; charset=utf-8",
    htm = "text/html; charset=utf-8",
    css = "text/css; charset=utf-8",
    js = "application/javascript; charset=utf-8",
    json = "application/json; charset=utf-8",
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    gif = "image/gif",
    svg = "image/svg+xml",
    txt = "text/plain; charset=utf-8",
    map = "application/json; charset=utf-8",
    "application/octet-stream"
  )
}

#' @keywords internal
ai_review_live_response <- function(status = 200L, body = "",
                                    content_type = "text/plain; charset=utf-8",
                                    headers = NULL) {
  if (is.character(body)) {
    body <- charToRaw(body)
  }

  base_headers <- list(
    "Content-Type" = content_type,
    "Cache-Control" = "no-store",
    "Access-Control-Allow-Origin" = "*",
    "Access-Control-Allow-Methods" = "GET, POST, OPTIONS",
    "Access-Control-Allow-Headers" = "Content-Type"
  )

  list(
    status = as.integer(status),
    headers = utils::modifyList(base_headers, headers %||% list()),
    body = body
  )
}

#' @keywords internal
parse_ai_review_live_body <- function(req) {
  if (!is.null(req$body)) {
    if (is.raw(req$body)) {
      return(rawToChar(req$body))
    }
    return(req$body)
  }

  if (!is.null(req$rook.input) && is.function(req$rook.input$read)) {
    raw_body <- req$rook.input$read()
    if (is.raw(raw_body)) {
      return(rawToChar(raw_body))
    }
    return(raw_body)
  }

  ""
}

#' @keywords internal
inject_ai_review_bridge_config <- function(html, bridge_url) {
  bridge_config <- build_ai_review_runtime_bridge_config(bridge_url)
  primary_url <- if (length(bridge_config$urls) > 0) bridge_config$urls[[1]] else NULL

  bridge_script <- paste0(
    "<script>",
    "window.AISDK_REVIEW_BRIDGE_URL = ",
    jsonlite::toJSON(primary_url, auto_unbox = TRUE, null = "null"),
    ";",
    "window.AISDK_REVIEW_RUNTIME = ",
    jsonlite::toJSON(bridge_config, auto_unbox = TRUE, null = "null"),
    ";",
    "</script>"
  )

  if (grepl("</head>", html, ignore.case = TRUE, perl = TRUE)) {
    sub("</head>", paste0(bridge_script, "</head>"), html, ignore.case = TRUE, perl = TRUE)
  } else {
    paste0("<head>", bridge_script, "</head>", html)
  }
}

#' @keywords internal
default_ai_review_runtime_port <- function() {
  port <- getOption("aisdk.review_runtime_port", 39393L)
  as.integer(port[[1]])
}

#' @keywords internal
default_ai_review_runtime_urls <- function(host = "127.0.0.1",
                                           port = default_ai_review_runtime_port()) {
  sprintf("http://%s:%d/__aisdk_review__", host, as.integer(port))
}

#' @keywords internal
build_ai_review_runtime_bridge_config <- function(bridge_url = NULL, host = "127.0.0.1") {
  urls <- unique(Filter(
    nzchar,
    unlist(c(bridge_url %||% NULL, default_ai_review_runtime_urls(host = host)), use.names = FALSE)
  ))

  list(
    version = 1L,
    urls = urls,
    ping_path = "/ping",
    handshake_path = "/handshake",
    action_path = "/action",
    saveback_path = "/saveback",
    session_path = "/session"
  )
}

#' @keywords internal
normalize_ai_review_live_path <- function(path) {
  if (is.null(path) || !nzchar(path)) {
    return(NULL)
  }

  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' @keywords internal
ai_review_live_document_identity <- function(path) {
  normalized <- normalize_ai_review_live_path(path)
  if (is.null(normalized)) {
    return(NULL)
  }

  digest::digest(normalized, algo = "sha256", serialize = FALSE)
}

#' @keywords internal
parse_ai_review_live_json <- function(req) {
  body <- parse_ai_review_live_body(req)
  if (is.null(body) || !nzchar(body)) {
    return(list())
  }

  jsonlite::fromJSON(body, simplifyVector = FALSE)
}

#' @keywords internal
is_ai_review_source_document <- function(path) {
  normalized <- normalize_ai_review_live_path(path)
  !is.null(normalized) &&
    file.exists(normalized) &&
    grepl("\\.(Rmd|rmd|qmd)$", normalized)
}

#' @keywords internal
find_ai_review_live_source_path <- function(html_path) {
  normalized_html <- normalize_ai_review_live_path(html_path)
  if (is.null(normalized_html)) {
    return(NULL)
  }

  stem <- sub("\\.(html|htm)$", "", normalized_html, ignore.case = TRUE)
  candidates <- c(
    paste0(stem, ".qmd"),
    paste0(stem, ".Rmd"),
    paste0(stem, ".rmd")
  )
  source_path <- candidates[file.exists(candidates)][1]
  if (length(source_path) == 0 || is.na(source_path) || !nzchar(source_path)) {
    return(NULL)
  }

  normalizePath(source_path, winslash = "/", mustWork = TRUE)
}

#' @keywords internal
ai_review_live_has_saveback_target <- function(path) {
  is_ai_review_source_document(path)
}

#' @keywords internal
ai_review_live_review_newer_than_request <- function(review_updated_at, request_updated_at) {
  review_updated_at <- ai_review_live_value(review_updated_at)
  request_updated_at <- ai_review_live_value(request_updated_at)

  if (is.null(review_updated_at) || is.null(request_updated_at)) {
    return(FALSE)
  }

  isTRUE(review_updated_at > request_updated_at)
}

#' @keywords internal
resolve_ai_review_live_source_path <- function(request_document = NULL,
                                               request_chunks = list(),
                                               memory = NULL,
                                               html_path = NULL) {
  requested_source_path <- normalize_ai_review_live_path(request_document$source_path %||% NULL)
  if (is_ai_review_source_document(requested_source_path)) {
    return(requested_source_path)
  }

  chunk_ids <- unique(Filter(
    nzchar,
    vapply(request_chunks %||% list(), function(chunk) {
      chunk$chunk_id %||% chunk$id %||% ""
    }, character(1))
  ))

  if (!is.null(memory) && length(chunk_ids) > 0) {
    for (chunk_id in chunk_ids) {
      review <- memory$get_review(chunk_id)
      source_path <- normalize_ai_review_live_path(review$file_path %||% NULL)
      if (is_ai_review_source_document(source_path)) {
        return(source_path)
      }
    }
  }

  inferred_source_path <- find_ai_review_live_source_path(html_path)
  if (is_ai_review_source_document(inferred_source_path)) {
    return(inferred_source_path)
  }

  NULL
}

#' @keywords internal
resolve_ai_review_live_html_output <- function(source_path, html_path = NULL) {
  normalized_source <- normalize_ai_review_live_path(source_path)
  if (is.null(normalized_source)) {
    return(normalize_ai_review_live_path(html_path))
  }

  ext <- tolower(tools::file_ext(normalized_source))
  if (ext %in% c("html", "htm")) {
    return(normalized_source)
  }

  if (ext %in% c("rmd", "qmd")) {
    return(normalize_ai_review_live_path(
      sub("\\.(Rmd|rmd|qmd)$", ".html", normalized_source, ignore.case = TRUE)
    ))
  }

  normalize_ai_review_live_path(html_path)
}

#' @keywords internal
review_document_base_url <- function(base_url) {
  if (is.null(base_url) || !nzchar(base_url)) {
    return(NULL)
  }

  sub("/__aisdk_review__/?$", "/", base_url)
}

#' @keywords internal
build_ai_review_live_chunk_payload <- function(memory, chunk, review,
                                               server_source_path = NULL,
                                               document_match = TRUE,
                                               ignore_review_updated_at = FALSE,
                                               fresh_tier = "connected") {
  chunk_id <- chunk$chunk_id %||% chunk$id %||% review$chunk_id %||% NULL
  review_path <- normalize_ai_review_live_path(review$file_path %||% NULL)
  artifact <- if (!is.null(review) && !is.null(chunk_id)) {
    memory$get_review_artifact(chunk_id)
  } else {
    NULL
  }
  has_code <- nzchar(review$final_code %||% artifact$final_code %||% "")
  transcript <- artifact$transcript %||% list()
  has_session <- (!is.null(review$session_id) && !is.na(review$session_id) && nzchar(review$session_id)) ||
    length(transcript) > 0
  has_saveback_target <- ai_review_live_has_saveback_target(review_path %||% server_source_path)

  stale_reason <- NULL
  chunk_stale <- FALSE

  if (!isTRUE(document_match)) {
    chunk_stale <- TRUE
    stale_reason <- "document_mismatch"
  } else if (is.null(review)) {
    chunk_stale <- TRUE
    stale_reason <- "missing"
  } else if (!is.null(server_source_path) && !identical(review_path, server_source_path)) {
    chunk_stale <- TRUE
    stale_reason <- "source_mismatch"
  } else if (!isTRUE(ignore_review_updated_at) &&
             ai_review_live_review_newer_than_request(
               review$updated_at %||% NULL,
               chunk$review_updated_at %||% NULL
             )) {
    chunk_stale <- TRUE
    stale_reason <- "review_updated"
  }

  action_tier <- if (!isTRUE(document_match)) {
    "error"
  } else if (chunk_stale) {
    "stale"
  } else {
    fresh_tier
  }

  list(
    chunk_id = chunk_id,
    chunk_label = chunk$chunk_label %||% chunk$label %||% review$chunk_label %||% NULL,
    available = !is.null(review),
    stale = chunk_stale,
    stale_reason = stale_reason,
    review_status = review$status %||% NULL,
    execution_status = review$execution_status %||% NULL,
    review_updated_at = review$updated_at %||% NULL,
    actions = build_ai_review_runtime_actions(
      tier = action_tier,
      review_mode = review$review_mode %||% "none",
      has_code = has_code,
      has_session = has_session,
      has_saveback_target = has_saveback_target
    )
  )
}

#' @keywords internal
build_ai_review_live_handshake_payload <- function(memory, request, base_url = NULL) {
  request <- request %||% list()
  request_document <- request$document %||% list()
  request_chunks <- request$chunks %||% list()

  requested_source_path <- normalize_ai_review_live_path(request_document$source_path %||% NULL)
  requested_identity <- request_document$identity %||% NULL

  reviews <- lapply(request_chunks, function(chunk) {
    chunk_id <- chunk$chunk_id %||% chunk$id %||% NULL
    if (is.null(chunk_id)) {
      return(NULL)
    }
    memory$get_review(chunk_id)
  })

  available_reviews <- Filter(Negate(is.null), reviews)
  review_paths <- unique(Filter(
    nzchar,
    vapply(available_reviews, function(review) {
      normalize_ai_review_live_path(review$file_path %||% NULL) %||% ""
    }, character(1))
  ))

  server_source_path <- if (length(review_paths) >= 1) {
    review_paths[[1]]
  } else {
    requested_source_path
  }
  server_identity <- ai_review_live_document_identity(server_source_path)

  document_match <- if (!is.null(requested_identity) && !is.null(server_identity)) {
    identical(requested_identity, server_identity)
  } else if (!is.null(requested_source_path) && !is.null(server_source_path)) {
    identical(requested_source_path, server_source_path)
  } else {
    TRUE
  }

  status <- if (isTRUE(document_match)) "connected" else "mismatch"

  chunks <- Map(function(chunk, review) {
    payload <- build_ai_review_live_chunk_payload(
      memory = memory,
      chunk = chunk,
      review = review,
      server_source_path = server_source_path,
      document_match = document_match
    )

    if (isTRUE(document_match) && isTRUE(payload$stale)) {
      status <<- "stale"
    }
    payload
  }, request_chunks, reviews)

  list(
    ok = TRUE,
    mode = "live",
    status = status,
    base_url = base_url,
    capabilities = list(
      ping = TRUE,
      handshake = TRUE,
      actions = TRUE,
      saveback = TRUE,
      session = TRUE
    ),
    document = list(
      source_path = server_source_path,
      identity = server_identity,
      requested_source_path = requested_source_path,
      requested_identity = requested_identity,
      match = isTRUE(document_match)
    ),
    chunks = unname(chunks)
  )
}

#' @keywords internal
get_ai_review_session_payload <- function(memory, chunk_id) {
  review <- memory$get_review(chunk_id)
  if (is.null(review)) {
    return(NULL)
  }

  artifact <- memory$get_review_artifact(chunk_id)
  list(
    chunk_id = chunk_id,
    session_id = review$session_id %||% NULL,
    state = review$status %||% "pending",
    transcript = artifact$transcript %||% list(),
    artifact = artifact
  )
}

#' @keywords internal
ai_review_live_value <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NULL)
  }

  if (length(value) == 1 && is.atomic(value) && is.na(value)) {
    return(NULL)
  }

  value
}

#' @keywords internal
abort_ai_review_live <- function(message, status = 400L,
                                 action = NULL, chunk_id = NULL, state = "error") {
  rlang::abort(
    message,
    class = "ai_review_live_error",
    http_status = as.integer(status),
    action = action,
    chunk_id = chunk_id,
    state = state
  )
}

#' @keywords internal
load_ai_review_runtime_record <- function(memory, chunk_id) {
  record <- if (is.function(memory$get_review_runtime_record)) {
    memory$get_review_runtime_record(chunk_id)
  } else {
    list(
      review = memory$get_review(chunk_id),
      artifact = memory$get_review_artifact(chunk_id)
    )
  }

  if (is.null(record) || is.null(record$review)) {
    abort_ai_review_live(
      sprintf("Chunk '%s' was not found.", chunk_id),
      status = 404L,
      chunk_id = chunk_id
    )
  }

  record$artifact <- record$artifact %||% list()
  record
}

#' @keywords internal
current_ai_review_runtime_state <- function(review, artifact = NULL) {
  ai_review_live_value(artifact$state) %||%
    derive_ai_review_card_state(
      review_status = ai_review_live_value(review$status),
      execution_status = ai_review_live_value(review$execution_status) %||%
        ai_review_live_value(artifact$execution$status)
    )
}

#' @keywords internal
refresh_ai_review_runtime_manifest <- function(review, artifact) {
  build_ai_review_runtime_manifest(
    chunk_id = review$chunk_id,
    chunk_label = review$chunk_label,
    file_path = review$file_path,
    review_mode = ai_review_live_value(review$review_mode) %||% "none",
    runtime_mode = ai_review_live_value(review$runtime_mode) %||% "static",
    review_status = ai_review_live_value(review$status),
    state = current_ai_review_runtime_state(review, artifact),
    execution_status = ai_review_live_value(review$execution_status) %||%
      ai_review_live_value(artifact$execution$status),
    final_code = ai_review_live_value(review$final_code) %||%
      ai_review_live_value(artifact$final_code) %||% "",
    timestamps = list(
      created_at = ai_review_live_value(review$created_at),
      updated_at = ai_review_live_value(review$updated_at),
      reviewed_at = ai_review_live_value(review$reviewed_at)
    ),
    embed_session = ai_review_live_value(artifact$embed_session) %||%
      if (length(artifact$transcript %||% list()) > 0) "summary" else "none"
  )
}

#' @keywords internal
build_ai_review_live_action_payload <- function(memory, chunk_id, action, message,
                                                rerender_required = FALSE,
                                                pending = FALSE,
                                                session = NULL) {
  record <- load_ai_review_runtime_record(memory, chunk_id)
  review <- record$review
  artifact <- record$artifact
  state <- current_ai_review_runtime_state(review, artifact)
  source_path <- normalize_ai_review_live_path(review$file_path %||% NULL)
  action_meta <- ai_review_action_catalog()[[action]] %||% list(mutating = FALSE)
  saveback_available <- ai_review_live_has_saveback_target(source_path)
  artifact$runtime_manifest <- refresh_ai_review_runtime_manifest(review, artifact)

  list(
    ok = TRUE,
    action = action,
    chunk_id = chunk_id,
    state = state,
    message = message,
    pending = pending,
    rerender_required = rerender_required,
    review = list(
      status = ai_review_live_value(review$status),
      state = state,
      mode = ai_review_live_value(review$review_mode),
      runtime = ai_review_live_value(review$runtime_mode)
    ),
    execution = list(
      status = ai_review_live_value(review$execution_status) %||%
        ai_review_live_value(artifact$execution$status),
      output = ai_review_live_value(review$execution_output) %||%
        ai_review_live_value(artifact$execution$output),
      error = ai_review_live_value(review$error_message) %||%
        ai_review_live_value(artifact$execution$error)
    ),
    code = list(
      final_code = ai_review_live_value(review$final_code) %||%
        ai_review_live_value(artifact$final_code)
    ),
    draft = list(
      response = ai_review_live_value(artifact$draft_response) %||%
        remove_r_code_blocks(ai_review_live_value(review$response) %||% "")
    ),
    timestamps = list(
      created_at = ai_review_live_value(review$created_at),
      updated_at = ai_review_live_value(review$updated_at),
      reviewed_at = ai_review_live_value(review$reviewed_at)
    ),
    runtime_manifest = artifact$runtime_manifest %||% NULL,
    document = list(
      source_path = source_path,
      identity = ai_review_live_document_identity(source_path)
    ),
    saveback = list(
      available = saveback_available,
      required = isTRUE(action_meta$mutating) && !identical(action, "saveback") && saveback_available,
      source_path = source_path
    ),
    session = session
  )
}

#' @keywords internal
build_ai_review_live_error_payload <- function(message, action = NULL,
                                               chunk_id = NULL, state = "error") {
  list(
    ok = FALSE,
    action = action,
    chunk_id = chunk_id,
    state = state,
    message = message
  )
}

#' @keywords internal
perform_ai_review_live_action <- function(memory, chunk_id, action, envir = globalenv()) {
  action <- normalize_ai_review_action_id(action)
  record <- load_ai_review_runtime_record(memory, chunk_id)
  review <- record$review
  artifact <- record$artifact

  switch(
    action,
    "approve-freeze" = {
      memory$update_review_status(chunk_id, "approved")
      review <- memory$get_review(chunk_id)
      artifact$state <- "frozen"
      artifact$runtime_manifest <- refresh_ai_review_runtime_manifest(review, artifact)
      memory$store_review_artifact(
        chunk_id = chunk_id,
        artifact = artifact,
        session_id = ai_review_live_value(review$session_id),
        review_mode = ai_review_live_value(review$review_mode),
        runtime_mode = ai_review_live_value(review$runtime_mode)
      )
      build_ai_review_live_action_payload(
        memory = memory,
        chunk_id = chunk_id,
        action = action,
        message = "Chunk approved and frozen for future renders.",
        rerender_required = FALSE
      )
    },
    "reject" = {
      memory$update_review_status(chunk_id, "rejected")
      review <- memory$get_review(chunk_id)
      artifact$state <- "rejected"
      artifact$runtime_manifest <- refresh_ai_review_runtime_manifest(review, artifact)
      memory$store_review_artifact(
        chunk_id = chunk_id,
        artifact = artifact,
        session_id = ai_review_live_value(review$session_id),
        review_mode = ai_review_live_value(review$review_mode),
        runtime_mode = ai_review_live_value(review$runtime_mode)
      )
      build_ai_review_live_action_payload(
        memory = memory,
        chunk_id = chunk_id,
        action = action,
        message = "Chunk rejected.",
        rerender_required = FALSE
      )
    },
    "run" = {
      code <- ai_review_live_value(review$final_code) %||%
        ai_review_live_value(artifact$final_code)
      if (!nzchar(code %||% "")) {
        abort_ai_review_live(
          "No executable draft code is available for this chunk.",
          status = 400L,
          action = action,
          chunk_id = chunk_id
        )
      }

      execution_output <- evaluate_ai_review_code(code, list(echo = FALSE), envir)
      error_message <- extract_ai_review_execution_error(execution_output)
      execution_status <- if (is.null(error_message)) "completed" else "error"

      review$status <- if ((ai_review_live_value(review$status) %||% "pending") %in% c("approved", "rejected")) {
        "pending"
      } else {
        ai_review_live_value(review$status) %||% "pending"
      }
      review$execution_status <- execution_status
      review$execution_output <- execution_output
      review$final_code <- code
      review$error_message <- error_message

      artifact$final_code <- code
      artifact$execution <- list(
        status = execution_status,
        output = execution_output,
        error = error_message
      )
      artifact$state <- derive_ai_review_card_state(review$status, execution_status)
      artifact$runtime_manifest <- refresh_ai_review_runtime_manifest(review, artifact)

      persist_ai_review_runtime_record(memory, review, artifact)
      memory$append_review_event(
        chunk_id = chunk_id,
        event_type = "action:run",
        payload = list(execution_status = execution_status)
      )

      build_ai_review_live_action_payload(
        memory = memory,
        chunk_id = chunk_id,
        action = action,
        message = if (identical(execution_status, "completed")) {
          "Draft executed successfully."
        } else {
          "Draft execution failed."
        },
        rerender_required = FALSE
      )
    },
    "regenerate" = {
      model_id <- ai_review_live_value(artifact$model) %||%
        get_model()
      session <- create_chat_session(
        model = model_id,
        system_prompt = get_default_system_prompt()
      )
      response <- tryCatch(
        session$send(construct_prompt(review$prompt, "")),
        error = function(e) list(text = paste0("**Error calling LLM:** ", e$message))
      )

      response_text <- response$text %||% ""
      draft_response <- remove_r_code_blocks(response_text)
      regenerated_code <- extract_r_code(response_text)
      if (nzchar(regenerated_code)) {
        regenerated_code <- sanitize_r_code(regenerated_code)
      }
      transcript <- ai_review_capture_transcript(session)

      review$status <- "pending"
      review$response <- response_text
      review$execution_status <- if (nzchar(regenerated_code)) "not_run" else "no_code"
      review$execution_output <- ""
      review$final_code <- regenerated_code
      review$error_message <- if (nzchar(regenerated_code)) NULL else "Regenerated response did not include executable R code."
      review$session_id <- ai_review_live_value(review$session_id) %||% chunk_id

      artifact$response_text <- response_text
      artifact$draft_response <- draft_response
      artifact$final_code <- regenerated_code
      artifact$transcript <- if (length(transcript) > 0) transcript else artifact$transcript %||% list()
      artifact$retries <- list()
      artifact$model <- tryCatch(session$get_model_id(), error = function(e) model_id)
      artifact$session_id <- review$session_id
      artifact$execution <- list(
        status = review$execution_status,
        output = "",
        error = review$error_message
      )
      artifact$state <- "draft"
      artifact$runtime_manifest <- refresh_ai_review_runtime_manifest(review, artifact)

      persist_ai_review_runtime_record(memory, review, artifact)
      memory$append_review_event(
        chunk_id = chunk_id,
        event_type = "action:regenerate",
        payload = list(model = artifact$model)
      )

      build_ai_review_live_action_payload(
        memory = memory,
        chunk_id = chunk_id,
        action = action,
        message = "Draft regenerated. Re-render the document to refresh embedded content.",
        rerender_required = TRUE
      )
    },
    "view-session" = {
      build_ai_review_live_action_payload(
        memory = memory,
        chunk_id = chunk_id,
        action = action,
        message = "Session transcript loaded.",
        rerender_required = FALSE,
        session = get_ai_review_session_payload(memory, chunk_id)
      )
    },
    abort_ai_review_live(
      sprintf("Unsupported live review action '%s'.", action),
      status = 400L,
      action = action,
      chunk_id = chunk_id
    )
  )
}

#' @keywords internal
rerender_ai_review_source <- function(path) {
  resolve_review_document_html(path)
}

#' @keywords internal
perform_ai_review_live_saveback <- function(memory, request, html_path, base_url = NULL) {
  request <- request %||% list()
  request_document <- request$document %||% list()
  request_chunks <- request$chunks %||% list()

  source_path <- resolve_ai_review_live_source_path(
    request_document = request_document,
    request_chunks = request_chunks,
    memory = memory,
    html_path = html_path
  )

  if (!is_ai_review_source_document(source_path)) {
    abort_ai_review_live(
      "No source document is available for saveback.",
      status = 400L,
      action = "saveback"
    )
  }

  source_reviews <- memory$get_reviews_for_file(source_path)
  requested_chunk_ids <- unique(Filter(
    nzchar,
    vapply(request_chunks, function(chunk) {
      chunk$chunk_id %||% chunk$id %||% ""
    }, character(1))
  ))
  chunk_ids <- if (length(requested_chunk_ids) > 0) {
    requested_chunk_ids
  } else if (nrow(source_reviews) > 0) {
    unique(source_reviews$chunk_id)
  } else {
    character(0)
  }

  if (length(chunk_ids) == 0) {
    abort_ai_review_live(
      "No chunk reviews were found for saveback.",
      status = 404L,
      action = "saveback"
    )
  }

  rerendered <- FALSE
  saveback_status <- "recorded"
  saveback_message <- "Review state recorded."
  output_html_path <- resolve_ai_review_live_html_output(source_path, html_path = html_path)

  if (!identical(request$rerender, FALSE)) {
    output_html_path <- normalize_ai_review_live_path(rerender_ai_review_source(source_path))
    rerendered <- TRUE
    saveback_status <- "rendered"
    saveback_message <- "Review HTML rerendered from source."
  }

  memory$record_review_saveback(
    chunk_ids = chunk_ids,
    source_path = source_path,
    html_path = output_html_path,
    status = saveback_status,
    rerendered = rerendered,
    message = saveback_message
  )

  chunk_request_map <- stats::setNames(
    request_chunks,
    vapply(request_chunks, function(chunk) chunk$chunk_id %||% chunk$id %||% "", character(1))
  )

  chunks <- lapply(chunk_ids, function(chunk_id) {
    review <- memory$get_review(chunk_id)
    request_chunk <- chunk_request_map[[chunk_id]] %||% list(chunk_id = chunk_id)
    build_ai_review_live_chunk_payload(
      memory = memory,
      chunk = request_chunk,
      review = review,
      server_source_path = source_path,
      document_match = TRUE,
      ignore_review_updated_at = TRUE
    )
  })

  reload_url <- request$reload_url %||% NULL
  if (is.null(reload_url) && rerendered) {
    candidate_url <- review_document_base_url(base_url)
    if (!is.null(candidate_url)) {
      reload_url <- candidate_url
    }
  }

  list(
    ok = TRUE,
    mode = "live",
    status = "connected",
    base_url = base_url,
    document = list(
      source_path = source_path,
      identity = ai_review_live_document_identity(source_path),
      requested_source_path = normalize_ai_review_live_path(request_document$source_path %||% NULL),
      requested_identity = request_document$identity %||% NULL,
      match = TRUE
    ),
    chunks = unname(chunks),
    saveback = list(
      status = saveback_status,
      rerendered = rerendered,
      needs_reload = rerendered && !is.null(reload_url),
      reload_url = reload_url,
      source_path = source_path,
      html_path = output_html_path,
      message = saveback_message
    )
  )
}

#' @keywords internal
ai_review_bridge_json <- function(payload, status = 200L) {
  ai_review_live_response(
    status = status,
    body = jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
    content_type = "application/json; charset=utf-8"
  )
}

#' @keywords internal
serve_ai_review_file <- function(path, root_dir) {
  safe_root <- normalizePath(root_dir, mustWork = TRUE)
  relative_path <- gsub("^/+", "", path)
  target <- normalizePath(file.path(safe_root, utils::URLdecode(relative_path)), mustWork = FALSE)

  in_root <- identical(target, safe_root) || startsWith(target, paste0(safe_root, .Platform$file.sep))
  if (!in_root || !file.exists(target) || dir.exists(target)) {
    return(ai_review_live_response(404L, "Not found"))
  }

  size <- file.info(target)$size
  body <- readBin(target, what = "raw", n = size)
  ai_review_live_response(
    status = 200L,
    body = body,
    content_type = ai_review_live_content_type(target)
  )
}

#' @keywords internal
create_ai_review_live_app <- function(html_path, memory, base_url) {
  html_path <- normalizePath(html_path, mustWork = TRUE)
  root_dir <- dirname(html_path)
  html_name <- basename(html_path)

  list(
    call = function(req) {
      method <- req$REQUEST_METHOD %||% "GET"
      path <- req$PATH_INFO %||% "/"

      if (identical(method, "OPTIONS")) {
        return(ai_review_live_response(status = 204L, body = raw(0)))
      }

      if (identical(path, "/__aisdk_review__/ping")) {
        return(ai_review_bridge_json(list(
          ok = TRUE,
          mode = "live",
          capabilities = list(
            ping = TRUE,
            handshake = TRUE,
            actions = TRUE,
            saveback = TRUE,
            session = TRUE
          )
        )))
      }

      if (identical(path, "/__aisdk_review__/handshake") && identical(method, "POST")) {
        payload <- parse_ai_review_live_json(req)
        result <- build_ai_review_live_handshake_payload(
          memory = memory,
          request = payload,
          base_url = base_url
        )
        return(ai_review_bridge_json(result))
      }

      if (identical(path, "/__aisdk_review__/action") && identical(method, "POST")) {
        payload <- parse_ai_review_live_json(req)
        result <- tryCatch(
          perform_ai_review_live_action(
            memory = memory,
            chunk_id = payload$chunk_id,
            action = payload$action
          ),
          ai_review_live_error = function(e) {
            return(structure(
              build_ai_review_live_error_payload(
                message = conditionMessage(e),
                action = e$action %||% payload$action %||% NULL,
                chunk_id = e$chunk_id %||% payload$chunk_id %||% NULL,
                state = e$state %||% "error"
              ),
              http_status = e$http_status %||% 400L
            ))
          }
        )
        return(ai_review_bridge_json(
          result,
          status = attr(result, "http_status", exact = TRUE) %||% 200L
        ))
      }

      if (identical(path, "/__aisdk_review__/saveback") && identical(method, "POST")) {
        payload <- parse_ai_review_live_json(req)
        result <- tryCatch(
          perform_ai_review_live_saveback(
            memory = memory,
            request = payload,
            html_path = html_path,
            base_url = base_url
          ),
          ai_review_live_error = function(e) {
            return(structure(
              build_ai_review_live_error_payload(
                message = conditionMessage(e),
                action = e$action %||% "saveback",
                chunk_id = e$chunk_id %||% NULL,
                state = e$state %||% "error"
              ),
              http_status = e$http_status %||% 400L
            ))
          },
          error = function(e) {
            chunk_ids <- unique(Filter(
              nzchar,
              vapply(payload$chunks %||% list(), function(chunk) {
                chunk$chunk_id %||% chunk$id %||% ""
              }, character(1))
            ))
            source_path <- resolve_ai_review_live_source_path(
              request_document = payload$document %||% list(),
              request_chunks = payload$chunks %||% list(),
              memory = memory,
              html_path = html_path
            )
            memory$record_review_saveback(
              chunk_ids = chunk_ids,
              source_path = source_path,
              html_path = resolve_ai_review_live_html_output(source_path, html_path = html_path),
              status = "error",
              rerendered = FALSE,
              message = conditionMessage(e)
            )
            return(structure(
              build_ai_review_live_error_payload(
                message = conditionMessage(e),
                action = "saveback",
                state = "error"
              ),
              http_status = 500L
            ))
          }
        )
        return(ai_review_bridge_json(
          result,
          status = attr(result, "http_status", exact = TRUE) %||% 200L
        ))
      }

      if (grepl("^/__aisdk_review__/session/", path)) {
        chunk_id <- sub("^/__aisdk_review__/session/", "", path)
        session_payload <- get_ai_review_session_payload(memory, chunk_id)
        if (is.null(session_payload)) {
          return(ai_review_bridge_json(list(ok = FALSE, message = "Chunk not found"), status = 404L))
        }
        return(ai_review_bridge_json(list(ok = TRUE, session = session_payload)))
      }

      if (path %in% c("/", "/document", paste0("/", html_name))) {
        html <- paste(readLines(html_path, warn = FALSE), collapse = "\n")
        html <- inject_ai_review_bridge_config(html, base_url)
        return(ai_review_live_response(
          status = 200L,
          body = html,
          content_type = "text/html; charset=utf-8"
        ))
      }

      serve_ai_review_file(path, root_dir)
    }
  )
}

#' @keywords internal
resolve_review_document_html <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("html", "htm")) {
    return(normalizePath(path, mustWork = TRUE))
  }

  if (ext == "rmd") {
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      rlang::abort("Package 'rmarkdown' is required to render .Rmd review documents.")
    }
    return(normalizePath(rmarkdown::render(path, quiet = TRUE), mustWork = TRUE))
  }

  if (ext == "qmd") {
    if (!requireNamespace("quarto", quietly = TRUE)) {
      rlang::abort("Package 'quarto' is required to render .qmd review documents.")
    }
    return(normalizePath(quarto::quarto_render(path, quiet = TRUE), mustWork = TRUE))
  }

  rlang::abort("`path` must point to an .html, .Rmd, or .qmd file.")
}

#' @keywords internal
pick_ai_review_port <- function(port = NULL) {
  if (!is.null(port)) {
    return(as.integer(port))
  }

  default_ai_review_runtime_port()
}

#' @title Legacy Review Runtime Launcher
#' @description Internal legacy helper for the older local review-runtime flow.
#' It is kept for backward compatibility inside the package, but the preferred
#' user experience is now the published read-only artifact card with embedded
#' trace and metadata.
#' @param path Path to an `.html`, `.Rmd`, or `.qmd` document.
#' @param memory ProjectMemory object. Defaults to `get_memory()`.
#' @param host Host interface to bind. Defaults to `"127.0.0.1"`.
#' @param port Optional port. If `NULL`, a random port is chosen.
#' @param browse Open the preview in a browser. Defaults to `interactive()`.
#' @return A launcher list with `url`, `base_url`, `html_path`, and `stop()`.
#' @keywords internal
review_document <- function(path, memory = get_memory(), host = "127.0.0.1",
                            port = NULL, browse = interactive()) {
  if (!requireNamespace("httpuv", quietly = TRUE)) {
    rlang::abort("Package 'httpuv' is required for live review author preview.")
  }

  html_path <- resolve_review_document_html(path)
  chosen_port <- pick_ai_review_port(port)
  app <- NULL
  server <- NULL
  attempt_ports <- unique(c(chosen_port, sample(seq.int(30000L, 39999L), size = 10)))

  for (candidate_port in attempt_ports) {
    candidate_base_url <- paste0("http://", host, ":", candidate_port, "/__aisdk_review__")
    app <- create_ai_review_live_app(
      html_path = html_path,
      memory = memory,
      base_url = candidate_base_url
    )
    server <- tryCatch(
      httpuv::startServer(host = host, port = candidate_port, app = app),
      error = function(e) NULL
    )
    if (!is.null(server)) {
      chosen_port <- candidate_port
      break
    }
  }

  if (is.null(server)) {
    rlang::abort("Failed to start live review server on an available local port.")
  }

  url <- paste0("http://", host, ":", chosen_port, "/")
  base_url <- paste0("http://", host, ":", chosen_port, "/__aisdk_review__")

  if (isTRUE(browse)) {
    utils::browseURL(url)
  }

  structure(
    list(
      url = url,
      base_url = base_url,
      html_path = html_path,
      stop = function() {
        httpuv::stopServer(server)
        invisible(TRUE)
      },
      server = server
    ),
    class = "aisdk_review_launcher"
  )
}
