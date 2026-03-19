#' @title AI Review Embedded Runtime Contract
#' @description Internal helpers for defining published HTML runtime capability
#' tiers, action gating, and deterministic manifest payloads.
#' @name ai_review_embedded_contract
NULL

#' @keywords internal
ai_review_runtime_tiers <- function() {
  c("inspect_only", "attachable", "connected", "busy", "stale", "error")
}

#' @keywords internal
normalize_ai_review_runtime_tier <- function(tier) {
  match_ai_review_arg(tier, ai_review_runtime_tiers(), "runtime tier")
}

#' @keywords internal
default_ai_review_runtime_tier <- function(review_mode = "none") {
  if (identical(review_mode, "none")) "inspect_only" else "attachable"
}

#' @keywords internal
ai_review_runtime_required_fields <- function(mode = c("offline", "connected")) {
  mode <- match.arg(mode)

  offline <- c(
    "version",
    "mode",
    "chunk.id",
    "chunk.label",
    "document.source_path",
    "document.identity",
    "review.mode",
    "review.state",
    "execution.status",
    "session.embedded",
    "code.available",
    "runtime.tier",
    "runtime.actions"
  )

  if (identical(mode, "offline")) {
    return(offline)
  }

  c(
    offline,
    "review.status",
    "runtime.attachable",
    "runtime.connected"
  )
}

#' @keywords internal
build_ai_review_runtime_actions <- function(tier,
                                            review_mode = "none",
                                            has_code = FALSE,
                                            has_session = FALSE,
                                            has_saveback_target = FALSE) {
  tier <- normalize_ai_review_runtime_tier(tier)
  review_mode <- review_mode %||% "none"

  catalog <- ai_review_action_catalog()
  action_ids <- names(catalog)
  if (identical(review_mode, "none")) {
    action_ids <- action_ids[!vapply(catalog[action_ids], function(meta) {
      isTRUE(meta$mutating)
    }, logical(1))]
  }

  runtime_enabled <- identical(tier, "connected")
  runtime_reason <- switch(
    tier,
    inspect_only = "Local runtime attachment is not available in this document.",
    attachable = "Connect a local review runtime to enable this action.",
    busy = "The local review runtime is busy.",
    stale = "This document is stale. Reconnect or rerender before mutating review state.",
    error = "The local review runtime reported an error.",
    NULL
  )

  out <- stats::setNames(vector("list", length(action_ids)), action_ids)

  for (action_id in action_ids) {
    meta <- catalog[[action_id]]
    enabled <- FALSE
    reason <- NULL

    if (identical(action_id, "view-session")) {
      enabled <- isTRUE(has_session)
      reason <- if (!enabled) "No embedded session data is available in this document." else NULL
    } else if (identical(action_id, "copy-code")) {
      enabled <- isTRUE(has_code)
      reason <- if (!enabled) "No extracted code is available to copy." else NULL
    } else if (identical(action_id, "saveback")) {
      enabled <- isTRUE(has_saveback_target) && tier %in% c("connected", "stale")
      reason <- if (!isTRUE(has_saveback_target)) {
        "No source document is available for saveback."
      } else if (!tier %in% c("connected", "stale")) {
        runtime_reason
      } else {
        NULL
      }
    } else if (identical(review_mode, "none")) {
      enabled <- FALSE
      reason <- "Review is not enabled for this chunk."
    } else {
      enabled <- runtime_enabled
      reason <- if (!enabled) runtime_reason else NULL
    }

    out[[action_id]] <- list(
      id = action_id,
      label = meta$label,
      enabled = enabled,
      mutating = isTRUE(meta$mutating),
      requires_connection = isTRUE(meta$mutating),
      reason = reason
    )
  }

  out
}

#' @keywords internal
build_ai_review_runtime_manifest <- function(chunk_id,
                                             chunk_label,
                                             file_path = NULL,
                                             review_mode = "none",
                                             runtime_mode = "static",
                                             review_status = NULL,
                                             state = NULL,
                                             execution_status = NULL,
                                             final_code = "",
                                             embed_session = "none",
                                             timestamps = NULL,
                                             tier = NULL) {
  review_mode <- review_mode %||% "none"
  runtime_mode <- runtime_mode %||% "static"
  embed_session <- embed_session %||% "none"
  tier <- normalize_ai_review_runtime_tier(
    tier %||% default_ai_review_runtime_tier(review_mode)
  )

  source_path <- file_path %||% NULL
  has_code <- nzchar(final_code %||% "")
  has_session <- !identical(embed_session, "none")
  has_saveback_target <- !is.null(source_path) && nzchar(source_path) &&
    file.exists(source_path) && grepl("\\.(Rmd|rmd|qmd)$", source_path)
  timestamps <- timestamps %||% list()

  actions <- build_ai_review_runtime_actions(
    tier = tier,
    review_mode = review_mode,
    has_code = has_code,
    has_session = has_session,
    has_saveback_target = has_saveback_target
  )

  list(
    version = 1L,
    mode = "published-review-runtime",
    chunk = list(
      id = chunk_id,
      label = chunk_label,
      file_path = source_path
    ),
    document = list(
      source_path = source_path,
      identity = if (!is.null(source_path) && nzchar(source_path)) {
        digest::digest(source_path, algo = "sha256", serialize = FALSE)
      } else {
        NULL
      }
    ),
    review = list(
      mode = review_mode,
      runtime = runtime_mode,
      status = review_status %||% NULL,
      state = state %||% NULL,
      updated_at = timestamps$updated_at %||% NULL
    ),
    execution = list(
      status = execution_status %||% NULL
    ),
    session = list(
      embedded = embed_session,
      available = has_session
    ),
    code = list(
      available = has_code
    ),
    runtime = list(
      tier = tier,
      attachable = tier %in% c("attachable", "connected", "busy", "stale"),
      connected = tier %in% c("connected", "busy", "stale"),
      busy = identical(tier, "busy"),
      stale = identical(tier, "stale"),
      error = identical(tier, "error"),
      saveback_available = has_saveback_target,
      actions = actions
    )
  )
}
