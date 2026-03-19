#' @title AI Review Contract
#' @description Internal helpers for normalizing AI review options and
#' validating chunk lifecycle transitions.
#' @name ai_review_contract
NULL

#' @keywords internal
ai_review_states <- function() {
  c("draft", "ran", "approved", "frozen", "rejected", "error")
}

#' @keywords internal
ai_review_action_catalog <- function() {
  list(
    `view-session` = list(label = "View Session", mutating = FALSE),
    `copy-code` = list(label = "Copy Code", mutating = FALSE),
    saveback = list(label = "Save & Refresh", mutating = TRUE),
    run = list(label = "Run Draft", mutating = TRUE),
    regenerate = list(label = "Regenerate", mutating = TRUE),
    `approve-freeze` = list(label = "Approve & Freeze", mutating = TRUE),
    reject = list(label = "Reject", mutating = TRUE)
  )
}

#' @keywords internal
ai_review_action_ids <- function() {
  names(ai_review_action_catalog())
}

#' @keywords internal
match_ai_review_arg <- function(value, choices, arg_name) {
  if (is.null(value) || length(value) != 1 || !is.character(value) || is.na(value)) {
    rlang::abort(sprintf("`%s` must be one of: %s.", arg_name, paste(choices, collapse = ", ")))
  }

  if (!value %in% choices) {
    rlang::abort(sprintf("`%s` must be one of: %s.", arg_name, paste(choices, collapse = ", ")))
  }

  value
}

#' @keywords internal
normalize_ai_review_action_id <- function(action) {
  match_ai_review_arg(action, ai_review_action_ids(), "action")
}

#' @keywords internal
normalize_ai_review_options <- function(options = list()) {
  options <- options %||% list()

  legacy_review <- !is.null(options$ai_agent) || !is.null(options$uncertainty)

  review <- options$review %||% if (legacy_review) "required" else "none"
  review <- match_ai_review_arg(review, c("none", "inline", "required"), "review")

  runtime <- options$runtime %||% "static"
  runtime <- match_ai_review_arg(runtime, c("static", "live"), "runtime")

  embed_session <- options$embed_session %||% if (review == "none") "none" else "summary"
  embed_session <- match_ai_review_arg(embed_session, c("none", "summary", "full"), "embed_session")

  if (!is.null(options$defer_eval) &&
      (!is.logical(options$defer_eval) || length(options$defer_eval) != 1)) {
    rlang::abort("`defer_eval` must be TRUE or FALSE.")
  }
  defer_eval <- isTRUE(options$defer_eval)

  if (review == "none" && runtime == "live") {
    rlang::abort("`runtime = \"live\"` requires `review` to be \"inline\" or \"required\".")
  }

  if (review == "none" && defer_eval) {
    rlang::abort("`defer_eval = TRUE` requires a review-enabled chunk.")
  }

  list(
    review = review,
    runtime = runtime,
    embed_session = embed_session,
    defer_eval = defer_eval,
    use_memory = review != "none"
  )
}

#' @keywords internal
transition_ai_review_state <- function(state, event) {
  state <- match.arg(state, ai_review_states())
  event <- match.arg(
    event,
    c("generate", "run", "approve", "freeze", "reject", "fail", "regenerate", "reopen")
  )

  transitions <- list(
    draft = c(run = "ran", approve = "approved", reject = "rejected", fail = "error"),
    ran = c(approve = "approved", reject = "rejected", fail = "error", regenerate = "draft"),
    approved = c(freeze = "frozen", reject = "rejected", regenerate = "draft"),
    frozen = c(reopen = "draft"),
    rejected = c(regenerate = "draft", reopen = "draft"),
    error = c(regenerate = "draft", reject = "rejected")
  )

  next_state <- unname(transitions[[state]][event])
  if (length(next_state) == 0 || is.na(next_state)) {
    rlang::abort(sprintf(
      "Cannot transition AI review state from '%s' via '%s'.",
      state, event
    ))
  }

  next_state
}
