#' @title Context Management Configuration
#' @description
#' Public helpers for configuring adaptive context management on `ChatSession`
#' objects without reaching into internal metadata keys.
#' @name context_management
NULL

#' @keywords internal
normalize_context_regime_name <- function(value = NULL) {
  value <- tolower(trimws(value %||% "green"))
  allowed <- c("green", "yellow", "orange", "red")
  if (!(value %in% allowed)) {
    rlang::abort(sprintf("LLM synthesis min regime must be one of: %s", paste(allowed, collapse = ", ")))
  }
  value
}

#' @keywords internal
normalize_llm_synthesis_policy <- function(value = NULL) {
  defaults <- list(
    min_regime = "green",
    recent_messages = 6L,
    max_items = 3L,
    temperature = 0
  )
  if (is.null(value)) {
    value <- list()
  }
  if (!is.list(value)) {
    rlang::abort("`llm_synthesis_policy` must be a named list.")
  }

  merged <- utils::modifyList(defaults, value)
  list(
    min_regime = normalize_context_regime_name(merged$min_regime),
    recent_messages = as.integer(merged$recent_messages %||% defaults$recent_messages),
    max_items = as.integer(merged$max_items %||% defaults$max_items),
    temperature = as.numeric(merged$temperature %||% defaults$temperature)
  )
}

#' @keywords internal
default_retrieval_provider_names <- function() {
  c(
    "project_memory_snippets",
    "project_memory_fixes",
    "semantic_objects",
    "task_state",
    "session_memory",
    "documents",
    "transcript_segments",
    "execution_monitor",
    "system_info",
    "runtime_state"
  )
}

#' @keywords internal
default_retrieval_provider_limits <- function() {
  list(
    project_memory_snippets = 3L,
    project_memory_fixes = 2L,
    semantic_objects = 3L,
    task_state = 4L,
    session_memory = 3L,
    documents = 2L,
    transcript_segments = 2L,
    execution_monitor = 3L,
    system_info = 2L,
    runtime_state = 3L
  )
}

#' @keywords internal
default_retrieval_min_hits <- function() {
  list(
    semantic_objects = 1L,
    task_state = 0L,
    session_memory = 1L,
    documents = 1L,
    transcript_segments = 1L,
    execution_monitor = 1L,
    system_info = 1L,
    runtime_state = 1L
  )
}

#' @keywords internal
default_retrieval_scoring_policy <- function() {
  list(
    provider_weights = list(
      project_memory_snippets = 1.0,
      project_memory_fixes = 1.2,
      semantic_objects = 1.1,
      task_state = 1.05,
      session_memory = 1.0,
      documents = 1.0,
      transcript_segments = 0.9,
      execution_monitor = 1.15,
      system_info = 0.95,
      runtime_state = 1.0
    ),
    token_match_weight = 1.0,
    exact_query_bonus = 0.75,
    title_match_weight = 0.4,
    summary_match_weight = 0.35,
    preview_match_weight = 0.2,
    bigram_match_weight = 0.3,
    coverage_weight = 0.4,
    workflow_hint_weight = 0.45,
    accessor_match_weight = 0.35,
    semantic_object_bonus = 0.25,
    task_signal_weight = 0.5,
    execution_error_weight = 0.4,
    system_metric_weight = 0.3,
    runtime_signal_weight = 0.35,
    source_kind_bonus = list(
      project_memory_fixes = 0.3,
      semantic_objects = 0.2,
      task_state = 0.2,
      documents = 0.1,
      session_memory = 0.1,
      transcript_segments = 0.05,
      project_memory_snippets = 0.15,
      execution_monitor = 0.25,
      system_info = 0.15,
      runtime_state = 0.18
    ),
    recency_weight = 0.15,
    max_total_results = 6L
  )
}

#' @keywords internal
normalize_retrieval_reranking_policy <- function(value = NULL) {
  defaults <- list(
    min_regime = "green",
    top_n = 4L,
    temperature = 0
  )
  if (is.null(value)) {
    value <- list()
  }
  if (!is.list(value)) {
    rlang::abort("`retrieval_reranking_policy` must be a named list.")
  }

  merged <- utils::modifyList(defaults, value)
  list(
    min_regime = normalize_context_regime_name(merged$min_regime),
    top_n = as.integer(merged$top_n %||% defaults$top_n),
    temperature = as.numeric(merged$temperature %||% defaults$temperature)
  )
}

#' @keywords internal
normalize_retrieval_scoring_policy <- function(value = NULL) {
  defaults <- default_retrieval_scoring_policy()
  if (is.null(value)) {
    value <- list()
  }
  if (!is.list(value)) {
    rlang::abort("`retrieval_scoring_policy` must be a named list.")
  }

  merged <- utils::modifyList(defaults, value)
  weights <- defaults$provider_weights
  custom_weights <- merged$provider_weights %||% list()
  if (!is.list(custom_weights)) {
    rlang::abort("`retrieval_scoring_policy$provider_weights` must be a named list.")
  }
  unknown <- setdiff(names(custom_weights), names(weights))
  if (length(unknown) > 0) {
    rlang::abort(sprintf("Unknown retrieval providers in scoring policy: %s", paste(unknown, collapse = ", ")))
  }
  for (name in names(custom_weights)) {
    weights[[name]] <- as.numeric(custom_weights[[name]])
  }

  source_kind_bonus <- defaults$source_kind_bonus
  custom_bonus <- merged$source_kind_bonus %||% list()
  if (!is.list(custom_bonus)) {
    rlang::abort("`retrieval_scoring_policy$source_kind_bonus` must be a named list.")
  }
  unknown_bonus <- setdiff(names(custom_bonus), names(source_kind_bonus))
  if (length(unknown_bonus) > 0) {
    rlang::abort(sprintf("Unknown retrieval providers in source_kind_bonus: %s", paste(unknown_bonus, collapse = ", ")))
  }
  for (name in names(custom_bonus)) {
    source_kind_bonus[[name]] <- as.numeric(custom_bonus[[name]])
  }

  list(
    provider_weights = weights,
    token_match_weight = as.numeric(merged$token_match_weight %||% defaults$token_match_weight),
    exact_query_bonus = as.numeric(merged$exact_query_bonus %||% defaults$exact_query_bonus),
    title_match_weight = as.numeric(merged$title_match_weight %||% defaults$title_match_weight),
    summary_match_weight = as.numeric(merged$summary_match_weight %||% defaults$summary_match_weight),
    preview_match_weight = as.numeric(merged$preview_match_weight %||% defaults$preview_match_weight),
    bigram_match_weight = as.numeric(merged$bigram_match_weight %||% defaults$bigram_match_weight),
    coverage_weight = as.numeric(merged$coverage_weight %||% defaults$coverage_weight),
    workflow_hint_weight = as.numeric(merged$workflow_hint_weight %||% defaults$workflow_hint_weight),
    accessor_match_weight = as.numeric(merged$accessor_match_weight %||% defaults$accessor_match_weight),
    semantic_object_bonus = as.numeric(merged$semantic_object_bonus %||% defaults$semantic_object_bonus),
    task_signal_weight = as.numeric(merged$task_signal_weight %||% defaults$task_signal_weight),
    execution_error_weight = as.numeric(merged$execution_error_weight %||% defaults$execution_error_weight),
    system_metric_weight = as.numeric(merged$system_metric_weight %||% defaults$system_metric_weight),
    runtime_signal_weight = as.numeric(merged$runtime_signal_weight %||% defaults$runtime_signal_weight),
    source_kind_bonus = source_kind_bonus,
    recency_weight = as.numeric(merged$recency_weight %||% defaults$recency_weight),
    max_total_results = as.integer(merged$max_total_results %||% defaults$max_total_results)
  )
}

#' @keywords internal
normalize_retrieval_provider_map <- function(value = NULL) {
  defaults <- stats::setNames(as.list(rep(TRUE, length(default_retrieval_provider_names()))), default_retrieval_provider_names())
  if (is.null(value)) {
    return(defaults)
  }

  if (is.character(value)) {
    enabled <- value[value %in% default_retrieval_provider_names()]
    out <- defaults
    out[] <- FALSE
    for (name in enabled) {
      out[[name]] <- TRUE
    }
    return(out)
  }

  if (!is.list(value) && !is.logical(value)) {
    rlang::abort("`retrieval_providers` must be NULL, a character vector, or a named logical/list.")
  }

  value <- as.list(value)
  if (is.null(names(value)) || any(!nzchar(names(value)))) {
    rlang::abort("`retrieval_providers` as list/logical must be named.")
  }
  unknown <- setdiff(names(value), default_retrieval_provider_names())
  if (length(unknown) > 0) {
    rlang::abort(sprintf("Unknown retrieval providers: %s", paste(unknown, collapse = ", ")))
  }

  out <- defaults
  for (name in names(value)) {
    out[[name]] <- isTRUE(value[[name]])
  }
  out
}

#' @keywords internal
normalize_retrieval_provider_order <- function(value = NULL) {
  defaults <- default_retrieval_provider_names()
  if (is.null(value)) {
    return(defaults)
  }
  if (!is.character(value)) {
    rlang::abort("`retrieval_provider_order` must be a character vector.")
  }
  value <- unique(value[nzchar(value)])
  unknown <- setdiff(value, default_retrieval_provider_names())
  if (length(unknown) > 0) {
    rlang::abort(sprintf("Unknown retrieval providers in order: %s", paste(unknown, collapse = ", ")))
  }
  c(value, setdiff(defaults, value))
}

#' @keywords internal
normalize_retrieval_provider_limits <- function(value = NULL) {
  defaults <- default_retrieval_provider_limits()
  if (is.null(value)) {
    return(defaults)
  }
  if (!is.list(value)) {
    rlang::abort("`retrieval_provider_limits` must be a named list.")
  }
  unknown <- setdiff(names(value), names(defaults))
  if (length(unknown) > 0) {
    rlang::abort(sprintf("Unknown retrieval providers in limits: %s", paste(unknown, collapse = ", ")))
  }
  for (name in names(value)) {
    defaults[[name]] <- as.integer(value[[name]])
  }
  defaults
}

#' @keywords internal
normalize_retrieval_min_hits <- function(value = NULL) {
  defaults <- default_retrieval_min_hits()
  if (is.null(value)) {
    return(defaults)
  }
  if (!is.list(value)) {
    rlang::abort("`retrieval_min_hits` must be a named list.")
  }
  unknown <- setdiff(names(value), names(defaults))
  if (length(unknown) > 0) {
    rlang::abort(sprintf("Unknown retrieval providers in min_hits: %s", paste(unknown, collapse = ", ")))
  }
  for (name in names(value)) {
    defaults[[name]] <- as.integer(value[[name]])
  }
  defaults
}

#' @keywords internal
normalize_context_management_config <- function(config = list()) {
  config <- config %||% list()
  if (!is.list(config)) {
    rlang::abort("Context management config must be a list.")
  }

  list(
    mode = if (!is.null(config$mode)) normalize_context_management_mode(config$mode) else normalize_context_management_mode(NULL),
    llm_synthesis = isTRUE(config$llm_synthesis),
    synthesis_model = config$synthesis_model %||% NULL,
    llm_synthesis_policy = normalize_llm_synthesis_policy(config$llm_synthesis_policy %||% NULL),
    context_window_override = if (!is.null(config$context_window_override)) as.numeric(config$context_window_override) else NULL,
    max_output_tokens_override = if (!is.null(config$max_output_tokens_override)) as.numeric(config$max_output_tokens_override) else NULL,
    project_memory_root = if (!is.null(config$project_memory_root)) normalizePath(config$project_memory_root, winslash = "/", mustWork = FALSE) else NULL,
    retrieval_providers = normalize_retrieval_provider_map(config$retrieval_providers %||% NULL),
    retrieval_provider_order = normalize_retrieval_provider_order(config$retrieval_provider_order %||% NULL),
    retrieval_provider_limits = normalize_retrieval_provider_limits(config$retrieval_provider_limits %||% NULL),
    retrieval_min_hits = normalize_retrieval_min_hits(config$retrieval_min_hits %||% NULL),
    retrieval_scoring_policy = normalize_retrieval_scoring_policy(config$retrieval_scoring_policy %||% NULL),
    retrieval_reranking = isTRUE(config$retrieval_reranking),
    retrieval_reranking_model = config$retrieval_reranking_model %||% NULL,
    retrieval_reranking_policy = normalize_retrieval_reranking_policy(config$retrieval_reranking_policy %||% NULL)
  )
}

#' @keywords internal
get_context_management_config_impl <- function(session) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    rlang::abort("`session` must be a ChatSession or SharedSession.")
  }

  normalize_context_management_config(list(
    mode = session$get_metadata("context_management_mode", default = NULL),
    llm_synthesis = session$get_metadata("context_llm_synthesis", default = FALSE),
    synthesis_model = session$get_metadata("context_synthesis_model", default = NULL),
    llm_synthesis_policy = session$get_metadata("context_llm_synthesis_policy", default = NULL),
    context_window_override = session$get_metadata("context_window_override", default = NULL),
    max_output_tokens_override = session$get_metadata("max_output_tokens_override", default = NULL),
    project_memory_root = session$get_metadata("project_memory_root", default = NULL),
    retrieval_providers = session$get_metadata("retrieval_providers", default = NULL),
    retrieval_provider_order = session$get_metadata("retrieval_provider_order", default = NULL),
    retrieval_provider_limits = session$get_metadata("retrieval_provider_limits", default = NULL),
    retrieval_min_hits = session$get_metadata("retrieval_min_hits", default = NULL),
    retrieval_scoring_policy = session$get_metadata("retrieval_scoring_policy", default = NULL),
    retrieval_reranking = session$get_metadata("retrieval_reranking", default = FALSE),
    retrieval_reranking_model = session$get_metadata("retrieval_reranking_model", default = NULL),
    retrieval_reranking_policy = session$get_metadata("retrieval_reranking_policy", default = NULL)
  ))
}

#' @keywords internal
set_context_management_config_impl <- function(session,
                                               config = NULL,
                                               mode = NULL,
                                               llm_synthesis = NULL,
                                               synthesis_model = NULL,
                                               llm_synthesis_policy = NULL,
                                               context_window_override = NULL,
                                               max_output_tokens_override = NULL,
                                               project_memory_root = NULL,
                                               retrieval_providers = NULL,
                                               retrieval_provider_order = NULL,
                                               retrieval_provider_limits = NULL,
                                               retrieval_min_hits = NULL,
                                               retrieval_scoring_policy = NULL,
                                               retrieval_reranking = NULL,
                                               retrieval_reranking_model = NULL,
                                               retrieval_reranking_policy = NULL) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    rlang::abort("`session` must be a ChatSession or SharedSession.")
  }

  base_config <- if (is.null(config)) get_context_management_config_impl(session) else normalize_context_management_config(config)
  merged <- utils::modifyList(base_config, Filter(Negate(is.null), list(
    mode = mode,
    llm_synthesis = llm_synthesis,
    synthesis_model = synthesis_model,
    llm_synthesis_policy = llm_synthesis_policy,
    context_window_override = context_window_override,
    max_output_tokens_override = max_output_tokens_override,
    project_memory_root = project_memory_root,
    retrieval_providers = retrieval_providers,
    retrieval_provider_order = retrieval_provider_order,
    retrieval_provider_limits = retrieval_provider_limits,
    retrieval_min_hits = retrieval_min_hits,
    retrieval_scoring_policy = retrieval_scoring_policy,
    retrieval_reranking = retrieval_reranking,
    retrieval_reranking_model = retrieval_reranking_model,
    retrieval_reranking_policy = retrieval_reranking_policy
  )))
  normalized <- normalize_context_management_config(merged)

  session$merge_metadata(list(
    context_management_mode = normalized$mode,
    context_llm_synthesis = normalized$llm_synthesis,
    context_synthesis_model = normalized$synthesis_model,
    context_llm_synthesis_policy = normalized$llm_synthesis_policy,
    context_window_override = normalized$context_window_override,
    max_output_tokens_override = normalized$max_output_tokens_override,
    project_memory_root = normalized$project_memory_root,
    retrieval_providers = normalized$retrieval_providers,
    retrieval_provider_order = normalized$retrieval_provider_order,
    retrieval_provider_limits = normalized$retrieval_provider_limits,
    retrieval_min_hits = normalized$retrieval_min_hits,
    retrieval_scoring_policy = normalized$retrieval_scoring_policy,
    retrieval_reranking = normalized$retrieval_reranking,
    retrieval_reranking_model = normalized$retrieval_reranking_model,
    retrieval_reranking_policy = normalized$retrieval_reranking_policy
  ))

  invisible(session)
}

#' Create Context Management Configuration
#'
#' Build a normalized context-management configuration list.
#'
#' @param mode One of `"off"`, `"basic"`, or `"adaptive"`.
#' @param llm_synthesis Logical; whether to enable optional LLM enrichment for
#'   working-memory synthesis.
#' @param synthesis_model Optional model reference used for LLM synthesis.
#' @param llm_synthesis_policy Optional named list controlling LLM synthesis
#'   behavior. Supported fields are `min_regime`, `recent_messages`,
#'   `max_items`, and `temperature`.
#' @param context_window_override Optional numeric context-window override.
#' @param max_output_tokens_override Optional numeric max-output override.
#' @param project_memory_root Optional project root used for `ProjectMemory`
#'   retrieval and learning.
#' @param retrieval_providers Optional retrieval-provider selection. Accepts
#'   either a character vector of enabled providers or a named logical/list map.
#' @param retrieval_provider_order Optional character vector controlling render
#'   and ranking priority across retrieval providers.
#' @param retrieval_provider_limits Optional named list of per-provider result limits.
#' @param retrieval_min_hits Optional named list of per-provider matching thresholds.
#' @param retrieval_scoring_policy Optional named list controlling cross-provider
#'   retrieval scoring. Supported fields are `provider_weights`,
#'   `token_match_weight`, `exact_query_bonus`, `title_match_weight`,
#'   `summary_match_weight`, `preview_match_weight`, `bigram_match_weight`,
#'   `coverage_weight`, `workflow_hint_weight`, `accessor_match_weight`,
#'   `semantic_object_bonus`, `task_signal_weight`, `execution_error_weight`,
#'   `system_metric_weight`, `runtime_signal_weight`, `source_kind_bonus`,
#'   `recency_weight`, and `max_total_results`.
#' @param retrieval_reranking Logical; whether to enable optional learned
#'   reranking on top of deterministic retrieval scoring.
#' @param retrieval_reranking_model Optional model reference used for reranking.
#' @param retrieval_reranking_policy Optional named list controlling reranking.
#'   Supported fields are `min_regime`, `top_n`, and `temperature`.
#' @return A normalized configuration list.
#' @export
create_context_management_config <- function(mode = NULL,
                                             llm_synthesis = FALSE,
                                             synthesis_model = NULL,
                                             llm_synthesis_policy = NULL,
                                             context_window_override = NULL,
                                             max_output_tokens_override = NULL,
                                             project_memory_root = NULL,
                                             retrieval_providers = NULL,
                                             retrieval_provider_order = NULL,
                                             retrieval_provider_limits = NULL,
                                             retrieval_min_hits = NULL,
                                             retrieval_scoring_policy = NULL,
                                             retrieval_reranking = FALSE,
                                             retrieval_reranking_model = NULL,
                                             retrieval_reranking_policy = NULL) {
  normalize_context_management_config(list(
    mode = mode,
    llm_synthesis = llm_synthesis,
    synthesis_model = synthesis_model,
    llm_synthesis_policy = llm_synthesis_policy,
    context_window_override = context_window_override,
    max_output_tokens_override = max_output_tokens_override,
    project_memory_root = project_memory_root,
    retrieval_providers = retrieval_providers,
    retrieval_provider_order = retrieval_provider_order,
    retrieval_provider_limits = retrieval_provider_limits,
    retrieval_min_hits = retrieval_min_hits,
    retrieval_scoring_policy = retrieval_scoring_policy,
    retrieval_reranking = retrieval_reranking,
    retrieval_reranking_model = retrieval_reranking_model,
    retrieval_reranking_policy = retrieval_reranking_policy
  ))
}

#' Get Context Management Configuration
#'
#' Resolve the active context-management configuration for a session.
#'
#' @param session A `ChatSession` or `SharedSession`.
#' @return A normalized configuration list.
#' @export
get_context_management_config <- function(session) {
  get_context_management_config_impl(session)
}

#' Set Context Management Configuration
#'
#' Apply adaptive context-management settings to a session.
#'
#' @param session A `ChatSession` or `SharedSession`.
#' @param config Optional config list created by `create_context_management_config()`.
#' @param mode Optional override for config mode.
#' @param llm_synthesis Optional override for config LLM synthesis flag.
#' @param synthesis_model Optional override for config synthesis model.
#' @param llm_synthesis_policy Optional override for config LLM synthesis policy.
#' @param context_window_override Optional override for context window.
#' @param max_output_tokens_override Optional override for max output tokens.
#' @param project_memory_root Optional override for project memory root.
#' @param retrieval_providers Optional override for retrieval-provider selection.
#' @param retrieval_provider_order Optional override for provider ranking/order.
#' @param retrieval_provider_limits Optional override for per-provider limits.
#' @param retrieval_min_hits Optional override for per-provider matching thresholds.
#' @param retrieval_scoring_policy Optional override for cross-provider scoring policy.
#' @param retrieval_reranking Optional override for learned reranking enablement.
#' @param retrieval_reranking_model Optional override for the reranker model.
#' @param retrieval_reranking_policy Optional override for reranking policy.
#' @return Invisible `session`.
#' @export
set_context_management_config <- function(session,
                                          config = NULL,
                                          mode = NULL,
                                          llm_synthesis = NULL,
                                          synthesis_model = NULL,
                                          llm_synthesis_policy = NULL,
                                          context_window_override = NULL,
                                          max_output_tokens_override = NULL,
                                          project_memory_root = NULL,
                                          retrieval_providers = NULL,
                                          retrieval_provider_order = NULL,
                                          retrieval_provider_limits = NULL,
                                          retrieval_min_hits = NULL,
                                          retrieval_scoring_policy = NULL,
                                          retrieval_reranking = NULL,
                                          retrieval_reranking_model = NULL,
                                          retrieval_reranking_policy = NULL) {
  set_context_management_config_impl(
    session = session,
    config = config,
    mode = mode,
    llm_synthesis = llm_synthesis,
    synthesis_model = synthesis_model,
    llm_synthesis_policy = llm_synthesis_policy,
    context_window_override = context_window_override,
    max_output_tokens_override = max_output_tokens_override,
    project_memory_root = project_memory_root,
    retrieval_providers = retrieval_providers,
    retrieval_provider_order = retrieval_provider_order,
    retrieval_provider_limits = retrieval_provider_limits,
    retrieval_min_hits = retrieval_min_hits,
    retrieval_scoring_policy = retrieval_scoring_policy,
    retrieval_reranking = retrieval_reranking,
    retrieval_reranking_model = retrieval_reranking_model,
    retrieval_reranking_policy = retrieval_reranking_policy
  )
}
