#' @title Adaptive Context Budget Helpers
#' @description
#' Internal helpers for estimating prompt occupancy, classifying context budget
#' regimes, storing compact session-side context state, and assembling a
#' budget-aware prompt view from raw session history.
#' @name context_budget
NULL

#' @keywords internal
normalize_context_management_mode <- function(mode = NULL) {
  mode <- tolower(trimws(mode %||% getOption("aisdk.context_management", "off")))
  if (!nzchar(mode)) {
    mode <- "off"
  }

  allowed <- c("off", "basic", "adaptive")
  if (!(mode %in% allowed)) {
    rlang::abort(
      sprintf(
        "Context management mode must be one of: %s.",
        paste(allowed, collapse = ", ")
      )
    )
  }

  mode
}

#' @keywords internal
get_session_context_management_mode <- function(session = NULL) {
  mode <- NULL
  if (!is.null(session) && inherits(session, "ChatSession")) {
    mode <- session$get_metadata("context_management_mode", default = NULL)
  }
  normalize_context_management_mode(mode)
}

#' @keywords internal
context_llm_synthesis_enabled <- function(session = NULL) {
  value <- if (!is.null(session) && inherits(session, "ChatSession")) {
    session$get_metadata("context_llm_synthesis", default = NULL)
  } else {
    NULL
  }
  value <- value %||% getOption("aisdk.context_llm_synthesis", FALSE)
  isTRUE(value)
}

#' @keywords internal
resolve_context_synthesis_model <- function(session = NULL) {
  if (!is.null(session) && inherits(session, "ChatSession")) {
    return(session$get_metadata("context_synthesis_model", default = session$get_model_id()))
  }
  NULL
}

#' @keywords internal
resolve_context_synthesis_policy <- function(session = NULL) {
  if (!is.null(session) && inherits(session, "ChatSession")) {
    config <- get_context_management_config_impl(session)
    return(config$llm_synthesis_policy %||% normalize_llm_synthesis_policy())
  }
  normalize_llm_synthesis_policy()
}

#' @keywords internal
context_regime_rank <- function(regime) {
  order <- c(green = 1L, yellow = 2L, orange = 3L, red = 4L, unknown = 0L)
  order[[regime %||% "unknown"]] %||% 0L
}

#' @keywords internal
context_text_char_count <- function(x) {
  if (is.null(x)) {
    return(0)
  }

  if (is.character(x)) {
    vals <- x[!is.na(x)]
    if (length(vals) == 0) {
      return(0)
    }
    return(sum(nchar(vals, type = "chars")))
  }

  if (is.list(x)) {
    rendered <- tryCatch(
      content_blocks_to_text(x),
      error = function(e) tryCatch(
        safe_to_json(x, auto_unbox = TRUE),
        error = function(e2) paste(capture.output(str(x, max.level = 1)), collapse = "\n")
      )
    )
    return(context_text_char_count(rendered))
  }

  context_text_char_count(as.character(x))
}

#' @keywords internal
estimate_prompt_tokens <- function(messages = list(), system = NULL) {
  messages <- messages %||% list()

  message_chars <- 0
  if (!is.null(system) && nzchar(system)) {
    message_chars <- message_chars + context_text_char_count(system)
  }

  for (msg in messages) {
    if (!is.list(msg)) {
      next
    }
    message_chars <- message_chars + context_text_char_count(msg$content %||% "")
    message_chars <- message_chars + context_text_char_count(msg$reasoning %||% "")
  }

  message_overhead <- length(messages) * 8 + if (!is.null(system) && nzchar(system)) 12 else 0
  ceiling(message_chars / 4) + message_overhead
}

#' @keywords internal
estimate_session_context_tokens <- function(session) {
  data <- tryCatch(session$as_list(), error = function(e) NULL)
  if (is.null(data)) {
    return(NA_real_)
  }

  estimate_prompt_tokens(
    messages = data$history %||% list(),
    system = data$system_prompt %||% ""
  )
}

#' @keywords internal
infer_session_context_window <- function(provider, model_id) {
  model_id <- tolower(model_id %||% "")

  if (provider == "openai" && grepl("^(gpt-4o|gpt-4\\.1|gpt-5)", model_id)) {
    return(128000L)
  }
  if (provider == "anthropic" && grepl("^claude", model_id)) {
    return(200000L)
  }
  if (provider == "gemini" && grepl("^gemini", model_id)) {
    return(1000000L)
  }
  if (provider == "deepseek" && grepl("^deepseek-v4", model_id)) {
    return(1000000L)
  }
  if (provider == "deepseek" && grepl("^deepseek", model_id)) {
    return(64000L)
  }

  NA_integer_
}

#' @keywords internal
get_context_regime_thresholds <- function() {
  defaults <- list(yellow = 0.35, orange = 0.60, red = 0.80)
  configured <- getOption("aisdk.context_regime_thresholds", defaults)
  if (!is.list(configured)) {
    configured <- defaults
  }
  configured <- utils::modifyList(defaults, configured)

  list(
    yellow = as.numeric(configured$yellow %||% defaults$yellow),
    orange = as.numeric(configured$orange %||% defaults$orange),
    red = as.numeric(configured$red %||% defaults$red)
  )
}

#' @keywords internal
classify_context_regime <- function(ratio, thresholds = get_context_regime_thresholds()) {
  if (is.null(ratio) || is.na(ratio) || !is.finite(ratio)) {
    return("unknown")
  }
  if (ratio >= thresholds$red) {
    return("red")
  }
  if (ratio >= thresholds$orange) {
    return("orange")
  }
  if (ratio >= thresholds$yellow) {
    return("yellow")
  }
  "green"
}

#' @keywords internal
default_context_recent_window <- function() {
  defaults <- list(green = Inf, yellow = 40L, orange = 24L, red = 12L, unknown = 40L)
  configured <- getOption("aisdk.context_recent_window", defaults)
  if (!is.list(configured)) {
    configured <- defaults
  }
  configured <- utils::modifyList(defaults, configured)
  configured
}

#' @keywords internal
recent_context_window_size <- function(regime) {
  windows <- default_context_recent_window()
  value <- windows[[regime %||% "unknown"]] %||% windows$unknown
  if (is.infinite(value)) {
    return(Inf)
  }
  as.integer(value)
}

#' @keywords internal
normalize_context_state <- function(state = NULL) {
  defaults <- list(
    version = 1L,
    occupancy = list(
      estimated_prompt_tokens = NA_real_,
      context_window = NA_real_,
      ratio = NA_real_,
      status = "green"
    ),
    rolling_summary = "",
    active_facts = list(),
    decisions = list(),
    open_loops = list(),
    tool_digest = list(),
    object_cards = list(),
    artifact_cards = list(),
    carryover = list(),
    transcript_segments = list(),
    retrieval_cache = list(),
    event_log = list(),
    compacted_message_count = 0L,
    last_compaction_at = NULL
  )

  state <- state %||% list()
  if (!is.list(state)) {
    state <- list()
  }

  normalized <- utils::modifyList(defaults, state, keep.null = TRUE)
  replacement_fields <- c(
    "active_facts",
    "decisions",
    "open_loops",
    "tool_digest",
    "object_cards",
    "artifact_cards",
    "carryover",
    "transcript_segments",
    "retrieval_cache",
    "event_log"
  )
  for (field in replacement_fields) {
    if (!is.null(state[[field]])) {
      normalized[[field]] <- state[[field]]
    }
  }
  normalized$version <- as.integer(normalized$version %||% 1L)
  normalized$rolling_summary <- as.character(normalized$rolling_summary %||% "")
  normalized$compacted_message_count <- as.integer(normalized$compacted_message_count %||% 0L)
  if (!is.null(normalized$occupancy$ratio) &&
      !is.na(normalized$occupancy$ratio) &&
      is.finite(normalized$occupancy$ratio)) {
    normalized$occupancy$status <- classify_context_regime(
      normalized$occupancy$ratio %||% NA_real_
    ) %||% (normalized$occupancy$status %||% "green")
  } else {
    normalized$occupancy$status <- normalized$occupancy$status %||% "green"
  }

  normalized
}

#' @keywords internal
append_context_event <- function(state, event_type, data = list()) {
  state <- normalize_context_state(state)
  event <- utils::modifyList(
    list(
      timestamp = as.character(Sys.time()),
      type = event_type
    ),
    data %||% list(),
    keep.null = TRUE
  )
  state$event_log <- c(state$event_log %||% list(), list(event))
  state
}

#' @keywords internal
create_context_state <- function(metrics = NULL,
                                 rolling_summary = "",
                                 active_facts = list(),
                                 decisions = list(),
                                 open_loops = list()) {
  state <- normalize_context_state(list(
    rolling_summary = rolling_summary,
    active_facts = active_facts,
    decisions = decisions,
    open_loops = open_loops
  ))

  if (!is.null(metrics)) {
    state$occupancy <- list(
      estimated_prompt_tokens = metrics$used_tokens %||% NA_real_,
      context_window = metrics$context_window %||% NA_real_,
      ratio = metrics$ratio %||% NA_real_,
      status = metrics$regime %||% "unknown"
    )
  }

  state
}

#' @keywords internal
trim_context_preview <- function(text, max_chars = 160L) {
  text <- gsub("\\s+", " ", trimws(text %||% ""))
  if (!nzchar(text)) {
    return("")
  }
  if (nchar(text, type = "chars") <= max_chars) {
    return(text)
  }
  paste0(substr(text, 1L, max_chars - 3L), "...")
}

#' @keywords internal
compact_state_items <- function(items, max_items = 6L) {
  items <- items %||% list()
  if (length(items) == 0) {
    return(list())
  }

  labels <- character(0)
  compacted <- list()
  for (item in items) {
    label <- item$text %||% item$summary %||% ""
    label <- trim_context_preview(label, max_chars = 220L)
    if (!nzchar(label) || label %in% labels) {
      next
    }
    labels <- c(labels, label)
    item$text <- label
    compacted[[length(compacted) + 1L]] <- item
    if (length(compacted) >= max_items) {
      break
    }
  }

  compacted
}

#' @keywords internal
merge_state_items <- function(base_items, new_items, max_items = 6L) {
  base_items <- base_items %||% list()
  new_items <- new_items %||% list()
  compact_state_items(c(base_items, new_items), max_items = max_items)
}

#' @keywords internal
extract_recent_message_candidates <- function(messages,
                                              max_messages = 8L,
                                              allowed_roles = c("user", "assistant")) {
  messages <- messages %||% list()
  if (length(messages) == 0) {
    return(list())
  }

  subset_messages <- tail(messages, max_messages)
  candidates <- list()
  for (message in subset_messages) {
    role <- message$role %||% "unknown"
    if (!(role %in% allowed_roles)) {
      next
    }
    content <- as.character(message$content %||% "")
    if (!nzchar(trimws(content))) {
      next
    }
    parts <- unlist(strsplit(content, "\n|(?<=[\u3002\uff01\uff1f!?;\uff1b])|(?<=[.?!;])\\s+", perl = TRUE), use.names = FALSE)
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0) {
      next
    }
    for (part in parts) {
      normalized <- trim_context_preview(part, max_chars = 220L)
      if (!nzchar(normalized)) {
        next
      }
      candidates[[length(candidates) + 1L]] <- list(
        role = role,
        text = normalized
      )
    }
  }

  candidates
}

#' @keywords internal
candidate_matches_any <- function(text, patterns) {
  if (!nzchar(text %||% "") || length(patterns) == 0) {
    return(FALSE)
  }
  lower <- tolower(text)
  any(vapply(patterns, function(pattern) grepl(pattern, lower, perl = TRUE), logical(1)))
}

#' @keywords internal
build_active_facts <- function(session, state = NULL, messages = NULL, max_items = 6L) {
  state <- normalize_context_state(state)
  facts <- list()

  carryover <- state$carryover %||% list()
  if (!is.null(carryover$current_agent) && nzchar(carryover$current_agent %||% "")) {
    facts[[length(facts) + 1L]] <- list(
      text = sprintf("Current agent is `%s`.", carryover$current_agent),
      source = "carryover"
    )
  }
  if (!is.null(carryover$global_task) && nzchar(carryover$global_task %||% "")) {
    facts[[length(facts) + 1L]] <- list(
      text = sprintf("Global task: %s", trim_context_preview(carryover$global_task, max_chars = 180L)),
      source = "carryover"
    )
  }

  object_cards <- state$object_cards %||% list()
  if (length(object_cards) > 0) {
    cards <- utils::head(unname(object_cards), 3L)
    for (card in cards) {
      facts[[length(facts) + 1L]] <- list(
        text = sprintf("Live %s `%s`: %s", card$kind %||% "object", card$name %||% "<unnamed>", card$summary %||% ""),
        source = "object_card"
      )
    }
  }

  artifact_cards <- state$artifact_cards %||% list()
  if (length(artifact_cards) > 0) {
    cards <- utils::head(artifact_cards, 2L)
    for (card in cards) {
      facts[[length(facts) + 1L]] <- list(
        text = sprintf("Recent artifact `%s` came from `%s`.", basename(card$path %||% ""), card$tool %||% "tool"),
        source = "artifact_card"
      )
    }
  }

  tool_digest <- state$tool_digest %||% list()
  if (length(tool_digest) > 0) {
    digests <- utils::head(tool_digest, 2L)
    for (digest in digests) {
      facts[[length(facts) + 1L]] <- list(
        text = sprintf("Recent tool `%s` finished with status `%s`.", digest$tool %||% "tool", digest$status %||% "ok"),
        source = "tool_digest"
      )
    }
  }

  message_candidates <- extract_recent_message_candidates(messages, max_messages = 6L)
  fact_patterns <- c("\\bmust\\b", "\\bshould\\b", "\\bavoid\\b", "\\bdo not\\b", "\\bdon't\\b", "\u5fc5\u987b", "\u9700\u8981", "\u4e0d\u8981", "\u907f\u514d")
  for (candidate in message_candidates) {
    if (candidate_matches_any(candidate$text, fact_patterns)) {
      facts[[length(facts) + 1L]] <- list(
        text = candidate$text,
        source = paste0("message:", candidate$role)
      )
    }
  }

  compact_state_items(facts, max_items = max_items)
}

#' @keywords internal
build_decisions <- function(messages = NULL, max_items = 5L) {
  candidates <- extract_recent_message_candidates(messages, max_messages = 8L)
  decision_patterns <- c(
    "\\bwe will\\b", "\\bi will\\b", "\\blet's\\b", "\\buse\\b", "\\bprefer\\b",
    "\\badopt\\b", "\\brecommend\\b", "\\bchoose\\b", "\\bselected\\b", "\\bdecid",
    "\\bplan\\b", "\u6309\u8ba1\u5212", "\u91c7\u7528", "\u9009\u62e9", "\u51b3\u5b9a", "\u5f00\u59cb\u5b9e\u73b0", "\u4e0b\u4e00\u6b65"
  )

  decisions <- list()
  for (candidate in candidates) {
    if (candidate_matches_any(candidate$text, decision_patterns)) {
      decisions[[length(decisions) + 1L]] <- list(
        text = candidate$text,
        source = paste0("message:", candidate$role)
      )
    }
  }

  compact_state_items(decisions, max_items = max_items)
}

#' @keywords internal
build_open_loops <- function(state = NULL,
                             messages = NULL,
                             generation_result = NULL,
                             max_items = 5L) {
  state <- normalize_context_state(state)
  open_loops <- list()

  carryover <- state$carryover %||% list()
  if (!is.null(carryover$current_agent) && length(carryover$stack %||% list()) > 0) {
    current_task <- tail(carryover$stack, 1L)[[1]]$task %||% NULL
    if (!is.null(current_task) && nzchar(current_task)) {
      open_loops[[length(open_loops) + 1L]] <- list(
        text = sprintf("Current delegated task still in focus: %s", trim_context_preview(current_task, max_chars = 180L)),
        source = "carryover"
      )
    }
  }

  candidates <- extract_recent_message_candidates(messages, max_messages = 8L)
  open_patterns <- c(
    "\\bneed to\\b", "\\bnext step\\b", "\\bfollow up\\b", "\\blater\\b", "\\bremaining\\b",
    "\\bstill need\\b", "\\btodo\\b", "\\bto do\\b", "\u5f85", "\u540e\u7eed", "\u8fd8\u9700\u8981", "\u4e0b\u4e00\u6b65", "TODO"
  )
  for (candidate in candidates) {
    if (candidate_matches_any(candidate$text, open_patterns)) {
      open_loops[[length(open_loops) + 1L]] <- list(
        text = candidate$text,
        source = paste0("message:", candidate$role)
      )
    }
  }

  tool_results <- generation_result$all_tool_results %||% list()
  for (tool_result in tool_results) {
    if (isTRUE(tool_result$is_error)) {
      open_loops[[length(open_loops) + 1L]] <- list(
        text = sprintf("Resolve tool error in `%s`: %s", tool_result$name %||% "tool", trim_context_preview(tool_result$result %||% "", max_chars = 160L)),
        source = "tool_error"
      )
    }
  }

  compact_state_items(open_loops, max_items = max_items)
}

#' @keywords internal
llm_synthesize_working_memory <- function(session,
                                          state = NULL,
                                          messages = NULL,
                                          max_items = 3L,
                                          regime = NULL) {
  if (!context_llm_synthesis_enabled(session)) {
    return(NULL)
  }

  model_ref <- resolve_context_synthesis_model(session)
  if (is.null(model_ref)) {
    return(NULL)
  }
  policy <- resolve_context_synthesis_policy(session)
  current_regime <- regime %||% "green"
  if (identical(current_regime, "unknown")) {
    current_regime <- "green"
  }
  if (context_regime_rank(current_regime) < context_regime_rank(policy$min_regime)) {
    return(NULL)
  }

  state <- normalize_context_state(state)
  recent_messages <- tail(messages %||% list(), policy$recent_messages %||% 6L)
  message_lines <- vapply(recent_messages, function(message) {
    sprintf("%s: %s", message$role %||% "unknown", trim_context_preview(as.character(message$content %||% ""), max_chars = 180L))
  }, character(1))

  deterministic_lines <- c(
    if (length(state$active_facts) > 0) c("Deterministic active facts:", vapply(utils::head(state$active_facts, 4L), function(item) paste0("- ", item$text %||% ""), character(1))) else NULL,
    if (length(state$decisions) > 0) c("Deterministic decisions:", vapply(utils::head(state$decisions, 4L), function(item) paste0("- ", item$text %||% ""), character(1))) else NULL,
    if (length(state$open_loops) > 0) c("Deterministic open loops:", vapply(utils::head(state$open_loops, 4L), function(item) paste0("- ", item$text %||% ""), character(1))) else NULL
  )

  schema <- z_object(
    active_facts = z_array(z_string("Short fact statement")),
    decisions = z_array(z_string("Short decision statement")),
    open_loops = z_array(z_string("Short unresolved follow-up"))
  )

  prompt <- paste(c(
    "Summarize the conversation into compact working-memory items.",
    "Use the deterministic candidates as high-confidence hints. Only add concise items that improve continuity.",
    "",
    "Recent conversation:",
    message_lines,
    "",
    deterministic_lines
  ), collapse = "\n")

  result <- tryCatch(
    generate_object(
      model = model_ref,
      prompt = prompt,
      schema = schema,
      schema_name = "context_working_memory",
      temperature = policy$temperature %||% 0
    ),
    error = function(e) NULL
  )

  object <- result$object %||% NULL
  if (is.null(object) || !is.list(object)) {
    return(NULL)
  }

  normalize_items <- function(values, source) {
    values <- values %||% character(0)
    values <- as.character(values)
    values <- values[nzchar(trimws(values))]
    lapply(utils::head(values, policy$max_items %||% max_items), function(value) {
      list(text = trim_context_preview(value, max_chars = 220L), source = source)
    })
  }

  list(
    active_facts = normalize_items(object$active_facts, "llm_synthesis"),
    decisions = normalize_items(object$decisions, "llm_synthesis"),
    open_loops = normalize_items(object$open_loops, "llm_synthesis")
  )
}

#' @keywords internal
message_preview_text <- function(message, max_chars = 160L) {
  role <- message$role %||% "unknown"
  content <- trim_context_preview(message$content %||% "", max_chars = max_chars)
  if (!nzchar(content) && !is.null(message$reasoning) && nzchar(message$reasoning)) {
    content <- trim_context_preview(message$reasoning, max_chars = max_chars)
  }
  if (!nzchar(content)) {
    content <- "(empty)"
  }
  sprintf("- %s: %s", role, content)
}

#' @keywords internal
compact_lines <- function(text, max_lines = 6L, max_chars_per_line = 180L) {
  text <- as.character(text %||% "")
  if (!nzchar(text)) {
    return(character(0))
  }

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) {
    return(character(0))
  }

  lines <- vapply(lines, trim_context_preview, character(1), max_chars = max_chars_per_line)
  if (length(lines) > max_lines) {
    lines <- c(lines[seq_len(max_lines)], sprintf("... %d more line(s)", length(lines) - max_lines))
  }
  lines
}

#' @keywords internal
format_context_tool_call <- function(tool_name, args = list()) {
  if (length(args %||% list()) == 0) {
    return(sprintf("%s()", tool_name))
  }

  arg_parts <- vapply(names(args), function(arg_name) {
    value <- args[[arg_name]]
    rendered <- if (is.character(value) && length(value) == 1) {
      sprintf('"%s"', gsub('"', '\\"', value, fixed = TRUE))
    } else if (is.logical(value) && length(value) == 1) {
      if (isTRUE(value)) "TRUE" else "FALSE"
    } else {
      paste(deparse(value, width.cutoff = 500L), collapse = "")
    }
    sprintf("%s = %s", arg_name, rendered)
  }, character(1))

  sprintf("%s(%s)", tool_name, paste(arg_parts, collapse = ", "))
}

#' @keywords internal
context_symbol_inspection_hint <- function(object_name, object = NULL, kind = NULL) {
  resolved_kind <- kind %||% if (is.function(object)) "function" else "object"
  if (identical(resolved_kind, "function")) {
    list(
      kind = "function",
      primary_tool = "inspect_r_function",
      full_call = format_context_tool_call("inspect_r_function", list(name = object_name, detail = "full")),
      source_call = format_context_tool_call("get_r_source", list(name = object_name)),
      docs_call = format_context_tool_call("get_r_documentation", list(name = object_name, section = "summary"))
    )
  } else {
    list(
      kind = "object",
      primary_tool = "inspect_r_object",
      full_call = format_context_tool_call("inspect_r_object", list(name = object_name, detail = "full")),
      structured_call = format_context_tool_call("inspect_r_object", list(name = object_name, detail = "structured"))
    )
  }
}

#' @keywords internal
summarize_message_span <- function(messages,
                                   max_messages = 8L,
                                   max_chars_per_message = 160L,
                                   max_total_chars = 1500L) {
  messages <- messages %||% list()
  if (length(messages) == 0) {
    return("")
  }

  selected <- if (length(messages) <= max_messages) {
    messages
  } else {
    head_count <- min(3L, length(messages))
    tail_count <- min(3L, length(messages) - head_count)
    c(
      messages[seq_len(head_count)],
      list(list(role = "system", content = sprintf("... %d earlier message(s) omitted ...", length(messages) - head_count - tail_count))),
      tail(messages, tail_count)
    )
  }

  lines <- c(
    sprintf("Compacted %d earlier message(s). Key trajectory:", length(messages)),
    vapply(selected, message_preview_text, character(1), max_chars = max_chars_per_message)
  )

  text <- paste(lines, collapse = "\n")
  if (nchar(text, type = "chars") > max_total_chars) {
    text <- paste0(substr(text, 1L, max_total_chars - 3L), "...")
  }
  text
}

#' @keywords internal
context_object_card_limit <- function(regime) {
  defaults <- list(green = 5L, yellow = 4L, orange = 3L, red = 2L, unknown = 3L)
  configured <- getOption("aisdk.context_object_card_limit", defaults)
  if (!is.list(configured)) {
    configured <- defaults
  }
  configured <- utils::modifyList(defaults, configured)
  as.integer(configured[[regime %||% "unknown"]] %||% configured$unknown)
}

#' @keywords internal
build_context_tool_digest <- function(generation_result = NULL, max_items = 5L) {
  tool_results <- generation_result$all_tool_results %||% list()
  if (length(tool_results) == 0) {
    return(list())
  }

  digests <- lapply(seq_along(tool_results), function(i) {
    tr <- tool_results[[i]]
    preview_lines <- compact_lines(tr$result %||% "", max_lines = 4L)
    list(
      tool = tr$name %||% "unknown_tool",
      status = if (isTRUE(tr$is_error)) "error" else "ok",
      summary = if (length(preview_lines) > 0) {
        paste(preview_lines, collapse = "\n")
      } else {
        trim_context_preview(tr$result %||% "")
      },
      timestamp = as.character(Sys.time()),
      index = i
    )
  })

  if (length(digests) > max_items) {
    digests <- tail(digests, max_items)
  }
  digests
}

#' @keywords internal
build_context_artifact_cards <- function(generation_result = NULL, max_items = 8L) {
  tool_results <- generation_result$all_tool_results %||% list()
  if (length(tool_results) == 0) {
    return(list())
  }

  cards <- list()
  for (tr in tool_results) {
    artifacts <- attr(tr$raw_result %||% NULL, "aisdk_artifacts", exact = TRUE) %||% list()
    if (length(artifacts) == 0) {
      next
    }
    for (artifact in artifacts) {
      path <- artifact$path %||% artifact$uri %||% NULL
      if (is.null(path) || !nzchar(path)) {
        next
      }
      cards[[length(cards) + 1L]] <- list(
        tool = tr$name %||% "unknown_tool",
        path = path,
        type = tools::file_ext(path %||% ""),
        summary = trim_context_preview(sprintf("Artifact from %s: %s", tr$name %||% "tool", basename(path))),
        timestamp = as.character(Sys.time())
      )
    }
  }

  if (length(cards) > max_items) {
    cards <- tail(cards, max_items)
  }
  cards
}

#' @keywords internal
build_context_object_cards <- function(session, regime = "green") {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    return(list())
  }

  env <- session$get_envir()
  if (is.null(env) || !is.environment(env)) {
    return(list())
  }

  object_names <- ls(env, all.names = FALSE)
  object_names <- object_names[!grepl("^\\.", object_names)]
  if (length(object_names) == 0) {
    return(list())
  }

  limit <- context_object_card_limit(regime)
  if (length(object_names) > limit) {
    object_names <- object_names[seq_len(limit)]
  }

  cards <- lapply(object_names, function(object_name) {
    obj <- tryCatch(get(object_name, envir = env, inherits = FALSE), error = function(e) NULL)
    if (is.null(obj)) {
      return(NULL)
    }

    payload <- tryCatch(
      describe_semantic_object(obj, name = object_name, session = session),
      error = function(e) NULL
    )
    summary_text <- tryCatch(
      semantic_render_summary(obj, name = object_name, envir = env),
      error = function(e) sprintf("%s (%s)", object_name, paste(class(obj), collapse = ", "))
    )
    workflow_hint <- tryCatch(
      get_semantic_workflow_hint(obj, session = session),
      error = function(e) NULL
    )

    list(
      name = object_name,
      kind = if (is.function(obj)) "function" else "object",
      class = class(obj),
      adapter = payload$adapter %||% "generic",
      summary = trim_context_preview(summary_text, max_chars = 220L),
      workflow_hint = workflow_hint,
      accessors = payload$accessors %||% character(0),
      task_tags = unique(c(
        if (is.function(obj)) c("function", "code", "inspect") else c("object", "data", "inspect"),
        if (length(workflow_hint$steps %||% character(0)) > 0) c("plan", "inspect") else character(0),
        if (length(payload$accessors %||% character(0)) > 0) c("code", "function") else character(0)
      )),
      inspection_hint = context_symbol_inspection_hint(object_name, obj),
      updated_at = as.character(Sys.time())
    )
  })

  cards <- Filter(Negate(is.null), cards)
  stats::setNames(cards, vapply(cards, function(card) card$name, character(1)))
}

#' @keywords internal
apply_context_card_updates_from_r_tools <- function(state, generation_result = NULL) {
  state <- normalize_context_state(state)
  tool_calls <- generation_result$all_tool_calls %||% list()
  tool_results <- generation_result$all_tool_results %||% list()
  if (length(tool_calls) == 0 || length(tool_results) == 0) {
    return(state)
  }

  calls_by_id <- setNames(tool_calls, vapply(tool_calls, function(tc) tc$id %||% "", character(1)))
  cards <- state$object_cards %||% list()
  updated_count <- 0L

  for (tool_result in tool_results) {
    if (!(tool_result$name %in% c("inspect_r_object", "inspect_r_function", "get_r_documentation", "get_r_source"))) {
      next
    }
    tool_call <- calls_by_id[[tool_result$id %||% ""]]
    if (is.null(tool_call)) {
      next
    }
    symbol_name <- tool_call$arguments$name %||% NULL
    if (is.null(symbol_name) || !nzchar(symbol_name)) {
      next
    }

    if (is.null(cards[[symbol_name]])) {
      cards[[symbol_name]] <- list(
        name = symbol_name,
        kind = if (identical(tool_result$name, "inspect_r_function") || identical(tool_result$name, "get_r_source") || identical(tool_result$name, "get_r_documentation")) "function" else "object",
        class = character(0),
        adapter = "unknown",
        summary = "",
        workflow_hint = NULL,
        accessors = character(0),
        inspection_hint = context_symbol_inspection_hint(symbol_name, kind = if (identical(tool_result$name, "inspect_r_function") || identical(tool_result$name, "get_r_source") || identical(tool_result$name, "get_r_documentation")) "function" else "object"),
        updated_at = as.character(Sys.time())
      )
    }

    preview <- if (!is.null(tool_result$raw_result) && is.list(tool_result$raw_result)) {
      trim_context_preview(safe_to_json(tool_result$raw_result, auto_unbox = TRUE), max_chars = 280L)
    } else {
      paste(compact_lines(tool_result$result %||% "", max_lines = 5L, max_chars_per_line = 140L), collapse = "\n")
    }

    cards[[symbol_name]]$last_inspection <- list(
      tool = tool_result$name,
      detail = tool_call$arguments$detail %||% tool_call$arguments$section %||% "default",
      preview = preview,
      timestamp = as.character(Sys.time())
    )
    cards[[symbol_name]]$updated_at <- as.character(Sys.time())
    if (identical(tool_result$name, "inspect_r_object") && nzchar(preview)) {
      cards[[symbol_name]]$summary <- trim_context_preview(preview, max_chars = 220L)
      cards[[symbol_name]]$inspection_hint <- context_symbol_inspection_hint(symbol_name, kind = "object")
    }
    if (identical(tool_result$name, "inspect_r_function") || identical(tool_result$name, "get_r_source") || identical(tool_result$name, "get_r_documentation")) {
      cards[[symbol_name]]$kind <- "function"
      cards[[symbol_name]]$inspection_hint <- context_symbol_inspection_hint(symbol_name, kind = "function")
    }
    updated_count <- updated_count + 1L
  }

  state$object_cards <- cards
  if (updated_count > 0) {
    state <- append_context_event(
      state,
      "object_cards_tool_enriched",
      list(count = as.character(updated_count))
    )
  }

  state
}

#' @keywords internal
build_context_carryover <- function(session) {
  if (is.null(session) || !inherits(session, "SharedSession")) {
    return(list())
  }

  ctx <- tryCatch(session$get_context(), error = function(e) NULL)
  if (is.null(ctx) || !is.list(ctx)) {
    return(list())
  }

  list(
    current_agent = ctx$current_agent %||% NULL,
    depth = ctx$depth %||% 0L,
    global_task = ctx$global_task %||% NULL,
    stack = ctx$stack %||% list()
  )
}

#' @keywords internal
render_context_state_block <- function(state,
                                       max_active_facts = 4L,
                                       max_decisions = 4L,
                                       max_open_loops = 4L,
                                       max_tool_digests = 4L,
                                       max_artifact_cards = 4L,
                                       max_object_cards = 3L,
                                       max_events = 4L) {
  state <- normalize_context_state(state)
  lines <- c("[STRUCTURED CONTEXT STATE]")

  carryover <- state$carryover %||% list()
  if (length(carryover) > 0 && (!is.null(carryover$current_agent) || length(carryover$stack %||% list()) > 0)) {
    lines <- c(lines, "Carryover state:")
    if (!is.null(carryover$current_agent) && nzchar(carryover$current_agent)) {
      lines <- c(lines, sprintf("- current_agent: %s", carryover$current_agent))
    }
    if (!is.null(carryover$global_task) && nzchar(carryover$global_task %||% "")) {
      lines <- c(lines, sprintf("- global_task: %s", trim_context_preview(carryover$global_task, max_chars = 160L)))
    }
    lines <- c(lines, sprintf("- depth: %s", as.integer(carryover$depth %||% 0L)))
    if (length(carryover$stack %||% list()) > 0) {
      stack_preview <- vapply(utils::head(carryover$stack, 3L), function(item) {
        sprintf("%s:%s", item$agent %||% "agent", trim_context_preview(item$task %||% "", max_chars = 60L))
      }, character(1))
      lines <- c(lines, paste0("- stack: ", paste(stack_preview, collapse = " -> ")))
    }
  }

  if (length(state$active_facts) > 0) {
    lines <- c(lines, "Active facts:")
    facts <- utils::head(state$active_facts, max_active_facts)
    lines <- c(lines, vapply(facts, function(item) paste0("- ", item$text %||% ""), character(1)))
  }

  if (length(state$decisions) > 0) {
    lines <- c(lines, "Decisions:")
    decisions <- utils::head(state$decisions, max_decisions)
    lines <- c(lines, vapply(decisions, function(item) paste0("- ", item$text %||% ""), character(1)))
  }

  if (length(state$open_loops) > 0) {
    lines <- c(lines, "Open loops:")
    loops <- utils::head(state$open_loops, max_open_loops)
    lines <- c(lines, vapply(loops, function(item) paste0("- ", item$text %||% ""), character(1)))
  }

  if (length(state$tool_digest) > 0) {
    lines <- c(lines, "Recent tool digests:")
    digests <- tail(state$tool_digest, max_tool_digests)
    for (digest in digests) {
      lines <- c(lines, sprintf("- %s [%s]", digest$tool %||% "tool", digest$status %||% "ok"))
      digest_lines <- compact_lines(digest$summary %||% "", max_lines = 3L, max_chars_per_line = 160L)
      lines <- c(lines, paste0("  ", digest_lines))
    }
  }

  if (length(state$artifact_cards) > 0) {
    lines <- c(lines, "Recent artifacts:")
    cards <- tail(state$artifact_cards, max_artifact_cards)
    for (card in cards) {
      lines <- c(lines, sprintf("- %s (%s)", basename(card$path %||% ""), card$tool %||% "tool"))
    }
  }

  if (length(state$object_cards) > 0) {
    lines <- c(lines, "Live object cards:")
    cards <- utils::head(unname(state$object_cards), max_object_cards)
    for (card in cards) {
      class_text <- paste(card$class %||% character(0), collapse = ", ")
      lines <- c(lines, sprintf("- %s [%s] via %s", card$name %||% "<unnamed>", class_text, card$adapter %||% "generic"))
      if (nzchar(card$summary %||% "")) {
        lines <- c(lines, paste0("  ", card$summary))
      }
      hint <- card$workflow_hint %||% NULL
      if (is.list(hint) && length(hint$steps %||% character(0)) > 0) {
        lines <- c(lines, paste0("  workflow: ", paste(utils::head(hint$steps, 3L), collapse = " -> ")))
      }
      inspect_hint <- card$inspection_hint %||% NULL
      if (is.list(inspect_hint) && nzchar(inspect_hint$full_call %||% "")) {
        lines <- c(lines, paste0("  deep inspect: ", inspect_hint$full_call))
      }
      if (is.list(inspect_hint) && nzchar(inspect_hint$source_call %||% "")) {
        lines <- c(lines, paste0("  source: ", inspect_hint$source_call))
      }
      if (is.list(inspect_hint) && nzchar(inspect_hint$docs_call %||% "")) {
        lines <- c(lines, paste0("  docs: ", inspect_hint$docs_call))
      }
      last_inspection <- card$last_inspection %||% NULL
      if (is.list(last_inspection) && nzchar(last_inspection$preview %||% "")) {
        lines <- c(lines, paste0("  last inspection: ", trim_context_preview(last_inspection$preview, max_chars = 180L)))
      }
    }
  }

  if (length(state$event_log) > 0) {
    lines <- c(lines, "Recent context events:")
    events <- tail(state$event_log, max_events)
    for (event in events) {
      details <- setdiff(names(event), c("timestamp", "type"))
      detail_preview <- ""
      if (length(details) > 0) {
        snippets <- vapply(details[seq_len(min(length(details), 2L))], function(name) {
          sprintf("%s=%s", name, trim_context_preview(as.character(event[[name]] %||% ""), max_chars = 40L))
        }, character(1))
        detail_preview <- paste(snippets, collapse = ", ")
      }
      lines <- c(lines, sprintf("- %s%s", event$type %||% "event", if (nzchar(detail_preview)) paste0(" (", detail_preview, ")") else ""))
    }
  }

  retrieval_cache <- state$retrieval_cache %||% list()
  ranked_hits <- retrieval_cache$ranked_hits %||% list()
  if (length(ranked_hits) > 0) {
    lines <- c(lines, "Ranked retrieval hits:")
    if (nzchar(retrieval_cache$query %||% "")) {
      lines <- c(lines, paste0("- query: ", retrieval_cache$query))
    }
    for (hit in ranked_hits) {
      provider <- hit$provider %||% "provider"
      title <- hit$title %||% hit$summary %||% "retrieval hit"
      lines <- c(lines, sprintf("- [%s | score=%.2f] %s", provider, hit$score %||% 0, title))
      if (nzchar(hit$summary %||% "")) {
        lines <- c(lines, paste0("  ", hit$summary))
      }
      if (nzchar(hit$preview %||% "")) {
        lines <- c(lines, paste0("  ", hit$preview))
      }
      if (length(hit$workflow_steps %||% character(0)) > 0) {
        lines <- c(lines, paste0("  workflow: ", paste(utils::head(hit$workflow_steps, 3L), collapse = " -> ")))
      }
      if (length(hit$accessors %||% character(0)) > 0) {
        lines <- c(lines, paste0("  accessors: ", paste(utils::head(hit$accessors, 4L), collapse = ", ")))
      }
    }
  }

  if (length(lines) <= 1) {
    return("")
  }

  paste(lines, collapse = "\n")
}

#' @keywords internal
synthesize_context_state <- function(session,
                                     state = NULL,
                                     generation_result = NULL,
                                     metrics = NULL,
                                     messages = NULL) {
  state <- normalize_context_state(state)
  regime <- metrics$regime %||% state$occupancy$status %||% "green"

  if (!is.null(generation_result)) {
    tool_digest <- build_context_tool_digest(generation_result)
    artifact_cards <- build_context_artifact_cards(generation_result)
    if (length(tool_digest) > 0) {
      state$tool_digest <- tool_digest
      state <- append_context_event(
        state,
        "tool_digest_refreshed",
        list(count = as.character(length(tool_digest)))
      )
    }
    if (length(artifact_cards) > 0) {
      state$artifact_cards <- artifact_cards
      state <- append_context_event(
        state,
        "artifact_cards_refreshed",
        list(count = as.character(length(artifact_cards)))
      )
    }
  }

  object_cards <- build_context_object_cards(session, regime = regime)
  if (length(object_cards) > 0) {
    state$object_cards <- object_cards
    state <- append_context_event(
      state,
      "object_cards_refreshed",
      list(count = as.character(length(object_cards)))
    )
  } else {
    state$object_cards <- list()
  }
  state <- apply_context_card_updates_from_r_tools(state, generation_result = generation_result)

  execution_log <- collect_execution_log(session = session, generation_result = generation_result)
  if (length(execution_log) > 0) {
    state$execution_log <- execution_log
    state <- append_context_event(
      state,
      "execution_log_refreshed",
      list(count = as.character(length(execution_log)))
    )
  }

  system_info <- collect_system_info()
  if (length(system_info) > 0) {
    state$system_info <- system_info
    state <- append_context_event(
      state,
      "system_info_refreshed",
      list(os = system_info$os$type %||% "unknown")
    )
  }

  runtime_state <- collect_runtime_state(session = session)
  if (length(runtime_state) > 0) {
    state$runtime_state <- runtime_state
    state <- append_context_event(
      state,
      "runtime_state_refreshed",
      list(
        connections = as.character(length(runtime_state$connections %||% list())),
        packages = as.character(length(runtime_state$loaded_packages %||% character(0)))
      )
    )
  }

  carryover <- build_context_carryover(session)
  state$carryover <- carryover
  if (length(carryover) > 0) {
    state <- append_context_event(
      state,
      "carryover_refreshed",
      list(
        current_agent = carryover$current_agent %||% "",
        depth = as.character(carryover$depth %||% 0L)
      )
    )
  }

  if (!is.null(metrics)) {
    state$occupancy <- list(
      estimated_prompt_tokens = metrics$used_tokens %||% NA_real_,
      context_window = metrics$context_window %||% NA_real_,
      ratio = metrics$ratio %||% NA_real_,
      status = metrics$regime %||% "unknown"
    )
    state <- append_context_event(
      state,
      "occupancy_updated",
      list(
        regime = metrics$regime %||% "unknown",
        used_tokens = as.character(round(metrics$used_tokens %||% NA_real_))
      )
    )
  }

  state$active_facts <- build_active_facts(session, state = state, messages = messages)
  state$decisions <- build_decisions(messages = messages)
  state$open_loops <- build_open_loops(state = state, messages = messages, generation_result = generation_result)

  llm_memory <- llm_synthesize_working_memory(
    session,
    state = state,
    messages = messages,
    regime = regime
  )
  if (!is.null(llm_memory)) {
    state$active_facts <- merge_state_items(state$active_facts, llm_memory$active_facts, max_items = 6L)
    state$decisions <- merge_state_items(state$decisions, llm_memory$decisions, max_items = 5L)
    state$open_loops <- merge_state_items(state$open_loops, llm_memory$open_loops, max_items = 5L)
    state <- append_context_event(
      state,
      "working_memory_llm_enriched",
      list(
        active_facts = as.character(length(llm_memory$active_facts %||% list())),
        decisions = as.character(length(llm_memory$decisions %||% list())),
        open_loops = as.character(length(llm_memory$open_loops %||% list()))
      )
    )
  }

  state <- append_context_event(
    state,
    "working_memory_refreshed",
    list(
      active_facts = as.character(length(state$active_facts)),
      decisions = as.character(length(state$decisions)),
      open_loops = as.character(length(state$open_loops))
    )
  )

  stored_recoveries <- record_project_memory_recoveries(session, generation_result = generation_result)
  if (stored_recoveries > 0) {
    state <- append_context_event(
      state,
      "project_memory_fix_stored",
      list(count = as.character(stored_recoveries))
    )
  }

  state
}

#' @keywords internal
resolve_project_memory_root <- function(session) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    return(NULL)
  }

  candidates <- list(
    session$get_metadata("project_memory_root", default = NULL),
    session$get_metadata("console_startup_dir", default = NULL)
  )
  candidates <- Filter(function(x) is.character(x) && length(x) >= 1 && !all(is.na(x)) && any(nzchar(x)), candidates)
  if (length(candidates) == 0) {
    return(NULL)
  }
  candidates <- unique(unlist(candidates, use.names = FALSE))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if (length(candidates) == 0) {
    return(NULL)
  }
  candidates <- candidates[dir.exists(candidates)]
  if (length(candidates) == 0) {
    return(NULL)
  }
  normalizePath(candidates[[1]], winslash = "/", mustWork = TRUE)
}

#' @keywords internal
build_project_memory_query <- function(messages = NULL, state = NULL) {
  messages <- messages %||% list()
  state <- normalize_context_state(state)

  user_messages <- Filter(function(msg) identical(msg$role %||% "", "user"), messages)
  if (length(user_messages) > 0) {
    latest <- normalize_retrieval_query(tail(user_messages, 1L)[[1]]$content %||% "")
    if (nzchar(latest)) {
      return(latest)
    }
  }

  if (length(state$open_loops) > 0) {
    return(normalize_retrieval_query(state$open_loops[[1]]$text %||% ""))
  }
  if (length(state$decisions) > 0) {
    return(normalize_retrieval_query(state$decisions[[1]]$text %||% ""))
  }
  ""
}

#' @keywords internal
normalize_retrieval_query <- function(text) {
  text <- tolower(trimws(text %||% ""))
  text <- gsub("[^[:alnum:]\u4e00-\u9f8d]+", " ", text, perl = TRUE)
  tokens <- strsplit(text, "\\s+")[[1]]
  stopwords <- c(
    "the", "a", "an", "and", "or", "for", "with", "from", "that", "this",
    "can", "you", "please", "just", "into", "when", "what", "will",
    "need", "later", "step", "use", "using", "results", "result"
  )
  tokens <- tokens[nzchar(tokens)]
  tokens <- tokens[!(tokens %in% stopwords)]
  if (length(tokens) == 0) {
    return("")
  }
  paste(utils::head(tokens, 4L), collapse = " ")
}

#' @keywords internal
retrieval_query_tokens <- function(query) {
  query <- normalize_retrieval_query(query)
  if (!nzchar(query)) {
    return(character(0))
  }
  strsplit(query, "\\s+")[[1]]
}

#' @keywords internal
retrieval_text_matches <- function(text, query, min_hits = 1L) {
  text <- tolower(text %||% "")
  tokens <- retrieval_query_tokens(query)
  if (length(tokens) == 0 || !nzchar(text)) {
    return(FALSE)
  }
  hits <- sum(vapply(tokens, function(token) grepl(token, text, fixed = TRUE), logical(1)))
  hits >= min_hits
}

#' @keywords internal
detect_query_task_signals <- function(query) {
  query <- tolower(query %||% "")
  signals <- character(0)

  if (grepl("error|fail|debug|trace|fix|broken|exception|warning", query)) {
    signals <- c(signals, "debug", "error", "runtime", "execution")
  }
  if (grepl("next|todo|follow up|follow-up|remaining|pending|later", query)) {
    signals <- c(signals, "next", "todo", "state")
  }
  if (grepl("decid|choose|adopt|prefer|plan", query)) {
    signals <- c(signals, "decision", "plan")
  }
  if (grepl("artifact|file|output|report|plot|png|pdf", query)) {
    signals <- c(signals, "artifact", "output", "file")
  }
  if (grepl("inspect|assay|metadata|cluster|dimension|reduceddim|coldata|rowdata|data", query)) {
    signals <- c(signals, "inspect", "data")
  }
  if (grepl("function|source|docs|documentation|method|accessor|code|script", query)) {
    signals <- c(signals, "function", "code", "inspect")
  }
  if (grepl("context|state|agent|task|system|environment|model|device|runtime", query)) {
    signals <- c(signals, "context", "state", "task", "runtime", "system")
  }
  if (grepl("memory|cpu|performance|timing|speed|slow", query)) {
    signals <- c(signals, "performance", "system", "memory")
  }
  if (grepl("connection|database|file|io|graphics|device", query)) {
    signals <- c(signals, "connection", "io", "runtime", "device")
  }
  if (grepl("package|library|search|path|option|setting", query)) {
    signals <- c(signals, "packages", "runtime", "settings", "environment")
  }
  if (grepl("execution|run|execute|command|result", query)) {
    signals <- c(signals, "execution", "runtime")
  }
  if (grepl("os|platform|version|r version", query)) {
    signals <- c(signals, "os", "version", "system")
  }

  unique(signals)
}

#' @keywords internal
task_signal_alignment <- function(candidate_tags, query_signals) {
  candidate_tags <- unique(candidate_tags %||% character(0))
  query_signals <- unique(query_signals %||% character(0))
  if (length(candidate_tags) == 0 || length(query_signals) == 0) {
    return(0)
  }
  sum(candidate_tags %in% query_signals) / length(query_signals)
}

#' @keywords internal
get_retrieval_runtime_config <- function(session) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    return(normalize_context_management_config(list()))
  }
  get_context_management_config_impl(session)
}

#' @keywords internal
retrieval_reranking_enabled <- function(session = NULL) {
  cfg <- get_retrieval_runtime_config(session)
  isTRUE(cfg$retrieval_reranking)
}

#' @keywords internal
resolve_retrieval_reranking_model <- function(session = NULL) {
  cfg <- get_retrieval_runtime_config(session)
  cfg$retrieval_reranking_model %||% NULL
}

#' @keywords internal
resolve_retrieval_reranking_policy <- function(session = NULL) {
  cfg <- get_retrieval_runtime_config(session)
  cfg$retrieval_reranking_policy %||% normalize_retrieval_reranking_policy()
}

#' @keywords internal
parse_retrieval_timestamp <- function(x) {
  if (is.null(x) || !nzchar(trimws(x %||% ""))) {
    return(NA)
  }
  parsed <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
  if (is.na(parsed)) {
    parsed <- suppressWarnings(as.POSIXct(x))
  }
  parsed
}

#' @keywords internal
compute_retrieval_match_stats <- function(text, query) {
  tokens <- retrieval_query_tokens(query)
  haystack <- tolower(text %||% "")
  if (length(tokens) == 0 || !nzchar(haystack)) {
    return(list(token_hits = 0L, token_total = max(length(tokens), 1L), exact_query = FALSE))
  }
  token_hits <- sum(vapply(tokens, function(token) grepl(token, haystack, fixed = TRUE), logical(1)))
  exact_query <- grepl(normalize_retrieval_query(query), haystack, fixed = TRUE)
  list(
    token_hits = as.integer(token_hits),
    token_total = as.integer(max(length(tokens), 1L)),
    exact_query = isTRUE(exact_query)
  )
}

#' @keywords internal
retrieval_bigrams <- function(query) {
  tokens <- retrieval_query_tokens(query)
  if (length(tokens) < 2) {
    return(character(0))
  }
  vapply(seq_len(length(tokens) - 1L), function(i) paste(tokens[[i]], tokens[[i + 1L]], collapse = " "), character(1))
}

#' @keywords internal
compute_bigram_hits <- function(text, query) {
  bigrams <- retrieval_bigrams(query)
  haystack <- tolower(text %||% "")
  if (length(bigrams) == 0 || !nzchar(haystack)) {
    return(0L)
  }
  sum(vapply(bigrams, function(bg) grepl(bg, haystack, fixed = TRUE), logical(1)))
}

#' @keywords internal
score_retrieval_candidate <- function(candidate, query, cfg, provider_rank = 999L) {
  policy <- cfg$retrieval_scoring_policy %||% default_retrieval_scoring_policy()
  provider <- candidate$provider %||% "unknown"
  provider_weight <- policy$provider_weights[[provider]] %||% 1.0
  title_text <- candidate$title %||% ""
  summary_text <- candidate$summary %||% ""
  preview_text <- candidate$preview %||% ""
  workflow_text <- paste(candidate$workflow_steps %||% character(0), collapse = " ")
  accessor_text <- paste(candidate$accessors %||% character(0), collapse = " ")
  text <- paste(title_text, summary_text, preview_text, collapse = " ")

  stats <- compute_retrieval_match_stats(text, query)
  title_stats <- compute_retrieval_match_stats(title_text, query)
  summary_stats <- compute_retrieval_match_stats(summary_text, query)
  preview_stats <- compute_retrieval_match_stats(preview_text, query)
  token_ratio <- stats$token_hits / stats$token_total
  title_ratio <- title_stats$token_hits / title_stats$token_total
  summary_ratio <- summary_stats$token_hits / summary_stats$token_total
  preview_ratio <- preview_stats$token_hits / preview_stats$token_total
  bigram_hits <- compute_bigram_hits(text, query)
  bigram_total <- max(length(retrieval_bigrams(query)), 1L)
  bigram_ratio <- bigram_hits / bigram_total
  coverage <- sum(c(title_stats$token_hits, summary_stats$token_hits, preview_stats$token_hits) > 0) / 3
  workflow_stats <- compute_retrieval_match_stats(workflow_text, query)
  accessor_stats <- compute_retrieval_match_stats(accessor_text, query)
  workflow_ratio <- workflow_stats$token_hits / workflow_stats$token_total
  accessor_ratio <- accessor_stats$token_hits / accessor_stats$token_total
  query_signals <- detect_query_task_signals(query)
  task_alignment <- task_signal_alignment(candidate$task_tags %||% character(0), query_signals)

  match_component <- policy$token_match_weight * token_ratio
  exact_component <- if (isTRUE(stats$exact_query)) policy$exact_query_bonus else 0
  title_component <- policy$title_match_weight * title_ratio
  summary_component <- policy$summary_match_weight * summary_ratio
  preview_component <- policy$preview_match_weight * preview_ratio
  bigram_component <- policy$bigram_match_weight * bigram_ratio
  coverage_component <- policy$coverage_weight * coverage
  workflow_component <- policy$workflow_hint_weight * workflow_ratio
  accessor_component <- policy$accessor_match_weight * accessor_ratio
  semantic_component <- if (identical(provider, "semantic_objects")) policy$semantic_object_bonus else 0
  task_signal_component <- policy$task_signal_weight * task_alignment
  source_component <- policy$source_kind_bonus[[provider]] %||% 0

  recency_component <- 0
  ts <- parse_retrieval_timestamp(candidate$timestamp %||% candidate$created_at %||% candidate$updated_at %||% "")
  if (!is.na(ts)) {
    age_days <- max(as.numeric(difftime(Sys.time(), ts, units = "days")), 0)
    freshness <- max(0, 1 - min(age_days, 30) / 30)
    recency_component <- policy$recency_weight * freshness
  }

  candidate$score <- as.numeric(
    provider_weight +
      match_component +
      exact_component +
      title_component +
      summary_component +
      preview_component +
      bigram_component +
      coverage_component +
      workflow_component +
      accessor_component +
      semantic_component +
      task_signal_component +
      source_component +
      recency_component
  )
  candidate$provider_rank <- as.integer(provider_rank)
  candidate$token_hits <- stats$token_hits
  candidate$token_total <- stats$token_total
  candidate$exact_query <- isTRUE(stats$exact_query)
  candidate$bigram_hits <- as.integer(bigram_hits)
  candidate$coverage <- coverage
  candidate$workflow_hits <- workflow_stats$token_hits
  candidate$accessor_hits <- accessor_stats$token_hits
  candidate$task_alignment <- task_alignment
  candidate
}

#' @keywords internal
rank_retrieval_hits <- function(retrieval_cache, query, cfg) {
  retrieval_cache <- retrieval_cache %||% list()
  order_vec <- retrieval_cache$provider_order %||% default_retrieval_provider_names()
  rank_map <- stats::setNames(seq_along(order_vec), order_vec)

  providers <- list(
    project_memory_snippets = retrieval_cache$snippets %||% list(),
    project_memory_fixes = retrieval_cache$fixes %||% list(),
    semantic_objects = retrieval_cache$semantic_objects %||% list(),
    task_state = retrieval_cache$task_state %||% list(),
    session_memory = retrieval_cache$session_memory %||% list(),
    documents = retrieval_cache$documents %||% list(),
    transcript_segments = retrieval_cache$transcript_segments %||% list(),
    execution_monitor = retrieval_cache$execution_monitor %||% list(),
    system_info = retrieval_cache$system_info %||% list(),
    runtime_state = retrieval_cache$runtime_state %||% list()
  )

  hits <- list()
  for (provider_name in names(providers)) {
    entries <- providers[[provider_name]]
    if (length(entries) == 0) {
      next
    }
    for (entry in entries) {
      candidate <- list(
        retrieval_id = sprintf("%s-%02d", provider_name, length(hits) + 1L),
        provider = provider_name,
        title = entry$title %||% entry$file_name %||% entry$key %||% entry$summary %||% provider_name,
        summary = entry$summary %||% entry$description %||% "",
        preview = entry$preview %||% "",
        timestamp = entry$timestamp %||% entry$created_at %||% entry$updated_at %||% "",
        workflow_steps = entry$workflow_steps %||% character(0),
        accessors = entry$accessors %||% character(0),
        task_tags = entry$task_tags %||% character(0)
      )
      hits[[length(hits) + 1L]] <- score_retrieval_candidate(
        candidate,
        query = query,
        cfg = cfg,
        provider_rank = rank_map[[provider_name]] %||% 999L
      )
    }
  }

  if (length(hits) == 0) {
    return(list())
  }

  scores <- vapply(hits, function(hit) hit$score %||% 0, numeric(1))
  ranks <- vapply(hits, function(hit) hit$provider_rank %||% 999L, integer(1))
  token_hits <- vapply(hits, function(hit) hit$token_hits %||% 0L, integer(1))
  bigram_hits <- vapply(hits, function(hit) hit$bigram_hits %||% 0L, integer(1))
  coverage <- vapply(hits, function(hit) hit$coverage %||% 0, numeric(1))
  ordered <- order(scores, coverage, token_hits, bigram_hits, -ranks, decreasing = TRUE)
  hits <- hits[ordered]

  max_total <- cfg$retrieval_scoring_policy$max_total_results %||% 6L
  hits[seq_len(min(length(hits), max_total))]
}

#' @keywords internal
learned_rerank_retrieval_hits <- function(hits, query, session = NULL, regime = "green") {
  if (length(hits) < 2 || !retrieval_reranking_enabled(session)) {
    return(hits)
  }

  model_ref <- resolve_retrieval_reranking_model(session)
  if (is.null(model_ref)) {
    return(hits)
  }

  policy <- resolve_retrieval_reranking_policy(session)
  current_regime <- regime %||% "green"
  if (identical(current_regime, "unknown")) {
    current_regime <- "green"
  }
  if (context_regime_rank(current_regime) < context_regime_rank(policy$min_regime)) {
    return(hits)
  }

  top_n <- min(length(hits), policy$top_n %||% 4L)
  rerank_hits <- hits[seq_len(top_n)]
  remainder <- if (length(hits) > top_n) hits[(top_n + 1L):length(hits)] else list()

  schema <- z_object(
    ordered_ids = z_array(z_string("Candidate retrieval_id in preferred order"))
  )
  prompt_lines <- c(
    "Rerank the retrieval candidates by which ones should be shown first to the agent.",
    "Prefer items that are most relevant to the query and most useful for the immediate task.",
    sprintf("Query: %s", query),
    "",
    "Candidates:"
  )
  prompt_lines <- c(prompt_lines, vapply(rerank_hits, function(hit) {
    sprintf(
      "- id=%s | provider=%s | deterministic_score=%.2f | title=%s | summary=%s | preview=%s",
      hit$retrieval_id %||% "",
      hit$provider %||% "",
      hit$score %||% 0,
      trim_context_preview(hit$title %||% "", max_chars = 80L),
      trim_context_preview(hit$summary %||% "", max_chars = 120L),
      trim_context_preview(hit$preview %||% "", max_chars = 120L)
    )
  }, character(1)))

  result <- tryCatch(
    generate_object(
      model = model_ref,
      prompt = paste(prompt_lines, collapse = "\n"),
      schema = schema,
      schema_name = "retrieval_rerank",
      temperature = policy$temperature %||% 0
    ),
    error = function(e) NULL
  )
  ordered_ids <- result$object$ordered_ids %||% character(0)
  ordered_ids <- as.character(ordered_ids)
  ordered_ids <- ordered_ids[ordered_ids %in% vapply(rerank_hits, function(hit) hit$retrieval_id %||% "", character(1))]
  if (length(ordered_ids) == 0) {
    return(hits)
  }

  by_id <- stats::setNames(rerank_hits, vapply(rerank_hits, function(hit) hit$retrieval_id %||% "", character(1)))
  reranked <- unname(by_id[ordered_ids])
  remaining_ids <- setdiff(names(by_id), ordered_ids)
  if (length(remaining_ids) > 0) {
    reranked <- c(reranked, unname(by_id[remaining_ids]))
  }
  reranked <- c(reranked, remainder)

  for (i in seq_along(reranked)) {
    reranked[[i]]$reranked <- TRUE
    reranked[[i]]$rerank_position <- i
  }
  reranked
}

#' @keywords internal
render_retrieval_value_preview <- function(value, max_chars = 180L) {
  if (is.null(value)) {
    return("")
  }
  if (is.character(value)) {
    return(trim_context_preview(paste(value, collapse = " "), max_chars = max_chars))
  }
  if (is.atomic(value)) {
    return(trim_context_preview(paste(as.character(utils::head(value, 10L)), collapse = ", "), max_chars = max_chars))
  }
  if (is.data.frame(value)) {
    preview <- paste(utils::capture.output(print(utils::head(value, 3L))), collapse = " ")
    return(trim_context_preview(preview, max_chars = max_chars))
  }
  if (is.list(value)) {
    preview <- tryCatch(safe_to_json(value, auto_unbox = TRUE), error = function(e) {
      paste(capture.output(str(value, max.level = 1, list.len = 5)), collapse = " ")
    })
    return(trim_context_preview(preview, max_chars = max_chars))
  }
  trim_context_preview(as.character(value), max_chars = max_chars)
}

#' @keywords internal
build_session_memory_retrieval <- function(session, query, limit = 3L, min_hits = 1L) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    return(list())
  }

  keys <- session$list_memory() %||% character(0)
  if (length(keys) == 0 || !nzchar(query)) {
    return(list())
  }

  matches <- list()
  for (key in keys) {
    value <- session$get_memory(key)
    preview <- render_retrieval_value_preview(value)
    haystack <- paste(key, preview, collapse = " ")
    if (!retrieval_text_matches(haystack, query, min_hits = min_hits)) {
      next
    }
    matches[[length(matches) + 1L]] <- list(
      key = key,
      summary = sprintf("Memory `%s`", key),
      preview = preview
    )
    if (length(matches) >= limit) {
      break
    }
  }

  matches
}

#' @keywords internal
build_document_retrieval <- function(session, query, limit = 2L, min_hits = 1L) {
  if (is.null(session) || !inherits(session, "ChatSession") || !nzchar(query)) {
    return(list())
  }

  documents <- session$get_metadata("channel_documents", default = list()) %||% list()
  if (length(documents) == 0) {
    return(list())
  }

  matches <- list()
  for (doc in rev(documents)) {
    summary <- doc$summary %||% ""
    chunks <- unlist(doc$chunks %||% list(), use.names = FALSE)
    matched_chunk <- ""
    if (!retrieval_text_matches(summary, query, min_hits = min_hits) && length(chunks) > 0) {
      for (chunk in chunks) {
        if (retrieval_text_matches(chunk, query, min_hits = min_hits)) {
          matched_chunk <- chunk
          break
        }
      }
      if (!nzchar(matched_chunk)) {
        next
      }
    }
    matches[[length(matches) + 1L]] <- list(
      file_name = doc$file_name %||% "document",
      summary = trim_context_preview(summary %||% matched_chunk, max_chars = 160L),
      preview = if (nzchar(matched_chunk)) trim_context_preview(matched_chunk, max_chars = 180L) else ""
    )
    if (length(matches) >= limit) {
      break
    }
  }

  matches
}

#' @keywords internal
build_transcript_segment_retrieval <- function(state, query, limit = 2L, min_hits = 1L) {
  state <- normalize_context_state(state)
  segments <- state$transcript_segments %||% list()
  if (length(segments) == 0 || !nzchar(query)) {
    return(list())
  }

  matches <- list()
  for (segment in rev(segments)) {
    summary <- segment$summary %||% ""
    if (!retrieval_text_matches(summary, query, min_hits = min_hits)) {
      next
    }
    matches[[length(matches) + 1L]] <- list(
      summary = trim_context_preview(summary, max_chars = 180L),
      preview = trim_context_preview(summary, max_chars = 180L)
    )
    if (length(matches) >= limit) {
      break
    }
  }

  matches
}

#' @keywords internal
build_semantic_object_retrieval <- function(state, query, limit = 3L, min_hits = 1L) {
  state <- normalize_context_state(state)
  cards <- state$object_cards %||% list()
  if (length(cards) == 0 || !nzchar(query)) {
    return(list())
  }

  matches <- list()
  for (card in unname(cards)) {
    workflow_steps <- card$workflow_hint$steps %||% character(0)
    accessors <- card$accessors %||% character(0)
    inspection_lines <- unlist(card$inspection_hint %||% list(), use.names = FALSE)
    haystack <- paste(
      card$name %||% "",
      paste(card$class %||% character(0), collapse = " "),
      card$adapter %||% "",
      card$summary %||% "",
      paste(workflow_steps, collapse = " "),
      paste(accessors, collapse = " "),
      paste(inspection_lines, collapse = " "),
      collapse = " "
    )
    if (!retrieval_text_matches(haystack, query, min_hits = min_hits)) {
      next
    }

    matches[[length(matches) + 1L]] <- list(
      key = card$name %||% "",
      summary = card$summary %||% "",
      preview = paste(c(
        if (length(workflow_steps) > 0) paste("workflow:", paste(utils::head(workflow_steps, 3L), collapse = " -> ")) else NULL,
        if (length(accessors) > 0) paste("accessors:", paste(utils::head(accessors, 4L), collapse = ", ")) else NULL
      ), collapse = " | "),
      workflow_steps = workflow_steps,
      accessors = accessors,
      adapter = card$adapter %||% "",
      kind = card$kind %||% "object",
      title = card$name %||% ""
    )
    if (length(matches) >= limit) {
      break
    }
  }

  matches
}

#' @keywords internal
build_task_state_retrieval <- function(state, query, limit = 4L, min_hits = 0L) {
  state <- normalize_context_state(state)
  if (!nzchar(query)) {
    return(list())
  }

  candidates <- list()

  for (item in state$active_facts %||% list()) {
    candidates[[length(candidates) + 1L]] <- list(
      title = "Active fact",
      summary = item$text %||% "",
      preview = "",
      task_tags = c("fact", "state", "context"),
      category = "active_fact"
    )
  }

  for (item in state$decisions %||% list()) {
    candidates[[length(candidates) + 1L]] <- list(
      title = "Decision",
      summary = item$text %||% "",
      preview = "",
      task_tags = c("decision", "plan", "state"),
      category = "decision"
    )
  }

  for (item in state$open_loops %||% list()) {
    candidates[[length(candidates) + 1L]] <- list(
      title = "Open loop",
      summary = item$text %||% "",
      preview = "",
      task_tags = c("next", "todo", "state", if (identical(item$source %||% "", "tool_error")) c("debug", "error", "runtime") else character(0)),
      category = "open_loop"
    )
  }

  carryover <- state$carryover %||% list()
  if (!is.null(carryover$current_agent) || length(carryover$stack %||% list()) > 0) {
    candidates[[length(candidates) + 1L]] <- list(
      title = "Carryover state",
      summary = trim_context_preview(paste(
        carryover$current_agent %||% "",
        carryover$global_task %||% "",
        paste(vapply(carryover$stack %||% list(), function(item) item$task %||% "", character(1)), collapse = " ")
      ), max_chars = 180L),
      preview = "",
      task_tags = c("task", "state", "context", "runtime"),
      category = "carryover"
    )
  }

  for (item in state$tool_digest %||% list()) {
    candidates[[length(candidates) + 1L]] <- list(
      title = sprintf("Tool %s", item$tool %||% "tool"),
      summary = item$summary %||% "",
      preview = "",
      task_tags = c("tool", "runtime", if (identical(item$status %||% "", "error")) c("debug", "error") else character(0)),
      category = "tool_digest",
      timestamp = item$timestamp %||% ""
    )
  }

  for (item in state$artifact_cards %||% list()) {
    candidates[[length(candidates) + 1L]] <- list(
      title = basename(item$path %||% ""),
      summary = item$summary %||% "",
      preview = "",
      task_tags = c("artifact", "output", "file", "state"),
      category = "artifact",
      timestamp = item$timestamp %||% ""
    )
  }

  matches <- list()
  query_signals <- detect_query_task_signals(query)
  for (candidate in candidates) {
    haystack <- paste(candidate$title %||% "", candidate$summary %||% "", collapse = " ")
    if (!retrieval_text_matches(haystack, query, min_hits = min_hits) &&
        task_signal_alignment(candidate$task_tags %||% character(0), query_signals) <= 0) {
      next
    }
    matches[[length(matches) + 1L]] <- candidate
    if (length(matches) >= limit) {
      break
    }
  }

  matches
}

#' @keywords internal
build_execution_monitor_retrieval <- function(state, query, limit = 3L, min_hits = 1L) {
  state <- normalize_context_state(state)
  execution_log <- state$execution_log %||% list()
  if (length(execution_log) == 0 || !nzchar(query)) {
    return(list())
  }

  matches <- list()
  for (entry in rev(execution_log)) {
    haystack <- paste(
      entry$command %||% "",
      entry$result %||% "",
      entry$error %||% "",
      entry$warning %||% "",
      collapse = " "
    )
    if (!retrieval_text_matches(haystack, query, min_hits = min_hits)) {
      next
    }

    matches[[length(matches) + 1L]] <- list(
      title = sprintf("Execution: %s", trim_context_preview(entry$command %||% "command", max_chars = 60L)),
      summary = if (!is.null(entry$error) && nzchar(entry$error %||% "")) {
        sprintf("Error: %s", trim_context_preview(entry$error, max_chars = 140L))
      } else if (!is.null(entry$warning) && nzchar(entry$warning %||% "")) {
        sprintf("Warning: %s", trim_context_preview(entry$warning, max_chars = 140L))
      } else {
        trim_context_preview(entry$result %||% "success", max_chars = 160L)
      },
      preview = paste(c(
        if (!is.null(entry$timing)) sprintf("timing: %.2fs", entry$timing) else NULL,
        if (!is.null(entry$memory_mb)) sprintf("memory: %.1fMB", entry$memory_mb) else NULL,
        if (!is.null(entry$exit_code)) sprintf("exit: %d", entry$exit_code) else NULL
      ), collapse = " | "),
      task_tags = c(
        "execution", "runtime",
        if (!is.null(entry$error) && nzchar(entry$error %||% "")) c("error", "debug") else character(0),
        if (!is.null(entry$warning) && nzchar(entry$warning %||% "")) "warning" else character(0)
      ),
      timestamp = entry$timestamp %||% ""
    )
    if (length(matches) >= limit) {
      break
    }
  }

  matches
}

#' @keywords internal
build_system_info_retrieval <- function(state, query, limit = 2L, min_hits = 1L) {
  state <- normalize_context_state(state)
  system_info <- state$system_info %||% list()
  if (length(system_info) == 0 || !nzchar(query)) {
    return(list())
  }

  candidates <- list()

  if (!is.null(system_info$os)) {
    candidates[[length(candidates) + 1L]] <- list(
      title = "Operating System",
      summary = sprintf("%s %s", system_info$os$type %||% "", system_info$os$version %||% ""),
      preview = paste(c(
        if (!is.null(system_info$os$platform)) sprintf("platform: %s", system_info$os$platform) else NULL,
        if (!is.null(system_info$os$arch)) sprintf("arch: %s", system_info$os$arch) else NULL
      ), collapse = " | "),
      task_tags = c("system", "os", "environment")
    )
  }

  if (!is.null(system_info$r_version)) {
    candidates[[length(candidates) + 1L]] <- list(
      title = "R Version",
      summary = sprintf("R %s", system_info$r_version %||% ""),
      preview = if (!is.null(system_info$r_platform)) sprintf("platform: %s", system_info$r_platform) else "",
      task_tags = c("r", "version", "runtime")
    )
  }

  if (!is.null(system_info$memory)) {
    candidates[[length(candidates) + 1L]] <- list(
      title = "Memory",
      summary = sprintf("Total: %.1fGB, Available: %.1fGB",
                       (system_info$memory$total_mb %||% 0) / 1024,
                       (system_info$memory$available_mb %||% 0) / 1024),
      preview = if (!is.null(system_info$memory$used_percent)) {
        sprintf("used: %.1f%%", system_info$memory$used_percent)
      } else {
        ""
      },
      task_tags = c("memory", "system", "performance")
    )
  }

  if (!is.null(system_info$cpu)) {
    candidates[[length(candidates) + 1L]] <- list(
      title = "CPU",
      summary = sprintf("%d cores", system_info$cpu$cores %||% 0),
      preview = if (!is.null(system_info$cpu$model)) {
        trim_context_preview(system_info$cpu$model, max_chars = 80L)
      } else {
        ""
      },
      task_tags = c("cpu", "system", "performance")
    )
  }

  if (!is.null(system_info$working_dir)) {
    candidates[[length(candidates) + 1L]] <- list(
      title = "Working Directory",
      summary = system_info$working_dir,
      preview = "",
      task_tags = c("directory", "environment", "path")
    )
  }

  matches <- list()
  for (candidate in candidates) {
    haystack <- paste(candidate$title %||% "", candidate$summary %||% "", candidate$preview %||% "", collapse = " ")
    if (!retrieval_text_matches(haystack, query, min_hits = min_hits)) {
      next
    }
    matches[[length(matches) + 1L]] <- candidate
    if (length(matches) >= limit) {
      break
    }
  }

  matches
}

#' @keywords internal
build_runtime_state_retrieval <- function(state, query, limit = 3L, min_hits = 1L) {
  state <- normalize_context_state(state)
  runtime_state <- state$runtime_state %||% list()
  if (length(runtime_state) == 0 || !nzchar(query)) {
    return(list())
  }

  candidates <- list()

  if (!is.null(runtime_state$connections) && length(runtime_state$connections) > 0) {
    for (conn in runtime_state$connections) {
      candidates[[length(candidates) + 1L]] <- list(
        title = sprintf("Connection: %s", conn$description %||% "unknown"),
        summary = sprintf("Type: %s, Status: %s", conn$class %||% "connection", conn$status %||% "open"),
        preview = if (!is.null(conn$mode)) sprintf("mode: %s", conn$mode) else "",
        task_tags = c("connection", "runtime", "io")
      )
    }
  }

  if (!is.null(runtime_state$graphics_devices) && length(runtime_state$graphics_devices) > 0) {
    candidates[[length(candidates) + 1L]] <- list(
      title = "Graphics Devices",
      summary = sprintf("%d active device(s)", length(runtime_state$graphics_devices)),
      preview = paste(vapply(runtime_state$graphics_devices, function(d) d$name %||% "device", character(1)), collapse = ", "),
      task_tags = c("graphics", "device", "runtime")
    )
  }

  if (!is.null(runtime_state$options) && length(runtime_state$options) > 0) {
    key_options <- c("width", "digits", "scipen", "stringsAsFactors", "warn", "error")
    relevant_opts <- runtime_state$options[names(runtime_state$options) %in% key_options]
    if (length(relevant_opts) > 0) {
      candidates[[length(candidates) + 1L]] <- list(
        title = "R Options",
        summary = paste(vapply(names(relevant_opts), function(n) {
          sprintf("%s=%s", n, as.character(relevant_opts[[n]]))
        }, character(1)), collapse = ", "),
        preview = "",
        task_tags = c("options", "settings", "runtime")
      )
    }
  }

  if (!is.null(runtime_state$search_path) && length(runtime_state$search_path) > 0) {
    candidates[[length(candidates) + 1L]] <- list(
      title = "Search Path",
      summary = sprintf("%d entries", length(runtime_state$search_path)),
      preview = paste(utils::head(runtime_state$search_path, 5L), collapse = ", "),
      task_tags = c("search", "path", "runtime", "packages")
    )
  }

  if (!is.null(runtime_state$loaded_packages) && length(runtime_state$loaded_packages) > 0) {
    candidates[[length(candidates) + 1L]] <- list(
      title = "Loaded Packages",
      summary = sprintf("%d package(s) loaded", length(runtime_state$loaded_packages)),
      preview = paste(utils::head(runtime_state$loaded_packages, 8L), collapse = ", "),
      task_tags = c("packages", "runtime", "environment")
    )
  }

  if (!is.null(runtime_state$env_vars) && length(runtime_state$env_vars) > 0) {
    key_vars <- c("PATH", "HOME", "R_LIBS", "R_LIBS_USER", "TMPDIR", "LANG")
    relevant_vars <- runtime_state$env_vars[names(runtime_state$env_vars) %in% key_vars]
    if (length(relevant_vars) > 0) {
      candidates[[length(candidates) + 1L]] <- list(
        title = "Environment Variables",
        summary = sprintf("%d key variable(s)", length(relevant_vars)),
        preview = paste(vapply(names(relevant_vars), function(n) {
          sprintf("%s=%s", n, trim_context_preview(relevant_vars[[n]], max_chars = 40L))
        }, character(1)), collapse = " | "),
        task_tags = c("environment", "variables", "system")
      )
    }
  }

  matches <- list()
  for (candidate in candidates) {
    haystack <- paste(candidate$title %||% "", candidate$summary %||% "", candidate$preview %||% "", collapse = " ")
    if (!retrieval_text_matches(haystack, query, min_hits = min_hits)) {
      next
    }
    matches[[length(matches) + 1L]] <- candidate
    if (length(matches) >= limit) {
      break
    }
  }

  matches
}

#' @keywords internal
extract_tool_recovery_input <- function(arguments = list()) {
  arguments <- arguments %||% list()
  for (field in c("code", "command", "path", "name")) {
    value <- arguments[[field]] %||% NULL
    if (is.character(value) && length(value) == 1 && nzchar(trimws(value))) {
      return(trimws(value))
    }
  }
  ""
}

#' @keywords internal
record_project_memory_recoveries <- function(session, generation_result = NULL) {
  if (is.null(generation_result) ||
      !requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RSQLite", quietly = TRUE)) {
    return(0L)
  }

  project_root <- resolve_project_memory_root(session)
  if (is.null(project_root) || !dir.exists(project_root)) {
    return(0L)
  }

  memory <- tryCatch(project_memory(project_root = project_root), error = function(e) NULL)
  if (is.null(memory)) {
    return(0L)
  }

  tool_calls <- generation_result$all_tool_calls %||% list()
  tool_results <- generation_result$all_tool_results %||% list()
  if (length(tool_calls) == 0 || length(tool_results) == 0) {
    return(0L)
  }

  calls_by_id <- setNames(tool_calls, vapply(tool_calls, function(tc) tc$id %||% "", character(1)))
  last_errors <- list()
  stored <- 0L
  recoverable_tools <- c("execute_r_code", "execute_r_code_local", "bash")

  for (tool_result in tool_results) {
    if (!(tool_result$name %in% recoverable_tools)) {
      next
    }
    tool_call <- calls_by_id[[tool_result$id %||% ""]]
    if (is.null(tool_call)) {
      next
    }
    input_text <- extract_tool_recovery_input(tool_call$arguments)
    if (!nzchar(input_text)) {
      next
    }

    if (isTRUE(tool_result$is_error)) {
      last_errors[[tool_result$name]] <- list(
        input = input_text,
        error = tool_result$result %||% "Tool execution error"
      )
      next
    }

    prior_error <- last_errors[[tool_result$name]] %||% NULL
    if (is.null(prior_error)) {
      next
    }
    if (identical(trimws(prior_error$input %||% ""), trimws(input_text))) {
      next
    }

    tryCatch(
      memory$store_fix(
        original_code = prior_error$input,
        error = prior_error$error %||% "Tool execution error",
        fixed_code = input_text,
        fix_description = sprintf("Recovered `%s` after a previous tool error during adaptive context execution.", tool_result$name)
      ),
      error = function(e) NULL
    )
    stored <- stored + 1L
    last_errors[[tool_result$name]] <- NULL
  }

  stored
}

#' @keywords internal
build_project_memory_fix_retrieval <- function(session, state = NULL, limit = 2L) {
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
    return(list())
  }

  project_root <- resolve_project_memory_root(session)
  if (is.null(project_root) || !dir.exists(project_root)) {
    return(list())
  }

  memory <- tryCatch(project_memory(project_root = project_root), error = function(e) NULL)
  if (is.null(memory)) {
    return(list())
  }

  state <- normalize_context_state(state)
  tool_errors <- Filter(function(item) identical(item$source %||% "", "tool_error"), state$open_loops %||% list())
  if (length(tool_errors) == 0) {
    return(list())
  }

  fixes <- list()
  for (item in tool_errors) {
    error_query <- sub("^Resolve tool error in `[^`]+`:\\s*", "", item$text %||% "", perl = TRUE)
    similar <- tryCatch(memory$find_similar_fix(error_query), error = function(e) NULL)
    if (is.null(similar) || length(similar) == 0) {
      next
    }
    fixes[[length(fixes) + 1L]] <- list(
      error = trim_context_preview(similar$error_message %||% similar$error %||% "", max_chars = 160L),
      fix = trim_context_preview(similar$fixed_code %||% "", max_chars = 180L),
      description = trim_context_preview(similar$fix_description %||% "Known fix", max_chars = 160L),
      created_at = similar$created_at %||% ""
    )
    if (length(fixes) >= limit) {
      break
    }
  }

  fixes
}

#' @keywords internal
build_project_memory_retrieval <- function(session, messages = NULL, state = NULL, limit = 3L) {
  cfg <- get_retrieval_runtime_config(session)
  providers <- cfg$retrieval_providers %||% list()
  provider_order <- cfg$retrieval_provider_order %||% default_retrieval_provider_names()
  limits <- cfg$retrieval_provider_limits %||% default_retrieval_provider_limits()
  min_hits <- cfg$retrieval_min_hits %||% default_retrieval_min_hits()

  query <- build_project_memory_query(messages = messages, state = state)
  snippets <- list()
  matched_query <- query
  project_root <- resolve_project_memory_root(session)
  project_memory_snippets_enabled <- isTRUE(providers$project_memory_snippets)
  project_memory_fixes_enabled <- isTRUE(providers$project_memory_fixes)
  project_memory_available <- nzchar(query) &&
    requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE) &&
    !is.null(project_root) &&
    dir.exists(project_root)

  if (isTRUE(project_memory_available) && isTRUE(project_memory_snippets_enabled)) {
    memory <- tryCatch(project_memory(project_root = project_root), error = function(e) NULL)
    if (!is.null(memory)) {
      query_variants <- unique(c(
        query,
        strsplit(query, "\\s+")[[1]][1],
        paste(utils::head(strsplit(query, "\\s+")[[1]], 2L), collapse = " ")
      ))
      query_variants <- query_variants[nzchar(query_variants)]

      hits <- NULL
      for (variant in query_variants) {
        hits <- tryCatch(memory$search_snippets(variant, limit = limits$project_memory_snippets %||% limit), error = function(e) NULL)
        if (!is.null(hits) && is.data.frame(hits) && nrow(hits) > 0) {
          matched_query <- variant
          break
        }
      }
      if (!is.null(hits) && is.data.frame(hits) && nrow(hits) > 0) {
        snippets <- lapply(seq_len(nrow(hits)), function(i) {
          row <- hits[i, , drop = FALSE]
          list(
            summary = trim_context_preview(row$description[[1]] %||% row$context[[1]] %||% "memory hit", max_chars = 160L),
            preview = trim_context_preview(row$code[[1]] %||% "", max_chars = 180L),
            tags = row$tags[[1]] %||% "",
            created_at = row$created_at[[1]] %||% ""
          )
        })
      }
    }
  }

  fixes <- if (isTRUE(project_memory_available) && isTRUE(project_memory_fixes_enabled)) {
    build_project_memory_fix_retrieval(session, state = state, limit = limits$project_memory_fixes %||% 2L)
  } else {
    list()
  }
  semantic_objects <- if (isTRUE(providers$semantic_objects)) {
    build_semantic_object_retrieval(
      state,
      query,
      limit = limits$semantic_objects %||% 3L,
      min_hits = min_hits$semantic_objects %||% 1L
    )
  } else {
    list()
  }
  task_state <- if (isTRUE(providers$task_state)) {
    build_task_state_retrieval(
      state,
      query,
      limit = limits$task_state %||% 4L,
      min_hits = min_hits$task_state %||% 0L
    )
  } else {
    list()
  }
  session_memory <- if (isTRUE(providers$session_memory)) {
    build_session_memory_retrieval(
      session,
      query,
      limit = limits$session_memory %||% 3L,
      min_hits = min_hits$session_memory %||% 1L
    )
  } else {
    list()
  }
  documents <- if (isTRUE(providers$documents)) {
    build_document_retrieval(
      session,
      query,
      limit = limits$documents %||% 2L,
      min_hits = min_hits$documents %||% 1L
    )
  } else {
    list()
  }
  transcript_segments <- if (isTRUE(providers$transcript_segments)) {
    build_transcript_segment_retrieval(
      state,
      query,
      limit = limits$transcript_segments %||% 2L,
      min_hits = min_hits$transcript_segments %||% 1L
    )
  } else {
    list()
  }
  execution_monitor <- if (isTRUE(providers$execution_monitor)) {
    build_execution_monitor_retrieval(
      state,
      query,
      limit = limits$execution_monitor %||% 3L,
      min_hits = min_hits$execution_monitor %||% 1L
    )
  } else {
    list()
  }
  system_info <- if (isTRUE(providers$system_info)) {
    build_system_info_retrieval(
      state,
      query,
      limit = limits$system_info %||% 2L,
      min_hits = min_hits$system_info %||% 1L
    )
  } else {
    list()
  }
  runtime_state <- if (isTRUE(providers$runtime_state)) {
    build_runtime_state_retrieval(
      state,
      query,
      limit = limits$runtime_state %||% 3L,
      min_hits = min_hits$runtime_state %||% 1L
    )
  } else {
    list()
  }
  if (length(snippets) == 0 &&
      length(fixes) == 0 &&
      length(semantic_objects) == 0 &&
      length(task_state) == 0 &&
      length(session_memory) == 0 &&
      length(documents) == 0 &&
      length(transcript_segments) == 0 &&
      length(execution_monitor) == 0 &&
      length(system_info) == 0 &&
      length(runtime_state) == 0) {
    return(list())
  }

  retrieval_cache <- list(
    query = if (length(snippets) > 0) matched_query else query,
    project_root = project_root %||% "",
    snippets = snippets,
    fixes = fixes,
    semantic_objects = semantic_objects,
    task_state = task_state,
    session_memory = session_memory,
    documents = documents,
    transcript_segments = transcript_segments,
    execution_monitor = execution_monitor,
    system_info = system_info,
    runtime_state = runtime_state,
    provider_order = provider_order
  )
  retrieval_cache$ranked_hits <- rank_retrieval_hits(retrieval_cache, retrieval_cache$query %||% query, cfg)
  retrieval_cache$ranked_hits <- learned_rerank_retrieval_hits(
    retrieval_cache$ranked_hits,
    query = retrieval_cache$query %||% query,
    session = session,
    regime = session$get_context_state()$occupancy$status %||% "green"
  )

  retrieval_cache
}

#' @keywords internal
get_session_context_metrics <- function(session,
                                        model_ref = NULL,
                                        system_prompt = NULL,
                                        messages = NULL) {
  model_ref <- model_ref %||% tryCatch(session$get_model_id(), error = function(e) NULL)
  if (is.null(model_ref) || !nzchar(model_ref)) {
    return(NULL)
  }

  sep_pos <- regexpr(":", model_ref, fixed = TRUE)[1]
  if (sep_pos <= 1) {
    return(NULL)
  }

  provider <- substr(model_ref, 1L, sep_pos - 1L)
  model_id <- substr(model_ref, sep_pos + 1L, nchar(model_ref))
  info <- tryCatch(get_model_info(provider, model_id), error = function(e) NULL)

  override_window <- if (!is.null(session) && inherits(session, "ChatSession")) {
    session$get_metadata("context_window_override", default = NULL)
  } else {
    NULL
  }
  override_max_output <- if (!is.null(session) && inherits(session, "ChatSession")) {
    session$get_metadata("max_output_tokens_override", default = NULL)
  } else {
    NULL
  }

  context_window <- override_window %||% info$context$context_window %||% infer_session_context_window(provider, model_id)
  max_output <- override_max_output %||% info$context$max_output_tokens %||% NA_integer_
  used_tokens <- estimate_prompt_tokens(
    messages = messages %||% tryCatch(session$get_history(), error = function(e) list()),
    system = system_prompt %||% tryCatch(session$as_list()$system_prompt %||% "", error = function(e) "")
  )
  remaining_tokens <- if (!is.na(context_window) && !is.na(used_tokens)) {
    max(context_window - used_tokens, 0)
  } else {
    NA_real_
  }
  ratio <- if (!is.na(context_window) && !is.na(used_tokens) && context_window > 0) {
    used_tokens / context_window
  } else {
    NA_real_
  }
  regime <- classify_context_regime(ratio)
  is_estimated <- !is.null(override_window) || is.null(info) || is.null(info$context$context_window)

  list(
    provider = provider,
    model_id = model_id,
    context_window = context_window,
    max_output = max_output,
    used_tokens = used_tokens,
    remaining_tokens = remaining_tokens,
    ratio = ratio,
    regime = regime,
    estimated = is_estimated
  )
}

#' @keywords internal
assemble_session_messages <- function(session,
                                      messages = NULL,
                                      system = NULL,
                                      persist = TRUE) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    rlang::abort("assemble_session_messages() requires a ChatSession.")
  }

  messages <- messages %||% session$get_history()
  mode <- get_session_context_management_mode(session)
  metrics <- get_session_context_metrics(
    session = session,
    system_prompt = system,
    messages = messages
  )

  state <- normalize_context_state(session$get_metadata("context_state", default = NULL))
  if (!is.null(metrics)) {
    state$occupancy <- list(
      estimated_prompt_tokens = metrics$used_tokens %||% NA_real_,
      context_window = metrics$context_window %||% NA_real_,
      ratio = metrics$ratio %||% NA_real_,
      status = metrics$regime %||% "unknown"
    )
  }

  assembled_messages <- messages
  summary_block <- ""

  if (mode != "off" && length(messages) > 0) {
    recent_n <- recent_context_window_size(metrics$regime %||% "unknown")
    if (is.finite(recent_n) && length(messages) > recent_n) {
      compacted_count <- length(messages) - recent_n
      older_messages <- messages[seq_len(compacted_count)]
      assembled_messages <- tail(messages, recent_n)

      if (compacted_count != state$compacted_message_count || !nzchar(state$rolling_summary)) {
        state$rolling_summary <- summarize_message_span(older_messages)
        state$compacted_message_count <- compacted_count
        state$transcript_segments <- list(list(
          compacted_message_count = compacted_count,
          summary = state$rolling_summary,
          created_at = as.character(Sys.time()),
          regime = metrics$regime %||% "unknown"
        ))
        state$last_compaction_at <- as.character(Sys.time())
        state <- append_context_event(
          state,
          "messages_compacted",
          list(
            compacted_message_count = as.character(compacted_count),
            regime = metrics$regime %||% "unknown"
          )
        )
      }
      summary_block <- state$rolling_summary
    } else if (length(messages) == 0) {
      state$rolling_summary <- ""
      state$compacted_message_count <- 0L
      state$transcript_segments <- list()
      state$last_compaction_at <- NULL
    } else {
      summary_block <- state$rolling_summary %||% ""
    }
  }

  structured_block <- ""
  if (mode != "off") {
    retrieval_cache <- build_project_memory_retrieval(session, messages = messages, state = state)
    if (length(retrieval_cache) > 0) {
      retrieval_count <- length(retrieval_cache$snippets %||% list()) +
        length(retrieval_cache$fixes %||% list()) +
        length(retrieval_cache$session_memory %||% list()) +
        length(retrieval_cache$documents %||% list()) +
        length(retrieval_cache$transcript_segments %||% list())
      state$retrieval_cache <- retrieval_cache
      state <- append_context_event(
        state,
        "project_memory_retrieved",
        list(count = as.character(retrieval_count))
      )
      if (any(vapply(retrieval_cache$ranked_hits %||% list(), function(hit) isTRUE(hit$reranked), logical(1)))) {
        state <- append_context_event(
          state,
          "retrieval_reranked",
          list(count = as.character(sum(vapply(retrieval_cache$ranked_hits, function(hit) isTRUE(hit$reranked), logical(1)))))
        )
      }
    } else {
      state$retrieval_cache <- list()
    }
    structured_block <- render_context_state_block(state)
  }

  system_parts <- character(0)
  if (!is.null(system) && nzchar(system)) {
    system_parts <- c(system_parts, system)
  }
  if (nzchar(summary_block)) {
    system_parts <- c(
      system_parts,
      paste(
        "[COMPACTED CONVERSATION CONTEXT]",
        "Older transcript turns were compacted for prompt budget control.",
        summary_block,
        sep = "\n"
      )
    )
  }
  if (nzchar(structured_block)) {
    system_parts <- c(system_parts, structured_block)
  }
  assembled_system <- if (length(system_parts) > 0) paste(system_parts, collapse = "\n\n") else NULL

  if (isTRUE(persist)) {
    session$set_context_state(state)
  }

  list(
    messages = assembled_messages,
    system = assembled_system,
    metrics = metrics,
    state = state
  )
}
