#' @title Default Model Configuration
#' @description
#' Utilities for reading and updating the package-wide default language model.
#' High-level helpers that accept `model = NULL`, including `generate_text()`,
#' `stream_text()`, `ChatSession$new()`, `create_chat_session()`,
#' `auto_fix()`, and the knitr `{ai}` engine, use this default when no
#' explicit model is supplied.
#' @name model_defaults
NULL

.model_env <- new.env(parent = emptyenv())
.model_env$default <- NULL
.model_env$options <- NULL

#' @keywords internal
list_get_exact <- function(x, name, default = NULL) {
  if (is.list(x) && name %in% names(x)) {
    x[[name]]
  } else {
    default
  }
}

#' @keywords internal
normalize_model_runtime_options <- function(options = list()) {
  options <- options %||% list()
  if (!is.list(options)) {
    rlang::abort("Model runtime options must be a list.")
  }

  context_window <- list_get_exact(options, "context_window") %||%
    list_get_exact(options, "context_window_override") %||% NULL
  max_output_tokens <- list_get_exact(options, "max_output_tokens") %||%
    list_get_exact(options, "max_output_tokens_override") %||% NULL

  if (!is.null(context_window)) {
    context_window <- as.numeric(context_window)
    if (is.na(context_window) || context_window <= 0) {
      rlang::abort("`context_window` must be a positive number.")
    }
  }
  if (!is.null(max_output_tokens)) {
    max_output_tokens <- as.numeric(max_output_tokens)
    if (is.na(max_output_tokens) || max_output_tokens <= 0) {
      rlang::abort("`max_output_tokens` must be a positive number.")
    }
  }

  call_options <- list_get_exact(options, "call_options", list())
  call_options <- utils::modifyList(
    call_options,
    Filter(Negate(is.null), list(
      max_tokens = list_get_exact(options, "max_tokens") %||% NULL,
      thinking = list_get_exact(options, "thinking") %||% NULL,
      thinking_budget = list_get_exact(options, "thinking_budget") %||% NULL,
      reasoning_effort = list_get_exact(options, "reasoning_effort") %||% NULL
    )),
    keep.null = TRUE
  )

  if (!is.null(list_get_exact(call_options, "max_tokens"))) {
    call_options$max_tokens <- as.numeric(list_get_exact(call_options, "max_tokens"))
    if (is.na(call_options$max_tokens) || call_options$max_tokens <= 0) {
      rlang::abort("`max_tokens` must be a positive number.")
    }
  }
  if (!is.null(list_get_exact(call_options, "thinking_budget"))) {
    call_options$thinking_budget <- as.numeric(list_get_exact(call_options, "thinking_budget"))
    if (is.na(call_options$thinking_budget) || call_options$thinking_budget <= 0) {
      rlang::abort("`thinking_budget` must be a positive number.")
    }
  }
  if (!is.null(list_get_exact(call_options, "reasoning_effort"))) {
    call_options$reasoning_effort <- tolower(as.character(list_get_exact(call_options, "reasoning_effort")))
    if (!call_options$reasoning_effort %in% c("low", "medium", "high")) {
      rlang::abort("`reasoning_effort` must be one of 'low', 'medium', or 'high'.")
    }
  }
  if (!is.null(list_get_exact(call_options, "thinking"))) {
    thinking_value <- list_get_exact(call_options, "thinking")
    if (is.character(thinking_value)) {
      thinking <- tolower(trimws(thinking_value))
      if (thinking %in% c("on", "true", "1", "yes", "enabled")) {
        call_options$thinking <- TRUE
      } else if (thinking %in% c("off", "false", "0", "no", "disabled")) {
        call_options$thinking <- FALSE
      } else {
        rlang::abort("`thinking` must be TRUE/FALSE or one of 'on', 'off', 'enabled', 'disabled'.")
      }
    } else if (!is.logical(thinking_value) && !is.list(thinking_value)) {
      rlang::abort("`thinking` must be logical, character, or a provider-native list.")
    }
  }

  call_options <- call_options[!vapply(call_options, is.null, logical(1))]

  list(
    context_window = context_window,
    max_output_tokens = max_output_tokens,
    call_options = call_options
  )
}

#' @keywords internal
compact_model_runtime_options <- function(options = list()) {
  normalized <- normalize_model_runtime_options(options)
  normalized <- normalized[!vapply(normalized, function(x) is.null(x) || (is.list(x) && length(x) == 0), logical(1))]
  normalized
}

#' @keywords internal
get_default_model_runtime_options <- function() {
  compact_model_runtime_options(.model_env$options %||% getOption("aisdk.default_model_options", list()))
}

#' @keywords internal
model_runtime_session_metadata <- function(options = list()) {
  options <- normalize_model_runtime_options(options)
  metadata <- list()
  if (!is.null(options$context_window)) {
    metadata$context_window_override <- options$context_window
  }
  if (!is.null(options$max_output_tokens)) {
    metadata$max_output_tokens_override <- options$max_output_tokens
  }
  if (length(options$call_options) > 0) {
    metadata$model_call_options <- options$call_options
  }
  metadata
}

#' @keywords internal
merge_call_options <- function(defaults = list(), overrides = list()) {
  defaults <- defaults %||% list()
  overrides <- overrides %||% list()
  overrides <- overrides[!vapply(overrides, is.null, logical(1))]
  utils::modifyList(defaults, overrides, keep.null = TRUE)
}

#' @title Get Default Model Runtime Options
#' @description
#' Returns runtime options configured with `set_model()`, including context
#' overrides and default generation options such as thinking settings.
#' @return A list with optional `context_window`, `max_output_tokens`, and
#'   `call_options` entries.
#' @export
get_model_options <- function() {
  get_default_model_runtime_options()
}

#' @keywords internal
default_model_id <- function(model) {
  if (is.null(model)) {
    return(NULL)
  }

  if (is.character(model)) {
    return(model[[1]])
  }

  if (inherits(model, "LanguageModelV1")) {
    provider <- model$provider %||% NULL
    model_id <- model$model_id %||% NULL
    if (!is.null(provider) && !is.null(model_id)) {
      return(paste0(provider, ":", model_id))
    }
  }

  NULL
}

#' @title Get Default Model
#' @description
#' Returns the current package-wide default language model. This is used by
#' high-level helpers when `model = NULL`. If no explicit default has been set,
#' `get_model()` falls back to `getOption("aisdk.default_model")` and then to
#' `"openai:gpt-4o"`.
#' @param default Fallback model identifier when no explicit default has been set.
#' @return A model identifier string or a `LanguageModelV1` object.
#' @export
#' @examples
#' get_model()
get_model <- function(default = "openai:gpt-4o") {
  current <- .model_env$default %||% getOption("aisdk.default_model")
  current %||% default
}

#' @keywords internal
get_explicit_default_model <- function() {
  .model_env$default %||% getOption("aisdk.default_model", NULL)
}

#' @title Set Default Model
#' @description
#' Sets the package-wide default language model. Pass `NULL` to restore the
#' built-in default (`"openai:gpt-4o"` unless overridden with
#' `options(aisdk.default_model = ...)`). If `new` is omitted and runtime
#' options are supplied, only the runtime options are updated.
#' @param new A model identifier string, a `LanguageModelV1` object, or `NULL`.
#' @param context_window Optional context-window override used by sessions
#'   created from this default model.
#' @param max_output_tokens Optional maximum output-token metadata override.
#' @param max_tokens Optional default generation token limit.
#' @param thinking Optional default thinking-mode value passed to providers that
#'   support it. Logical values are normalized by each provider.
#' @param thinking_budget Optional default thinking budget.
#' @param reasoning_effort Optional default reasoning effort (`"low"`,
#'   `"medium"`, or `"high"`).
#' @param reset_options Logical. If `TRUE`, clears default runtime options.
#' @return Invisibly returns the previous default model.
#' @export
#' @examples
#' old <- set_model("deepseek:deepseek-chat")
#' current <- get_model()
#' set_model(old)
#' set_model(NULL)
set_model <- function(new = NULL,
                      context_window = NULL,
                      max_output_tokens = NULL,
                      max_tokens = NULL,
                      thinking = NULL,
                      thinking_budget = NULL,
                      reasoning_effort = NULL,
                      reset_options = FALSE) {
  new_missing <- missing(new)
  old <- get_model()
  option_updates <- Filter(Negate(is.null), list(
    context_window = context_window,
    max_output_tokens = max_output_tokens,
    max_tokens = max_tokens,
    thinking = thinking,
    thinking_budget = thinking_budget,
    reasoning_effort = reasoning_effort
  ))

  update_model <- !new_missing || (length(option_updates) == 0 && !isTRUE(reset_options))

  if (isTRUE(update_model)) {
    is_valid <- is.null(new) ||
      (is.character(new) && length(new) == 1 && nzchar(new)) ||
      inherits(new, "LanguageModelV1")

    if (!is_valid) {
      rlang::abort("`new` must be NULL, a non-empty model ID string, or a LanguageModelV1 object.")
    }

    .model_env$default <- new
    options(aisdk.default_model = new)
  }

  if (isTRUE(reset_options)) {
    .model_env$options <- list()
    options(aisdk.default_model_options = list())
  } else if (length(option_updates) > 0) {
    current_options <- get_default_model_runtime_options()
    merged <- compact_model_runtime_options(utils::modifyList(current_options, option_updates, keep.null = TRUE))
    .model_env$options <- merged
    options(aisdk.default_model_options = merged)
  }

  invisible(old)
}

#' @title Model Shortcut
#' @description
#' Shortcut for default model configuration. Call with no arguments to read the
#' current default model, or pass a model to update it. This is equivalent to
#' calling `get_model()` and `set_model()` directly. Runtime options can be
#' supplied without `new` to update the current default model's options.
#' @param new Optional model identifier string or `LanguageModelV1` object.
#' @param ... Runtime options forwarded to `set_model()`.
#' @return When `new` is missing, returns the current default model. Otherwise
#'   invisibly returns the previous default model.
#' @export
#' @examples
#' model()
#' model("openai:gpt-4o-mini")
#' model(NULL)
model <- function(new, ...) {
  runtime_options <- list(...)
  if (missing(new)) {
    if (length(runtime_options) > 0) {
      return(do.call(set_model, runtime_options))
    }
    return(get_model())
  }

  set_model(new, ...)
}
