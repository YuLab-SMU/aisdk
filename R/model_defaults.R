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

#' @title Set Default Model
#' @description
#' Sets the package-wide default language model. Pass `NULL` to restore the
#' built-in default (`"openai:gpt-4o"` unless overridden with
#' `options(aisdk.default_model = ...)`).
#' @param new A model identifier string, a `LanguageModelV1` object, or `NULL`.
#' @return Invisibly returns the previous default model.
#' @export
#' @examples
#' old <- set_model("deepseek:deepseek-chat")
#' current <- get_model()
#' set_model(old)
#' set_model(NULL)
set_model <- function(new = NULL) {
  old <- get_model()

  is_valid <- is.null(new) ||
    (is.character(new) && length(new) == 1 && nzchar(new)) ||
    inherits(new, "LanguageModelV1")

  if (!is_valid) {
    rlang::abort("`new` must be NULL, a non-empty model ID string, or a LanguageModelV1 object.")
  }

  .model_env$default <- new
  options(aisdk.default_model = new)

  invisible(old)
}

#' @title Model Shortcut
#' @description
#' Shortcut for default model configuration. Call with no arguments to read the
#' current default model, or pass a model to update it. This is equivalent to
#' calling `get_model()` and `set_model()` directly.
#' @param new Optional model identifier string or `LanguageModelV1` object.
#' @return When `new` is missing, returns the current default model. Otherwise
#'   invisibly returns the previous default model.
#' @export
#' @examples
#' model()
#' model("openai:gpt-4o-mini")
#' model(NULL)
model <- function(new) {
  if (missing(new)) {
    return(get_model())
  }

  set_model(new)
}
