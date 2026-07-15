#' @title Gemini Explicit Context Caching
#' @description
#' Create, reuse, and manage Gemini `CachedContent` resources. Unlike the inline
#' prefix caching of other providers, Gemini's explicit cache is a stateful
#' server object: you create it once from a large stable prefix (system prompt,
#' tools, and/or content), then reference it by name on later
#' `generate_text(..., cached_content = <cache>)` calls so those tokens are
#' billed once (plus storage) rather than resent every turn. Gemini enforces a
#' minimum cached token count (~2048 for 2.5 models, ~4096 for 3.x).
#' @name gemini_cache
NULL

#' @keywords internal
gemini_cache_endpoint_url <- function(config, name = NULL) {
  bases <- normalize_base_urls(config$base_urls %||% config$base_url)
  # cachedContents is a sibling of /models under the API version root.
  base <- sub("/models/?$", "", bases[[1]])
  path <- if (is.null(name)) "cachedContents" else name # name is "cachedContents/<id>"
  paste0(base, "/", path, "?key=", config$api_key %||% "")
}

#' @keywords internal
# Accept either a cache handle or a raw "cachedContents/<id>" name.
gemini_cache_name <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "aisdk_gemini_cache")) {
    return(x$name)
  }
  if (is.character(x) && length(x) == 1 && nzchar(x)) {
    return(x)
  }
  NULL
}

#' @keywords internal
gemini_cache_from_response <- function(resp, model_id = NULL) {
  if (is.null(resp) || is.null(resp$name)) {
    return(NULL)
  }
  structure(
    list(
      name = resp$name,
      model = resp$model %||% (if (!is.null(model_id)) paste0("models/", model_id) else NULL),
      display_name = resp$displayName,
      create_time = resp$createTime,
      expire_time = resp$expireTime,
      token_count = resp$usageMetadata$totalTokenCount,
      model_id = model_id
    ),
    class = "aisdk_gemini_cache"
  )
}

#' @keywords internal
resolve_gemini_model <- function(model, registry = NULL) {
  model <- resolve_model(model, registry, type = "language")
  if (!inherits(model, "GeminiLanguageModel")) {
    rlang::abort("Gemini context caching requires a Gemini model (e.g. \"gemini:gemini-2.5-flash\").")
  }
  model
}

#' Create a Gemini context cache
#'
#' @param model A Gemini model object or id string (e.g. "gemini:gemini-2.5-flash").
#' @param system Optional system prompt to cache.
#' @param contents Optional messages (aisdk format) to cache.
#' @param tools Optional list of [aisdk::tool()] objects to cache.
#' @param ttl Time-to-live duration string (e.g. "300s"). Ignored when
#'   `expire_time` is set.
#' @param expire_time Optional absolute RFC-3339 expiry (overrides `ttl`).
#' @param display_name Optional human-readable label.
#' @param registry Optional ProviderRegistry.
#' @return An `aisdk_gemini_cache` handle (pass it as `cached_content` to
#'   [aisdk::generate_text()]).
#' @seealso [gemini_delete_cache()], [gemini_list_caches()]
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   cache <- gemini_create_cache("gemini:gemini-2.5-flash",
#'                                system = big_stable_instructions, ttl = "600s")
#'   generate_text("gemini:gemini-2.5-flash", "First question", cached_content = cache)
#'   generate_text("gemini:gemini-2.5-flash", "Second question", cached_content = cache)
#'   gemini_delete_cache("gemini:gemini-2.5-flash", cache)
#' }
#' }
gemini_create_cache <- function(model, system = NULL, contents = NULL, tools = NULL,
                                ttl = "300s", expire_time = NULL, display_name = NULL,
                                registry = NULL) {
  resolve_gemini_model(model, registry)$create_cache(
    system = system, contents = contents, tools = tools,
    ttl = ttl, expire_time = expire_time, display_name = display_name
  )
}

#' List Gemini context caches
#' @inheritParams gemini_create_cache
#' @return A list of `aisdk_gemini_cache` handles.
#' @export
gemini_list_caches <- function(model, registry = NULL) {
  resolve_gemini_model(model, registry)$list_caches()
}

#' Fetch a Gemini context cache
#' @inheritParams gemini_create_cache
#' @param cache A cache handle or `"cachedContents/..."` name.
#' @return An `aisdk_gemini_cache` handle.
#' @export
gemini_get_cache <- function(model, cache, registry = NULL) {
  resolve_gemini_model(model, registry)$get_cache(cache)
}

#' Update a Gemini context cache's expiry
#' @inheritParams gemini_get_cache
#' @param ttl New TTL duration string (e.g. "600s").
#' @param expire_time Optional absolute RFC-3339 expiry (overrides `ttl`).
#' @return The updated `aisdk_gemini_cache` handle.
#' @export
gemini_update_cache <- function(model, cache, ttl = NULL, expire_time = NULL, registry = NULL) {
  resolve_gemini_model(model, registry)$update_cache(cache, ttl = ttl, expire_time = expire_time)
}

#' Delete a Gemini context cache
#' @inheritParams gemini_get_cache
#' @return Invisibly `TRUE`.
#' @export
gemini_delete_cache <- function(model, cache, registry = NULL) {
  resolve_gemini_model(model, registry)$delete_cache(cache)
}

#' @title Print a Gemini context cache handle
#' @param x An `aisdk_gemini_cache`.
#' @param ... Ignored.
#' @export
print.aisdk_gemini_cache <- function(x, ...) {
  cat("<Gemini context cache>\n")
  cat("  name:   ", x$name, "\n", sep = "")
  if (!is.null(x$token_count)) cat("  tokens: ", x$token_count, "\n", sep = "")
  if (!is.null(x$expire_time)) cat("  expires:", x$expire_time, "\n", sep = "")
  if (!is.null(x$display_name)) cat("  label:  ", x$display_name, "\n", sep = "")
  invisible(x)
}
