#' @title Utilities: HTTP and Retry Logic
#' @description
#' Provides standardized HTTP request handling with exponential backoff retry.
#'
#' Implements multi-layer defense strategy for handling API responses:
#' - Empty response body handling (returns {} instead of parse error)
#' - JSON parsing with repair fallback
#' - SSE stream error recovery
#' - Graceful degradation on malformed data
#'
#' @name utils_http
NULL

should_skip_internet_check <- function() {
  opt <- getOption("aisdk.skip_internet_check", NULL)
  if (isTRUE(opt)) {
    return(TRUE)
  }

  env <- Sys.getenv("AISDK_SKIP_INTERNET_CHECK", "")
  identical(tolower(trimws(env)), "true") || identical(trimws(env), "1")
}

#' @keywords internal
normalize_base_urls <- function(base_url, default = NULL) {
  base_url <- base_url %||% default
  if (is.null(base_url)) {
    return(character(0))
  }

  values <- unlist(strsplit(as.character(base_url), "[,;\\n]+", perl = TRUE), use.names = FALSE)
  values <- trimws(values)
  values <- values[nzchar(values)]
  values <- sub("/+$", "", values)
  unique(values)
}

#' @title Build API Endpoint URLs (extension API)
#' @description
#' Joins a provider configuration's base URL(s) with a request path. Exported
#' as part of the stable extension surface used by companion provider packages
#' such as `aisdk.providers`; not intended for general end-user use.
#' @param config A provider configuration list carrying `base_urls` or
#'   `base_url`.
#' @param path The request path to append to each base URL.
#' @return A character vector of fully-qualified endpoint URLs.
#' @keywords internal
#' @export
api_endpoint_urls <- function(config, path) {
  bases <- config$base_urls %||% config$base_url
  paste0(normalize_base_urls(bases), path)
}

#' @keywords internal
# Resolve a retry delay (ms) from Retry-After headers, robustly:
#  - `retry-after-ms` (OpenAI) or a numeric `retry-after` (seconds) are used;
#  - an HTTP-date `retry-after` (RFC 7231, e.g. "Wed, 21 Oct 2025 07:28:00 GMT")
#    is parsed to a delta, instead of as.numeric() -> NA -> Sys.sleep(NA), which
#    used to throw an uncaught error and MASK the 429;
#  - an unparseable/negative value falls back to the caller's backoff delay;
#  - the result is capped (default 60s, options(aisdk.max_retry_after_ms=)) so a
#    misbehaving server can't pin the session on Sys.sleep() for minutes.
resolve_retry_after_ms <- function(retry_after, retry_after_ms, fallback_ms,
                                   cap_ms = getOption("aisdk.max_retry_after_ms", 60000)) {
  ms <- NA_real_
  if (!is.null(retry_after_ms)) {
    ms <- suppressWarnings(as.numeric(retry_after_ms))
  } else if (!is.null(retry_after)) {
    n <- suppressWarnings(as.numeric(retry_after))
    if (!is.na(n)) {
      ms <- n * 1000
    } else {
      when <- suppressWarnings(tryCatch(
        as.POSIXct(retry_after, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT"),
        error = function(e) NA
      ))
      if (!is.na(when)) {
        ms <- max(0, as.numeric(difftime(when, Sys.time(), units = "secs")) * 1000)
      }
    }
  }
  if (is.na(ms) || ms < 0) {
    ms <- fallback_ms
  }
  min(ms, cap_ms)
}

# A value in [0, 1) that varies per call and per process, WITHOUT touching the
# caller's RNG stream (a package must not consume the user's set.seed()
# sequence). Sub-microsecond wall-clock entropy gives per-call variation; the
# process-id offset de-synchronizes forked workers that retry at the same
# instant (the generate_batch case). Not cryptographic — just enough to spread
# retries; jitter does not need uniform randomness.
#' @keywords internal
jitter_unit <- function() {
  micros <- (as.numeric(Sys.time()) * 1e6) %% 1000 / 1000 # sub-ms fraction
  pid_off <- (Sys.getpid() %% 1000) / 1000
  (micros + pid_off) %% 1
}

# Spread out a retry delay so concurrent requests that back off together (e.g.
# generate_batch workers all hitting one 429) don't retry in lockstep and
# re-trigger the limit — the canonical "exponential backoff AND jitter". Jitter
# is ADDITIVE (delay .. delay*(1+frac)) so it never sleeps less than a
# server-specified Retry-After. Disable/tune with options(aisdk.retry_jitter=).
#' @keywords internal
apply_backoff_jitter <- function(delay_ms) {
  if (is.null(delay_ms) || !is.finite(delay_ms) || delay_ms <= 0) {
    return(delay_ms)
  }
  frac <- getOption("aisdk.retry_jitter", 0.5)
  if (!is.numeric(frac) || length(frac) != 1 || is.na(frac) || frac <= 0) {
    return(delay_ms)
  }
  delay_ms * (1 + jitter_unit() * min(frac, 1))
}

# Tracks which URLs we've already warned about so a single false-negative
# from curl::has_internet() doesn't spam the console on every retry.
.aisdk_preflight_warned <- new.env(parent = emptyenv())

# Per-route health state used only when callers provide multiple URL candidates.
.aisdk_api_route_state <- new.env(parent = emptyenv())

# Most-recent rate-limit snapshot parsed from provider response headers. An
# agent in a request loop needs the remaining budget to pace itself BEFORE it
# hits a 429 (a retry-on-429 loop with no backoff is a self-inflicted DDoS); the
# providers report it on every response but aisdk otherwise discards all headers.
.aisdk_rate_limit_state <- new.env(parent = emptyenv())

# Parse the OpenAI (x-ratelimit-*) and Anthropic (anthropic-ratelimit-*) header
# families into one normalized snapshot and stash the latest. Non-fatal by
# design: it must never break a request path, and it only overwrites state when
# the response actually carried rate-limit headers (Gemini, for instance, sends
# none) so a header-less 200 doesn't erase a good snapshot.
#' @keywords internal
capture_rate_limit_headers <- function(resp, url = NULL) {
  tryCatch({
    h <- function(name) {
      v <- tryCatch(httr2::resp_header(resp, name), error = function(e) NULL)
      if (is.null(v) || !nzchar(v)) NULL else v
    }
    num <- function(x) if (is.null(x)) NULL else suppressWarnings(as.numeric(x))
    snap <- list(
      remaining_requests = num(h("x-ratelimit-remaining-requests") %||% h("anthropic-ratelimit-requests-remaining")),
      remaining_tokens   = num(h("x-ratelimit-remaining-tokens") %||% h("anthropic-ratelimit-tokens-remaining")),
      limit_requests     = num(h("x-ratelimit-limit-requests") %||% h("anthropic-ratelimit-requests-limit")),
      limit_tokens       = num(h("x-ratelimit-limit-tokens") %||% h("anthropic-ratelimit-tokens-limit")),
      reset_requests     = h("x-ratelimit-reset-requests") %||% h("anthropic-ratelimit-requests-reset"),
      reset_tokens       = h("x-ratelimit-reset-tokens") %||% h("anthropic-ratelimit-tokens-reset"),
      retry_after        = num(h("retry-after")),
      status             = tryCatch(httr2::resp_status(resp), error = function(e) NA_integer_),
      url                = url,
      timestamp          = Sys.time()
    )
    tracked <- c("remaining_requests", "remaining_tokens", "limit_requests",
                 "limit_tokens", "reset_requests", "reset_tokens")
    if (any(!vapply(snap[tracked], is.null, logical(1)))) {
      assign("latest", snap, envir = .aisdk_rate_limit_state)
    }
  }, error = function(e) NULL)
  invisible(NULL)
}

#' @title Latest API Rate-Limit Status
#' @description
#' Returns the most recent rate-limit snapshot parsed from a provider's response
#' headers, or `NULL` if none has been seen this session. Use it to pace a
#' request loop (e.g. slow down or pause when `remaining_requests` /
#' `remaining_tokens` runs low) instead of only reacting to a 429 after the fact.
#' OpenAI and Anthropic report these headers; Gemini does not.
#' @return A list with `remaining_requests`, `remaining_tokens`, `limit_requests`,
#'   `limit_tokens`, `reset_requests`, `reset_tokens`, `retry_after`, `status`,
#'   `url`, and `timestamp`; or `NULL` when nothing has been captured.
#' @export
rate_limit_status <- function() {
  if (exists("latest", envir = .aisdk_rate_limit_state, inherits = FALSE)) {
    get("latest", envir = .aisdk_rate_limit_state)
  } else {
    NULL
  }
}

#' @keywords internal
api_route_key <- function(url) {
  parsed <- tryCatch(httr2::url_parse(url), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$hostname) || !nzchar(parsed$hostname)) {
    return(url)
  }
  scheme <- parsed$scheme %||% "https"
  port <- parsed$port %||% ""
  paste0(scheme, "://", parsed$hostname, if (nzchar(port)) paste0(":", port) else "")
}

#' @keywords internal
read_http_failover_cooldown_seconds <- function() {
  read_timeout_setting(
    option_name = "aisdk.http_failover_cooldown_seconds",
    env_name = "AISDK_HTTP_FAILOVER_COOLDOWN_SECONDS",
    default = 60,
    arg_name = "http_failover_cooldown_seconds"
  )
}

#' @keywords internal
api_route_state <- function(url) {
  .aisdk_api_route_state[[api_route_key(url)]] %||% list(failures = 0L, cooldown_until = NULL)
}

#' @keywords internal
api_route_is_cooling_down <- function(url, now = Sys.time()) {
  state <- api_route_state(url)
  until <- state$cooldown_until %||% NULL
  !is.null(until) && now < until
}

#' @keywords internal
order_api_url_candidates <- function(urls) {
  urls <- unique(as.character(urls))
  if (length(urls) <= 1) {
    return(urls)
  }
  active <- urls[!vapply(urls, api_route_is_cooling_down, logical(1))]
  cooled <- urls[!urls %in% active]
  if (length(active) == 0) {
    return(urls)
  }
  c(active, cooled)
}

#' @keywords internal
mark_api_route_success <- function(url) {
  key <- api_route_key(url)
  if (exists(key, envir = .aisdk_api_route_state, inherits = FALSE)) {
    rm(list = key, envir = .aisdk_api_route_state)
  }
  invisible(TRUE)
}

#' @keywords internal
mark_api_route_failure <- function(url, reason = NULL) {
  key <- api_route_key(url)
  state <- api_route_state(url)
  state$failures <- as.integer(state$failures %||% 0L) + 1L
  state$last_error <- reason %||% ""
  state$last_failure <- Sys.time()
  state$cooldown_until <- state$last_failure + read_http_failover_cooldown_seconds()
  .aisdk_api_route_state[[key]] <- state
  invisible(state)
}

#' @keywords internal
api_status_retryable <- function(status) {
  status %in% c(408L, 409L, 425L, 429L) || status >= 500L
}

#' @keywords internal
is_retryable_api_error <- function(error) {
  inherits(error, "aisdk_api_retryable_error") ||
    inherits(error, "aisdk_api_timeout_error") ||
    inherits(error, "aisdk_api_network_error") ||
    inherits(error, "aisdk_api_server_error")
}

#' @keywords internal
normalize_api_url_candidates <- function(url) {
  urls <- as.character(url)
  urls <- trimws(urls)
  urls[nzchar(urls)]
}

# Connectivity preflight that no longer short-circuits the request.
#
# Historically aisdk returned NULL immediately when `curl::has_internet()`
# returned FALSE — this caused silent empty results behind corporate proxies,
# VPNs and certain custom DNS setups where the libcurl heuristic produces a
# false negative even though specific API endpoints are reachable (see
# https://github.com/jeroen/curl/issues/277). The real HTTP error is always
# a more useful signal than a generic preflight, so we now:
#
#   - log a one-time message per URL when the preflight looks bad,
#   - return TRUE/FALSE without aborting so callers proceed with the actual
#     request and surface the authoritative error (timeout, 5xx, DNS, ...).
#
# Set `options(aisdk.preflight_mode = "abort")` (or
# `AISDK_PREFLIGHT_MODE=abort`) to restore the old short-circuit behavior.
preflight_internet <- function(url) {
  if (should_skip_internet_check()) {
    return(TRUE)
  }
  ok <- tryCatch(curl::has_internet(), error = function(e) NA)
  if (isTRUE(ok)) {
    return(TRUE)
  }
  # Soft path: warn once per URL and keep going.
  mode <- getOption("aisdk.preflight_mode",
                    Sys.getenv("AISDK_PREFLIGHT_MODE", "warn"))
  if (identical(tolower(mode), "abort")) {
    message("Internet connection is not available. Cannot reach: ", url)
    message("Hint: Run check_api(url = '", url, "') to diagnose connection issues.")
    return(FALSE)
  }
  key <- substr(url %||% "<no url>", 1, 200)
  if (is.null(.aisdk_preflight_warned[[key]])) {
    .aisdk_preflight_warned[[key]] <- TRUE
    message("aisdk: curl::has_internet() reports no connectivity for ", url,
            "; attempting request anyway (set AISDK_PREFLIGHT_MODE=abort to restore the old short-circuit).")
  }
  TRUE
}

resolve_positive_timeout_seconds <- function(value, arg_name = "timeout_seconds") {
  if (is.null(value)) {
    return(NULL)
  }

  if (!is.numeric(value) || length(value) != 1 || is.na(value) || !is.finite(value) || value <= 0) {
    rlang::abort(paste0("`", arg_name, "` must be a single positive number."))
  }

  as.numeric(value)
}

read_timeout_setting <- function(explicit = NULL,
                                 option_name = NULL,
                                 env_name = NULL,
                                 default = NULL,
                                 arg_name = "timeout_seconds") {
  value <- explicit

  if (is.null(value) && !is.null(option_name)) {
    value <- getOption(option_name, NULL)
  }

  if (is.null(value) && !is.null(env_name)) {
    env_value <- trimws(Sys.getenv(env_name, ""))
    if (nzchar(env_value)) {
      value <- suppressWarnings(as.numeric(env_value))
    }
  }

  if (is.null(value)) {
    value <- default
  }

  resolve_positive_timeout_seconds(value, arg_name = arg_name)
}

resolve_request_timeout_config <- function(timeout_seconds = NULL,
                                           total_timeout_seconds = NULL,
                                           first_byte_timeout_seconds = NULL,
                                           connect_timeout_seconds = NULL,
                                           idle_timeout_seconds = NULL,
                                           request_type = c("request", "stream")) {
  request_type <- match.arg(request_type)

  total_default <- NULL
  connect_default <- 10
  first_byte_default <- 300
  idle_default <- 120

  total_timeout_seconds <- read_timeout_setting(
    explicit = total_timeout_seconds,
    option_name = "aisdk.http_total_timeout_seconds",
    env_name = "AISDK_HTTP_TOTAL_TIMEOUT_SECONDS",
    default = NULL,
    arg_name = "total_timeout_seconds"
  )

  if (is.null(total_timeout_seconds)) {
    total_timeout_seconds <- read_timeout_setting(
      explicit = timeout_seconds,
      option_name = "aisdk.http_timeout_seconds",
      env_name = "AISDK_HTTP_TIMEOUT_SECONDS",
      default = total_default,
      arg_name = "timeout_seconds"
    )
  }

  first_byte_timeout_seconds <- read_timeout_setting(
    explicit = first_byte_timeout_seconds,
    option_name = "aisdk.http_first_byte_timeout_seconds",
    env_name = "AISDK_HTTP_FIRST_BYTE_TIMEOUT_SECONDS",
    default = first_byte_default,
    arg_name = "first_byte_timeout_seconds"
  )

  connect_timeout_seconds <- read_timeout_setting(
    explicit = connect_timeout_seconds,
    option_name = "aisdk.http_connect_timeout_seconds",
    env_name = "AISDK_HTTP_CONNECT_TIMEOUT_SECONDS",
    default = connect_default,
    arg_name = "connect_timeout_seconds"
  )

  idle_timeout_seconds <- read_timeout_setting(
    explicit = idle_timeout_seconds,
    option_name = "aisdk.http_idle_timeout_seconds",
    env_name = "AISDK_HTTP_IDLE_TIMEOUT_SECONDS",
    default = idle_default,
    arg_name = "idle_timeout_seconds"
  )

  list(
    total_timeout_seconds = total_timeout_seconds,
    first_byte_timeout_seconds = first_byte_timeout_seconds,
    connect_timeout_seconds = connect_timeout_seconds,
    idle_timeout_seconds = idle_timeout_seconds
  )
}

resolve_request_timeout_seconds <- function(timeout_seconds = NULL) {
  resolve_request_timeout_config(timeout_seconds = timeout_seconds, request_type = "request")$total_timeout_seconds
}

curl_option_available <- function(option_name) {
  option_name %in% names(curl::curl_options())
}

apply_request_timeout_config <- function(req, timeout_config) {
  if (!is.null(timeout_config$total_timeout_seconds)) {
    req <- httr2::req_timeout(req, timeout_config$total_timeout_seconds)
  }

  curl_options <- list()

  if (!is.null(timeout_config$connect_timeout_seconds)) {
    curl_options$connecttimeout <- as.integer(ceiling(timeout_config$connect_timeout_seconds))
  }

  if (
    !is.null(timeout_config$first_byte_timeout_seconds) &&
      curl_option_available("server_response_timeout")
  ) {
    curl_options$server_response_timeout <- as.integer(ceiling(timeout_config$first_byte_timeout_seconds))
  }

  if (!is.null(timeout_config$idle_timeout_seconds)) {
    curl_options$low_speed_limit <- 1L
    curl_options$low_speed_time <- as.integer(ceiling(timeout_config$idle_timeout_seconds))
  }

  if (length(curl_options) > 0) {
    req <- do.call(httr2::req_options, c(list(.req = req), curl_options))
  }

  req
}

prepare_json_post_request <- function(req, body) {
  req <- httr2::req_method(req, "POST")
  httr2::req_body_json(req, body)
}

prepare_multipart_post_request <- function(req, body) {
  req <- httr2::req_method(req, "POST")
  do.call(httr2::req_body_multipart, c(list(.req = req), body))
}

safe_parse_api_error_body <- function(error_body) {
  if (is.null(error_body) || !is.character(error_body) || !nzchar(trimws(error_body))) {
    return(NULL)
  }

  tryCatch(
    jsonlite::fromJSON(error_body, simplifyVector = FALSE),
    error = function(...) NULL
  )
}

http_error_classes <- function(status, error_body = "") {
  payload <- safe_parse_api_error_body(error_body)
  body_lower <- tolower(error_body %||% "")
  message_text <- tolower(payload$error$message %||% payload$message %||% "")
  param_text <- tolower(payload$error$param %||% "")
  classes <- c("aisdk_api_error")

  if (api_status_retryable(status)) {
    classes <- c("aisdk_api_retryable_error", classes)
  }

  if (status == 429) {
    classes <- c("aisdk_api_rate_limit_error", classes)
  }

  # Timeout classification is status-code-first. A definitive client error
  # (400/401/403/404/405/409/422) is permanent even if its body happens to
  # mention "timeout" (e.g. an upstream quoting `Invalid value for 'timeout'`);
  # classing it as a retryable timeout would re-POST the doomed request to
  # every failover endpoint and cooldown healthy routes.
  permanent_client_error <- status %in% c(400, 401, 403, 404, 405, 409, 422)
  timeout_in_text <- grepl("timeout|timed out", body_lower) || grepl("timeout|timed out", message_text)
  if (status %in% c(408, 504) || (timeout_in_text && !permanent_client_error)) {
    classes <- c("aisdk_api_timeout_error", classes)
  }

  if (
    status %in% c(400, 404, 422) &&
      (
        grepl("unknown parameter|unsupported parameter|unsupported field|invalid_request_error", body_lower) ||
          grepl("unknown parameter|unsupported parameter|unsupported field", message_text) ||
          grepl("unknown_parameter", body_lower) ||
          nzchar(param_text)
      )
  ) {
    classes <- c("aisdk_api_compatibility_error", classes)
  }

  if (status >= 500) {
    classes <- c("aisdk_api_server_error", classes)
  }

  unique(classes)
}

request_error_classes <- function(error) {
  msg <- tolower(conditionMessage(error) %||% "")
  classes <- c("aisdk_api_error")

  if (grepl("timeout|timed out|server_response_timeout|connecttimeout|deadline", msg)) {
    classes <- c("aisdk_api_timeout_error", classes)
  } else {
    classes <- c("aisdk_api_network_error", classes)
  }

  unique(classes)
}

is_stream_transport_error <- function(error) {
  msg <- conditionMessage(error) %||% ""
  grepl(
    paste(c(
      "Failed to perform HTTP request",
      "cannot open the connection",
      "connection reset",
      "connection refused",
      "connection closed",
      "transfer closed",
      "failure when receiving data",
      "could not resolve host",
      "host unreachable",
      "timeout",
      "timed out",
      "server_response_timeout",
      "connecttimeout",
      "low speed",
      "SSL"
    ), collapse = "|"),
    msg,
    ignore.case = TRUE
  )
}

stream_perform_connection <- function(req) {
  httr2::req_perform_connection(req)
}

perform_request <- function(req) {
  httr2::req_perform(req)
}

stream_response_status <- function(resp) {
  httr2::resp_status(resp)
}

stream_response_body_string <- function(resp) {
  httr2::resp_body_string(resp)
}

stream_response_is_complete <- function(resp) {
  httr2::resp_stream_is_complete(resp)
}

stream_response_sse <- function(resp) {
  httr2::resp_stream_sse(resp)
}

stream_response_close <- function(resp) {
  if (is.null(resp)) {
    return(invisible(NULL))
  }
  tryCatch(close(resp), error = function(e) NULL)
  invisible(NULL)
}

abort_http_api_error <- function(status, url, error_body) {
  # Extract the provider's structured error fields so the message is readable and
  # the condition is programmatically inspectable (tryCatch on $error_code etc.),
  # instead of dumping a raw JSON blob. Handles the OpenAI {error:{message,type,
  # param,code}}, Anthropic {error:{type,message}}, and Gemini {error:{code,
  # message,status}} shapes.
  payload <- safe_parse_api_error_body(error_body)
  err <- if (is.list(payload)) (payload$error %||% payload) else list()
  as_scalar_chr <- function(x) if (is.character(x) && length(x) == 1 && nzchar(x)) x else NULL
  emsg <- as_scalar_chr(err$message) %||% as_scalar_chr(payload$message)
  etype <- as_scalar_chr(err$type) %||% as_scalar_chr(err$status) # Gemini uses `status`
  ecode <- if (!is.null(err$code) && length(err$code) == 1) err$code else NULL
  eparam <- as_scalar_chr(err$param)

  header <- paste0("API request failed with status ", status)
  if (!is.null(etype)) {
    header <- paste0(header, " (", etype, ")")
  }
  detail <- emsg %||% error_body
  if (!is.null(eparam)) {
    detail <- paste0(detail, " [param: ", eparam, "]")
  }

  fields <- list(
    status = status, error_type = etype, error_code = ecode,
    error_param = eparam, error_body = error_body
  )
  fields <- fields[!vapply(fields, is.null, logical(1))]

  rlang::abort(
    c(
      header,
      "i" = paste0("URL: ", url),
      "x" = detail
    ),
    class = http_error_classes(status, error_body),
    !!!fields
  )
}

abort_retry_api_error <- function(url, error) {
  preserved_classes <- grep("^aisdk_", class(error), value = TRUE)
  classes <- if (length(preserved_classes) > 0) {
    unique(c(preserved_classes, "aisdk_api_error"))
  } else {
    request_error_classes(error)
  }
  header <- if ("aisdk_api_timeout_error" %in% classes) {
    "API request timed out after all retries"
  } else {
    "API request failed after all retries"
  }
  url_label <- if (length(normalize_api_url_candidates(url)) > 1 || grepl(",", url, fixed = TRUE)) "URLs: " else "URL: "

  rlang::abort(
    c(
      header,
      "i" = paste0(url_label, url),
      "x" = conditionMessage(error)
    ),
    class = classes,
    parent = error
  )
}

# Generate a unique idempotency key for one logical request. Uses tempfile()'s
# internal counter for the random component so it does NOT perturb the global
# RNG stream (a package must not consume the user's set.seed() sequence), plus a
# millisecond timestamp and the process id for cross-process/time uniqueness.
#' @keywords internal
generate_idempotency_key <- function() {
  token <- basename(tempfile(pattern = ""))
  stamp <- sprintf("%.0f", as.numeric(Sys.time()) * 1000)
  sprintf("aisdk-%s-%d-%s", stamp, Sys.getpid(), token)
}

# Attach an Idempotency-Key header so aisdk's automatic retries (and failover)
# of a POST are safe against duplicate side effects / double billing when a
# request actually succeeded server-side but its response was lost. Providers
# that honor idempotency keys (e.g. OpenAI) dedupe on it; where unsupported the
# extra header is ignored. The key is generated ONCE per logical request and the
# same list is reused across every retry, so all attempts carry one key.
#
# Idempotent by design: a caller/provider-supplied key (case-insensitive) is
# respected, and re-applying to headers that already carry one is a no-op — so
# post_to_api and post_to_api_failover can both call it without double-keying.
# Disable with options(aisdk.idempotency_key = FALSE); rename the header with
# options(aisdk.idempotency_header = "...").
#' @keywords internal
apply_idempotency_key <- function(headers) {
  if (!isTRUE(getOption("aisdk.idempotency_key", TRUE))) {
    return(headers)
  }
  header_name <- getOption("aisdk.idempotency_header", "Idempotency-Key")
  existing <- names(headers)
  if (length(existing) > 0 && any(tolower(existing) == tolower(header_name))) {
    return(headers) # respect a caller/provider-supplied key
  }
  headers[[header_name]] <- generate_idempotency_key()
  headers
}

#' @title Post to API with Retry
#' @description
#' Makes a POST request to an API endpoint with automatic retry on failure.
#' Implements exponential backoff and respects `retry-after` headers.
#'
#' @param url The API endpoint URL.
#' @param headers A named list of HTTP headers.
#' @param body The request body (will be converted to JSON).
#' @param max_retries Maximum number of retries (default: 2).
#' @param initial_delay_ms Initial delay in milliseconds (default: 2000).
#' @param backoff_factor Multiplier for delay on each retry (default: 2).
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total request timeout in seconds.
#'   Defaults to `getOption("aisdk.http_total_timeout_seconds")`, then
#'   `AISDK_HTTP_TOTAL_TIMEOUT_SECONDS`, then legacy
#'   `aisdk.http_timeout_seconds` / `AISDK_HTTP_TIMEOUT_SECONDS`. No total
#'   timeout is applied by default.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in
#'   seconds. Defaults to `getOption("aisdk.http_first_byte_timeout_seconds")`,
#'   then `AISDK_HTTP_FIRST_BYTE_TIMEOUT_SECONDS`, then 300.
#' @param connect_timeout_seconds Optional connection-establishment timeout in
#'   seconds. Defaults to `getOption("aisdk.http_connect_timeout_seconds")`,
#'   then `AISDK_HTTP_CONNECT_TIMEOUT_SECONDS`, then 10.
#' @param idle_timeout_seconds Optional stall timeout in seconds. When set, the
#'   request is aborted only if transfer progress drops below 1 byte/second for
#'   the full interval. Defaults to `getOption("aisdk.http_idle_timeout_seconds")`,
#'   then `AISDK_HTTP_IDLE_TIMEOUT_SECONDS`, then 120.
#' @return The parsed JSON response.
#' @keywords internal
#' @export
post_to_api <- function(url, headers, body,
                        max_retries = 2,
                        initial_delay_ms = 2000,
                        backoff_factor = 2,
                        timeout_seconds = NULL,
                        total_timeout_seconds = NULL,
                        first_byte_timeout_seconds = NULL,
                        connect_timeout_seconds = NULL,
                        idle_timeout_seconds = NULL) {
  urls <- normalize_api_url_candidates(url)
  if (length(urls) == 0) {
    rlang::abort("`url` must contain at least one non-empty API endpoint URL.")
  }
  # One idempotency key for this logical request, fixed before the retry loop so
  # every retry (and, below, every failover candidate) carries the same key.
  headers <- apply_idempotency_key(headers)
  if (length(urls) > 1) {
    return(post_to_api_failover(
      urls = urls,
      headers = headers,
      body = body,
      max_retries = max_retries,
      initial_delay_ms = initial_delay_ms,
      backoff_factor = backoff_factor,
      timeout_seconds = timeout_seconds,
      total_timeout_seconds = total_timeout_seconds,
      first_byte_timeout_seconds = first_byte_timeout_seconds,
      connect_timeout_seconds = connect_timeout_seconds,
      idle_timeout_seconds = idle_timeout_seconds
    ))
  }
  url <- urls[[1]]

  # CRAN policy: fail gracefully when internet is unavailable. The preflight
  # is non-fatal by default; see `preflight_internet()` for the rationale.
  if (!preflight_internet(url)) {
    return(NULL)
  }

  timeout_config <- resolve_request_timeout_config(
    timeout_seconds = timeout_seconds,
    total_timeout_seconds = total_timeout_seconds,
    first_byte_timeout_seconds = first_byte_timeout_seconds,
    connect_timeout_seconds = connect_timeout_seconds,
    idle_timeout_seconds = idle_timeout_seconds,
    request_type = "request"
  )

  attempt <- 0
  delay_ms <- initial_delay_ms

  repeat {
    attempt <- attempt + 1

    tryCatch(
      {
        req <- httr2::request(url)
        req <- httr2::req_headers(req, !!!headers)
        req <- prepare_json_post_request(req, body)
        req <- apply_request_timeout_config(req, timeout_config)
        req <- httr2::req_error(req, is_error = function(resp) FALSE) # Handle errors manually

        resp <- perform_request(req)
        status <- httr2::resp_status(resp)
        capture_rate_limit_headers(resp)

        if (status >= 200 && status < 300) {
          # Handle empty response body (like Opencode does)
          # Some servers return 200 with no Content-Length and empty body
          resp_text <- tryCatch(
            httr2::resp_body_string(resp),
            error = function(e) ""
          )

          if (is.null(resp_text) || nchar(trimws(resp_text)) == 0) {
            return(list()) # Return empty list instead of parse error
          }

          # Try to parse JSON with repair fallback
          return(tryCatch(
            jsonlite::fromJSON(resp_text, simplifyVector = FALSE),
            error = function(e) {
              # Try to repair and re-parse
              repaired <- fix_json(resp_text)
              tryCatch(
                jsonlite::fromJSON(repaired, simplifyVector = FALSE),
                error = function(e2) {
                  # Return raw text wrapped in a list as last resort
                  list(`_raw_response` = resp_text)
                }
              )
            }
          ))
        } else {
          # Check if retryable (rate limit or server error)
          is_retryable <- api_status_retryable(status)

          if (!is_retryable || attempt > max_retries) {
            error_body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
            abort_http_api_error(status = status, url = url, error_body = error_body)
          }

          # Get retry delay from headers if available (robust: HTTP-date safe,
          # capped, falls back to the exponential delay).
          delay_ms <- resolve_retry_after_ms(
            httr2::resp_header(resp, "retry-after"),
            httr2::resp_header(resp, "retry-after-ms"),
            fallback_ms = delay_ms
          )

          message(sprintf("Retrying in %d ms (attempt %d/%d)...", as.integer(delay_ms), attempt, max_retries + 1))
          Sys.sleep(apply_backoff_jitter(delay_ms) / 1000)
          delay_ms <- delay_ms * backoff_factor
        }
      },
      error = function(e) {
        if (inherits(e, "aisdk_api_error")) {
          rlang::cnd_signal(e)
        }
        if (attempt > max_retries) {
          abort_retry_api_error(url = url, error = e)
        }
        message(sprintf("Network error, retrying in %d ms...", delay_ms))
        Sys.sleep(apply_backoff_jitter(delay_ms) / 1000)
        delay_ms <- delay_ms * backoff_factor
      }
    )
  }
}

#' @keywords internal
post_to_api_failover <- function(urls, headers, body,
                                 max_retries = 2,
                                 initial_delay_ms = 2000,
                                 backoff_factor = 2,
                                 timeout_seconds = NULL,
                                 total_timeout_seconds = NULL,
                                 first_byte_timeout_seconds = NULL,
                                 connect_timeout_seconds = NULL,
                                 idle_timeout_seconds = NULL) {
  urls <- order_api_url_candidates(urls)
  last_error <- NULL

  # Fix one idempotency key across all failover candidates (post_to_api sees it
  # as already present and won't mint a per-candidate key).
  headers <- apply_idempotency_key(headers)

  for (candidate in urls) {
    result <- tryCatch(
      post_to_api(
        url = candidate,
        headers = headers,
        body = body,
        max_retries = max_retries,
        initial_delay_ms = initial_delay_ms,
        backoff_factor = backoff_factor,
        timeout_seconds = timeout_seconds,
        total_timeout_seconds = total_timeout_seconds,
        first_byte_timeout_seconds = first_byte_timeout_seconds,
        connect_timeout_seconds = connect_timeout_seconds,
        idle_timeout_seconds = idle_timeout_seconds
      ),
      error = function(e) e
    )

    if (!inherits(result, "error")) {
      mark_api_route_success(candidate)
      return(result)
    }

    last_error <- result
    retryable <- is_retryable_api_error(result)

    if (!retryable) {
      rlang::cnd_signal(result)
    }

    mark_api_route_failure(candidate, conditionMessage(result))
    if (length(urls) > 1) {
      message("aisdk: API route failed; trying next configured endpoint.")
    }
  }

  if (!is.null(last_error)) {
    abort_retry_api_error(url = paste(urls, collapse = ", "), error = last_error)
  }
  rlang::abort("API request failed: no API endpoints were attempted.", class = "aisdk_api_error")
}

#' @title Post Multipart to API with Retry
#' @description
#' Makes a multipart POST request to an API endpoint with automatic retry on
#' failure. This is used for file-upload APIs such as image editing.
#'
#' @param url The API endpoint URL.
#' @param headers A named list of HTTP headers.
#' @param body A named list of multipart fields.
#' @param max_retries Maximum number of retries (default: 2).
#' @param initial_delay_ms Initial delay in milliseconds (default: 2000).
#' @param backoff_factor Multiplier for delay on each retry (default: 2).
#' @inheritParams post_to_api
#' @return The parsed JSON response.
#' @keywords internal
#' @export
post_multipart_to_api <- function(url, headers, body,
                                  max_retries = 2,
                                  initial_delay_ms = 2000,
                                  backoff_factor = 2,
                                  timeout_seconds = NULL,
                                  total_timeout_seconds = NULL,
                                  first_byte_timeout_seconds = NULL,
                                  connect_timeout_seconds = NULL,
                                  idle_timeout_seconds = NULL) {
  urls <- normalize_api_url_candidates(url)
  if (length(urls) == 0) {
    rlang::abort("`url` must contain at least one non-empty API endpoint URL.")
  }
  if (length(urls) > 1) {
    return(post_multipart_to_api_failover(
      urls = urls,
      headers = headers,
      body = body,
      max_retries = max_retries,
      initial_delay_ms = initial_delay_ms,
      backoff_factor = backoff_factor,
      timeout_seconds = timeout_seconds,
      total_timeout_seconds = total_timeout_seconds,
      first_byte_timeout_seconds = first_byte_timeout_seconds,
      connect_timeout_seconds = connect_timeout_seconds,
      idle_timeout_seconds = idle_timeout_seconds
    ))
  }
  url <- urls[[1]]

  if (!preflight_internet(url)) {
    return(NULL)
  }

  timeout_config <- resolve_request_timeout_config(
    timeout_seconds = timeout_seconds,
    total_timeout_seconds = total_timeout_seconds,
    first_byte_timeout_seconds = first_byte_timeout_seconds,
    connect_timeout_seconds = connect_timeout_seconds,
    idle_timeout_seconds = idle_timeout_seconds,
    request_type = "request"
  )

  attempt <- 0
  delay_ms <- initial_delay_ms

  repeat {
    attempt <- attempt + 1

    tryCatch(
      {
        req <- httr2::request(url)
        req <- httr2::req_headers(req, !!!headers)
        req <- prepare_multipart_post_request(req, body)
        req <- apply_request_timeout_config(req, timeout_config)
        req <- httr2::req_error(req, is_error = function(resp) FALSE)

        resp <- perform_request(req)
        status <- httr2::resp_status(resp)
        capture_rate_limit_headers(resp)

        if (status >= 200 && status < 300) {
          resp_text <- tryCatch(
            httr2::resp_body_string(resp),
            error = function(e) ""
          )

          if (is.null(resp_text) || nchar(trimws(resp_text)) == 0) {
            return(list())
          }

          return(tryCatch(
            jsonlite::fromJSON(resp_text, simplifyVector = FALSE),
            error = function(e) {
              repaired <- fix_json(resp_text)
              tryCatch(
                jsonlite::fromJSON(repaired, simplifyVector = FALSE),
                error = function(e2) list(`_raw_response` = resp_text)
              )
            }
          ))
        }

        is_retryable <- api_status_retryable(status)

        if (!is_retryable || attempt > max_retries) {
          error_body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
          abort_http_api_error(status = status, url = url, error_body = error_body)
        }

        delay_ms <- resolve_retry_after_ms(
          httr2::resp_header(resp, "retry-after"),
          httr2::resp_header(resp, "retry-after-ms"),
          fallback_ms = delay_ms
        )

        message(sprintf("Retrying in %d ms (attempt %d/%d)...", as.integer(delay_ms), attempt, max_retries + 1))
        Sys.sleep(apply_backoff_jitter(delay_ms) / 1000)
        delay_ms <- delay_ms * backoff_factor
      },
      error = function(e) {
        if (inherits(e, "aisdk_api_error")) {
          rlang::cnd_signal(e)
        }
        if (attempt > max_retries) {
          abort_retry_api_error(url = url, error = e)
        }
        message(sprintf("Network error, retrying in %d ms...", delay_ms))
        Sys.sleep(apply_backoff_jitter(delay_ms) / 1000)
        delay_ms <- delay_ms * backoff_factor
      }
    )
  }
}

#' @keywords internal
post_multipart_to_api_failover <- function(urls, headers, body,
                                           max_retries = 2,
                                           initial_delay_ms = 2000,
                                           backoff_factor = 2,
                                           timeout_seconds = NULL,
                                           total_timeout_seconds = NULL,
                                           first_byte_timeout_seconds = NULL,
                                           connect_timeout_seconds = NULL,
                                           idle_timeout_seconds = NULL) {
  urls <- order_api_url_candidates(urls)
  last_error <- NULL

  for (candidate in urls) {
    result <- tryCatch(
      post_multipart_to_api(
        url = candidate,
        headers = headers,
        body = body,
        max_retries = max_retries,
        initial_delay_ms = initial_delay_ms,
        backoff_factor = backoff_factor,
        timeout_seconds = timeout_seconds,
        total_timeout_seconds = total_timeout_seconds,
        first_byte_timeout_seconds = first_byte_timeout_seconds,
        connect_timeout_seconds = connect_timeout_seconds,
        idle_timeout_seconds = idle_timeout_seconds
      ),
      error = function(e) e
    )

    if (!inherits(result, "error")) {
      mark_api_route_success(candidate)
      return(result)
    }

    last_error <- result
    retryable <- is_retryable_api_error(result)

    if (!retryable) {
      rlang::cnd_signal(result)
    }

    mark_api_route_failure(candidate, conditionMessage(result))
    if (length(urls) > 1) {
      message("aisdk: API route failed; trying next configured endpoint.")
    }
  }

  if (!is.null(last_error)) {
    abort_retry_api_error(url = paste(urls, collapse = ", "), error = last_error)
  }
  rlang::abort("Multipart API request failed: no API endpoints were attempted.", class = "aisdk_api_error")
}

#' @keywords internal
request_json_from_api <- function(url, headers, method = "GET", body = NULL,
                                  max_retries = 2,
                                  initial_delay_ms = 2000,
                                  backoff_factor = 2,
                                  timeout_seconds = NULL,
                                  total_timeout_seconds = NULL,
                                  first_byte_timeout_seconds = NULL,
                                  connect_timeout_seconds = NULL,
                                  idle_timeout_seconds = NULL) {
  urls <- normalize_api_url_candidates(url)
  if (length(urls) == 0) {
    rlang::abort("`url` must contain at least one non-empty API endpoint URL.")
  }

  if (length(urls) > 1) {
    urls <- order_api_url_candidates(urls)
    last_error <- NULL
    for (candidate in urls) {
      result <- tryCatch(
        request_json_from_api(
          url = candidate,
          headers = headers,
          method = method,
          body = body,
          max_retries = max_retries,
          initial_delay_ms = initial_delay_ms,
          backoff_factor = backoff_factor,
          timeout_seconds = timeout_seconds,
          total_timeout_seconds = total_timeout_seconds,
          first_byte_timeout_seconds = first_byte_timeout_seconds,
          connect_timeout_seconds = connect_timeout_seconds,
          idle_timeout_seconds = idle_timeout_seconds
        ),
        error = function(e) e
      )
      if (!inherits(result, "error")) {
        mark_api_route_success(candidate)
        return(result)
      }
      last_error <- result
      retryable <- is_retryable_api_error(result)
      if (!retryable) {
        rlang::cnd_signal(result)
      }
      mark_api_route_failure(candidate, conditionMessage(result))
      message("aisdk: API route failed; trying next configured endpoint.")
    }
    abort_retry_api_error(url = paste(urls, collapse = ", "), error = last_error)
  }

  url <- urls[[1]]
  if (!preflight_internet(url)) {
    return(NULL)
  }

  timeout_config <- resolve_request_timeout_config(
    timeout_seconds = timeout_seconds,
    total_timeout_seconds = total_timeout_seconds,
    first_byte_timeout_seconds = first_byte_timeout_seconds,
    connect_timeout_seconds = connect_timeout_seconds,
    idle_timeout_seconds = idle_timeout_seconds,
    request_type = "request"
  )

  attempt <- 0L
  delay_ms <- initial_delay_ms

  repeat {
    attempt <- attempt + 1L
    tryCatch(
      {
        req <- httr2::request(url)
        req <- httr2::req_headers(req, !!!headers)
        req <- httr2::req_method(req, method)
        if (!is.null(body) && length(body) > 0) {
          req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
        }
        req <- apply_request_timeout_config(req, timeout_config)
        req <- httr2::req_error(req, is_error = function(resp) FALSE)

        resp <- perform_request(req)
        status <- httr2::resp_status(resp)
        capture_rate_limit_headers(resp)
        if (status >= 200 && status < 300) {
          resp_text <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
          if (!nzchar(trimws(resp_text %||% ""))) {
            return(list())
          }
          return(jsonlite::fromJSON(resp_text, simplifyVector = FALSE))
        }

        if (!api_status_retryable(status) || attempt > max_retries) {
          error_body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
          abort_http_api_error(status = status, url = url, error_body = error_body)
        }

        delay_ms <- resolve_retry_after_ms(
          httr2::resp_header(resp, "retry-after"),
          httr2::resp_header(resp, "retry-after-ms"),
          fallback_ms = delay_ms
        )
        message(sprintf("Retrying in %d ms (attempt %d/%d)...", as.integer(delay_ms), attempt, max_retries + 1))
        Sys.sleep(apply_backoff_jitter(delay_ms) / 1000)
        delay_ms <- delay_ms * backoff_factor
      },
      error = function(e) {
        if (inherits(e, "aisdk_api_error")) {
          rlang::cnd_signal(e)
        }
        if (attempt > max_retries) {
          abort_retry_api_error(url = url, error = e)
        }
        message(sprintf("Network error, retrying in %d ms...", delay_ms))
        Sys.sleep(apply_backoff_jitter(delay_ms) / 1000)
        delay_ms <- delay_ms * backoff_factor
      }
    )
  }
}

#' @title Stream from API
#' @description
#' Makes a streaming POST request and processes Server-Sent Events (SSE) using httr2.
#' Implements robust error recovery for malformed SSE data.
#'
#' @param url The API endpoint URL.
#' @param headers A named list of HTTP headers.
#' @param body The request body (will be converted to JSON).
#' @param callback A function called for each parsed SSE data chunk.
#' @param max_retries Maximum number of connection/first-event retries
#'   before any stream chunk has been delivered (default: 5).
#' @param initial_delay_ms Initial delay in milliseconds (default: 2000).
#' @param backoff_factor Multiplier for delay on each retry (default: 2).
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total stream timeout in seconds.
#'   Defaults to `getOption("aisdk.http_total_timeout_seconds")`, then
#'   `AISDK_HTTP_TOTAL_TIMEOUT_SECONDS`, then legacy
#'   `aisdk.http_timeout_seconds` / `AISDK_HTTP_TIMEOUT_SECONDS`. Streams do
#'   not apply a default total timeout when none is configured.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in
#'   seconds. Defaults to `getOption("aisdk.http_first_byte_timeout_seconds")`,
#'   then `AISDK_HTTP_FIRST_BYTE_TIMEOUT_SECONDS`, then 300.
#' @param connect_timeout_seconds Optional connection-establishment timeout in
#'   seconds. Defaults to `getOption("aisdk.http_connect_timeout_seconds")`,
#'   then `AISDK_HTTP_CONNECT_TIMEOUT_SECONDS`, then 10.
#' @param idle_timeout_seconds Optional stall timeout in seconds. Streams
#'   default to `getOption("aisdk.http_idle_timeout_seconds")`, then
#'   `AISDK_HTTP_IDLE_TIMEOUT_SECONDS`, then 120. As long as bytes keep
#'   arriving, the stream is considered healthy.
#' @keywords internal
stream_from_api <- function(url, headers, body, callback,
                            max_retries = 5,
                            initial_delay_ms = 2000,
                            backoff_factor = 2,
                            timeout_seconds = NULL,
                            total_timeout_seconds = NULL,
                            first_byte_timeout_seconds = NULL,
                            connect_timeout_seconds = NULL,
                            idle_timeout_seconds = NULL) {
  urls <- normalize_api_url_candidates(url)
  if (length(urls) == 0) {
    rlang::abort("`url` must contain at least one non-empty API endpoint URL.")
  }
  if (length(urls) > 1) {
    return(stream_from_api_failover(
      urls = urls,
      headers = headers,
      body = body,
      callback = callback,
      max_retries = max_retries,
      initial_delay_ms = initial_delay_ms,
      backoff_factor = backoff_factor,
      timeout_seconds = timeout_seconds,
      total_timeout_seconds = total_timeout_seconds,
      first_byte_timeout_seconds = first_byte_timeout_seconds,
      connect_timeout_seconds = connect_timeout_seconds,
      idle_timeout_seconds = idle_timeout_seconds
    ))
  }
  url <- urls[[1]]

  # CRAN policy: fail gracefully when internet is unavailable. The preflight
  # is non-fatal by default; see `preflight_internet()` for the rationale.
  if (!preflight_internet(url)) {
    return(invisible(NULL))
  }

  timeout_config <- resolve_request_timeout_config(
    timeout_seconds = timeout_seconds,
    total_timeout_seconds = total_timeout_seconds,
    first_byte_timeout_seconds = first_byte_timeout_seconds,
    connect_timeout_seconds = connect_timeout_seconds,
    idle_timeout_seconds = idle_timeout_seconds,
    request_type = "stream"
  )

  req <- httr2::request(url)
  req <- httr2::req_headers(req, !!!headers)
  req <- prepare_json_post_request(req, body)
  req <- apply_request_timeout_config(req, timeout_config)
  req <- httr2::req_error(req, is_error = function(resp) FALSE) # Handle errors manually

  attempt <- 0
  delay_ms <- initial_delay_ms

  run_stream_attempt <- function() {
    resp <- NULL
    stream_state <- new.env(parent = emptyenv())
    stream_state$consecutive_errors <- 0
    stream_state$delivered_events <- FALSE
    stream_state$done_emitted <- FALSE
    max_consecutive_errors <- 10

    on.exit(stream_response_close(resp), add = TRUE)

    # Establish connection
    resp <- stream_perform_connection(req)

    # Check status code immediately
    status <- stream_response_status(resp)
    if (status >= 400) {
      # If error, try to read the body to give a helpful message
      error_text <- tryCatch(
        stream_response_body_string(resp),
        error = function(e) "Unknown error (could not read body)"
      )

      abort_http_api_error(status = status, url = url, error_body = error_text)
    }

    abort_stream_transport_error <- function(e) {
      stream_class <- if (isTRUE(stream_state$delivered_events)) {
        "aisdk_stream_partial_error"
      } else {
        "aisdk_stream_start_error"
      }
      header <- if (isTRUE(stream_state$delivered_events)) {
        "API stream interrupted after data was received"
      } else {
        "API stream failed before the first event"
      }
      rlang::abort(
        c(
          header,
          "i" = paste0("URL: ", url),
          "x" = conditionMessage(e)
        ),
        class = c(stream_class, request_error_classes(e)),
        parent = e
      )
    }

    # Iterate over the stream using standard SSE parsing
    repeat {
      stream_complete <- tryCatch(
        stream_response_is_complete(resp),
        error = function(e) {
          if (is_stream_transport_error(e)) {
            abort_stream_transport_error(e)
          }
          rlang::cnd_signal(e)
        }
      )
      if (isTRUE(stream_complete)) {
        break
      }

      # resp_stream_sse returns a list(type=..., data=..., id=..., retry=...) or NULL
      event <- tryCatch(
        stream_response_sse(resp),
        error = function(e) {
          if (is_stream_transport_error(e)) {
            abort_stream_transport_error(e)
          }

          stream_state$consecutive_errors <- stream_state$consecutive_errors + 1
          if (stream_state$consecutive_errors >= max_consecutive_errors) {
            rlang::abort(c(
              "Too many consecutive SSE parsing errors",
              "x" = conditionMessage(e)
            ), class = "aisdk_stream_error")
          }
          NULL
        }
      )

      if (!is.null(event)) {
        stream_state$consecutive_errors <- 0 # Reset on successful event

        # Standard SSE handling
        # OpenAI and compatible APIs usually send JSON in the 'data' field
        if (!is.null(event$data) && nzchar(event$data)) {
          if (event$data == "[DONE]") {
            callback(NULL, done = TRUE)
            stream_state$done_emitted <- TRUE
            break
          }

          # Parse JSON data with repair fallback
          tryCatch(
            {
              data <- jsonlite::fromJSON(event$data, simplifyVector = FALSE)
              # Pass parsed data to callback
              callback(data, done = FALSE)
              stream_state$delivered_events <- TRUE
            },
            error = function(e) {
              # Try to repair JSON before giving up
              tryCatch(
                {
                  repaired_data <- fix_json(event$data)
                  data <- jsonlite::fromJSON(repaired_data, simplifyVector = FALSE)
                  callback(data, done = FALSE)
                  stream_state$delivered_events <- TRUE
                },
                error = function(e2) {
                  # Log warning but don't crash - graceful degradation
                  debug_opt <- getOption("aisdk.debug", FALSE)
                  if (isTRUE(debug_opt)) {
                    message(
                      "Warning: Malformed JSON in SSE data (skipping): ",
                      substr(event$data, 1, 100)
                    )
                  }
                }
              )
            }
          )
        }
      }
    }

    # Signal completion exactly once. Streams that end by connection close
    # (Gemini always; some OpenAI-compatible servers) never send a "[DONE]"
    # sentinel, so without this the callback's done=TRUE branch never fires:
    # finalizers don't run, an open <think> block never closes, and any
    # held-back tail buffer is dropped.
    if (!isTRUE(stream_state$done_emitted)) {
      callback(NULL, done = TRUE)
      stream_state$done_emitted <- TRUE
    }

    TRUE
  }

  repeat {
    attempt <- attempt + 1

    attempt_result <- tryCatch(
      run_stream_attempt(),
      error = function(e) e
    )

    if (isTRUE(attempt_result)) {
      return(invisible(NULL))
    }

    retryable_start_failure <- inherits(attempt_result, "aisdk_stream_start_error") ||
      (!inherits(attempt_result, "aisdk_api_error") && is_stream_transport_error(attempt_result))

    if (retryable_start_failure && attempt <= max_retries) {
      message(sprintf("Network error, retrying stream in %d ms...", delay_ms))
      Sys.sleep(apply_backoff_jitter(delay_ms) / 1000)
      delay_ms <- delay_ms * backoff_factor
      next
    }

    if (retryable_start_failure) {
      abort_retry_api_error(url = url, error = attempt_result)
    }

    rlang::cnd_signal(attempt_result)
  }
}

#' @keywords internal
stream_from_api_failover <- function(urls, headers, body, callback,
                                     max_retries = 5,
                                     initial_delay_ms = 2000,
                                     backoff_factor = 2,
                                     timeout_seconds = NULL,
                                     total_timeout_seconds = NULL,
                                     first_byte_timeout_seconds = NULL,
                                     connect_timeout_seconds = NULL,
                                     idle_timeout_seconds = NULL) {
  urls <- order_api_url_candidates(urls)
  last_error <- NULL

  for (candidate in urls) {
    result <- tryCatch(
      stream_from_api(
        url = candidate,
        headers = headers,
        body = body,
        callback = callback,
        max_retries = max_retries,
        initial_delay_ms = initial_delay_ms,
        backoff_factor = backoff_factor,
        timeout_seconds = timeout_seconds,
        total_timeout_seconds = total_timeout_seconds,
        first_byte_timeout_seconds = first_byte_timeout_seconds,
        connect_timeout_seconds = connect_timeout_seconds,
        idle_timeout_seconds = idle_timeout_seconds
      ),
      error = function(e) e
    )

    if (!inherits(result, "error")) {
      mark_api_route_success(candidate)
      return(invisible(NULL))
    }

    last_error <- result
    if (inherits(result, "aisdk_stream_partial_error")) {
      rlang::cnd_signal(result)
    }

    retryable <- inherits(result, "aisdk_stream_start_error") ||
      is_retryable_api_error(result)

    if (!retryable) {
      rlang::cnd_signal(result)
    }

    mark_api_route_failure(candidate, conditionMessage(result))
    if (length(urls) > 1) {
      message("aisdk: API stream route failed before data was received; trying next configured endpoint.")
    }
  }

  if (!is.null(last_error)) {
    abort_retry_api_error(url = paste(urls, collapse = ", "), error = last_error)
  }
  rlang::abort("API stream failed: no API endpoints were attempted.", class = "aisdk_api_error")
}
