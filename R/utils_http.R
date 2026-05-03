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

  if (status %in% c(408, 504) || grepl("timeout|timed out", body_lower) || grepl("timeout|timed out", message_text)) {
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

abort_http_api_error <- function(status, url, error_body) {
  rlang::abort(
    c(
      paste0("API request failed with status ", status),
      "i" = paste0("URL: ", url),
      "x" = error_body
    ),
    class = http_error_classes(status, error_body)
  )
}

abort_retry_api_error <- function(url, error) {
  classes <- request_error_classes(error)
  header <- if ("aisdk_api_timeout_error" %in% classes) {
    "API request timed out after all retries"
  } else {
    "API request failed after all retries"
  }

  rlang::abort(
    c(
      header,
      "i" = paste0("URL: ", url),
      "x" = conditionMessage(error)
    ),
    class = classes,
    parent = error
  )
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
post_to_api <- function(url, headers, body,
                        max_retries = 2,
                        initial_delay_ms = 2000,
                        backoff_factor = 2,
                        timeout_seconds = NULL,
                        total_timeout_seconds = NULL,
                        first_byte_timeout_seconds = NULL,
                        connect_timeout_seconds = NULL,
                        idle_timeout_seconds = NULL) {
  # CRAN policy: fail gracefully when internet is unavailable
  if (!should_skip_internet_check() && !curl::has_internet()) {
    message("Internet connection is not available. Cannot reach: ", url)
    message("Hint: Run check_api(url = '", url, "') to diagnose connection issues.")
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
        req <- httr2::req_body_json(req, body)
        req <- apply_request_timeout_config(req, timeout_config)
        req <- httr2::req_error(req, is_error = function(resp) FALSE) # Handle errors manually

        resp <- httr2::req_perform(req)
        status <- httr2::resp_status(resp)

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
          is_retryable <- status == 429 || status >= 500

          if (!is_retryable || attempt > max_retries) {
            error_body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
            abort_http_api_error(status = status, url = url, error_body = error_body)
          }

          # Get retry delay from headers if available
          retry_after <- httr2::resp_header(resp, "retry-after")
          retry_after_ms <- httr2::resp_header(resp, "retry-after-ms")

          if (!is.null(retry_after_ms)) {
            delay_ms <- as.numeric(retry_after_ms)
          } else if (!is.null(retry_after)) {
            delay_ms <- as.numeric(retry_after) * 1000
          }

          message(sprintf("Retrying in %d ms (attempt %d/%d)...", delay_ms, attempt, max_retries + 1))
          Sys.sleep(delay_ms / 1000)
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
        Sys.sleep(delay_ms / 1000)
        delay_ms <- delay_ms * backoff_factor
      }
    )
  }
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
post_multipart_to_api <- function(url, headers, body,
                                  max_retries = 2,
                                  initial_delay_ms = 2000,
                                  backoff_factor = 2,
                                  timeout_seconds = NULL,
                                  total_timeout_seconds = NULL,
                                  first_byte_timeout_seconds = NULL,
                                  connect_timeout_seconds = NULL,
                                  idle_timeout_seconds = NULL) {
  if (!should_skip_internet_check() && !curl::has_internet()) {
    message("Internet connection is not available. Cannot reach: ", url)
    message("Hint: Run check_api(url = '", url, "') to diagnose connection issues.")
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
        req <- httr2::req_body_multipart(req, !!!body)
        req <- apply_request_timeout_config(req, timeout_config)
        req <- httr2::req_error(req, is_error = function(resp) FALSE)

        resp <- httr2::req_perform(req)
        status <- httr2::resp_status(resp)

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

        is_retryable <- status == 429 || status >= 500

        if (!is_retryable || attempt > max_retries) {
          error_body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
          abort_http_api_error(status = status, url = url, error_body = error_body)
        }

        retry_after <- httr2::resp_header(resp, "retry-after")
        retry_after_ms <- httr2::resp_header(resp, "retry-after-ms")

        if (!is.null(retry_after_ms)) {
          delay_ms <- as.numeric(retry_after_ms)
        } else if (!is.null(retry_after)) {
          delay_ms <- as.numeric(retry_after) * 1000
        }

        message(sprintf("Retrying in %d ms (attempt %d/%d)...", delay_ms, attempt, max_retries + 1))
        Sys.sleep(delay_ms / 1000)
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
        Sys.sleep(delay_ms / 1000)
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
                            timeout_seconds = NULL,
                            total_timeout_seconds = NULL,
                            first_byte_timeout_seconds = NULL,
                            connect_timeout_seconds = NULL,
                            idle_timeout_seconds = NULL) {
  # CRAN policy: fail gracefully when internet is unavailable
  if (!should_skip_internet_check() && !curl::has_internet()) {
    message("Internet connection is not available. Cannot reach: ", url)
    message("Hint: Run check_api(url = '", url, "') to diagnose connection issues.")
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
  req <- httr2::req_body_json(req, body)
  req <- apply_request_timeout_config(req, timeout_config)
  req <- httr2::req_error(req, is_error = function(resp) FALSE) # Handle errors manually

  # Establish connection
  resp <- httr2::req_perform_connection(req)

  # Ensure connection is closed when function exits
  on.exit(close(resp), add = TRUE)

  # Check status code immediately
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    # If error, try to read the body to give a helpful message
    error_text <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(e) "Unknown error (could not read body)"
    )

    rlang::abort(c(
      paste0("API request failed with status ", status),
      "i" = paste0("URL: ", url),
      "x" = error_text
    ), class = "aisdk_api_error")
  }

  # Track consecutive errors for circuit breaker

  stream_state <- new.env(parent = emptyenv())
  stream_state$consecutive_errors <- 0
  max_consecutive_errors <- 10

  # Iterate over the stream using standard SSE parsing
  while (!httr2::resp_stream_is_complete(resp)) {
    # resp_stream_sse returns a list(type=..., data=..., id=..., retry=...) or NULL
    event <- tryCatch(
      httr2::resp_stream_sse(resp),
      error = function(e) {
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
          break
        }

        # Parse JSON data with repair fallback
        tryCatch(
          {
            data <- jsonlite::fromJSON(event$data, simplifyVector = FALSE)
            # Pass parsed data to callback
            callback(data, done = FALSE)
          },
          error = function(e) {
            # Try to repair JSON before giving up
            tryCatch(
              {
                repaired_data <- fix_json(event$data)
                data <- jsonlite::fromJSON(repaired_data, simplifyVector = FALSE)
                callback(data, done = FALSE)
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
}
