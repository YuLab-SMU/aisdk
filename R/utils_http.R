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
#' @return The parsed JSON response.
#' @keywords internal
post_to_api <- function(url, headers, body,
                        max_retries = 2,
                        initial_delay_ms = 2000,
                        backoff_factor = 2) {
  attempt <- 0
  delay_ms <- initial_delay_ms

  repeat {
    attempt <- attempt + 1

    tryCatch({
      req <- httr2::request(url)
      req <- httr2::req_headers(req, !!!headers)
      req <- httr2::req_body_json(req, body)
      req <- httr2::req_timeout(req, 120)  # 2 minute timeout
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
          return(list())  # Return empty list instead of parse error
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
          rlang::abort(c(
            paste0("API request failed with status ", status),
            "i" = paste0("URL: ", url),
            "x" = error_body
          ), class = "aisdk_api_error")
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

    }, error = function(e) {
      if (inherits(e, "aisdk_api_error")) {
        rlang::cnd_signal(e)
      }
      if (attempt > max_retries) {
        rlang::abort(c(
          "API request failed after all retries",
          "i" = paste0("URL: ", url),
          "x" = conditionMessage(e)
        ), class = "aisdk_api_error", parent = e)
      }
      message(sprintf("Network error, retrying in %d ms...", delay_ms))
      Sys.sleep(delay_ms / 1000)
      delay_ms <- delay_ms * backoff_factor
    })
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
#' @keywords internal
stream_from_api <- function(url, headers, body, callback) {
  req <- httr2::request(url)
  req <- httr2::req_headers(req, !!!headers)
  req <- httr2::req_body_json(req, body)
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

  consecutive_errors <- 0
  max_consecutive_errors <- 10

  # Iterate over the stream using standard SSE parsing
  while (!httr2::resp_stream_is_complete(resp)) {
    # resp_stream_sse returns a list(type=..., data=..., id=..., retry=...) or NULL
    event <- tryCatch(
      httr2::resp_stream_sse(resp),
      error = function(e) {
        consecutive_errors <<- consecutive_errors + 1
        if (consecutive_errors >= max_consecutive_errors) {
          rlang::abort(c(
            "Too many consecutive SSE parsing errors",
            "x" = conditionMessage(e)
          ), class = "aisdk_stream_error")
        }
        NULL
      }
    )

    if (!is.null(event)) {
      consecutive_errors <- 0  # Reset on successful event

      # Standard SSE handling
      # OpenAI and compatible APIs usually send JSON in the 'data' field
      if (!is.null(event$data) && nzchar(event$data)) {
        if (event$data == "[DONE]") {
          callback(NULL, done = TRUE)
          break
        }

        # Parse JSON data with repair fallback
        tryCatch({
          data <- jsonlite::fromJSON(event$data, simplifyVector = FALSE)
          # Pass parsed data to callback
          callback(data, done = FALSE)
        }, error = function(e) {
          # Try to repair JSON before giving up
          tryCatch({
            repaired_data <- fix_json(event$data)
            data <- jsonlite::fromJSON(repaired_data, simplifyVector = FALSE)
            callback(data, done = FALSE)
          }, error = function(e2) {
            # Log warning but don't crash - graceful degradation
            debug_opt <- getOption("aisdk.debug", FALSE)
            if (isTRUE(debug_opt)) {
              message("Warning: Malformed JSON in SSE data (skipping): ",
                      substr(event$data, 1, 100))
            }
          })
        })
      }
    }
  }
}
