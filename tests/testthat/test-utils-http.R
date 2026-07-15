# Tests for HTTP utility helpers
library(testthat)
library(aisdk)

test_that("resolve_request_timeout_seconds uses default when unset", {
  withr::local_options(list(aisdk.http_timeout_seconds = NULL))
  withr::local_envvar(c(AISDK_HTTP_TIMEOUT_SECONDS = ""))

  expect_null(aisdk:::resolve_request_timeout_seconds())
})

test_that("resolve_request_timeout_seconds prefers explicit argument", {
  withr::local_options(list(aisdk.http_timeout_seconds = 300))
  withr::local_envvar(c(AISDK_HTTP_TIMEOUT_SECONDS = "600"))

  expect_equal(aisdk:::resolve_request_timeout_seconds(42), 42)
})

test_that("resolve_request_timeout_seconds falls back to option then env", {
  withr::local_options(list(aisdk.http_timeout_seconds = 300))
  withr::local_envvar(c(AISDK_HTTP_TIMEOUT_SECONDS = "600"))
  expect_equal(aisdk:::resolve_request_timeout_seconds(), 300)

  withr::local_options(list(aisdk.http_timeout_seconds = NULL))
  withr::local_envvar(c(AISDK_HTTP_TIMEOUT_SECONDS = "600"))
  expect_equal(aisdk:::resolve_request_timeout_seconds(), 600)
})

test_that("resolve_request_timeout_seconds rejects invalid values", {
  expect_error(
    aisdk:::resolve_request_timeout_seconds(0),
    "`timeout_seconds` must be a single positive number."
  )
  expect_error(
    aisdk:::resolve_request_timeout_seconds(-5),
    "`timeout_seconds` must be a single positive number."
  )
})

test_that("resolve_request_timeout_config uses different defaults for request and stream", {
  withr::local_options(list(
    aisdk.http_timeout_seconds = NULL,
    aisdk.http_total_timeout_seconds = NULL,
    aisdk.http_first_byte_timeout_seconds = NULL,
    aisdk.http_connect_timeout_seconds = NULL,
    aisdk.http_idle_timeout_seconds = NULL
  ))
  withr::local_envvar(c(
    AISDK_HTTP_TIMEOUT_SECONDS = "",
    AISDK_HTTP_TOTAL_TIMEOUT_SECONDS = "",
    AISDK_HTTP_FIRST_BYTE_TIMEOUT_SECONDS = "",
    AISDK_HTTP_CONNECT_TIMEOUT_SECONDS = "",
    AISDK_HTTP_IDLE_TIMEOUT_SECONDS = ""
  ))

  request_cfg <- aisdk:::resolve_request_timeout_config(request_type = "request")
  stream_cfg <- aisdk:::resolve_request_timeout_config(request_type = "stream")

  expect_null(request_cfg$total_timeout_seconds)
  expect_equal(request_cfg$first_byte_timeout_seconds, 300)
  expect_equal(request_cfg$connect_timeout_seconds, 10)
  expect_equal(request_cfg$idle_timeout_seconds, 120)
  expect_null(stream_cfg$total_timeout_seconds)
  expect_equal(stream_cfg$first_byte_timeout_seconds, 300)
  expect_equal(stream_cfg$connect_timeout_seconds, 10)
  expect_equal(stream_cfg$idle_timeout_seconds, 120)
})

test_that("resolve_request_timeout_config prefers explicit layered settings over defaults", {
  cfg <- aisdk:::resolve_request_timeout_config(
    timeout_seconds = 30,
    total_timeout_seconds = 90,
    first_byte_timeout_seconds = 45,
    connect_timeout_seconds = 5,
    idle_timeout_seconds = 15,
    request_type = "stream"
  )

  expect_equal(cfg$total_timeout_seconds, 90)
  expect_equal(cfg$first_byte_timeout_seconds, 45)
  expect_equal(cfg$connect_timeout_seconds, 5)
  expect_equal(cfg$idle_timeout_seconds, 15)
})

test_that("apply_request_timeout_config skips unsupported curl options", {
  req <- httr2::request("https://example.com")
  cfg <- list(
    total_timeout_seconds = NULL,
    first_byte_timeout_seconds = 45,
    connect_timeout_seconds = 5,
    idle_timeout_seconds = 15
  )

  req <- testthat::with_mocked_bindings(
    curl_option_available = function(option_name) FALSE,
    aisdk:::apply_request_timeout_config(req, cfg),
    .package = "aisdk"
  )

  expect_equal(req$options$connecttimeout, 5L)
  expect_null(req$options$server_response_timeout)
  expect_equal(req$options$low_speed_limit, 1L)
  expect_equal(req$options$low_speed_time, 15L)
})

test_that("prepare_json_post_request sets an explicit POST method", {
  req <- httr2::request("https://example.com")
  req <- aisdk:::prepare_json_post_request(req, list(message = "hi"))

  expect_equal(req$method, "POST")
  expect_equal(req$body$type, "json")
})

test_that("prepare_multipart_post_request sets an explicit POST method", {
  req <- httr2::request("https://example.com")
  req <- aisdk:::prepare_multipart_post_request(req, list(field = "value"))

  expect_equal(req$method, "POST")
  expect_equal(req$body$type, "multipart")
})

test_that("http_error_classes detects compatibility errors", {
  classes <- aisdk:::http_error_classes(
    status = 400,
    error_body = '{"error":{"message":"Unknown parameter: response_format","type":"invalid_request_error","param":"response_format","code":"unknown_parameter"}}'
  )

  expect_true("aisdk_api_compatibility_error" %in% classes)
  expect_true("aisdk_api_error" %in% classes)
})

test_that("http_error_classes detects timeout errors", {
  classes <- aisdk:::http_error_classes(
    status = 504,
    error_body = '{"error":{"message":"Request timed out"}}'
  )

  expect_true("aisdk_api_timeout_error" %in% classes)
})

test_that("request_error_classes separates timeout from generic network failures", {
  timeout_err <- simpleError("Connection timed out after 30 seconds")
  network_err <- simpleError("Could not resolve host")

  expect_true("aisdk_api_timeout_error" %in% aisdk:::request_error_classes(timeout_err))
  expect_true("aisdk_api_network_error" %in% aisdk:::request_error_classes(network_err))
})

test_that("normalize_base_urls accepts comma and newline separated URLs", {
  urls <- aisdk:::normalize_base_urls("https://one.test/v1, https://two.test/v1\nhttps://one.test/v1/")

  expect_equal(urls, c("https://one.test/v1", "https://two.test/v1"))
})

test_that("post_to_api fails over to the next configured endpoint", {
  rm(list = ls(envir = aisdk:::.aisdk_api_route_state),
     envir = aisdk:::.aisdk_api_route_state)
  withr::defer(
    rm(list = ls(envir = aisdk:::.aisdk_api_route_state),
       envir = aisdk:::.aisdk_api_route_state)
  )

  attempts <- character()

  testthat::local_mocked_bindings(
    should_skip_internet_check = function() TRUE,
    perform_request = function(req) {
      attempts <<- c(attempts, req$url)
      if (grepl("primary", req$url, fixed = TRUE)) {
        stop(simpleError("Could not resolve host"))
      }
      httr2::response(
        status_code = 200L,
        url = req$url,
        method = req$method %||% "POST",
        body = charToRaw('{"ok":true}')
      )
    },
    .package = "aisdk"
  )

  result <- suppressMessages(
    aisdk:::post_to_api(
      url = c("https://primary.test/v1/chat/completions", "https://backup.test/v1/chat/completions"),
      headers = list(),
      body = list(model = "mock"),
      max_retries = 0,
      initial_delay_ms = 0
    )
  )

  expect_true(isTRUE(result$ok))
  expect_equal(attempts, c("https://primary.test/v1/chat/completions", "https://backup.test/v1/chat/completions"))
})

test_that("post_to_api fails over on retryable HTTP statuses", {
  rm(list = ls(envir = aisdk:::.aisdk_api_route_state),
     envir = aisdk:::.aisdk_api_route_state)
  withr::defer(
    rm(list = ls(envir = aisdk:::.aisdk_api_route_state),
       envir = aisdk:::.aisdk_api_route_state)
  )

  attempts <- character()

  testthat::local_mocked_bindings(
    should_skip_internet_check = function() TRUE,
    perform_request = function(req) {
      attempts <<- c(attempts, req$url)
      if (grepl("primary", req$url, fixed = TRUE)) {
        return(httr2::response(
          status_code = 429L,
          url = req$url,
          method = req$method %||% "POST",
          headers = list(`retry-after-ms` = "0"),
          body = charToRaw('{"error":{"message":"rate limited"}}')
        ))
      }
      httr2::response(
        status_code = 200L,
        url = req$url,
        method = req$method %||% "POST",
        body = charToRaw('{"ok":true}')
      )
    },
    .package = "aisdk"
  )

  result <- suppressMessages(
    aisdk:::post_to_api(
      url = c("https://primary.test/v1/chat/completions", "https://backup.test/v1/chat/completions"),
      headers = list(),
      body = list(model = "mock"),
      max_retries = 0,
      initial_delay_ms = 0
    )
  )

  expect_true(isTRUE(result$ok))
  expect_equal(attempts, c("https://primary.test/v1/chat/completions", "https://backup.test/v1/chat/completions"))
})

test_that("failover preserves retryable HTTP error classes when all endpoints fail", {
  rm(list = ls(envir = aisdk:::.aisdk_api_route_state),
     envir = aisdk:::.aisdk_api_route_state)
  withr::defer(
    rm(list = ls(envir = aisdk:::.aisdk_api_route_state),
       envir = aisdk:::.aisdk_api_route_state)
  )

  testthat::local_mocked_bindings(
    should_skip_internet_check = function() TRUE,
    perform_request = function(req) {
      httr2::response(
        status_code = 429L,
        url = req$url,
        method = req$method %||% "POST",
        headers = list(`retry-after-ms` = "0"),
        body = charToRaw('{"error":{"message":"rate limited"}}')
      )
    },
    .package = "aisdk"
  )

  expect_error(
    suppressMessages(
      aisdk:::post_to_api(
        url = c("https://primary.test/v1/chat/completions", "https://backup.test/v1/chat/completions"),
        headers = list(),
        body = list(model = "mock"),
        max_retries = 0,
        initial_delay_ms = 0
      )
    ),
    class = "aisdk_api_rate_limit_error"
  )
})

test_that("failed routes cool down and are de-prioritized", {
  rm(list = ls(envir = aisdk:::.aisdk_api_route_state),
     envir = aisdk:::.aisdk_api_route_state)
  withr::defer(
    rm(list = ls(envir = aisdk:::.aisdk_api_route_state),
       envir = aisdk:::.aisdk_api_route_state)
  )

  primary <- "https://primary.test/v1/chat/completions"
  backup <- "https://backup.test/v1/chat/completions"

  aisdk:::mark_api_route_failure(primary, "network error")
  expect_equal(aisdk:::order_api_url_candidates(c(primary, backup)), c(backup, primary))

  aisdk:::mark_api_route_success(primary)
  expect_equal(aisdk:::order_api_url_candidates(c(primary, backup)), c(primary, backup))
})

test_that("stream_from_api retries connection failures before any event is delivered", {
  attempts <- 0L
  chunks <- list()
  done_seen <- FALSE

  testthat::local_mocked_bindings(
    should_skip_internet_check = function() TRUE,
    stream_perform_connection = function(req) {
      attempts <<- attempts + 1L
      if (attempts == 1L) {
        stop(simpleError("Failed to perform HTTP request. Caused by error in `open.connection()`: cannot open the connection"))
      }
      resp <- new.env(parent = emptyenv())
      resp$events <- list(
        list(data = "{\"choices\":[{\"delta\":{\"content\":\"ok\"}}]}"),
        list(data = "[DONE]")
      )
      resp
    },
    stream_response_status = function(resp) 200L,
    stream_response_is_complete = function(resp) FALSE,
    stream_response_sse = function(resp) {
      event <- resp$events[[1]]
      resp$events <- resp$events[-1]
      event
    },
    .package = "aisdk"
  )

  suppressMessages(
    aisdk:::stream_from_api(
      url = "https://example.test/chat/completions",
      headers = list(),
      body = list(model = "mock", stream = TRUE),
      callback = function(data, done) {
        if (isTRUE(done)) {
          done_seen <<- TRUE
        } else {
          chunks <<- c(chunks, list(data))
        }
      },
      initial_delay_ms = 0
    )
  )

  expect_equal(attempts, 2L)
  expect_length(chunks, 1)
  expect_equal(chunks[[1]]$choices[[1]]$delta$content, "ok")
  expect_true(done_seen)
})

test_that("stream_from_api signals done=TRUE when the stream ends without [DONE]", {
  # Gemini and some OpenAI-compatible servers close the connection without a
  # "[DONE]" sentinel; done=TRUE must still fire exactly once.
  done_count <- 0L
  chunks <- list()

  testthat::local_mocked_bindings(
    should_skip_internet_check = function() TRUE,
    stream_perform_connection = function(req) {
      resp <- new.env(parent = emptyenv())
      resp$events <- list(list(data = "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"hi\"}]}}]}"))
      resp
    },
    stream_response_status = function(resp) 200L,
    # Complete once the single event has been consumed — no [DONE] is ever sent.
    stream_response_is_complete = function(resp) length(resp$events) == 0L,
    stream_response_sse = function(resp) {
      if (length(resp$events) == 0L) return(NULL)
      event <- resp$events[[1]]
      resp$events <- resp$events[-1]
      event
    },
    .package = "aisdk"
  )

  suppressMessages(
    aisdk:::stream_from_api(
      url = "https://example.test/v1beta/models/gemini:streamGenerateContent",
      headers = list(),
      body = list(model = "mock", stream = TRUE),
      callback = function(data, done) {
        if (isTRUE(done)) done_count <<- done_count + 1L else chunks <<- c(chunks, list(data))
      },
      initial_delay_ms = 0
    )
  )

  expect_length(chunks, 1)
  expect_equal(done_count, 1L) # exactly once — not zero (bug), not twice
})

test_that("stream_from_api emits done exactly once when [DONE] IS sent", {
  done_count <- 0L
  testthat::local_mocked_bindings(
    should_skip_internet_check = function() TRUE,
    stream_perform_connection = function(req) {
      resp <- new.env(parent = emptyenv())
      resp$events <- list(
        list(data = "{\"choices\":[{\"delta\":{\"content\":\"ok\"}}]}"),
        list(data = "[DONE]")
      )
      resp
    },
    stream_response_status = function(resp) 200L,
    stream_response_is_complete = function(resp) length(resp$events) == 0L,
    stream_response_sse = function(resp) {
      if (length(resp$events) == 0L) return(NULL)
      event <- resp$events[[1]]
      resp$events <- resp$events[-1]
      event
    },
    .package = "aisdk"
  )

  suppressMessages(
    aisdk:::stream_from_api(
      url = "https://example.test/chat/completions",
      headers = list(), body = list(model = "mock", stream = TRUE),
      callback = function(data, done) if (isTRUE(done)) done_count <<- done_count + 1L,
      initial_delay_ms = 0
    )
  )
  expect_equal(done_count, 1L)
})

test_that("http_error_classes keeps a permanent 4xx with a timeout-ish body non-retryable", {
  cls <- aisdk:::http_error_classes(
    400, '{"error":{"message":"Invalid value for \'timeout\'; the request timed out"}}'
  )
  expect_false("aisdk_api_timeout_error" %in% cls)
  # A real gateway timeout is still classified.
  expect_true("aisdk_api_timeout_error" %in% aisdk:::http_error_classes(504, "gateway timeout"))
})

test_that("stream_from_api fails over before the first event", {
  rm(list = ls(envir = aisdk:::.aisdk_api_route_state),
     envir = aisdk:::.aisdk_api_route_state)
  withr::defer(
    rm(list = ls(envir = aisdk:::.aisdk_api_route_state),
       envir = aisdk:::.aisdk_api_route_state)
  )

  attempts <- character()
  chunks <- list()
  done_seen <- FALSE

  testthat::local_mocked_bindings(
    should_skip_internet_check = function() TRUE,
    stream_perform_connection = function(req) {
      attempts <<- c(attempts, req$url)
      if (grepl("primary", req$url, fixed = TRUE)) {
        stop(simpleError("cannot open the connection"))
      }
      resp <- new.env(parent = emptyenv())
      resp$events <- list(
        list(data = "{\"choices\":[{\"delta\":{\"content\":\"ok\"}}]}"),
        list(data = "[DONE]")
      )
      resp
    },
    stream_response_status = function(resp) 200L,
    stream_response_is_complete = function(resp) FALSE,
    stream_response_sse = function(resp) {
      event <- resp$events[[1]]
      resp$events <- resp$events[-1]
      event
    },
    .package = "aisdk"
  )

  suppressMessages(
    aisdk:::stream_from_api(
      url = c("https://primary.test/v1/chat/completions", "https://backup.test/v1/chat/completions"),
      headers = list(),
      body = list(model = "mock", stream = TRUE),
      callback = function(data, done) {
        if (isTRUE(done)) {
          done_seen <<- TRUE
        } else {
          chunks <<- c(chunks, list(data))
        }
      },
      max_retries = 0,
      initial_delay_ms = 0
    )
  )

  expect_equal(attempts, c("https://primary.test/v1/chat/completions", "https://backup.test/v1/chat/completions"))
  expect_length(chunks, 1)
  expect_true(done_seen)
})

test_that("stream_from_api defaults to five retries before any event is delivered", {
  attempts <- 0L

  testthat::local_mocked_bindings(
    should_skip_internet_check = function() TRUE,
    stream_perform_connection = function(req) {
      attempts <<- attempts + 1L
      if (attempts <= 5L) {
        stop(simpleError("Failed to perform HTTP request. Caused by error in `open.connection()`: cannot open the connection"))
      }
      resp <- new.env(parent = emptyenv())
      resp$events <- list(list(data = "[DONE]"))
      resp
    },
    stream_response_status = function(resp) 200L,
    stream_response_is_complete = function(resp) FALSE,
    stream_response_sse = function(resp) {
      event <- resp$events[[1]]
      resp$events <- resp$events[-1]
      event
    },
    .package = "aisdk"
  )

  suppressMessages(
    aisdk:::stream_from_api(
      url = "https://example.test/chat/completions",
      headers = list(),
      body = list(model = "mock", stream = TRUE),
      callback = function(data, done) NULL,
      initial_delay_ms = 0
    )
  )

  expect_equal(attempts, 6L)
})

test_that("stream_from_api does not retry after a stream event is delivered", {
  attempts <- 0L
  chunks <- list()

  testthat::local_mocked_bindings(
    should_skip_internet_check = function() TRUE,
    stream_perform_connection = function(req) {
      attempts <<- attempts + 1L
      resp <- new.env(parent = emptyenv())
      resp$events <- list(
        list(data = "{\"choices\":[{\"delta\":{\"content\":\"partial\"}}]}"),
        simpleError("Connection reset by peer")
      )
      resp
    },
    stream_response_status = function(resp) 200L,
    stream_response_is_complete = function(resp) FALSE,
    stream_response_sse = function(resp) {
      event <- resp$events[[1]]
      resp$events <- resp$events[-1]
      if (inherits(event, "error")) {
        stop(event)
      }
      event
    },
    .package = "aisdk"
  )

  expect_error(
    aisdk:::stream_from_api(
      url = "https://example.test/chat/completions",
      headers = list(),
      body = list(model = "mock", stream = TRUE),
      callback = function(data, done) {
        if (!isTRUE(done)) {
          chunks <<- c(chunks, list(data))
        }
      },
      initial_delay_ms = 0
    ),
    class = "aisdk_stream_partial_error"
  )

  expect_equal(attempts, 1L)
  expect_length(chunks, 1)
  expect_equal(chunks[[1]]$choices[[1]]$delta$content, "partial")
})

# --- Issue 2: preflight is non-fatal by default ---------------------------

test_that("preflight_internet returns TRUE when has_internet() is TRUE", {
  testthat::local_mocked_bindings(
    has_internet = function() TRUE, .package = "curl"
  )
  withr::with_envvar(c(AISDK_SKIP_INTERNET_CHECK = NA, AISDK_PREFLIGHT_MODE = NA), {
    expect_true(aisdk:::preflight_internet("https://example.test"))
  })
})

test_that("preflight_internet warns once but returns TRUE when has_internet=FALSE (default mode)", {
  testthat::local_mocked_bindings(
    has_internet = function() FALSE, .package = "curl"
  )
  rm(list = ls(envir = aisdk:::.aisdk_preflight_warned),
     envir = aisdk:::.aisdk_preflight_warned)
  withr::with_envvar(c(AISDK_SKIP_INTERNET_CHECK = NA, AISDK_PREFLIGHT_MODE = NA), {
    expect_message(
      result <- aisdk:::preflight_internet("https://uncrawlable.test/v1/x"),
      "attempting request anyway"
    )
    expect_true(result)
    expect_silent(aisdk:::preflight_internet("https://uncrawlable.test/v1/x"))
  })
})

test_that("preflight_internet returns FALSE when AISDK_PREFLIGHT_MODE=abort and offline", {
  testthat::local_mocked_bindings(
    has_internet = function() FALSE, .package = "curl"
  )
  withr::with_envvar(
    c(AISDK_SKIP_INTERNET_CHECK = NA, AISDK_PREFLIGHT_MODE = "abort"),
    {
      expect_message(result <- aisdk:::preflight_internet("https://offline.test"),
                     "Internet connection is not available")
      expect_false(result)
    }
  )
})

test_that("preflight_internet honors AISDK_SKIP_INTERNET_CHECK=1", {
  testthat::local_mocked_bindings(
    has_internet = function() FALSE, .package = "curl"
  )
  withr::with_envvar(c(AISDK_SKIP_INTERNET_CHECK = "1"), {
    expect_silent(result <- aisdk:::preflight_internet("https://skipped.test"))
    expect_true(result)
  })
})

# --- Z7: robust, capped Retry-After parsing ----------------------------------

test_that("resolve_retry_after_ms handles seconds, ms, HTTP-date, garbage, and caps", {
  f <- aisdk:::resolve_retry_after_ms
  expect_equal(f("2", NULL, fallback_ms = 1000), 2000)
  expect_equal(f(NULL, "500", fallback_ms = 1000), 500)
  # HTTP-date form must not become NA (which caused Sys.sleep(NA) to mask the 429).
  future <- format(Sys.time() + 30, "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  d <- f(future, NULL, fallback_ms = 1000)
  expect_false(is.na(d))
  expect_true(d > 20000 && d <= 60000)
  # Unparseable -> the caller's backoff delay.
  expect_equal(f("soon", NULL, fallback_ms = 1234), 1234)
  # A hostile huge value is capped.
  expect_equal(f("999999", NULL, fallback_ms = 1000), 60000)
  # No headers -> fallback.
  expect_equal(f(NULL, NULL, fallback_ms = 2000), 2000)
  # Configurable cap.
  withr::local_options(aisdk.max_retry_after_ms = 5000)
  expect_equal(f("100", NULL, fallback_ms = 1000), 5000)
})

# --- AC1: idempotency key on retried POSTs -----------------------------------

test_that("generate_idempotency_key is unique and doesn't perturb the RNG stream", {
  set.seed(42); before <- runif(1)
  k1 <- aisdk:::generate_idempotency_key()
  k2 <- aisdk:::generate_idempotency_key()
  set.seed(42); after <- runif(1)
  expect_identical(before, after)     # a package must not consume the user's RNG
  expect_false(identical(k1, k2))
  expect_match(k1, "^aisdk-")
})

test_that("apply_idempotency_key adds, respects existing, and is configurable", {
  h <- aisdk:::apply_idempotency_key(list(`Content-Type` = "application/json"))
  expect_false(is.null(h[["Idempotency-Key"]]))

  # A caller/provider-supplied key wins (case-insensitive), with no duplicate.
  h2 <- aisdk:::apply_idempotency_key(list(`idempotency-key` = "mine"))
  expect_equal(h2[["idempotency-key"]], "mine")
  expect_null(h2[["Idempotency-Key"]])

  withr::with_options(list(aisdk.idempotency_key = FALSE), {
    expect_null(aisdk:::apply_idempotency_key(list(a = 1))[["Idempotency-Key"]])
  })
  withr::with_options(list(aisdk.idempotency_header = "X-Idem"), {
    expect_false(is.null(aisdk:::apply_idempotency_key(list(a = 1))[["X-Idem"]]))
  })
})

test_that("post_to_api sends ONE stable idempotency key across retries", {
  keys <- character()
  testthat::local_mocked_bindings(
    should_skip_internet_check = function() TRUE,
    perform_request = function(req) {
      keys <<- c(keys, req$headers[["Idempotency-Key"]] %||% NA_character_)
      if (length(keys) == 1L) {
        return(httr2::response(
          status_code = 503L, url = req$url, method = req$method %||% "POST",
          body = charToRaw('{"error":"transient"}')
        ))
      }
      httr2::response(status_code = 200L, url = req$url,
                      method = req$method %||% "POST", body = charToRaw('{"ok":true}'))
    },
    .package = "aisdk"
  )
  result <- suppressMessages(aisdk:::post_to_api(
    url = "https://api.test/v1/chat/completions", headers = list(),
    body = list(model = "mock"), max_retries = 2, initial_delay_ms = 0
  ))
  expect_true(isTRUE(result$ok))
  expect_length(keys, 2)               # one 503 then a 200
  expect_false(anyNA(keys))            # both attempts carried a key
  expect_equal(keys[[1]], keys[[2]])   # the SAME key — this is the whole point
})

test_that("failover candidates share one idempotency key", {
  rm(list = ls(envir = aisdk:::.aisdk_api_route_state), envir = aisdk:::.aisdk_api_route_state)
  withr::defer(rm(list = ls(envir = aisdk:::.aisdk_api_route_state),
                  envir = aisdk:::.aisdk_api_route_state))
  keys <- character()
  testthat::local_mocked_bindings(
    should_skip_internet_check = function() TRUE,
    perform_request = function(req) {
      keys <<- c(keys, req$headers[["Idempotency-Key"]] %||% NA_character_)
      if (grepl("primary", req$url, fixed = TRUE)) stop(simpleError("Could not resolve host"))
      httr2::response(status_code = 200L, url = req$url,
                      method = req$method %||% "POST", body = charToRaw('{"ok":true}'))
    },
    .package = "aisdk"
  )
  result <- suppressMessages(aisdk:::post_to_api(
    url = c("https://primary.test/v1/x", "https://backup.test/v1/x"),
    headers = list(), body = list(model = "mock"), max_retries = 0, initial_delay_ms = 0
  ))
  expect_true(isTRUE(result$ok))
  expect_length(keys, 2)
  expect_equal(keys[[1]], keys[[2]])   # same key across primary + backup
})

# --- AG1: rate-limit header surfacing ----------------------------------------

test_that("post_to_api captures OpenAI and Anthropic rate-limit headers", {
  # Isolate the module state across this test.
  withr::defer(rm(list = ls(envir = aisdk:::.aisdk_rate_limit_state),
                  envir = aisdk:::.aisdk_rate_limit_state))
  rm(list = ls(envir = aisdk:::.aisdk_rate_limit_state), envir = aisdk:::.aisdk_rate_limit_state)
  expect_null(rate_limit_status())

  mk <- function(hdrs) function(req) {
    httr2::response(status_code = 200L, url = req$url, method = "POST",
                    headers = hdrs, body = charToRaw('{"ok":true}'))
  }

  testthat::local_mocked_bindings(
    should_skip_internet_check = function() TRUE,
    perform_request = mk(list(
      `x-ratelimit-remaining-requests` = "4999",
      `x-ratelimit-remaining-tokens` = "1990000",
      `x-ratelimit-limit-requests` = "5000",
      `x-ratelimit-reset-tokens` = "6ms"
    )),
    .package = "aisdk"
  )
  suppressMessages(aisdk:::post_to_api("https://api.openai.test/v1/x", list(),
                                       list(m = "x"), max_retries = 0))
  s <- rate_limit_status()
  expect_equal(s$remaining_requests, 4999)
  expect_equal(s$remaining_tokens, 1990000)
  expect_equal(s$limit_requests, 5000)
  expect_equal(s$reset_tokens, "6ms")
})

test_that("Anthropic header family maps to the same normalized fields", {
  withr::defer(rm(list = ls(envir = aisdk:::.aisdk_rate_limit_state),
                  envir = aisdk:::.aisdk_rate_limit_state))
  rm(list = ls(envir = aisdk:::.aisdk_rate_limit_state), envir = aisdk:::.aisdk_rate_limit_state)

  mk <- function(hdrs) function(req) {
    httr2::response(status_code = 200L, url = req$url, method = "POST",
                    headers = hdrs, body = charToRaw('{"ok":true}'))
  }
  testthat::local_mocked_bindings(
    should_skip_internet_check = function() TRUE,
    perform_request = mk(list(
      `anthropic-ratelimit-requests-remaining` = "49",
      `anthropic-ratelimit-tokens-remaining` = "39000",
      `anthropic-ratelimit-tokens-limit` = "40000"
    )),
    .package = "aisdk"
  )
  suppressMessages(aisdk:::post_to_api("https://api.anthropic.test/v1/x", list(),
                                       list(m = "x"), max_retries = 0))
  s <- rate_limit_status()
  expect_equal(s$remaining_requests, 49)
  expect_equal(s$remaining_tokens, 39000)
  expect_equal(s$limit_tokens, 40000)
})

test_that("a response without rate-limit headers does not erase the last snapshot", {
  withr::defer(rm(list = ls(envir = aisdk:::.aisdk_rate_limit_state),
                  envir = aisdk:::.aisdk_rate_limit_state))
  rm(list = ls(envir = aisdk:::.aisdk_rate_limit_state), envir = aisdk:::.aisdk_rate_limit_state)

  mk <- function(hdrs) function(req) {
    httr2::response(status_code = 200L, url = req$url, method = "POST",
                    headers = hdrs, body = charToRaw('{"ok":true}'))
  }
  testthat::local_mocked_bindings(
    should_skip_internet_check = function() TRUE,
    perform_request = mk(list(`x-ratelimit-remaining-tokens` = "500")),
    .package = "aisdk"
  )
  suppressMessages(aisdk:::post_to_api("https://a.test/x", list(), list(m = "x"), max_retries = 0))
  expect_equal(rate_limit_status()$remaining_tokens, 500)

  # A Gemini-style header-less 200 must leave the good snapshot intact.
  testthat::local_mocked_bindings(
    should_skip_internet_check = function() TRUE,
    perform_request = mk(list()),
    .package = "aisdk"
  )
  suppressMessages(aisdk:::post_to_api("https://gemini.test/x", list(), list(m = "x"), max_retries = 0))
  expect_equal(rate_limit_status()$remaining_tokens, 500)
})

# --- AL1: exponential backoff jitter -----------------------------------------

test_that("apply_backoff_jitter spreads delays additively without undershooting", {
  vals <- numeric(40)
  for (i in seq_along(vals)) { vals[i] <- aisdk:::apply_backoff_jitter(1000); Sys.sleep(0.001) }
  # Additive jitter: never below the base (so a Retry-After minimum is respected)
  # and never above base*(1+frac); default frac = 0.5.
  expect_true(all(vals >= 1000))
  expect_true(all(vals <= 1500))
  # It actually varies (would be pointless otherwise).
  expect_gt(length(unique(round(vals))), 5)
})

test_that("apply_backoff_jitter does not perturb the caller's RNG stream", {
  set.seed(99); a <- runif(3)
  invisible(replicate(10, aisdk:::apply_backoff_jitter(2000)))
  set.seed(99); b <- runif(3)
  expect_identical(a, b)
})

test_that("apply_backoff_jitter is tunable and disablable, and ignores bad delays", {
  withr::with_options(list(aisdk.retry_jitter = 0), {
    expect_equal(aisdk:::apply_backoff_jitter(1000), 1000)
  })
  withr::with_options(list(aisdk.retry_jitter = 0.25), {
    v <- replicate(20, { d <- aisdk:::apply_backoff_jitter(1000); Sys.sleep(2e-4); d })
    expect_true(all(v >= 1000 & v <= 1250))
  })
  expect_true(is.na(aisdk:::apply_backoff_jitter(NA_real_)))
  expect_equal(aisdk:::apply_backoff_jitter(0), 0)
})

# --- AM1: structured API error surfacing -------------------------------------

test_that("abort_http_api_error surfaces OpenAI error fields and a clean message", {
  e <- tryCatch(
    aisdk:::abort_http_api_error(
      400, "https://x.test",
      '{"error":{"message":"Unknown parameter: response_format","type":"invalid_request_error","param":"response_format","code":"unknown_parameter"}}'
    ),
    error = function(e) e
  )
  msg <- conditionMessage(e)
  expect_match(msg, "status 400 (invalid_request_error)", fixed = TRUE)
  expect_match(msg, "Unknown parameter: response_format", fixed = TRUE)
  expect_match(msg, "[param: response_format]", fixed = TRUE)
  # Programmatically inspectable condition fields.
  expect_equal(e$status, 400)
  expect_equal(e$error_type, "invalid_request_error")
  expect_equal(e$error_code, "unknown_parameter")
  expect_equal(e$error_param, "response_format")
  expect_equal(e$error_body, '{"error":{"message":"Unknown parameter: response_format","type":"invalid_request_error","param":"response_format","code":"unknown_parameter"}}')
  # Classification still works.
  expect_s3_class(e, "aisdk_api_compatibility_error")
})

test_that("abort_http_api_error handles Anthropic and Gemini error shapes", {
  a <- tryCatch(
    aisdk:::abort_http_api_error(401, "https://x.test",
      '{"type":"error","error":{"type":"authentication_error","message":"invalid x-api-key"}}'),
    error = function(e) e
  )
  expect_equal(a$error_type, "authentication_error")
  expect_null(a$error_code)                       # Anthropic has no code
  expect_match(conditionMessage(a), "invalid x-api-key", fixed = TRUE)

  g <- tryCatch(
    aisdk:::abort_http_api_error(429, "https://x.test",
      '{"error":{"code":429,"message":"Resource exhausted","status":"RESOURCE_EXHAUSTED"}}'),
    error = function(e) e
  )
  expect_equal(g$error_type, "RESOURCE_EXHAUSTED")  # Gemini's `status` maps to type
  expect_equal(g$error_code, 429)
})

test_that("abort_http_api_error falls back gracefully on a non-JSON body", {
  e <- tryCatch(aisdk:::abort_http_api_error(500, "https://x.test", "upstream timeout"),
                error = function(e) e)
  expect_match(conditionMessage(e), "status 500", fixed = TRUE)
  expect_match(conditionMessage(e), "upstream timeout", fixed = TRUE)
  expect_equal(e$error_body, "upstream timeout")
  expect_null(e$error_type)
})
