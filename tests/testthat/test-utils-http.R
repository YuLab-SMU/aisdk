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
