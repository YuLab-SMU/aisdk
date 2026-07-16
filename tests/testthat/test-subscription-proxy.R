library(aisdk)

# Helper: run body with a clean default registry, restored afterwards.
with_clean_registry <- function(body) {
  registry_env <- get(".registry_env", envir = asNamespace("aisdk"))
  old_default <- registry_env$default %||% NULL
  on.exit(registry_env$default <- old_default, add = TRUE)
  registry_env$default <- NULL
  force(body)
}

test_that("use_codex_subscription registers a Chat Completions provider", {
  with_clean_registry({
    use_codex_subscription(name = "codex_test", check = FALSE, quiet = TRUE)
    model <- get_default_registry()$language_model("codex_test:gpt-5.4-codex")
    expect_s3_class(model, "OpenAILanguageModel")
    expect_equal(model$get_config()$base_url, "http://127.0.0.1:10531/v1")
    expect_equal(model$get_config()$api_key, "unused")
  })
})

test_that("use_claude_subscription registers an Anthropic Messages provider", {
  with_clean_registry({
    use_claude_subscription(name = "claude_test", check = FALSE, quiet = TRUE)
    model <- get_default_registry()$language_model("claude_test:claude-sonnet-4-6")
    expect_s3_class(model, "AnthropicLanguageModel")
    base_url <- model$get_config()$base_url
    expect_match(base_url, "/v1$")
    expect_equal(base_url, "http://127.0.0.1:3000/v1")
  })
})

test_that("explicit base_url and port override the default", {
  with_clean_registry({
    use_codex_subscription(
      name = "codex_url", base_url = "http://localhost:9999/v1",
      check = FALSE, quiet = TRUE
    )
    m1 <- get_default_registry()$language_model("codex_url:m")
    expect_equal(m1$get_config()$base_url, "http://localhost:9999/v1")

    use_codex_subscription(name = "codex_port", port = 12345L, check = FALSE, quiet = TRUE)
    m2 <- get_default_registry()$language_model("codex_port:m")
    expect_equal(m2$get_config()$base_url, "http://127.0.0.1:12345/v1")
  })
})

test_that("AISDK_CODEX_PROXY_URL env var overrides the default", {
  old <- Sys.getenv("AISDK_CODEX_PROXY_URL", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("AISDK_CODEX_PROXY_URL") else Sys.setenv(AISDK_CODEX_PROXY_URL = old)
  }, add = TRUE)
  Sys.setenv(AISDK_CODEX_PROXY_URL = "http://127.0.0.1:7000/v1")

  with_clean_registry({
    use_codex_subscription(name = "codex_env", check = FALSE, quiet = TRUE)
    m <- get_default_registry()$language_model("codex_env:m")
    expect_equal(m$get_config()$base_url, "http://127.0.0.1:7000/v1")
  })
})

test_that("explicit port beats the env var", {
  old <- Sys.getenv("AISDK_CODEX_PROXY_URL", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("AISDK_CODEX_PROXY_URL") else Sys.setenv(AISDK_CODEX_PROXY_URL = old)
  }, add = TRUE)
  Sys.setenv(AISDK_CODEX_PROXY_URL = "http://127.0.0.1:7000/v1")

  with_clean_registry({
    use_codex_subscription(name = "codex_pri", port = 8123L, check = FALSE, quiet = TRUE)
    m <- get_default_registry()$language_model("codex_pri:m")
    expect_equal(m$get_config()$base_url, "http://127.0.0.1:8123/v1")
  })
})

test_that("api_key passthrough overrides the placeholder", {
  with_clean_registry({
    use_codex_subscription(name = "codex_key", api_key = "sk-shared", check = FALSE, quiet = TRUE)
    m <- get_default_registry()$language_model("codex_key:m")
    expect_equal(m$get_config()$api_key, "sk-shared")
  })
})

test_that("set_as_default requires a model", {
  with_clean_registry({
    expect_error(
      use_codex_subscription(name = "codex_nd", set_as_default = TRUE, check = FALSE, quiet = TRUE),
      "requires a non-empty `model`"
    )
  })
})

test_that("subscription_proxy_status reports reachable + ready via probe", {
  testthat::local_mocked_bindings(
    probe_proxy_endpoint = function(url, timeout = 2) {
      list(reachable = TRUE, status = 200L, body = "{\"status\":\"ok\"}")
    },
    .package = "aisdk"
  )
  status <- subscription_proxy_status("claude")
  expect_equal(nrow(status), 1L)
  expect_true(status$reachable[[1]])
  expect_true(status$auth_ready[[1]])
  expect_match(status$hint[[1]], "credentials are loaded")
})

test_that("subscription_proxy_status flags an unreachable proxy with a start hint", {
  testthat::local_mocked_bindings(
    probe_proxy_endpoint = function(url, timeout = 2) {
      list(reachable = FALSE, status = NA_integer_, body = NULL)
    },
    .package = "aisdk"
  )
  status <- subscription_proxy_status("codex")
  expect_false(status$reachable[[1]])
  expect_false(status$auth_ready[[1]])
  expect_match(status$hint[[1]], "No proxy reachable")
})

test_that("subscription_proxy_status marks up-but-not-ready when /ready fails", {
  testthat::local_mocked_bindings(
    probe_proxy_endpoint = function(url, timeout = 2) {
      if (grepl("/ready$", url)) {
        list(reachable = TRUE, status = 503L, body = NULL)
      } else {
        list(reachable = TRUE, status = 200L, body = "{\"status\":\"ok\"}")
      }
    },
    .package = "aisdk"
  )
  status <- subscription_proxy_status("claude")
  expect_true(status$reachable[[1]])
  expect_false(status$auth_ready[[1]])
  expect_match(status$hint[[1]], "not ready")
})

test_that("detect_subscription_proxies returns one row per known proxy", {
  testthat::local_mocked_bindings(
    probe_proxy_endpoint = function(url, timeout = 2) {
      list(reachable = grepl(":10531", url), status = if (grepl(":10531", url)) 200L else NA_integer_, body = NULL)
    },
    .package = "aisdk"
  )
  detected <- detect_subscription_proxies()
  expect_setequal(detected$kind, c("codex", "claude"))
  expect_true(detected$running[detected$kind == "codex"])
  expect_false(detected$running[detected$kind == "claude"])
})

test_that("use_* warns (not errors) when the proxy is unreachable", {
  testthat::local_mocked_bindings(
    probe_proxy_endpoint = function(url, timeout = 2) {
      list(reachable = FALSE, status = NA_integer_, body = NULL)
    },
    .package = "aisdk"
  )
  with_clean_registry({
    expect_warning(
      use_codex_subscription(name = "codex_warn", check = TRUE, quiet = TRUE),
      "No proxy reachable"
    )
    # Provider still registered despite the warning.
    m <- get_default_registry()$language_model("codex_warn:m")
    expect_s3_class(m, "OpenAILanguageModel")
  })
})

test_that("live proxy end-to-end (opt-in)", {
  skip_if(Sys.getenv("AISDK_LIVE_PROXY_TEST") == "", "Set AISDK_LIVE_PROXY_TEST=1 with a running proxy")
  with_clean_registry({
    use_codex_subscription(check = FALSE, quiet = TRUE)
    status <- subscription_proxy_status("codex")
    expect_true(status$reachable[[1]])
    res <- generate_text(model = "codex:gpt-5.4-codex", prompt = "Say hi in one word.")
    expect_true(nchar(res$text) > 0)
  })
})
