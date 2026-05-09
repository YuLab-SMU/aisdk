# Tests for Moonshot / Kimi Provider
library(testthat)
library(aisdk)

helper_path <- file.path(test_path("helper-env.R"))
source(helper_path)

test_that("create_moonshot() creates Open Platform provider by default", {
  provider <- safe_create_provider(create_moonshot, api_key = "sk-test")
  model <- provider$language_model()

  expect_s3_class(provider, "MoonshotProvider")
  expect_s3_class(model, "MoonshotLanguageModel")
  expect_equal(model$provider, "moonshot")
  expect_equal(model$model_id, "kimi-k2.6")
  expect_equal(model$get_config()$base_url, "https://api.moonshot.cn/v1")
})

test_that("create_kimi_code() defaults to Anthropic-compatible Kimi Code", {
  provider <- safe_create_provider(create_kimi_code, api_key = "sk-kimi-test")
  model <- provider$language_model()

  expect_s3_class(provider, "AnthropicProvider")
  expect_s3_class(model, "AnthropicLanguageModel")
  expect_equal(model$provider, "kimi")
  expect_equal(model$model_id, "kimi-for-coding")
  expect_equal(model$get_config()$base_url, "https://api.kimi.com/coding/v1")
})

test_that("create_kimi_code(api_format = 'openai') configures OpenAI-compatible Kimi Code", {
  provider <- safe_create_provider(create_kimi_code, api_key = "sk-kimi-test", api_format = "openai")
  model <- provider$language_model()

  expect_s3_class(provider, "MoonshotProvider")
  expect_s3_class(model, "MoonshotLanguageModel")
  expect_equal(model$provider, "kimi")
  expect_equal(model$model_id, "kimi-for-coding")
  expect_equal(model$get_config()$base_url, "https://api.kimi.com/coding/v1")
  expect_match(model$get_config()$headers[["User-Agent"]], "^aisdk/")
})

test_that("create_moonshot() infers Kimi Code platform from base_url", {
  provider <- safe_create_provider(
    create_moonshot,
    api_key = "sk-kimi-test",
    base_url = "https://api.kimi.com/coding/v1"
  )
  model <- provider$language_model()

  expect_equal(model$provider, "kimi")
  expect_equal(model$model_id, "kimi-for-coding")
})

test_that("Moonshot provider warns when platform key and base_url are mixed", {
  expect_warning(
    create_moonshot(
      api_key = "sk-kimi-test",
      base_url = "https://api.moonshot.cn/v1"
    ),
    "separate keys"
  )
})

test_that("Moonshot payload uses Kimi-compatible parameters", {
  provider <- safe_create_provider(create_moonshot, api_key = "sk-test")
  model <- provider$language_model("kimi-k2.6")

  payload <- model$build_payload(list(
    messages = list(list(role = "user", content = "Hello")),
    temperature = 0.7,
    max_tokens = 100,
    thinking = FALSE
  ))

  expect_equal(payload$url, "https://api.moonshot.cn/v1/chat/completions")
  expect_equal(payload$body$temperature, 1)
  expect_null(payload$body$max_tokens)
  expect_equal(payload$body$max_completion_tokens, 100)
  expect_equal(payload$body$thinking, list(type = "disabled"))
})

test_that("Kimi Code payload includes prompt_cache_key", {
  provider <- safe_create_provider(
    create_kimi_code,
    api_key = "sk-kimi-test",
    api_format = "openai",
    prompt_cache_key = "task-123"
  )
  model <- provider$language_model()

  payload <- model$build_payload(list(
    messages = list(list(role = "user", content = "Hello")),
    temperature = 0.7,
    max_tokens = 100
  ))

  expect_equal(payload$url, "https://api.kimi.com/coding/v1/chat/completions")
  expect_equal(payload$body$model, "kimi-for-coding")
  expect_equal(payload$body$temperature, 1)
  expect_equal(payload$body$prompt_cache_key, "task-123")
  expect_equal(payload$body$max_completion_tokens, 100)
})

test_that("create_kimi_code_anthropic() normalizes base_url for aisdk Anthropic provider", {
  provider <- safe_create_provider(create_kimi_code_anthropic, api_key = "sk-kimi-test")
  model <- provider$language_model("kimi-for-coding")

  captured_url <- NULL
  local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_url <<- url
      list(
        content = list(list(type = "text", text = "ok")),
        stop_reason = "end_turn",
        usage = list(input_tokens = 1, output_tokens = 1)
      )
    },
    .package = "aisdk"
  )

  model$do_generate(list(
    messages = list(list(role = "user", content = "Hello")),
    max_tokens = 10
  ))

  expect_equal(captured_url, "https://api.kimi.com/coding/v1/messages")
})

test_that("Default registry resolves moonshot and kimi providers", {
  registry <- get_default_registry()

  moonshot_model <- registry$language_model("moonshot:kimi-k2.6")
  kimi_model <- registry$language_model("kimi:kimi-for-coding")

  expect_s3_class(moonshot_model, "MoonshotLanguageModel")
  expect_equal(moonshot_model$provider, "moonshot")
  expect_s3_class(kimi_model, "AnthropicLanguageModel")
  expect_equal(kimi_model$provider, "kimi")
})

test_that("Moonshot provider can make a real Open Platform API call", {
  skip_if_no_api_key("Moonshot")
  skip_on_cran()

  provider <- create_moonshot()
  model <- provider$language_model(Sys.getenv("MOONSHOT_MODEL", "kimi-k2.6"))

  result <- generate_text(
    model,
    "Reply exactly with OK.",
    temperature = 1,
    max_tokens = 10
  )

  expect_true(!is.null(result$text))
  expect_true(nchar(result$text) > 0)
})

test_that("Kimi Code provider can make a real API call", {
  skip_if_no_api_key("kimi")
  skip_on_cran()

  provider <- create_kimi_code()
  model <- provider$language_model()

  result <- generate_text(
    model,
    "Reply exactly with OK.",
    max_tokens = 10
  )

  expect_true(!is.null(result$text))
  expect_true(nchar(result$text) > 0)
})
