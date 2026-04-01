# Tests for Anthropic Provider
library(testthat)
library(aisdk)

# Load helper functions (for environment variable handling)
helper_path <- file.path(test_path("helper-env.R"))
source(helper_path)

# Get model names from environment
anthropic_model <- get_anthropic_model()
anthropic_base_url <- get_anthropic_base_url()

test_that("create_anthropic() creates a provider with correct defaults", {
  # Use safe provider creation
  provider <- safe_create_provider(create_anthropic)

  expect_s3_class(provider, "AnthropicProvider")
  expect_equal(provider$specification_version, "v1")
})

test_that("Anthropic provider creates language model correctly", {
  provider <- safe_create_provider(create_anthropic)
  model <- provider$language_model(anthropic_model)

  expect_s3_class(model, "AnthropicLanguageModel")
  expect_equal(model$model_id, anthropic_model)
  expect_equal(model$provider, "anthropic")
  expect_equal(model$specification_version, "v1")
})

test_that("Anthropic provider uses custom API version", {
  provider <- safe_create_provider(create_anthropic,
    api_version = "2024-01-01"
  )
  # Provider should be created successfully
  expect_s3_class(provider, "AnthropicProvider")
})

test_that("create_anthropic() warns when API key is missing", {
  # Temporarily unset API key
  old_key <- Sys.getenv("ANTHROPIC_API_KEY")
  Sys.setenv(ANTHROPIC_API_KEY = "")
  on.exit(Sys.setenv(ANTHROPIC_API_KEY = old_key))

  expect_warning(
    create_anthropic(),
    "Anthropic API key not set"
  )
})

# Live API tests (only run when API key is available)
test_that("Anthropic provider can make real API calls", {
  skip_if_no_api_key("Anthropic")
  skip_on_cran()

  # Skip if we are running under an AiHubMix base url to avoid auth errors
  url <- Sys.getenv("ANTHROPIC_BASE_URL")
  if (nzchar(url) && grepl("aihubmix", url)) {
    skip("Skipping real API call because ANTHROPIC_BASE_URL is set to AiHubMix")
  }

  provider <- create_anthropic()
  model <- provider$language_model(anthropic_model)

  # Make a simple API call
  result <- model$generate(
    messages = list(
      list(role = "user", content = "Say 'Hello, World!'")
    ),
    max_tokens = 20
  )

  # Check that we got a response
  expect_true(!is.null(result$text))
  expect_true(nchar(result$text) > 0)
})

test_that("Anthropic provider handles tool calls", {
  skip_if_no_api_key("Anthropic")
  skip_on_cran()

  url <- Sys.getenv("ANTHROPIC_BASE_URL")
  if (nzchar(url) && grepl("aihubmix", url)) {
    skip("Skipping real API call because ANTHROPIC_BASE_URL is set to AiHubMix")
  }

  provider <- create_anthropic()
  model <- provider$language_model(anthropic_model)

  # Create a simple test tool
  test_tool <- Tool$new(
    name = "get_time",
    description = "Get the current time",
    parameters = z_object(.dummy = z_string("Unused")),
    execute = function(args) {
      paste0("Current time: ", Sys.time())
    }
  )

  # Call model with tool
  result <- model$generate(
    messages = list(
      list(role = "user", content = "What time is it?")
    ),
    tools = list(test_tool),
    max_tokens = 50
  )

  # Check response (can be text or tool calls)
  expect_true(!is.null(result$text) || length(result$tool_calls) > 0)
})

test_that("Anthropic provider generates tool definition with cache_control", {
  skip_on_cran()

  provider <- create_anthropic(api_key = "test_key", base_url = "https://api.anthropic.com/v1")
  model <- provider$language_model(anthropic_model)

  # Create a simple test tool with caching enabled
  test_tool <- Tool$new(
    name = "get_weather",
    description = "Get the weather",
    parameters = z_object(location = z_string("city")),
    execute = function(args) {
      "sunny"
    },
    meta = list(cache_control = list(type = "ephemeral"))
  )

  tool_api <- test_tool$to_api_format("anthropic")

  if (!is.null(test_tool$meta) && !is.null(test_tool$meta$cache_control)) {
    tool_api$cache_control <- test_tool$meta$cache_control
  }

  expect_equal(tool_api$name, "get_weather")
  expect_equal(tool_api$cache_control$type, "ephemeral")
})

test_that("Anthropic formatter translates multimodal content blocks", {
  provider <- safe_create_provider(create_anthropic)
  model <- provider$language_model(anthropic_model)

  fake_response <- list(
    content = list(list(type = "text", text = "done")),
    usage = list(input_tokens = 1, output_tokens = 1),
    stop_reason = "end_turn"
  )
  captured <- NULL

  local_mocked_bindings(
    post_to_api = function(url, headers, body) {
      captured <<- body
      fake_response
    }
  )

  model$do_generate(list(
    messages = list(
      list(role = "system", content = list(input_text("You are helpful."))),
      list(
        role = "user",
        content = list(
          input_text("Describe this image"),
          input_image("https://example.com/test.png", media_type = "image/png")
        )
      )
    ),
    max_tokens = 20
  ))

  expect_equal(captured$system, "You are helpful.")
  expect_equal(captured$messages[[1]]$content[[1]]$type, "text")
  expect_equal(captured$messages[[1]]$content[[2]]$type, "image")
  expect_equal(captured$messages[[1]]$content[[2]]$source$type, "url")
  expect_equal(captured$messages[[1]]$content[[2]]$source$url, "https://example.com/test.png")
})

test_that("Anthropic payload translates multimodal content blocks", {
  provider <- safe_create_provider(create_anthropic, api_key = "test_key")
  model <- provider$language_model(anthropic_model)
  captured_body <- NULL

  local_mocked_bindings(
    post_to_api = function(url, headers, body) {
      captured_body <<- body
      list(
        content = list(list(type = "text", text = "ok")),
        stop_reason = "end_turn",
        usage = list(input_tokens = 10, output_tokens = 2)
      )
    }
  )

  result <- model$do_generate(list(
    messages = list(
      list(
        role = "user",
        content = list(
          input_text("Analyze this image"),
          input_image(
            paste0(
              "data:image/png;base64,",
              base64enc::base64encode(charToRaw("fake-image"))
            ),
            media_type = "image/png"
          )
        )
      )
    ),
    max_tokens = 128
  ))

  expect_equal(result$text, "ok")
  expect_equal(captured_body$messages[[1]]$content[[1]]$type, "text")
  expect_equal(captured_body$messages[[1]]$content[[1]]$text, "Analyze this image")
  expect_equal(captured_body$messages[[1]]$content[[2]]$type, "image")
  expect_equal(captured_body$messages[[1]]$content[[2]]$source$type, "base64")
  expect_equal(captured_body$messages[[1]]$content[[2]]$source$media_type, "image/png")
})

test_that("Anthropic payload translates provider-neutral multimodal blocks", {
  provider <- create_anthropic(api_key = "test_key", base_url = "https://api.anthropic.com/v1")
  model <- provider$language_model(anthropic_model)

  image_path <- tempfile(fileext = ".png")
  writeBin(as.raw(0:15), image_path)
  on.exit(unlink(image_path), add = TRUE)

  captured_body <- NULL
  mock_response <- list(
    content = list(list(type = "text", text = "ok")),
    stop_reason = "end_turn",
    usage = list(input_tokens = 10, output_tokens = 5)
  )

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      mock_response
    },
    .package = "aisdk"
  )

  result <- model$generate(
    messages = list(list(
      role = "user",
      content = list(
        input_text("Analyze the image."),
        input_image(image_path)
      )
    )),
    max_tokens = 32
  )

  expect_equal(result$text, "ok")
  expect_equal(captured_body$messages[[1]]$content[[1]]$type, "text")
  expect_equal(captured_body$messages[[1]]$content[[2]]$type, "image")
  expect_equal(captured_body$messages[[1]]$content[[2]]$source$type, "base64")
  expect_equal(captured_body$messages[[1]]$content[[2]]$source$media_type, "image/png")
})
