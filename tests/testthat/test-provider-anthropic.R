# Tests for Anthropic Provider
library(testthat)
library(aisdk)

# Load helper functions (for environment variable handling)
helper_path = file.path(test_path("helper-env.R"))
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
  
  provider <- create_anthropic()
  model <- provider$language_model(anthropic_model)
  
  # Make a simple API call
  result <- model$generate(
    messages = list(
      list(role = "user", content = "Say 'Hello, World!'")
    ),
    max_tokens = 10
  )
  
  # Check that we got a response
  expect_true(!is.null(result$text))
  expect_true(nchar(result$text) > 0)
})

test_that("Anthropic provider handles tool calls", {
  skip_if_no_api_key("Anthropic")
  skip_on_cran()
  
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
  
  # Check response
  expect_true(!is.null(result$text))
})
