# Tests for OpenAI Provider
library(testthat)
library(aisdk)

# Load helper functions (for environment variable handling)
helper_path = file.path(test_path("helper-env.R"))
source(helper_path)
       
openai_model <- get_openai_model()
openai_embedding_model <- get_openai_embedding_model()
openai_base_url <- get_openai_base_url()

test_that("create_openai() creates a provider with correct defaults", {
  # Use safe provider creation
  provider <- safe_create_provider(create_openai)
  
  expect_s3_class(provider, "OpenAIProvider")
  expect_equal(provider$specification_version, "v1")
})

test_that("OpenAI provider creates language model correctly", {
  provider <- safe_create_provider(create_openai)
  model <- provider$language_model(openai_model)
  
  expect_s3_class(model, "OpenAILanguageModel")
  expect_equal(model$model_id, openai_model)
  expect_equal(model$provider, "openai")
  expect_equal(model$specification_version, "v1")
})

test_that("OpenAI provider creates embedding model correctly", {
  provider <- safe_create_provider(create_openai)
  model <- provider$embedding_model(openai_embedding_model)
  
  expect_s3_class(model, "OpenAIEmbeddingModel")
  expect_equal(model$model_id, openai_embedding_model)
  expect_equal(model$provider, "openai")
})

test_that("create_openai() accepts custom base_url", {
  provider <- safe_create_provider(create_openai,
    base_url = openai_base_url
  )
  model <- provider$language_model(openai_model)
  
  # Model should be created successfully
  expect_s3_class(model, "OpenAILanguageModel")
})

test_that("create_openai() warns when API key is missing", {
  # Temporarily unset API key
  old_key <- Sys.getenv("OPENAI_API_KEY")
  Sys.setenv(OPENAI_API_KEY = "")
  on.exit(Sys.setenv(OPENAI_API_KEY = old_key))
  
  expect_warning(
    create_openai(),
    "OpenAI API key not set"
  )
})

# Live API tests (only run when API key is available)
test_that("OpenAI provider can make real API calls", {
  skip_if_no_api_key("OpenAI")
  skip_on_cran()
  
  provider <- create_openai()
  model <- provider$language_model(openai_model)
  
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

test_that("OpenAI provider handles tool calls", {
  skip_if_no_api_key("OpenAI")
  skip_on_cran()

  provider <- create_openai()
  model <- provider$language_model(openai_model)

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

# ============================================================================
# Responses API Tests
# ============================================================================

test_that("OpenAI provider creates responses model correctly", {
  provider <- safe_create_provider(create_openai)
  model <- provider$responses_model("o1")

  expect_s3_class(model, "OpenAIResponsesLanguageModel")
  expect_equal(model$model_id, "o1")
  expect_equal(model$provider, "openai")
  expect_equal(model$specification_version, "v1")
})

test_that("OpenAI responses model has reset method", {
  provider <- safe_create_provider(create_openai)
  model <- provider$responses_model("o1")

  # Should have reset method

  expect_true("reset" %in% names(model))

  # Reset should return self for chaining
  result <- model$reset()
  expect_identical(result, model)
})

test_that("OpenAI responses model tracks response ID", {
  provider <- safe_create_provider(create_openai)
  model <- provider$responses_model("o1")

  # Initially no response ID
  expect_null(model$get_last_response_id())
})

test_that("smart_model selects correct API for reasoning models", {
  provider <- safe_create_provider(create_openai)

  # Reasoning models should use Responses API
  model_o1 <- provider$smart_model("o1")
  expect_s3_class(model_o1, "OpenAIResponsesLanguageModel")

  model_o3 <- provider$smart_model("o3-mini")
  expect_s3_class(model_o3, "OpenAIResponsesLanguageModel")

  model_o1_mini <- provider$smart_model("o1-mini")
  expect_s3_class(model_o1_mini, "OpenAIResponsesLanguageModel")
})

test_that("smart_model selects correct API for chat models", {
  provider <- safe_create_provider(create_openai)

  # Chat models should use Chat Completions API
  model_gpt4o <- provider$smart_model("gpt-4o")
  expect_s3_class(model_gpt4o, "OpenAILanguageModel")

  model_gpt4 <- provider$smart_model("gpt-4")
  expect_s3_class(model_gpt4, "OpenAILanguageModel")

  model_gpt35 <- provider$smart_model("gpt-3.5-turbo")
  expect_s3_class(model_gpt35, "OpenAILanguageModel")
})

test_that("smart_model respects explicit api_format", {
  provider <- safe_create_provider(create_openai)

  # Force chat API for reasoning model
  model_chat <- provider$smart_model("o1", api_format = "chat")
  expect_s3_class(model_chat, "OpenAILanguageModel")

  # Force responses API for chat model
  model_resp <- provider$smart_model("gpt-4o", api_format = "responses")
  expect_s3_class(model_resp, "OpenAIResponsesLanguageModel")
})

test_that("GenerateResult has reasoning and response_id fields", {
  result <- GenerateResult$new(
    text = "Hello",
    reasoning = "Let me think...",
    response_id = "resp_123"
  )

  expect_equal(result$text, "Hello")
  expect_equal(result$reasoning, "Let me think...")
  expect_equal(result$response_id, "resp_123")
})

test_that("OpenAI responses model returns correct history format", {
  provider <- safe_create_provider(create_openai)
  model <- provider$responses_model("o1")

  expect_equal(model$get_history_format(), "openai_responses")
})

# Live API tests for Responses API (only run when API key is available)
test_that("OpenAI Responses API can make real API calls", {
  skip_if_no_api_key("OpenAI")
  skip_on_cran()
  skip("Responses API requires specific model access")

  provider <- create_openai()
  model <- provider$responses_model("o1-mini")

  # Make a simple API call
  result <- model$generate(
    messages = list(
      list(role = "user", content = "What is 2+2?")
    )
  )

  # Check that we got a response
  expect_true(!is.null(result$text))
  expect_true(nchar(result$text) > 0)

  # Should have response_id for multi-turn
  expect_true(!is.null(result$response_id))
})

test_that("OpenAI Responses API maintains conversation state", {
  skip_if_no_api_key("OpenAI")
  skip_on_cran()
  skip("Responses API requires specific model access")

  provider <- create_openai()
  model <- provider$responses_model("o1-mini")

  # First message
  result1 <- model$generate(
    messages = list(
      list(role = "user", content = "Remember the number 42.")
    )
  )

  # Model should now have a response ID
  expect_true(!is.null(model$get_last_response_id()))

  # Second message should have context
  result2 <- model$generate(
    messages = list(
      list(role = "user", content = "What number did I ask you to remember?")
    )
  )

  # Response should reference 42
  expect_true(grepl("42", result2$text))

  # Reset and verify state is cleared
  model$reset()
  expect_null(model$get_last_response_id())
})
