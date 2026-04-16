# Tests for Core API functions
library(testthat)
library(aisdk)

# Load helper functions (for environment variable handling)
helper_path <- file.path(test_path("helper-env.R"))
source(helper_path)

openai_model <- get_openai_model()
openai_model_id <- paste0("openai:", openai_model)

# === Tests for generate_text ===

test_that("generate_text accepts a model object", {
  skip_if_offline()
  skip_on_cran()

  # Force invalid key to ensure network error (401)
  provider <- suppressWarnings(create_openai(api_key = "invalid"))
  model <- provider$language_model(openai_model)

  # This will fail due to API key error (which is what we want to verify - that it attempts to call)
  expect_error(
    generate_text(model = model, prompt = "Hello")
  )
})

test_that("generate_text accepts a string model identifier", {
  skip_if_offline()
  skip_on_cran()

  # Register a mock provider with invalid key
  registry <- get_default_registry()
  provider <- suppressWarnings(create_openai(api_key = "invalid"))
  registry$register("test-openai-fail", provider)

  model_id <- paste0("test-openai-fail:", openai_model)

  # This will fail due to API key error
  expect_error(
    generate_text(model = model_id, prompt = "Hello")
  )
})

test_that("generate_text recovers tool calls embedded in assistant text", {
  tool_invocations <- list()
  echo_tool <- tool(
    name = "echo",
    description = "Echo a message",
    parameters = z_object(message = z_string("Message to echo")),
    execute = function(args) {
      tool_invocations[[length(tool_invocations) + 1L]] <<- args$message
      paste("Echo:", args$message)
    }
  )

  mock_model <- MockModel$new()
  mock_model$add_response(
    text = paste0(
      "<tool_call>\n",
      "{\"name\":\"echo\",\"arguments\":{\"message\":\"hello from text\"}}\n",
      "</tool_call>"
    ),
    tool_calls = NULL
  )
  mock_model$add_response(text = "Tool execution worked.")

  result <- generate_text(
    model = mock_model,
    prompt = "Say hello",
    tools = list(echo_tool),
    max_steps = 3
  )

  expect_equal(result$text, "Tool execution worked.")
  expect_equal(tool_invocations, list("hello from text"))
  expect_length(result$all_tool_calls, 1)
  expect_equal(result$all_tool_calls[[1]]$name, "echo")
  expect_equal(result$all_tool_calls[[1]]$arguments$message, "hello from text")
})

# === Tests for ProviderRegistry ===

test_that("ProviderRegistry registers and retrieves providers", {
  registry <- ProviderRegistry$new()
  provider <- suppressWarnings(create_openai())

  registry$register("test-openai", provider)
  # Verify provider is registered by checking list_providers
  expect_true("test-openai" %in% registry$list_providers())
})

test_that("ProviderRegistry parses provider:model syntax", {
  registry <- ProviderRegistry$new()
  provider <- suppressWarnings(create_openai())
  registry$register("myopenai", provider)

  model <- registry$language_model(paste0("myopenai:", openai_model))

  expect_s3_class(model, "OpenAILanguageModel")
  expect_equal(model$model_id, openai_model)
})

test_that("ProviderRegistry errors on unregistered provider", {
  registry <- ProviderRegistry$new()

  expect_error(
    registry$language_model("nonexistent:model"),
    "Provider not found"
  )
})

# === Tests for get_default_registry ===

test_that("get_default_registry returns singleton", {
  reg1 <- get_default_registry()
  reg2 <- get_default_registry()

  expect_identical(reg1, reg2)
})
