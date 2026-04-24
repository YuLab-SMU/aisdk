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
  provider <- suppressWarnings(create_openai(api_key = "test-key"))
  model <- provider$language_model(openai_model)
  captured_body <- NULL

  local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(
        choices = list(list(
          message = list(content = "Mocked hello"),
          finish_reason = "stop"
        )),
        usage = list(prompt_tokens = 1, completion_tokens = 1, total_tokens = 2)
      )
    },
    .package = "aisdk"
  )

  result <- generate_text(model = model, prompt = "Hello")

  expect_equal(result$text, "Mocked hello")
  expect_equal(captured_body$messages[[1]]$content[[1]]$type, "text")
  expect_equal(captured_body$messages[[1]]$content[[1]]$text, "Hello")
})

test_that("generate_text accepts a string model identifier", {
  registry <- ProviderRegistry$new()
  provider <- suppressWarnings(create_openai(api_key = "test-key"))
  registry$register("test-openai-fail", provider)

  model_id <- paste0("test-openai-fail:", openai_model)
  captured_body <- NULL

  local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(
        choices = list(list(
          message = list(content = "Mocked via registry"),
          finish_reason = "stop"
        )),
        usage = list(prompt_tokens = 1, completion_tokens = 1, total_tokens = 2)
      )
    },
    .package = "aisdk"
  )

  result <- generate_text(model = model_id, prompt = "Hello", registry = registry)

  expect_equal(result$text, "Mocked via registry")
  expect_equal(captured_body$model, openai_model)
  expect_equal(captured_body$messages[[1]]$content[[1]]$type, "text")
  expect_equal(captured_body$messages[[1]]$content[[1]]$text, "Hello")
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
