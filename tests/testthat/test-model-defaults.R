test_that("set_model and model manage the package default", {
  old <- get_model()
  on.exit(set_model(old, reset_options = TRUE), add = TRUE)

  previous <- set_model("mock:test-model")
  expect_identical(previous, old)
  expect_identical(get_model(), "mock:test-model")
  expect_identical(model(), "mock:test-model")

  previous <- model("mock:next-model")
  expect_identical(previous, "mock:test-model")
  expect_identical(get_model(), "mock:next-model")
})

test_that("set_model can adjust runtime options without changing the model", {
  old <- get_model()
  on.exit(set_model(old, reset_options = TRUE), add = TRUE)

  set_model("mock:runtime-target")
  previous <- set_model(context_window = 64000, thinking = "on")

  expect_identical(previous, "mock:runtime-target")
  expect_identical(get_model(), "mock:runtime-target")
  expect_equal(get_model_options()$context_window, 64000)
  expect_true(aisdk:::list_get_exact(get_model_options()$call_options, "thinking"))

  model(max_tokens = 512)
  expect_identical(get_model(), "mock:runtime-target")
  expect_equal(aisdk:::list_get_exact(get_model_options()$call_options, "max_tokens"), 512)
})

test_that("set_model stores runtime options for default model calls and sessions", {
  old <- get_model()
  on.exit(set_model(old, reset_options = TRUE), add = TRUE)

  captured <- NULL
  mock_model <- structure(
    list(
      provider = "mock",
      model_id = "runtime-default",
      do_generate = function(params) {
        captured <<- params
        list(text = "ok", tool_calls = NULL, finish_reason = "stop", usage = NULL)
      },
      get_history_format = function() "openai"
    ),
    class = "LanguageModelV1"
  )

  set_model(
    mock_model,
    context_window = 1000000,
    max_output_tokens = 384000,
    max_tokens = 700,
    thinking = TRUE,
    reasoning_effort = "high"
  )

  options <- get_model_options()
  expect_equal(options$context_window, 1000000)
  expect_equal(options$max_output_tokens, 384000)
  expect_equal(options$call_options$max_tokens, 700)
  expect_true(options$call_options$thinking)
  expect_equal(options$call_options$reasoning_effort, "high")

  result <- generate_text(prompt = "Hello")
  expect_equal(result$text, "ok")
  expect_equal(captured$max_tokens, 700)
  expect_true(captured$thinking)
  expect_equal(captured$reasoning_effort, "high")

  session <- create_chat_session()
  session_options <- session$get_model_options()
  expect_equal(session_options$context_window, 1000000)
  expect_equal(session_options$max_output_tokens, 384000)
  expect_equal(session_options$call_options$max_tokens, 700)
})

test_that("generate_text uses the package default model when model is NULL", {
  old <- get_model()
  on.exit(set_model(old, reset_options = TRUE), add = TRUE)

  mock_model <- structure(
    list(
      provider = "mock",
      model_id = "default",
      do_generate = function(params) {
        list(
          text = "default-response",
          tool_calls = NULL,
          finish_reason = "stop",
          usage = NULL
        )
      },
      get_history_format = function() "openai"
    ),
    class = "LanguageModelV1"
  )

  set_model(mock_model)

  result <- generate_text(prompt = "Hello")

  expect_equal(result$text, "default-response")
})

test_that("ChatSession uses the package default model when no model is provided", {
  old <- get_model()
  on.exit(set_model(old, reset_options = TRUE), add = TRUE)

  registry <- ProviderRegistry$new()
  registry$register("mock", function(model_id) {
    structure(
      list(
        provider = "mock",
        model_id = model_id,
        do_generate = function(params) {
          list(
            text = paste("reply-from", model_id),
            tool_calls = NULL,
            finish_reason = "stop",
            usage = NULL
          )
        },
        get_history_format = function() "openai"
      ),
      class = "LanguageModelV1"
    )
  })

  set_model("mock:test-model")

  session <- ChatSession$new(registry = registry)
  expect_equal(session$get_model_id(), "mock:test-model")

  result <- session$send("Hello")
  expect_equal(result$text, "reply-from test-model")
})

test_that("ChatSession model options are applied to send calls", {
  captured <- NULL
  mock_model <- structure(
    list(
      provider = "mock",
      model_id = "session-options",
      do_generate = function(params) {
        captured <<- params
        list(text = "ok", tool_calls = NULL, finish_reason = "stop", usage = NULL)
      },
      get_history_format = function() "openai"
    ),
    class = "LanguageModelV1"
  )

  session <- create_chat_session(model = mock_model)
  session$set_model_options(
    max_tokens = 500,
    thinking = TRUE,
    thinking_budget = 2000,
    reasoning_effort = "medium"
  )

  session$send("Hello")

  expect_equal(captured$max_tokens, 500)
  expect_true(captured$thinking)
  expect_equal(captured$thinking_budget, 2000)
  expect_equal(captured$reasoning_effort, "medium")
})
