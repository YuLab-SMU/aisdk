test_that("set_model and model manage the package default", {
  old <- get_model()
  on.exit(set_model(old), add = TRUE)

  previous <- set_model("mock:test-model")
  expect_identical(previous, old)
  expect_identical(get_model(), "mock:test-model")
  expect_identical(model(), "mock:test-model")

  previous <- model("mock:next-model")
  expect_identical(previous, "mock:test-model")
  expect_identical(get_model(), "mock:next-model")
})

test_that("generate_text uses the package default model when model is NULL", {
  old <- get_model()
  on.exit(set_model(old), add = TRUE)

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
  on.exit(set_model(old), add = TRUE)

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
