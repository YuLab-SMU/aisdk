library(testthat)
library(aisdk)

test_that("validate_model_messages only rejects explicit non-vision models", {
  messages <- list(list(
    role = "user",
    content = list(
      input_text("describe"),
      input_image("https://example.com/test.png")
    )
  ))

  model_unknown <- LanguageModelV1$new("test", "unknown", capabilities = list())
  expect_invisible(validate_model_messages(model_unknown, messages))

  model_yes <- LanguageModelV1$new("test", "vision", capabilities = list(vision_input = TRUE))
  expect_invisible(validate_model_messages(model_yes, messages))

  model_no <- LanguageModelV1$new("test", "text-only", capabilities = list(vision_input = FALSE))
  expect_error(
    validate_model_messages(model_no, messages),
    "does not advertise multimodal image input support"
  )
})
