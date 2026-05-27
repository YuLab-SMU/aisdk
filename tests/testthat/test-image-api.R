library(testthat)
library(aisdk)

MockImageModel <- R6::R6Class(
  "MockImageModel",
  inherit = ImageModelV1,
  public = list(
    initialize = function() {
      super$initialize(provider = "mock", model_id = "mock-image", capabilities = list(image_output = TRUE))
    },
    do_generate_image = function(params) {
      GenerateImageResult$new(
        images = list(list(
          media_type = "image/png",
          bytes = charToRaw("png-bytes")
        )),
        text = params$prompt
      )
    },
    do_edit_image = function(params) {
      GenerateImageResult$new(
        images = list(list(
          media_type = "image/png",
          bytes = charToRaw("edited")
        )),
        text = params$prompt %||% ""
      )
    }
  )
)

MockLanguageModel <- R6::R6Class(
  "MockLanguageModel",
  inherit = LanguageModelV1,
  public = list(
    last_params = NULL,
    initialize = function() {
      super$initialize(provider = "mock", model_id = "mock-vision", capabilities = list(vision_input = TRUE))
    },
    do_generate = function(params) {
      self$last_params <- params
      GenerateResult$new(text = "ok")
    },
    do_stream = function(params, callback) {
      self$last_params <- params
      callback("ok", FALSE)
      callback("", TRUE)
      GenerateResult$new(text = "ok")
    },
    format_tool_result = function(tool_call_id, tool_name, result_content) {
      list(role = "tool", content = result_content)
    }
  )
)

test_that("analyze_image wraps prompt and image into multimodal message content", {
  model <- MockLanguageModel$new()

  result <- analyze_image(
    model = model,
    image = "https://example.com/cat.png",
    prompt = "Describe this image"
  )

  expect_equal(result$text, "ok")
  content <- model$last_params$messages[[1]]$content
  expect_equal(content[[1]]$type, "input_text")
  expect_equal(content[[1]]$text, "Describe this image")
  expect_equal(content[[2]]$type, "input_image")
  expect_equal(content[[2]]$source$kind, "url")
})

test_that("generate_image materializes returned image bytes", {
  model <- MockImageModel$new()
  out_dir <- tempfile("aisdk-image-out-")

  result <- generate_image(
    model = model,
    prompt = "draw a cat",
    output_dir = out_dir
  )

  expect_s3_class(result, "GenerateImageResult")
  expect_true(file.exists(result$images[[1]]$path))
  expect_equal(result$text, "draw a cat")
})

test_that("edit_image materializes edited image bytes", {
  model <- MockImageModel$new()
  out_dir <- tempfile("aisdk-image-edit-out-")

  result <- edit_image(
    model = model,
    image = "https://example.com/cat.png",
    prompt = "make it blue",
    output_dir = out_dir
  )

  expect_true(file.exists(result$images[[1]]$path))
  expect_equal(result$text, "make it blue")
})

test_that("provider registry resolves image models", {
  provider <- structure(list(
    image_model = function(model_id) {
      model <- MockImageModel$new()
      model$model_id <- model_id
      model
    }
  ), class = "R6")

  registry <- ProviderRegistry$new()
  registry$register("mock", provider)

  model <- registry$image_model("mock:demo-image")
  expect_s3_class(model, "MockImageModel")
  expect_equal(model$model_id, "demo-image")
})
