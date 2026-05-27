library(testthat)
library(aisdk)

test_that("input_text creates normalized text blocks", {
  block <- input_text("hello")

  expect_equal(block$type, "input_text")
  expect_equal(block$text, "hello")
})

test_that("input_image detects URL and data URI sources", {
  url_block <- input_image("https://example.com/cat.png", media_type = "image/png")
  data_block <- input_image("data:image/png;base64,ZmFrZQ==")

  expect_equal(url_block$source$kind, "url")
  expect_equal(url_block$value, "https://example.com/cat.png")
  expect_equal(data_block$source$kind, "data_uri")
  expect_equal(data_block$media_type, "image/png")
})

test_that("normalize_content_blocks coerces legacy OpenAI blocks", {
  blocks <- normalize_content_blocks(list(
    list(type = "text", text = "describe this"),
    list(type = "image_url", image_url = list(url = "https://example.com/dog.png", detail = "high"))
  ))

  expect_equal(blocks[[1]]$type, "input_text")
  expect_equal(blocks[[1]]$text, "describe this")
  expect_equal(blocks[[2]]$type, "input_image")
  expect_equal(blocks[[2]]$source$kind, "url")
  expect_equal(blocks[[2]]$value, "https://example.com/dog.png")
})

test_that("normalize_content_blocks accepts a single block without dropping array shape", {
  image <- input_image("https://example.com/dog.png")
  blocks <- normalize_content_blocks(image)

  expect_length(blocks, 1)
  expect_equal(blocks[[1]]$type, "input_image")
})

test_that("normalize_content_blocks removes names so JSON stays array-shaped", {
  blocks <- normalize_content_blocks(list(
    `1` = input_image("https://example.com/dog.png")
  ))

  expect_null(names(blocks))
  expect_match(
    jsonlite::toJSON(blocks, auto_unbox = TRUE),
    "^\\[",
    perl = TRUE
  )
})

test_that("content_blocks_to_text rejects non-text blocks", {
  expect_error(
    content_blocks_to_text(list(input_text("hello"), input_image("https://example.com/dog.png"))),
    "must contain only text blocks"
  )
})

test_that("context previews render multimodal content safely", {
  message <- list(
    role = "user",
    content = list(
      input_text("describe this"),
      input_image("https://example.com/dog.png")
    )
  )

  rendered <- aisdk:::context_message_content_text(message$content)

  expect_match(rendered, "describe this", fixed = TRUE)
  expect_match(rendered, "[image: https://example.com/dog.png]", fixed = TRUE)
  expect_match(aisdk:::message_preview_text(message), "user: describe this", fixed = TRUE)
})
