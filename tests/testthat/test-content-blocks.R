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

# --- AH1: input_file (document/PDF) blocks -----------------------------------

test_that("input_file infers source kind, media type, and filename", {
  f <- withr::local_tempfile(fileext = ".pdf")
  writeBin(as.raw(c(0x25, 0x50, 0x44, 0x46)), f) # "%PDF"

  b <- input_file(f)
  expect_equal(b$type, "input_file")
  expect_equal(b$source$kind, "file")
  expect_equal(b$media_type, "application/pdf")
  expect_equal(b$filename, basename(f))

  # data URI: media type from the URI, generic filename
  bd <- input_file("data:application/pdf;base64,JVBERi0=")
  expect_equal(bd$source$kind, "data_uri")
  expect_equal(bd$media_type, "application/pdf")
  expect_equal(bd$filename, "document.pdf")

  # URL: basename filename, explicit media type honored
  bu <- input_file("https://example.com/report.pdf")
  expect_equal(bu$source$kind, "url")
  expect_equal(bu$filename, "report.pdf")

  # non-PDF extension inference + explicit filename override
  bt <- input_file("data:text/plain;base64,aGk=", filename = "notes.txt")
  expect_equal(bt$media_type, "text/plain")
  expect_equal(bt$filename, "notes.txt")
})

test_that("input_file rejects empty input and validates in a message", {
  expect_error(input_file(""), "non-empty")
  blocks <- aisdk:::normalize_content_blocks(list(
    input_text("Summarize this:"),
    input_file("https://example.com/doc.pdf")
  ))
  expect_length(blocks, 2)
  expect_equal(vapply(blocks, function(b) b$type, ""), c("input_text", "input_file"))
})

test_that("input_file translates to each provider's native document shape", {
  b <- input_file("https://example.com/doc.pdf")           # url source (no file IO)
  data_b <- input_file("data:application/pdf;base64,JVBERi0=")

  # OpenAI Chat: {type:"file", file:{filename, file_data}}
  oc <- aisdk:::translate_message_content(list(data_b), "openai_chat")[[1]]
  expect_equal(oc$type, "file")
  expect_equal(oc$file$filename, "document.pdf")
  expect_match(oc$file$file_data, "^data:application/pdf;base64,")

  # OpenAI Responses: flat input_file
  orr <- aisdk:::translate_message_content(list(data_b), "openai_responses")[[1]]
  expect_equal(orr$type, "input_file")
  expect_match(orr$file_data, "^data:application/pdf;base64,")

  # Anthropic: document block, base64 vs url source
  an <- aisdk:::translate_message_content(list(data_b), "anthropic")[[1]]
  expect_equal(an$type, "document")
  expect_equal(an$source$type, "base64")
  expect_equal(an$source$media_type, "application/pdf")
  anu <- aisdk:::translate_message_content(list(b), "anthropic")[[1]]
  expect_equal(anu$type, "document")
  expect_equal(anu$source$type, "url")
  expect_equal(anu$source$url, "https://example.com/doc.pdf")

  # Gemini: inlineData (base64) / fileData (url) — media-type agnostic path
  ge <- aisdk:::translate_message_content(list(data_b), "gemini")[[1]]
  expect_equal(ge$inlineData$mimeType, "application/pdf")
  expect_true(nzchar(ge$inlineData$data))
  geu <- aisdk:::translate_message_content(list(b), "gemini")[[1]]
  expect_equal(geu$fileData$mimeType, "application/pdf")
  expect_equal(geu$fileData$fileUri, "https://example.com/doc.pdf")
})
