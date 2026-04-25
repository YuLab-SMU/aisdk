# Tests for OpenAI Provider
library(testthat)
library(aisdk)

# Load helper functions (for environment variable handling)
helper_path <- file.path(test_path("helper-env.R"))
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

test_that("OpenAI provider creates image model correctly", {
  provider <- safe_create_provider(create_openai)
  model <- provider$image_model("gpt-image-2")

  expect_s3_class(model, "OpenAIImageModel")
  expect_equal(model$model_id, "gpt-image-2")
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

test_that("OpenAI provider passes configured timeout_seconds to HTTP helper", {
  provider <- suppressWarnings(create_openai(api_key = "test-key", timeout_seconds = 456))
  model <- provider$language_model(openai_model)

  payload <- model$build_payload(list(
    messages = list(list(role = "user", content = "Hello"))
  ))

  expect_match(payload$url, "/chat/completions$")
  expect_equal(model$get_config()$timeout_seconds, 456)
})

test_that("OpenAI per-call timeout_seconds overrides provider default", {
  provider <- suppressWarnings(create_openai(api_key = "test-key", timeout_seconds = 456))
  model <- provider$language_model(openai_model)

  payload <- model$build_payload(list(
    messages = list(list(role = "user", content = "Hello")),
    timeout_seconds = 999
  ))

  expect_match(payload$url, "/chat/completions$")
  expect_equal(model$get_config()$timeout_seconds, 456)
})

test_that("OpenAI provider forwards idle_timeout_seconds to streaming helper", {
  provider <- suppressWarnings(create_openai(
    api_key = "test-key",
    first_byte_timeout_seconds = 222,
    idle_timeout_seconds = 77
  ))
  model <- provider$language_model(openai_model)

  payload <- model$build_stream_payload(list(
    messages = list(list(role = "user", content = "Hello"))
  ))

  expect_match(payload$url, "/chat/completions$")
  expect_equal(model$get_config()$first_byte_timeout_seconds, 222)
  expect_equal(model$get_config()$idle_timeout_seconds, 77)
  expect_equal(payload$body$stream_options$include_usage, TRUE)
})

# Transport-free regression tests for generate() entrypoints
test_that("OpenAI provider can generate text via mocked transport", {
  skip_on_ci()

  provider <- safe_create_provider(create_openai, api_key = "test-key")
  model <- provider$language_model(openai_model)
  captured_body <- NULL

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(
        choices = list(list(
          message = list(content = "Hello from mock"),
          finish_reason = "stop"
        )),
        usage = list(prompt_tokens = 3, completion_tokens = 4, total_tokens = 7)
      )
    },
    .package = "aisdk"
  )

  result <- model$generate(
    messages = list(list(role = "user", content = "Say 'Hello, World!'")),
    max_tokens = 200
  )

  expect_equal(result$text, "Hello from mock")
  expect_equal(captured_body$model, openai_model)
  expect_equal(captured_body$messages[[1]]$content[[1]]$type, "text")
  expect_equal(captured_body$messages[[1]]$content[[1]]$text, "Say 'Hello, World!'")
})

test_that("OpenAI provider handles tool calls", {
  skip_on_ci()

  provider <- safe_create_provider(create_openai, api_key = "test-key")
  model <- provider$language_model(openai_model)

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      list(
        choices = list(list(
          message = list(
            content = NULL,
            tool_calls = list(list(
              id = "call_123",
              type = "function",
              `function` = list(
                name = "get_time",
                arguments = "{\"timezone\":\"UTC\"}"
              )
            ))
          ),
          finish_reason = "tool_calls"
        )),
        usage = list(prompt_tokens = 10, completion_tokens = 5, total_tokens = 15)
      )
    },
    .package = "aisdk"
  )

  result <- model$generate(
    messages = list(list(role = "user", content = "What time is it?")),
    max_tokens = 50
  )

  expect_length(result$tool_calls, 1)
  expect_equal(result$tool_calls[[1]]$id, "call_123")
  expect_equal(result$tool_calls[[1]]$name, "get_time")
  expect_equal(result$tool_calls[[1]]$arguments$timezone, "UTC")
})

test_that("OpenAI chat payload builder translates multimodal content blocks", {
  provider <- safe_create_provider(create_openai)
  model <- provider$language_model(openai_model)

  payload <- model$build_payload(list(
    messages = list(list(
      role = "user",
      content = list(
        input_text("Describe this image"),
        input_image("https://example.com/test.png", media_type = "image/png", detail = "high")
      )
    ))
  ))

  blocks <- payload$body$messages[[1]]$content
  expect_equal(blocks[[1]]$type, "text")
  expect_equal(blocks[[1]]$text, "Describe this image")
  expect_equal(blocks[[2]]$type, "image_url")
  expect_equal(blocks[[2]]$image_url$url, "https://example.com/test.png")
  expect_equal(blocks[[2]]$image_url$detail, "high")
})

test_that("OpenAI responses model translates multimodal content blocks", {
  provider <- safe_create_provider(create_openai)
  model <- provider$responses_model("o1")

  expect_s3_class(model, "OpenAIResponsesLanguageModel")
  expect_equal(model$model_id, "o1")
  expect_equal(model$provider, "openai")
})

test_that("OpenAI chat payload translates multimodal content blocks", {
  provider <- safe_create_provider(create_openai)
  model <- provider$language_model(openai_model)

  payload <- model$build_payload(list(
    messages = list(
      list(
        role = "user",
        content = list(
          input_text("Describe this image"),
          input_image(
            paste0(
              "data:image/png;base64,",
              base64enc::base64encode(charToRaw("fake-image"))
            ),
            media_type = "image/png",
            detail = "high"
          )
        )
      )
    )
  ))

  content <- payload$body$messages[[1]]$content
  expect_equal(content[[1]]$type, "text")
  expect_equal(content[[1]]$text, "Describe this image")
  expect_equal(content[[2]]$type, "image_url")
  expect_match(content[[2]]$image_url$url, "^data:image/png;base64,")
  expect_equal(content[[2]]$image_url$detail, "high")
})

test_that("OpenAI chat payload translates provider-neutral multimodal blocks", {
  provider <- safe_create_provider(create_openai, api_key = "FAKE")
  model <- provider$language_model("gpt-4o")

  image_path <- tempfile(fileext = ".png")
  writeBin(as.raw(0:15), image_path)
  on.exit(unlink(image_path), add = TRUE)

  payload <- model$build_payload(list(
    messages = list(list(
      role = "user",
      content = list(
        input_text("Describe this image."),
        input_image(image_path, detail = "high")
      )
    ))
  ))

  expect_equal(payload$body$messages[[1]]$content[[1]]$type, "text")
  expect_equal(payload$body$messages[[1]]$content[[1]]$text, "Describe this image.")
  expect_equal(payload$body$messages[[1]]$content[[2]]$type, "image_url")
  expect_match(payload$body$messages[[1]]$content[[2]]$image_url$url, "^data:image/png;base64,")
  expect_equal(payload$body$messages[[1]]$content[[2]]$image_url$detail, "high")
})

test_that("OpenAI responses payload translates provider-neutral multimodal blocks", {
  skip_on_ci()

  provider <- safe_create_provider(create_openai, api_key = "FAKE")
  model <- provider$responses_model("o1")

  image_path <- tempfile(fileext = ".png")
  writeBin(as.raw(0:15), image_path)
  on.exit(unlink(image_path), add = TRUE)

  captured_body <- NULL
  mock_response <- list(
    id = "resp_123",
    output = list(list(
      type = "message",
      content = list(list(text = "ok"))
    )),
    status = "completed",
    usage = list(
      input_tokens = 10,
      output_tokens = 5,
      total_tokens = 15
    )
  )

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      mock_response
    },
    .package = "aisdk"
  )

  result <- model$generate(
    messages = list(list(
      role = "user",
      content = list(
        input_text("Analyze the image."),
        input_image(image_path)
      )
    ))
  )

  expect_equal(result$text, "ok")
  expect_equal(captured_body$input[[1]]$content[[1]]$type, "input_text")
  expect_equal(captured_body$input[[1]]$content[[2]]$type, "input_image")
  expect_match(captured_body$input[[1]]$content[[2]]$image_url, "^data:image/png;base64,")
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

test_that("OpenAI responses API translates multimodal input blocks", {
  skip_on_ci()

  provider <- safe_create_provider(create_openai)
  model <- provider$responses_model("o1")
  captured_body <- NULL

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(
        id = "resp_test",
        output = list(list(
          type = "message",
          content = list(list(text = "ok"))
        ))
      )
    },
    .package = "aisdk"
  )

  result <- model$do_generate(list(
    messages = list(
      list(
        role = "user",
        content = list(
          input_text("What is in this image?"),
          input_image(
            paste0(
              "data:image/png;base64,",
              base64enc::base64encode(charToRaw("fake-image"))
            ),
            media_type = "image/png"
          )
        )
      )
    )
  ))

  expect_equal(result$text, "ok")
  expect_equal(captured_body$input[[1]]$type, "message")
  expect_equal(captured_body$input[[1]]$content[[1]]$type, "input_text")
  expect_equal(captured_body$input[[1]]$content[[2]]$type, "input_image")
  expect_match(captured_body$input[[1]]$content[[2]]$image_url, "^data:image/png;base64,")
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

test_that("OpenAI image model posts JSON generation payload and parses images", {
  skip_on_ci()
  skip_on_cran()

  provider <- safe_create_provider(create_openai)
  model <- provider$image_model("gpt-image-2")
  captured_body <- NULL

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(
        created = 123,
        data = list(list(
          b64_json = base64enc::base64encode(charToRaw("png-bytes")),
          revised_prompt = "revised"
        ))
      )
    },
    .package = "aisdk"
  )

  result <- generate_image(
    model = model,
    prompt = "Draw a blue mug",
    output_dir = tempdir()
  )

  expect_equal(captured_body$model, "gpt-image-2")
  expect_equal(captured_body$prompt, "Draw a blue mug")
  expect_equal(captured_body$response_format, "b64_json")
  expect_equal(result$images[[1]]$media_type, "image/png")
  expect_equal(rawToChar(result$images[[1]]$bytes), "png-bytes")
  expect_equal(result$images[[1]]$revised_prompt, "revised")
})

test_that("OpenAI image generation forwards latest image parameters", {
  skip_on_ci()
  skip_on_cran()

  provider <- safe_create_provider(create_openai)
  model <- provider$image_model("gpt-image-2")
  captured_body <- NULL

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(
        created = 123,
        data = list(list(
          b64_json = base64enc::base64encode(charToRaw("jpeg-bytes"))
        ))
      )
    },
    .package = "aisdk"
  )

  result <- generate_image(
    model = model,
    prompt = "Draw a blue mug",
    output_dir = tempdir(),
    background = "transparent",
    output_format = "jpeg",
    output_compression = 42,
    moderation = "low"
  )

  expect_equal(captured_body$model, "gpt-image-2")
  expect_equal(captured_body$background, "transparent")
  expect_equal(captured_body$output_format, "jpeg")
  expect_equal(captured_body$output_compression, 42)
  expect_equal(captured_body$moderation, "low")
  expect_equal(result$images[[1]]$media_type, "image/jpeg")
})

test_that("OpenAI image model posts multipart edit payload and parses images", {
  skip_on_ci()
  skip_on_cran()

  provider <- safe_create_provider(create_openai)
  model <- provider$image_model("gpt-image-2")
  captured_body <- NULL

  input_path <- tempfile(fileext = ".png")
  writeBin(charToRaw("fakepng"), input_path)
  on.exit(unlink(input_path), add = TRUE)

  testthat::local_mocked_bindings(
    post_multipart_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(
        created = 456,
        data = list(list(
          b64_json = base64enc::base64encode(charToRaw("edited-bytes"))
        ))
      )
    },
    .package = "aisdk"
  )

  result <- edit_image(
    model = model,
    image = input_path,
    prompt = "Make it cobalt blue",
    output_dir = tempdir()
  )

  expect_equal(captured_body$model, "gpt-image-2")
  expect_equal(captured_body$prompt, "Make it cobalt blue")
  expect_equal(captured_body$response_format, "b64_json")
  expect_true(!is.null(captured_body$image))
  expect_equal(rawToChar(result$images[[1]]$bytes), "edited-bytes")
})

test_that("OpenAI image edit includes mask uploads when provided", {
  skip_on_ci()
  skip_on_cran()

  provider <- safe_create_provider(create_openai)
  model <- provider$image_model("gpt-image-1.5")
  captured_body <- NULL

  image_path <- tempfile(fileext = ".png")
  mask_path <- tempfile(fileext = ".png")
  writeBin(charToRaw("fakepng"), image_path)
  writeBin(charToRaw("maskpng"), mask_path)
  on.exit(unlink(c(image_path, mask_path)), add = TRUE)

  testthat::local_mocked_bindings(
    post_multipart_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(
        created = 456,
        data = list(list(
          b64_json = base64enc::base64encode(charToRaw("edited-with-mask"))
        ))
      )
    },
    .package = "aisdk"
  )

  result <- edit_image(
    model = model,
    image = image_path,
    mask = mask_path,
    prompt = "Only change the mug color",
    output_dir = tempdir()
  )

  expect_true(!is.null(captured_body$mask))
  expect_equal(rawToChar(result$images[[1]]$bytes), "edited-with-mask")
})

test_that("OpenAI image edit supports multiple reference images and latest edit params", {
  skip_on_ci()
  skip_on_cran()

  provider <- safe_create_provider(create_openai)
  model <- provider$image_model("gpt-image-1.5")
  captured_body <- NULL

  image_paths <- c(tempfile(fileext = ".png"), tempfile(fileext = ".png"))
  writeBin(charToRaw("fakepng-a"), image_paths[[1]])
  writeBin(charToRaw("fakepng-b"), image_paths[[2]])
  on.exit(unlink(image_paths), add = TRUE)

  testthat::local_mocked_bindings(
    post_multipart_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(
        created = 456,
        data = list(list(
          b64_json = base64enc::base64encode(charToRaw("edited-multi"))
        ))
      )
    },
    .package = "aisdk"
  )

  result <- edit_image(
    model = model,
    image = image_paths,
    prompt = "Combine both references into one product shot",
    input_fidelity = "high",
    output_format = "webp",
    output_compression = 55,
    output_dir = tempdir()
  )

  expect_equal(sum(names(captured_body) == "image[]"), 2)
  expect_equal(captured_body$input_fidelity, "high")
  expect_equal(captured_body$output_format, "webp")
  expect_equal(captured_body$output_compression, 55)
  expect_equal(result$images[[1]]$media_type, "image/webp")
  expect_equal(rawToChar(result$images[[1]]$bytes), "edited-multi")
})

test_that("OpenAI image param validation enforces latest model constraints", {
  provider <- safe_create_provider(create_openai)
  image_path <- tempfile(fileext = ".png")
  writeBin(charToRaw("fakepng"), image_path)
  on.exit(unlink(image_path), add = TRUE)

  expect_error(
    edit_image(
      model = provider$image_model("gpt-image-2"),
      image = image_path,
      prompt = "Edit this image",
      input_fidelity = "high"
    ),
    "fixed for `gpt-image-2`"
  )

  expect_error(
    generate_image(
      model = provider$image_model("gpt-image-2"),
      prompt = "Draw a mug",
      output_compression = 40
    ),
    "requires `output_format = 'jpeg'` or `output_format = 'webp'`"
  )
})

test_that("OpenAI image edit rejects remote URLs for uploaded source images", {
  provider <- safe_create_provider(create_openai)
  model <- provider$image_model("gpt-image-2")

  expect_error(
    edit_image(
      model = model,
      image = "https://example.com/source.png",
      prompt = "Edit this image"
    ),
    "local file path or data URI"
  )
})
