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

test_that("create_openai() falls back when base URL env vars are empty", {
  withr::with_envvar(c(OPENAI_BASE_URL = "", OPENAI_BASE_URLS = ""), {
    provider <- safe_create_provider(create_openai, api_key = "sk-test")
    model <- provider$language_model(openai_model)

    expect_s3_class(model, "OpenAILanguageModel")
  })
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

test_that("OpenAI chat payload keeps single image content as an array", {
  provider <- safe_create_provider(create_openai, api_key = "FAKE")
  model <- provider$language_model("gpt-4o")

  payload <- model$build_payload(list(
    messages = list(list(
      role = "user",
      content = input_image("https://example.com/test.png", media_type = "image/png")
    ))
  ))

  content <- payload$body$messages[[1]]$content
  expect_length(content, 1)
  expect_equal(content[[1]]$type, "image_url")

  json <- jsonlite::toJSON(payload$body, auto_unbox = TRUE, null = "null")
  expect_match(
    json,
    '"content":\\[\\{"type":"image_url"',
    perl = TRUE
  )
})

test_that("OpenAI chat payload keeps ChatSession single image history as an array", {
  provider <- safe_create_provider(create_openai, api_key = "FAKE")
  model <- provider$language_model("gpt-4o")
  session <- ChatSession$new(model = model)
  session$append_message("user", input_image("https://example.com/test.png", media_type = "image/png"))

  payload <- model$build_payload(list(messages = session$get_history()))
  json <- jsonlite::toJSON(payload$body, auto_unbox = TRUE, null = "null")

  expect_match(
    json,
    '"content":\\[\\{"type":"image_url"',
    perl = TRUE
  )
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

test_that("OpenAI responses model nests flat reasoning_effort/summary into body$reasoning", {
  skip_on_ci()

  provider <- safe_create_provider(create_openai)
  model <- provider$responses_model("gpt-5")
  captured_body <- NULL

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(id = "resp_x", output = list(list(type = "message", content = list(list(text = "ok")))))
    },
    .package = "aisdk"
  )

  model$do_generate(list(
    messages = list(list(role = "user", content = "hi")),
    reasoning_effort = "high",
    reasoning_summary = "detailed"
  ))

  expect_equal(captured_body$reasoning$effort, "high")
  expect_equal(captured_body$reasoning$summary, "detailed")
  # The flat keys must NOT leak into the body alongside the nested form
  expect_null(captured_body$reasoning_effort)
  expect_null(captured_body$reasoning_summary)
})

test_that("OpenAI responses model defaults reasoning model to reasoning summary auto", {
  provider <- safe_create_provider(create_openai, api_key = "FAKE")
  model <- provider$responses_model("gpt-5-mini")
  captured_body <- NULL

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(id = "resp_x", output = list(list(type = "message", content = list(list(text = "ok")))))
    },
    .package = "aisdk"
  )

  model$do_generate(list(
    messages = list(list(role = "user", content = "hi"))
  ))

  expect_equal(captured_body$reasoning$summary, "auto")
  expect_null(captured_body$thinking)
})

test_that("OpenAI responses model does not request reasoning summary when thinking is off", {
  provider <- safe_create_provider(create_openai, api_key = "FAKE")
  model <- provider$responses_model("gpt-5-mini")
  captured_body <- NULL

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(id = "resp_x", output = list(list(type = "message", content = list(list(text = "ok")))))
    },
    .package = "aisdk"
  )

  model$do_generate(list(
    messages = list(list(role = "user", content = "hi")),
    thinking = FALSE
  ))

  expect_null(captured_body$reasoning)
  expect_null(captured_body$thinking)
})

test_that("OpenAI responses model preserves explicit reasoning summary over thinking default", {
  provider <- safe_create_provider(create_openai, api_key = "FAKE")
  model <- provider$responses_model("gpt-5-mini")
  captured_body <- NULL

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(id = "resp_x", output = list(list(type = "message", content = list(list(text = "ok")))))
    },
    .package = "aisdk"
  )

  model$do_generate(list(
    messages = list(list(role = "user", content = "hi")),
    thinking = TRUE,
    reasoning_summary = "detailed"
  ))

  expect_equal(captured_body$reasoning$summary, "detailed")
  expect_null(captured_body$thinking)
})

test_that("OpenAI responses streaming emits reasoning summary into thinking UI", {
  provider <- safe_create_provider(create_openai, api_key = "FAKE")
  model <- provider$responses_model("gpt-5-mini")
  captured_body <- NULL
  chunks <- character()

  testthat::local_mocked_bindings(
    stream_responses_api = function(url, headers, body, callback, ...) {
      captured_body <<- body
      callback("response.created", list(response = list(id = "resp_stream_reason")), done = FALSE)
      callback(
        "response.reasoning_summary_text.delta",
        list(delta = "Checking the model fit."),
        done = FALSE
      )
      callback(
        "response.output_text.delta",
        list(delta = "Done."),
        done = FALSE
      )
      callback(
        "response.completed",
        list(response = list(
          id = "resp_stream_reason",
          status = "completed",
          usage = list(input_tokens = 1, output_tokens = 2, total_tokens = 3)
        )),
        done = FALSE
      )
      callback(NULL, NULL, done = TRUE)
    },
    .package = "aisdk"
  )

  result <- model$do_stream(
    list(
      messages = list(list(role = "user", content = "hi")),
      thinking = TRUE
    ),
    callback = function(text, done) {
      if (!isTRUE(done) && nzchar(text %||% "")) {
        chunks <<- c(chunks, text)
      }
    }
  )

  expect_equal(captured_body$reasoning$summary, "auto")
  expect_equal(chunks[1], "<think>\n")
  expect_equal(chunks[2], "Checking the model fit.")
  expect_equal(result$reasoning, "Checking the model fit.")
  expect_equal(result$text, "Done.")
})

test_that("OpenAI responses model accepts nested reasoning list and preserves extra keys", {
  skip_on_ci()

  provider <- safe_create_provider(create_openai)
  model <- provider$responses_model("gpt-5")
  captured_body <- NULL

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(id = "resp_x", output = list(list(type = "message", content = list(list(text = "ok")))))
    },
    .package = "aisdk"
  )

  model$do_generate(list(
    messages = list(list(role = "user", content = "hi")),
    reasoning = list(effort = "minimal", summary = "auto", generate_summary = TRUE)
  ))

  expect_equal(captured_body$reasoning$effort, "minimal")
  expect_equal(captured_body$reasoning$summary, "auto")
  expect_true(captured_body$reasoning$generate_summary)
})

test_that("OpenAI responses model forwards `include` for stateless reasoning continuity", {
  skip_on_ci()

  provider <- safe_create_provider(create_openai)
  model <- provider$responses_model("gpt-5")
  captured_body <- NULL

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(id = "resp_x", output = list(list(type = "message", content = list(list(text = "ok")))))
    },
    .package = "aisdk"
  )

  model$do_generate(list(
    messages = list(list(role = "user", content = "hi")),
    include = c("reasoning.encrypted_content")
  ))

  expect_equal(captured_body$include, list("reasoning.encrypted_content"))
})

test_that("flat reasoning_effort takes precedence over nested reasoning$effort", {
  skip_on_ci()

  provider <- safe_create_provider(create_openai)
  model <- provider$responses_model("gpt-5")
  captured_body <- NULL

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(id = "resp_x", output = list(list(type = "message", content = list(list(text = "ok")))))
    },
    .package = "aisdk"
  )

  model$do_generate(list(
    messages = list(list(role = "user", content = "hi")),
    reasoning_effort = "high",
    reasoning = list(effort = "low", summary = "concise")
  ))

  expect_equal(captured_body$reasoning$effort, "high")
  expect_equal(captured_body$reasoning$summary, "concise")
})

test_that("OpenAI responses model forwards conversation id (string and list forms)", {
  skip_on_ci()

  provider <- safe_create_provider(create_openai)
  model <- provider$responses_model("gpt-5")
  bodies <- list()

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      bodies[[length(bodies) + 1]] <<- body
      list(id = "resp_x", output = list(list(type = "message", content = list(list(text = "ok")))))
    },
    .package = "aisdk"
  )

  # String form
  model$do_generate(list(
    messages = list(list(role = "user", content = "hi")),
    conversation = "conv_abc123"
  ))
  expect_equal(bodies[[1]]$conversation, "conv_abc123")

  # List-with-$id form (the shape returned by create_conversation())
  model$do_generate(list(
    messages = list(list(role = "user", content = "hi")),
    conversation = list(id = "conv_xyz789", object = "conversation")
  ))
  expect_equal(bodies[[2]]$conversation, "conv_xyz789")
})

test_that("OpenAI responses model rejects malformed conversation argument", {
  skip_on_ci()
  provider <- safe_create_provider(create_openai)
  model <- provider$responses_model("gpt-5")
  expect_error(
    model$do_generate(list(
      messages = list(list(role = "user", content = "hi")),
      conversation = 42
    )),
    "conversation id string"
  )
})

test_that("OpenAIProvider conversations CRUD hits the right endpoints", {
  skip_on_ci()

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://api.openai.com/v1"
  )

  calls <- list()
  fake_perform <- function(req) {
    calls[[length(calls) + 1]] <<- list(
      method = req$method %||% "GET",
      url = req$url,
      body = req$body
    )
    structure(
      list(status = 200L, body_text = '{"id":"conv_new","object":"conversation","created_at":1}'),
      class = "fake_resp"
    )
  }
  fake_status <- function(resp) resp$status
  fake_body_string <- function(resp) resp$body_text

  testthat::local_mocked_bindings(
    req_perform = fake_perform,
    resp_status = fake_status,
    resp_body_string = fake_body_string,
    .package = "httr2"
  )

  created <- provider$create_conversation(metadata = list(topic = "demo"))
  expect_equal(created$id, "conv_new")
  expect_equal(calls[[1]]$method, "POST")
  expect_match(calls[[1]]$url, "/conversations$")

  provider$get_conversation("conv_new")
  expect_equal(calls[[2]]$method, "GET")
  expect_match(calls[[2]]$url, "/conversations/conv_new$")

  provider$delete_conversation("conv_new")
  expect_equal(calls[[3]]$method, "DELETE")
  expect_match(calls[[3]]$url, "/conversations/conv_new$")
})

test_that("create_conversation surfaces API errors with body text", {
  skip_on_ci()

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://api.openai.com/v1"
  )

  testthat::local_mocked_bindings(
    req_perform = function(req) {
      structure(list(status = 401L, body_text = '{"error":{"message":"bad key"}}'),
                class = "fake_resp")
    },
    resp_status = function(resp) resp$status,
    resp_body_string = function(resp) resp$body_text,
    .package = "httr2"
  )

  expect_error(
    provider$create_conversation(),
    "401"
  )
})

test_that("get_conversation / delete_conversation validate the id argument", {
  provider <- safe_create_provider(create_openai)
  expect_error(provider$get_conversation(""), "non-empty string")
  expect_error(provider$delete_conversation(NULL), "non-empty string")
})

test_that("reasoning_effort enum accepts none/minimal/xhigh and rejects typos", {
  expect_error(
    aisdk:::normalize_model_runtime_options(list(call_options = list(reasoning_effort = "nope"))),
    "reasoning_effort"
  )
  for (v in c("none", "minimal", "low", "medium", "high", "xhigh")) {
    opts <- aisdk:::normalize_model_runtime_options(list(call_options = list(reasoning_effort = v)))
    expect_equal(opts$call_options$reasoning_effort, v)
  }
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

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://api.openai.com/v1"
  )
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

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://api.openai.com/v1"
  )
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

test_that("OpenAI image responses fallback forwards image params into the tool config", {
  skip_on_ci()
  skip_on_cran()

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://proxy.example/v1"
  )
  model <- provider$image_model("gpt-image-1.5")
  fallback_body <- NULL

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      if (grepl("/images/generations$", url)) {
        rlang::abort("HTTP 404 invalid_api_path: images/generations not available on this endpoint")
      }
      fallback_body <<- body
      list(
        id = "resp_first",
        output = list(list(
          type = "image_generation_call",
          result = base64enc::base64encode(charToRaw("webp-bytes")),
          revised_prompt = "neon"
        ))
      )
    },
    .package = "aisdk"
  )

  result <- suppressMessages(generate_image(
    model = model,
    prompt = "Draw a green triangle",
    output_dir = tempdir(),
    quality = "high",
    output_format = "webp",
    background = "transparent",
    output_compression = 80,
    moderation = "low"
  ))

  expect_equal(fallback_body$model, "gpt-image-1.5")
  expect_equal(fallback_body$input, "Draw a green triangle")
  expect_length(fallback_body$tools, 1)
  tool <- fallback_body$tools[[1]]
  expect_equal(tool$type, "image_generation")
  expect_equal(tool$model, "gpt-image-1.5")
  expect_equal(tool$quality, "high")
  expect_equal(tool$output_format, "webp")
  expect_equal(tool$output_compression, 80)
  expect_equal(tool$background, "transparent")
  expect_equal(tool$moderation, "low")
  expect_null(fallback_body$previous_response_id)
  expect_equal(result$images[[1]]$media_type, "image/webp")
})

test_that("OpenAI image responses fallback injects previous_response_id on multi-turn", {
  skip_on_ci()
  skip_on_cran()

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://proxy.example/v1"
  )
  model <- provider$image_model("gpt-image-1.5")
  call_log <- list()
  next_id <- "resp_first"

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      if (grepl("/images/generations$", url)) {
        rlang::abort("HTTP 404 invalid_api_path: not available")
      }
      call_log[[length(call_log) + 1]] <<- body
      out <- list(
        id = next_id,
        output = list(list(
          type = "image_generation_call",
          result = base64enc::base64encode(charToRaw("img-bytes"))
        ))
      )
      next_id <<- "resp_second"
      out
    },
    .package = "aisdk"
  )

  suppressMessages(generate_image(model = model, prompt = "a cat", output_dir = tempdir()))
  expect_equal(model$get_last_response_id(), "resp_first")
  expect_null(call_log[[1]]$previous_response_id)

  suppressMessages(generate_image(model = model, prompt = "now make it realistic", output_dir = tempdir()))
  expect_equal(call_log[[2]]$previous_response_id, "resp_first")
  expect_equal(model$get_last_response_id(), "resp_second")

  model$reset()
  expect_null(model$get_last_response_id())
})

test_that("OpenAI image responses fallback omits unsupported fields from tool config", {
  skip_on_ci()
  skip_on_cran()

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://proxy.example/v1"
  )
  model <- provider$image_model("gpt-image-1.5")
  fallback_body <- NULL

  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      if (grepl("/images/generations$", url)) {
        rlang::abort("HTTP 404 invalid_api_path: images/generations not available")
      }
      fallback_body <<- body
      list(
        id = "resp_xyz",
        output = list(list(
          type = "image_generation_call",
          result = base64enc::base64encode(charToRaw("ok"))
        ))
      )
    },
    .package = "aisdk"
  )

  suppressMessages(generate_image(
    model = model,
    prompt = "a tree",
    output_dir = tempdir(),
    response_format = "b64_json",
    timeout_seconds = 30
  ))

  tool <- fallback_body$tools[[1]]
  expect_null(tool$response_format)
  expect_null(tool$timeout_seconds)
  expect_null(tool$output_dir)
  expect_null(tool$prompt)
  expect_null(fallback_body$response_format)
})

test_that("OpenAI image model posts multipart edit payload and parses images", {
  skip_on_ci()
  skip_on_cran()

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://api.openai.com/v1"
  )
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

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://api.openai.com/v1"
  )
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

test_that("stream_image emits partial events, captures final image, and chains response id", {
  skip_on_ci()
  skip_on_cran()

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://api.openai.com/v1"
  )
  model <- provider$image_model("gpt-image-1.5")

  captured_body <- NULL
  events <- list()

  testthat::local_mocked_bindings(
    stream_responses_api = function(url, headers, body, callback, ...) {
      captured_body <<- body
      # Synthetic SSE: 2 partials, one per-call completion with the final result,
      # then a response.completed envelope with usage and the response id.
      callback("response.created", list(response = list(id = "resp_stream_1")), done = FALSE)
      for (i in 1:2) {
        callback(
          "response.image_generation_call.partial_image",
          list(
            partial_image_index = i,
            partial_image_b64 = base64enc::base64encode(charToRaw(paste0("preview-", i)))
          ),
          done = FALSE
        )
      }
      callback(
        "response.image_generation_call.completed",
        list(result = base64enc::base64encode(charToRaw("final-img")), revised_prompt = "shiny"),
        done = FALSE
      )
      callback(
        "response.completed",
        list(response = list(id = "resp_stream_1", usage = list(total_tokens = 42))),
        done = FALSE
      )
      callback(NULL, NULL, done = TRUE)
    },
    .package = "aisdk"
  )

  result <- stream_image(
    model = model,
    prompt = "Draw a glowing nebula",
    callback = function(event) {
      events[[length(events) + 1]] <<- event
    },
    output_dir = tempdir(),
    partial_images = 2,
    quality = "high",
    output_format = "png"
  )

  # Body shape
  expect_true(captured_body$stream)
  expect_equal(captured_body$input, "Draw a glowing nebula")
  expect_equal(captured_body$tools[[1]]$type, "image_generation")
  expect_identical(captured_body$tools[[1]]$partial_images, 2L)
  expect_equal(captured_body$tools[[1]]$quality, "high")

  # Callback event sequence: 2 partial + 1 completed
  expect_length(events, 3)
  expect_equal(events[[1]]$type, "partial")
  expect_identical(events[[1]]$index, 1L)
  expect_equal(rawToChar(events[[1]]$bytes), "preview-1")
  expect_equal(events[[2]]$type, "partial")
  expect_identical(events[[2]]$index, 2L)
  expect_equal(events[[3]]$type, "completed")
  expect_true(events[[3]]$done)
  expect_equal(rawToChar(events[[3]]$bytes), "final-img")

  # Result
  expect_length(result$images, 1)
  expect_equal(rawToChar(result$images[[1]]$bytes), "final-img")
  expect_equal(result$images[[1]]$media_type, "image/png")
  expect_equal(result$usage$total_tokens, 42)

  # Response id captured for chaining
  expect_equal(model$get_last_response_id(), "resp_stream_1")
})

test_that("stream_image second call attaches previous_response_id", {
  skip_on_ci()
  skip_on_cran()

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://api.openai.com/v1"
  )
  model <- provider$image_model("gpt-image-1.5")
  bodies <- list()
  next_id <- "resp_a"

  testthat::local_mocked_bindings(
    stream_responses_api = function(url, headers, body, callback, ...) {
      bodies[[length(bodies) + 1]] <<- body
      callback("response.image_generation_call.completed",
               list(result = base64enc::base64encode(charToRaw("img"))),
               done = FALSE)
      callback("response.completed",
               list(response = list(id = next_id)),
               done = FALSE)
      callback(NULL, NULL, done = TRUE)
      next_id <<- "resp_b"
    },
    .package = "aisdk"
  )

  noop <- function(event) invisible(NULL)
  stream_image(model, "a cat", callback = noop, output_dir = tempdir(), partial_images = 0)
  stream_image(model, "now neon", callback = noop, output_dir = tempdir(), partial_images = 0)

  expect_null(bodies[[1]]$previous_response_id)
  expect_equal(bodies[[2]]$previous_response_id, "resp_a")
  # partial_images = 0 -> tool config does not include the field
  expect_null(bodies[[1]]$tools[[1]]$partial_images)
})

test_that("stream_image rejects out-of-range partial_images and non-function callback", {
  skip_on_ci()

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://api.openai.com/v1"
  )
  model <- provider$image_model("gpt-image-1.5")

  expect_error(
    stream_image(model, "x", callback = "not a function"),
    "callback"
  )
  expect_error(
    stream_image(model, "x", callback = function(e) NULL, partial_images = 99),
    "0\\.\\.3"
  )
})

test_that("stream_image errors when the stream completes without a final image", {
  skip_on_ci()

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://api.openai.com/v1"
  )
  model <- provider$image_model("gpt-image-1.5")

  testthat::local_mocked_bindings(
    stream_responses_api = function(url, headers, body, callback, ...) {
      # Only partials, no completion event
      callback("response.image_generation_call.partial_image",
               list(partial_image_b64 = base64enc::base64encode(charToRaw("p"))),
               done = FALSE)
      callback(NULL, NULL, done = TRUE)
    },
    .package = "aisdk"
  )

  expect_error(
    stream_image(model, "x", callback = function(e) NULL, output_dir = tempdir()),
    "no final image"
  )
})

test_that("OpenAI image edit falls back to Responses API on 404 invalid_api_path", {
  skip_on_ci()
  skip_on_cran()

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://proxy.example/v1"
  )
  model <- provider$image_model("gpt-image-1.5")
  fallback_body <- NULL

  src_path <- tempfile(fileext = ".png")
  writeBin(charToRaw("source-png-bytes"), src_path)
  on.exit(unlink(src_path), add = TRUE)

  testthat::local_mocked_bindings(
    post_multipart_to_api = function(url, headers, body, ...) {
      rlang::abort("HTTP 404 invalid_api_path: images/edits not available on this endpoint")
    },
    post_to_api = function(url, headers, body, ...) {
      fallback_body <<- body
      list(
        id = "resp_edit_first",
        output = list(list(
          type = "image_generation_call",
          result = base64enc::base64encode(charToRaw("edited-via-responses"))
        ))
      )
    },
    .package = "aisdk"
  )

  result <- suppressMessages(edit_image(
    model = model,
    image = src_path,
    prompt = "Add a flamingo",
    output_dir = tempdir(),
    quality = "high",
    output_format = "webp",
    input_fidelity = "high"
  ))

  expect_equal(fallback_body$model, "gpt-image-1.5")
  expect_length(fallback_body$input, 1)
  user_msg <- fallback_body$input[[1]]
  expect_equal(user_msg$role, "user")
  expect_equal(user_msg$content[[1]]$type, "input_text")
  expect_equal(user_msg$content[[1]]$text, "Add a flamingo")
  expect_equal(user_msg$content[[2]]$type, "input_image")
  expect_match(user_msg$content[[2]]$image_url, "^data:image/png;base64,")

  tool <- fallback_body$tools[[1]]
  expect_equal(tool$type, "image_generation")
  expect_equal(tool$action, "edit")
  expect_equal(tool$quality, "high")
  expect_equal(tool$output_format, "webp")
  expect_equal(tool$input_fidelity, "high")
  expect_null(tool$input_image_mask)

  expect_equal(rawToChar(result$images[[1]]$bytes), "edited-via-responses")
  expect_equal(result$images[[1]]$media_type, "image/webp")
  expect_equal(model$get_last_response_id(), "resp_edit_first")
})

test_that("OpenAI image edit fallback inlines mask into input_image_mask", {
  skip_on_ci()
  skip_on_cran()

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://proxy.example/v1"
  )
  model <- provider$image_model("gpt-image-1.5")
  fallback_body <- NULL

  src_path <- tempfile(fileext = ".png")
  mask_path <- tempfile(fileext = ".png")
  writeBin(charToRaw("source-bytes"), src_path)
  writeBin(charToRaw("mask-bytes"), mask_path)
  on.exit(unlink(c(src_path, mask_path)), add = TRUE)

  testthat::local_mocked_bindings(
    post_multipart_to_api = function(url, headers, body, ...) {
      rlang::abort("HTTP 404 invalid_api_path: images/edits not available")
    },
    post_to_api = function(url, headers, body, ...) {
      fallback_body <<- body
      list(
        id = "resp_edit_masked",
        output = list(list(
          type = "image_generation_call",
          result = base64enc::base64encode(charToRaw("masked-out"))
        ))
      )
    },
    .package = "aisdk"
  )

  suppressMessages(edit_image(
    model = model,
    image = src_path,
    mask = mask_path,
    prompt = "Replace the masked area with a flamingo",
    output_dir = tempdir()
  ))

  tool <- fallback_body$tools[[1]]
  expect_equal(tool$action, "edit")
  expect_match(tool$input_image_mask$image_url, "^data:image/png;base64,")
})

test_that("OpenAI image edit fallback re-raises non-fallback errors untouched", {
  skip_on_ci()
  skip_on_cran()

  provider <- safe_create_provider(
    create_openai,
    api_key = "test-key",
    base_url = "https://api.openai.com/v1"
  )
  model <- provider$image_model("gpt-image-2")

  src_path <- tempfile(fileext = ".png")
  writeBin(charToRaw("src"), src_path)
  on.exit(unlink(src_path), add = TRUE)

  testthat::local_mocked_bindings(
    post_multipart_to_api = function(url, headers, body, ...) {
      rlang::abort("HTTP 401 invalid_api_key: bad token")
    },
    post_to_api = function(url, headers, body, ...) {
      stop("post_to_api should not be reached for non-404 errors")
    },
    .package = "aisdk"
  )

  expect_error(
    edit_image(model = model, image = src_path, prompt = "anything"),
    "invalid_api_key"
  )
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

test_that("OpenAI-compatible AiHubMix base_url omits response_format for image edit compatibility", {
  provider <- safe_create_provider(
    create_openai,
    api_key = "test_key",
    base_url = "https://aihubmix.com/v1"
  )
  model <- provider$image_model("gpt-image-2")
  captured_body <- NULL

  input_path <- tempfile(fileext = ".png")
  writeBin(charToRaw("fakepng"), input_path)
  on.exit(unlink(input_path), add = TRUE)

  captured_body <- model$.__enclos_env__$private$build_edit_body(list(
    image = input_path,
    prompt = "Make it cobalt blue",
    output_dir = tempdir()
  ))

  expect_false("response_format" %in% names(captured_body))
  expect_true("image" %in% names(captured_body))
})

test_that("OpenAI-compatible AiHubMix base_url maps generation width and transparency", {
  provider <- safe_create_provider(
    create_openai,
    api_key = "test_key",
    base_url = "https://aihubmix.com/v1"
  )
  model <- provider$image_model("gpt-image-2")

  captured_body <- model$.__enclos_env__$private$build_generation_body(list(
    prompt = "Draw a transparent wide hero figure",
    output_dir = tempdir(),
    width = 1536,
    height = 1024,
    transparent_background = TRUE
  ))

  expect_false("response_format" %in% names(captured_body))
  expect_equal(captured_body$size, "1536x1024")
  expect_equal(captured_body$background, "transparent")
  expect_false("width" %in% names(captured_body))
  expect_false("height" %in% names(captured_body))
  expect_false("transparent_background" %in% names(captured_body))
})

# --- Issue 1: reasoning models drop sampling params ------------------------

test_that("Chat Completions drops temperature/top_p for reasoning models", {
  provider <- safe_create_provider(create_openai, api_key = "sk-test")
  model <- provider$language_model("gpt-5")
  expect_true(model$has_capability("is_reasoning_model"))

  payload <- model$build_payload(list(
    messages = list(list(role = "user", content = "hi")),
    temperature = 0.7,
    top_p = 0.9,
    presence_penalty = 0.1,
    frequency_penalty = 0.1
  ))
  expect_null(payload$body$temperature)
  expect_null(payload$body$top_p)
  expect_null(payload$body$presence_penalty)
  expect_null(payload$body$frequency_penalty)
})

test_that("Chat Completions keeps sampling params for non-reasoning models", {
  provider <- safe_create_provider(create_openai, api_key = "sk-test")
  model <- provider$language_model("gpt-4o")
  expect_false(model$has_capability("is_reasoning_model"))

  payload <- model$build_payload(list(
    messages = list(list(role = "user", content = "hi")),
    temperature = 0.3,
    top_p = 0.5
  ))
  expect_equal(payload$body$temperature, 0.3)
  expect_equal(payload$body$top_p, 0.5)
})

test_that("Responses API auto-detects reasoning and drops temperature", {
  provider <- safe_create_provider(create_openai, api_key = "sk-test")
  m_reason <- provider$responses_model("gpt-5.4-mini")
  m_chat   <- provider$responses_model("some-non-reasoning")
  expect_true(m_reason$has_capability("is_reasoning_model"))
  expect_false(m_chat$has_capability("is_reasoning_model"))
})

# --- Issue 3: api_format on create_openai() --------------------------------

test_that("create_openai(api_format=) routes provider$model() to the right surface", {
  prov_auto <- create_openai(api_key = "sk-test", api_format = "auto")
  prov_chat <- create_openai(api_key = "sk-test", api_format = "chat")
  prov_resp <- create_openai(api_key = "sk-test", api_format = "responses")

  expect_s3_class(prov_auto$model("gpt-4o"),       "OpenAILanguageModel")
  expect_s3_class(prov_auto$model("gpt-5"),        "OpenAIResponsesLanguageModel")
  expect_s3_class(prov_chat$model("gpt-5"),        "OpenAILanguageModel")
  expect_s3_class(prov_resp$model("gpt-4o"),       "OpenAIResponsesLanguageModel")
})

# --- Responses API server-side state mode -----------------------------------
# Regression coverage for the "implicit statefulness" defect: the Responses
# adapter used to always resend the full history AND inject previous_response_id,
# which 400s on HTTP-stateless proxies and double-bills tokens on OpenAI.

test_that("responses_normalize_state_mode coerces unknown/empty values to stateless", {
  expect_equal(aisdk:::responses_normalize_state_mode(NULL), "stateless")
  expect_equal(aisdk:::responses_normalize_state_mode("server"), "server")
  expect_equal(aisdk:::responses_normalize_state_mode("AUTO"), "auto")
  expect_equal(aisdk:::responses_normalize_state_mode("nonsense"), "stateless")
})

test_that("previous_response_id retry detection is scoped to previous_response_id errors", {
  unrelated <- rlang::error_cnd(
    class = c("aisdk_api_compatibility_error", "aisdk_api_error"),
    message = "Unknown parameter: response_format"
  )
  previous_id <- rlang::error_cnd(
    class = c("aisdk_api_compatibility_error", "aisdk_api_error"),
    message = "previous_response_id is only supported on Responses WebSocket v2"
  )

  expect_false(aisdk:::responses_previous_id_rejected(unrelated))
  expect_true(aisdk:::responses_previous_id_rejected(previous_id))
  expect_true(aisdk:::responses_previous_id_unsupported(previous_id))
})

test_that("custom Responses provider defaults to stateless (no previous_response_id)", {
  provider <- create_custom_provider(
    provider_name = "p_stateless",
    base_url = "https://proxy.example/v1",
    api_key = "FAKE",
    api_format = "responses"
  )
  model <- provider$language_model("gpt-5.5")
  bodies <- list()
  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      bodies[[length(bodies) + 1]] <<- body
      list(id = paste0("resp_", length(bodies)),
           output = list(list(type = "message", content = list(list(text = "ok")))))
    },
    .package = "aisdk"
  )
  model$do_generate(list(messages = list(list(role = "user", content = "a"))))
  model$do_generate(list(messages = list(
    list(role = "user", content = "a"),
    list(role = "assistant", content = "ok"),
    list(role = "user", content = "b")
  )))
  # Even after a response id exists, stateless mode never injects it...
  expect_null(bodies[[2]]$previous_response_id)
  # ...and it resends the full transcript.
  expect_equal(length(bodies[[2]]$input), 3L)
  # The replayed assistant turn must use `output_text` (the Responses API
  # rejects assistant `input_text`), while user turns keep `input_text`.
  expect_equal(bodies[[2]]$input[[1]]$content[[1]]$type, "input_text")
  expect_equal(bodies[[2]]$input[[2]]$role, "assistant")
  expect_equal(bodies[[2]]$input[[2]]$content[[1]]$type, "output_text")
  expect_equal(bodies[[2]]$input[[2]]$content[[1]]$text, "ok")
})

test_that("stateless Responses tool loop replays function_call before function_call_output", {
  provider <- safe_create_provider(create_openai, api_key = "FAKE", responses_state_mode = "stateless")
  model <- provider$responses_model("gpt-5")
  echo <- tool(
    name = "echo",
    description = "Echo a message",
    parameters = z_object(message = z_string("Message")),
    execute = function(message) paste("Echo:", message)
  )

  bodies <- list()
  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      bodies[[length(bodies) + 1]] <<- body
      if (length(bodies) == 1L) {
        return(list(
          id = "resp_1",
          output = list(list(
            type = "function_call",
            id = "fc_1",
            call_id = "call_1",
            name = "echo",
            arguments = "{\"message\":\"hi\"}"
          ))
        ))
      }
      list(
        id = "resp_2",
        output = list(list(
          type = "message",
          content = list(list(text = "done"))
        ))
      )
    },
    .package = "aisdk"
  )

  result <- generate_text(
    model = model,
    prompt = "Use echo",
    tools = list(echo),
    max_steps = 2
  )

  expect_equal(result$text, "done")
  expect_null(bodies[[2]]$previous_response_id)
  expect_equal(vapply(bodies[[2]]$input, `[[`, character(1), "type"), c(
    "message",
    "function_call",
    "function_call_output"
  ))
  expect_equal(bodies[[2]]$input[[2]]$call_id, "call_1")
  expect_equal(bodies[[2]]$input[[2]]$name, "echo")
  expect_equal(bodies[[2]]$input[[2]]$arguments, "{\"message\":\"hi\"}")
  expect_equal(bodies[[2]]$input[[3]]$call_id, "call_1")
  expect_match(bodies[[2]]$input[[3]]$output, "Echo: hi", fixed = TRUE)
})

test_that("first-party Responses model (auto) chains via previous_response_id and sends only the new turn", {
  provider <- safe_create_provider(create_openai, api_key = "FAKE")
  model <- provider$responses_model("gpt-5")
  bodies <- list()
  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      bodies[[length(bodies) + 1]] <<- body
      list(id = paste0("resp_", length(bodies)),
           output = list(list(type = "message", content = list(list(text = "ok")))))
    },
    .package = "aisdk"
  )
  model$do_generate(list(messages = list(list(role = "user", content = "a"))))
  model$do_generate(list(messages = list(
    list(role = "user", content = "a"),
    list(role = "assistant", content = "ok"),
    list(role = "user", content = "b")
  )))
  expect_equal(bodies[[2]]$previous_response_id, "resp_1")
  # Only the trailing user turn is sent, not the whole history.
  expect_equal(length(bodies[[2]]$input), 1L)
  expect_equal(bodies[[2]]$input[[1]]$role, "user")
})

test_that("Responses model auto-degrades to stateless when endpoint rejects previous_response_id", {
  provider <- safe_create_provider(create_openai, api_key = "FAKE")
  model <- provider$responses_model("gpt-5")
  call_n <- 0
  captured <- list()
  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      call_n <<- call_n + 1
      if (call_n == 2) {
        rlang::abort(
          c("API request failed with status 400",
            "x" = "previous_response_id is only supported on Responses WebSocket v2"),
          class = c("aisdk_api_compatibility_error", "aisdk_api_error")
        )
      }
      captured[[length(captured) + 1]] <<- body
      list(id = paste0("resp_", call_n),
           output = list(list(type = "message", content = list(list(text = "ok")))))
    },
    .package = "aisdk"
  )
  # Turn 1 succeeds and records a response id.
  model$do_generate(list(messages = list(list(role = "user", content = "hi"))))
  # Turn 2: first attempt injects previous_response_id -> 400 -> auto-retry w/o it.
  hist2 <- list(
    list(role = "user", content = "hi"),
    list(role = "assistant", content = "ok"),
    list(role = "user", content = "again")
  )
  res2 <- model$do_generate(list(messages = hist2))
  expect_s3_class(res2, "GenerateResult")
  # The successful retry resends history and carries no previous_response_id.
  retry_body <- captured[[length(captured)]]
  expect_null(retry_body$previous_response_id)
  expect_equal(length(retry_body$input), 3L)
  # Turn 3 stays stateless within the session (no second 400).
  model$do_generate(list(messages = c(hist2, list(list(role = "user", content = "more")))))
  expect_null(captured[[length(captured)]]$previous_response_id)
})

test_that("ChatSession$reset_model_state resets Responses models but is a no-op for chat models", {
  resp_model <- safe_create_provider(create_openai, api_key = "FAKE")$responses_model("gpt-5")
  s1 <- ChatSession$new(model = resp_model)
  expect_true(s1$reset_model_state())

  chat_model <- safe_create_provider(create_openai, api_key = "FAKE")$language_model("gpt-4o")
  s2 <- ChatSession$new(model = chat_model)
  expect_false(s2$reset_model_state())
})

test_that("image Responses path respects state mode", {
  swallow <- function(expr) tryCatch(suppressWarnings(suppressMessages(expr)), error = function(e) NULL)

  # auto (first-party) chains edits via previous_response_id
  model_auto <- safe_create_provider(create_openai, api_key = "FAKE")$image_model("gpt-image-2")
  cap_auto <- list()
  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      cap_auto[[length(cap_auto) + 1]] <<- body
      list(id = "img_1", output = list())
    },
    .package = "aisdk"
  )
  swallow(model_auto$do_generate_image_via_responses(list(prompt = "a cat")))
  swallow(model_auto$do_generate_image_via_responses(list(prompt = "make it realistic")))
  expect_equal(cap_auto[[length(cap_auto)]]$previous_response_id, "img_1")

  # stateless never chains
  model_stateless <- create_openai(api_key = "FAKE", responses_state_mode = "stateless")$image_model("gpt-image-2")
  cap_sl <- list()
  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      cap_sl[[length(cap_sl) + 1]] <<- body
      list(id = "img_9", output = list())
    },
    .package = "aisdk"
  )
  swallow(model_stateless$do_generate_image_via_responses(list(prompt = "a cat")))
  swallow(model_stateless$do_generate_image_via_responses(list(prompt = "again")))
  expect_null(cap_sl[[length(cap_sl)]]$previous_response_id)
})
