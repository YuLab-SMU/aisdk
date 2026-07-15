# Tests for Gemini Provider
library(testthat)
library(aisdk)

# Load helper functions (for environment variable handling)
helper_path <- file.path(test_path("helper-env.R"))
if (file.exists(helper_path)) {
    source(helper_path)
}

# Provide defaults if helper-env doesn't define them
if (!exists("get_gemini_model")) {
    get_gemini_model <- function() "gemini-2.5-flash"
}
if (!exists("get_gemini_base_url")) {
    get_gemini_base_url <- function() "https://generativelanguage.googleapis.com/v1beta/models"
}
if (!exists("safe_create_provider")) {
    safe_create_provider <- function(constructor, ...) {
        res <- tryCatch(constructor(...), warning = function(w) constructor(...))
        if (!inherits(res, "R6")) res <- constructor(...)
        res
    }
}
if (!exists("skip_if_no_api_key")) {
    skip_if_no_api_key <- function(provider_name) {
        env_var <- paste0(toupper(provider_name), "_API_KEY")
        key <- Sys.getenv(env_var)
        if (nchar(key) == 0) {
            skip(paste(provider_name, "API key not found, skipping tests."))
        }
    }
}

gemini_model <- get_gemini_model()
gemini_base_url <- get_gemini_base_url()

test_that("create_gemini() creates a provider with correct defaults", {
    # Use safe provider creation
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")

    expect_s3_class(provider, "GeminiProvider")
    expect_equal(provider$specification_version, "v1")
})

test_that("Gemini provider creates language model correctly", {
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")
    model <- provider$language_model(gemini_model)

    expect_s3_class(model, "GeminiLanguageModel")
    expect_equal(model$model_id, gemini_model)
    expect_equal(model$provider, "gemini")
    expect_equal(model$specification_version, "v1")
})

test_that("Gemini provider creates image model correctly", {
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")
    model <- provider$image_model("gemini-2.5-flash-image")

    expect_s3_class(model, "GeminiImageModel")
    expect_equal(model$model_id, "gemini-2.5-flash-image")
    expect_equal(model$provider, "gemini")
})

test_that("Gemini text-to-image does not misroute image_config as a source image", {
    # W2 regression: `params$image` used to partial-match the documented
    # `image_config` option (no `image` key on text-to-image), pushing the
    # config list into input_image() and aborting the call.
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")
    model <- provider$image_model("gemini-2.5-flash-image")

    payload <- model$.__enclos_env__$private$build_payload(list(
        prompt = "a cat",
        output_dir = tempdir(),
        image_config = list(aspectRatio = "16:9")
    ))

    parts <- payload$body$contents[[1]]$parts
    # Only the text prompt part — no inlineData part fabricated from the config.
    expect_equal(length(parts), 1L)
    expect_equal(parts[[1]]$text, "a cat")
    expect_false(any(vapply(parts, function(p) !is.null(p$inlineData), logical(1))))
})

test_that("create_gemini() accepts custom base_url", {
    provider <- safe_create_provider(create_gemini,
        api_key = "FAKE",
        base_url = gemini_base_url
    )
    model <- provider$language_model(gemini_model)

    # Model should be created successfully
    expect_s3_class(model, "GeminiLanguageModel")
})

test_that("Gemini provider stores multiple base URLs for failover", {
    provider <- safe_create_provider(create_gemini,
        api_key = "FAKE",
        base_url = "https://primary.example/v1beta/models, https://backup.example/v1beta"
    )
    model <- provider$language_model(gemini_model)
    config <- model$get_config()

    expect_equal(config$base_url, "https://primary.example/v1beta/models")
    expect_equal(config$base_urls, c(
        "https://primary.example/v1beta/models",
        "https://backup.example/v1beta"
    ))
})

test_that("create_gemini() warns when API key is missing", {
    # Temporarily unset API key
    old_key <- Sys.getenv("GEMINI_API_KEY")
    Sys.setenv(GEMINI_API_KEY = "")
    on.exit(Sys.setenv(GEMINI_API_KEY = old_key))

    expect_warning(
        create_gemini(),
        "Gemini API key not set"
    )
})

# Offline Payload Verification
test_that("Gemini payload builder forms correctly", {
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")
    model <- provider$language_model(gemini_model)

    payload <- model$build_payload_internal(list(
        messages = list(
            list(role = "system", content = "You are an assistant."),
            list(role = "user", content = "Hello!")
        ),
        temperature = 0.5,
        max_tokens = 100
    ), stream = FALSE)

    body <- payload$body
    expect_true(!is.null(body$contents))
    expect_equal(body$contents[[1]]$role, "user")
    expect_equal(body$contents[[1]]$parts[[1]]$text, "Hello!")

    expect_true(!is.null(body$systemInstruction))
    expect_equal(body$systemInstruction$parts[[1]]$text, "You are an assistant.")

    expect_true(!is.null(body$generationConfig))
    expect_equal(body$generationConfig$temperature, 0.5)
    expect_equal(body$generationConfig$maxOutputTokens, 100)
})

test_that("Gemini payload builder emits candidate URLs for failover", {
    provider <- safe_create_provider(create_gemini,
        api_key = "FAKE",
        base_url = "https://primary.example/v1beta/models, https://backup.example/v1beta"
    )
    model <- provider$language_model("gemini-test")

    payload <- model$build_payload_internal(list(
        messages = list(list(role = "user", content = "Hello!"))
    ), stream = FALSE)

    expect_equal(payload$url, c(
        "https://primary.example/v1beta/models/gemini-test:generateContent?key=FAKE",
        "https://backup.example/v1beta/models/gemini-test:generateContent?key=FAKE"
    ))
})

test_that("Gemini payload builder translates multimodal content blocks", {
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")
    model <- provider$language_model(gemini_model)

    payload <- model$build_payload_internal(list(
        messages = list(
            list(
                role = "user",
                content = list(
                    list(type = "input_text", text = "Describe this image"),
                    list(
                        type = "input_image",
                        source = list(kind = "data_uri"),
                        data_uri = paste0(
                            "data:image/png;base64,",
                            base64enc::base64encode(charToRaw("fake-image"))
                        ),
                        media_type = "image/png",
                        detail = "high"
                    )
                )
            )
        )
    ), stream = FALSE)

    parts <- payload$body$contents[[1]]$parts
    expect_equal(parts[[1]]$text, "Describe this image")
    expect_equal(parts[[2]]$inlineData$mimeType, "image/png")
    expect_true(nzchar(parts[[2]]$inlineData$data))
})

test_that("Gemini payload builder passes image generation config through generationConfig", {
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")
    model <- provider$language_model(gemini_model)

    payload <- model$build_payload_internal(list(
        messages = list(
            list(role = "user", content = "Generate an image")
        ),
        response_modalities = c("TEXT", "IMAGE"),
        image_config = list(aspectRatio = "1:1")
    ), stream = FALSE)

    expect_equal(payload$body$generationConfig$responseModalities, c("TEXT", "IMAGE"))
    expect_equal(payload$body$generationConfig$imageConfig$aspectRatio, "1:1")
})

test_that("Gemini payload builder handles custom tool objects like google_search", {
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")
    model <- provider$language_model(gemini_model)

    payload <- model$build_payload_internal(list(
        messages = list(
            list(role = "user", content = "Hello!")
        ),
        tools = list(
            list(google_search = list())
        )
    ), stream = FALSE)

    body <- payload$body
    expect_true(!is.null(body$tools))
    expect_equal(length(body$tools), 1)
    expect_true("google_search" %in% names(body$tools[[1]]))
})

test_that("Gemini payload builder handles function declarations and custom tools together", {
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")
    model <- provider$language_model(gemini_model)

    # Mock tool
    mock_tool <- structure(list(
        to_api_format = function(fmt) {
            list(`function` = list(name = "test_func", description = "test", parameters = list(type = "object")))
        }
    ), class = "Tool")

    payload <- model$build_payload_internal(list(
        messages = list(
            list(role = "user", content = "Hello!")
        ),
        tools = list(
            list(google_search = list()),
            mock_tool
        )
    ), stream = FALSE)

    body <- payload$body
    expect_true(!is.null(body$tools))
    expect_equal(length(body$tools), 2)
    # Check that one is functionDeclarations and one is google_search
    tool_names <- unlist(lapply(body$tools, names))
    expect_true("functionDeclarations" %in% tool_names)
    expect_true("google_search" %in% tool_names)
})

test_that("Gemini parser preserves inline image parts on GenerateResult", {
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")
    model <- provider$language_model(gemini_model)

    response <- list(
        candidates = list(list(
            finishReason = "STOP",
            content = list(parts = list(
                list(text = "Rendered image."),
                list(inlineData = list(
                    mimeType = "image/png",
                    data = base64enc::base64encode(charToRaw("png-bytes"))
                ))
            ))
        )),
        usageMetadata = list(
            promptTokenCount = 10,
            candidatesTokenCount = 5,
            totalTokenCount = 15
        )
    )

    local_mocked_bindings(
        post_to_api = function(url, headers, body, ...) response
    )

    result <- model$do_generate(list(
        messages = list(
            list(role = "user", content = "Generate an image")
        )
    ))

    expect_equal(result$text, "Rendered image.")
    expect_equal(result$images[[1]]$media_type, "image/png")
    expect_equal(rawToChar(result$images[[1]]$bytes), "png-bytes")
})

test_that("Gemini image model writes image artifacts", {
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")
    model <- provider$image_model("gemini-2.5-flash-image")

    response <- list(
        candidates = list(list(
            finishReason = "STOP",
            content = list(parts = list(
                list(text = "done"),
                list(inlineData = list(
                    mimeType = "image/png",
                    data = base64enc::base64encode(charToRaw("png-bytes"))
                ))
            ))
        )),
        usageMetadata = list(
            promptTokenCount = 10,
            candidatesTokenCount = 5,
            totalTokenCount = 15
        )
    )

    local_mocked_bindings(
        post_to_api = function(url, headers, body, ...) response
    )

    out_dir <- tempfile("gemini-image-out-")
    result <- generate_image(
        model = model,
        prompt = "Generate a product photo",
        output_dir = out_dir
    )

    expect_equal(result$text, "done")
    expect_equal(result$images[[1]]$media_type, "image/png")
    expect_true(file.exists(result$images[[1]]$path))
})

test_that("Gemini image model saves inline image output to disk", {
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")
    model <- provider$image_model("gemini-2.5-flash-image")

    png_bytes <- as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))
    response <- list(
        candidates = list(list(
            finishReason = "STOP",
            content = list(parts = list(
                list(inlineData = list(
                    mimeType = "image/png",
                    data = base64enc::base64encode(png_bytes)
                )),
                list(text = "done")
            ))
        )),
        usageMetadata = list(
            promptTokenCount = 5,
            candidatesTokenCount = 7,
            totalTokenCount = 12
        )
    )

    captured_body <- NULL
    local_mocked_bindings(
        post_to_api = function(url, headers, body, ...) {
            captured_body <<- body
            response
        }
    )

    output_dir <- tempfile("gemini_images_")
    dir.create(output_dir, recursive = TRUE)
    on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

    result <- model$generate_image(
        prompt = "Generate a test image.",
        output_dir = output_dir
    )

    expect_equal(captured_body$generationConfig$responseModalities[[1]], "IMAGE")
    expect_equal(length(result$images), 1)
    expect_true(file.exists(result$images[[1]]$path))
    expect_equal(result$images[[1]]$media_type, "image/png")
    expect_equal(result$text, "done")
})

test_that("Gemini image edit sends prompt plus source image and writes output", {
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")
    model <- provider$image_model("gemini-2.5-flash-image")

    png_bytes <- as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))
    response <- list(
        candidates = list(list(
            finishReason = "STOP",
            content = list(parts = list(
                list(inlineData = list(
                    mimeType = "image/png",
                    data = base64enc::base64encode(png_bytes)
                )),
                list(text = "edited")
            ))
        ))
    )

    captured_body <- NULL
    local_mocked_bindings(
        post_to_api = function(url, headers, body, ...) {
            captured_body <<- body
            response
        }
    )

    output_dir <- tempfile("gemini_edit_")
    dir.create(output_dir, recursive = TRUE)
    on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

    result <- edit_image(
        model = model,
        image = "https://example.com/source.png",
        prompt = "Turn this into a watercolor illustration.",
        output_dir = output_dir
    )

    parts <- captured_body$contents[[1]]$parts
    expect_equal(parts[[1]]$text, "Turn this into a watercolor illustration.")
    expect_equal(parts[[2]]$fileData$fileUri, "https://example.com/source.png")
    expect_equal(length(result$images), 1)
    expect_true(file.exists(result$images[[1]]$path))
    expect_equal(result$text, "edited")
})

test_that("Gemini image edit rejects masks for now", {
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")
    model <- provider$image_model("gemini-2.5-flash-image")

    expect_error(
        edit_image(
            model = model,
            image = "https://example.com/source.png",
            prompt = "Edit this image",
            mask = "https://example.com/mask.png"
        ),
        "does not support `mask` yet"
    )
})

test_that("Gemini image model edit_image sends reference image content", {
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")
    model <- provider$image_model("gemini-2.5-flash-image")
    captured_body <- NULL

    local_mocked_bindings(
        post_to_api = function(url, headers, body, ...) {
            captured_body <<- body
            list(
                candidates = list(list(
                    finishReason = "STOP",
                    content = list(parts = list(
                        list(inlineData = list(
                            mimeType = "image/png",
                            data = base64enc::base64encode(charToRaw("edited-image"))
                        ))
                    ))
                ))
            )
        }
    )

    result <- model$edit_image(
        image = paste0(
            "data:image/png;base64,",
            base64enc::base64encode(charToRaw("source-image"))
        ),
        prompt = "Make the mug blue.",
        output_dir = tempdir()
    )

    expect_equal(captured_body$contents[[1]]$parts[[1]]$text, "Make the mug blue.")
    expect_equal(captured_body$contents[[1]]$parts[[2]]$inlineData$mimeType, "image/png")
    expect_true(file.exists(result$images[[1]]$path))
})

test_that("Gemini image model edit_image rejects masks for now", {
    provider <- safe_create_provider(create_gemini, api_key = "FAKE")
    model <- provider$image_model("gemini-2.5-flash-image")

    expect_error(
        model$edit_image(
            image = "https://example.com/cat.png",
            prompt = "Make it blue.",
            mask = "https://example.com/mask.png"
        ),
        "does not support `mask` yet"
    )
})

# Live API tests (only run when API key is available)
test_that("Gemini provider can make real API calls", {
    skip_if_no_api_key("Gemini")
    skip_on_cran()

    provider <- create_gemini()
    model <- provider$language_model(gemini_model)

    # Make a simple API call
    result <- model$generate(
        messages = list(
            list(role = "user", content = "Say 'Test OK'")
        ),
        max_tokens = 10
    )

    # Check that we got a response
    expect_true(!is.null(result$text))
    expect_true(nchar(result$text) > 0)
})

# --- W3: Gemini streaming parallel tool calls + history round-trip -----------

test_that("Gemini streaming keeps parallel tool calls in distinct slots", {
  # Regression: the mock index used length(candidates)-1 (always 0), so two
  # functionCall parts merged into one call with a concatenated name.
  agg <- SSEAggregator$new(function(text, done) invisible())
  mk <- function(name, args, idx) list(list(
    index = idx,
    id = sprintf("call_%s_%d", name, idx),
    `function` = list(name = name, arguments = jsonlite::toJSON(args, auto_unbox = TRUE))
  ))
  agg$on_tool_call_delta(mk("get_weather", list(city = "SF"), 0L))
  agg$on_tool_call_delta(mk("get_time", list(tz = "UTC"), 1L))
  res <- agg$build_result()

  expect_length(res$tool_calls, 2)
  expect_setequal(vapply(res$tool_calls, function(t) t$name, character(1)),
                  c("get_weather", "get_time"))
})

test_that("agent runtime attaches tool_calls for a Gemini assistant turn", {
  # Regression: history_format "gemini" had no runtime branch, so the model's
  # functionCall turn was dropped and the next turn 400'd.
  model <- safe_create_provider(create_gemini, api_key = "FAKE")$language_model("gemini-2.5-flash")
  tcs <- list(list(id = "c1", name = "get_weather", arguments = list(city = "SF")))
  res <- list(text = "", tool_calls = tcs, raw_response = NULL)
  msgs <- aisdk:::agent_runtime_append_provider_messages(list(), model, res, list())$messages
  assistant <- msgs[[1]]
  expect_length(assistant$tool_calls, 1)
  expect_equal(assistant$tool_calls[[1]]$name, "get_weather")
  expect_null(assistant$content) # pure tool-call turn: no empty text part
})

# --- AK1: reasoning_effort -> Gemini thinkingConfig.thinkingBudget -----------
# Completes portable reasoning across providers: OpenAI Responses handles
# reasoning_effort natively and Anthropic maps it to a thinking budget (AA1);
# Gemini previously dropped it silently.

test_that("reasoning_effort maps to a Gemini thinking budget in generationConfig", {
  model <- create_gemini(api_key = "FAKE")$language_model("gemini-2.5-flash")
  msgs <- list(list(role = "user", content = "hi"))
  gc <- function(p) model$build_payload_internal(p, stream = FALSE)$body$generationConfig

  expected <- list(low = 2048L, medium = 8192L, high = 16384L)
  for (eff in names(expected)) {
    cfg <- gc(list(messages = msgs, reasoning_effort = eff))
    expect_equal(cfg$thinkingConfig$thinkingBudget, expected[[eff]])
  }
  # No effort / unrecognized effort -> no thinkingConfig; raw param never leaks.
  expect_null(gc(list(messages = msgs))$thinkingConfig)
  expect_null(gc(list(messages = msgs, reasoning_effort = "turbo"))$thinkingConfig)
  body <- model$build_payload_internal(list(messages = msgs, reasoning_effort = "high"),
                                       stream = FALSE)$body
  expect_null(body$reasoning_effort)
})

test_that("an explicit thinking_config wins and budgets are option-overridable", {
  model <- create_gemini(api_key = "FAKE")$language_model("gemini-2.5-flash")
  msgs <- list(list(role = "user", content = "hi"))
  gc <- function(p) model$build_payload_internal(p, stream = FALSE)$body$generationConfig

  won <- gc(list(messages = msgs, reasoning_effort = "high",
                 thinking_config = list(thinkingBudget = 999, includeThoughts = TRUE)))
  expect_equal(won$thinkingConfig$thinkingBudget, 999)
  expect_true(won$thinkingConfig$includeThoughts)

  withr::with_options(
    list(aisdk.gemini_reasoning_budgets = list(low = 100L, medium = 200L, high = 300L)),
    expect_equal(gc(list(messages = msgs, reasoning_effort = "medium"))$thinkingConfig$thinkingBudget, 200L)
  )
})
