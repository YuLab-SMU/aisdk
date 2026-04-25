# Tests for AiHubMix Provider
library(testthat)
library(aisdk)

# Load helper functions (for environment variable handling)
helper_path <- file.path(test_path("helper-env.R"))
source(helper_path)

test_that("create_aihubmix() initializes correctly", {
    # Test with explicit arguments
    provider <- create_aihubmix(
        api_key = "test_key",
        base_url = "https://custom.aihubmix.com/v1"
    )

    expect_s3_class(provider, "AiHubMixProvider")
    expect_s3_class(provider, "OpenAIProvider")

    config <- provider$language_model("test-model")$get_config()
    expect_equal(config$api_key, "test_key")
    expect_equal(config$base_url, "https://custom.aihubmix.com/v1")
    expect_equal(config$provider_name, "aihubmix")
})

test_that("create_aihubmix() uses environment variables", {
    # Save current env vars
    old_key <- Sys.getenv("AIHUBMIX_API_KEY")
    old_url <- Sys.getenv("AIHUBMIX_BASE_URL")
    old_model <- Sys.getenv("AIHUBMIX_MODEL")

    # Set test env vars
    Sys.setenv(AIHUBMIX_API_KEY = "env_key")
    Sys.setenv(AIHUBMIX_BASE_URL = "https://env.aihubmix.com")
    Sys.setenv(AIHUBMIX_MODEL = "env-model")

    on.exit({
        Sys.setenv(AIHUBMIX_API_KEY = old_key)
        Sys.setenv(AIHUBMIX_BASE_URL = old_url)
        Sys.setenv(AIHUBMIX_MODEL = old_model)
    })

    provider <- create_aihubmix()
    model <- provider$language_model()
    config <- model$get_config()

    expect_equal(config$api_key, "env_key")
    expect_equal(config$base_url, "https://env.aihubmix.com")
    expect_equal(model$model_id, "env-model")
})

test_that("AiHubMix provider creates image model correctly", {
    provider <- create_aihubmix(
        api_key = "test_key",
        base_url = "https://custom.aihubmix.com/v1"
    )

    model <- provider$image_model("gpt-image-2")

    expect_s3_class(model, "OpenAIImageModel")
    expect_equal(model$model_id, "gpt-image-2")
    expect_equal(model$provider, "aihubmix")
})

test_that("AiHubMix image generation omits response_format for compatibility", {
    provider <- create_aihubmix(
        api_key = "test_key",
        base_url = "https://custom.aihubmix.com/v1"
    )
    model <- provider$image_model("gpt-image-2")
    captured_body <- NULL

    testthat::local_mocked_bindings(
        post_to_api = function(url, headers, body, ...) {
            captured_body <<- body
            list(
                created = 123,
                data = list(list(
                    b64_json = base64enc::base64encode(charToRaw("png-bytes"))
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
    expect_false("response_format" %in% names(captured_body))
    expect_equal(rawToChar(result$images[[1]]$bytes), "png-bytes")
})

test_that("AiHubMix image generation maps width and height into supported size", {
    provider <- create_aihubmix(
        api_key = "test_key",
        base_url = "https://custom.aihubmix.com/v1"
    )
    model <- provider$image_model("gpt-image-2")
    captured_body <- NULL

    testthat::local_mocked_bindings(
        post_to_api = function(url, headers, body, ...) {
            captured_body <<- body
            list(
                created = 123,
                data = list(list(
                    b64_json = base64enc::base64encode(charToRaw("png-bytes"))
                ))
            )
        },
        .package = "aisdk"
    )

    result <- generate_image(
        model = model,
        prompt = "Draw a wide hero figure",
        output_dir = tempdir(),
        width = 1536,
        height = 1024
    )

    expect_equal(captured_body$size, "1536x1024")
    expect_false("width" %in% names(captured_body))
    expect_false("height" %in% names(captured_body))
    expect_equal(rawToChar(result$images[[1]]$bytes), "png-bytes")
})

test_that("AiHubMix image generation maps transparent_background into background", {
    provider <- create_aihubmix(
        api_key = "test_key",
        base_url = "https://custom.aihubmix.com/v1"
    )
    model <- provider$image_model("gpt-image-2")
    captured_body <- NULL

    testthat::local_mocked_bindings(
        post_to_api = function(url, headers, body, ...) {
            captured_body <<- body
            list(
                created = 123,
                data = list(list(
                    b64_json = base64enc::base64encode(charToRaw("png-bytes"))
                ))
            )
        },
        .package = "aisdk"
    )

    result <- generate_image(
        model = model,
        prompt = "Draw a transparent icon",
        output_dir = tempdir(),
        width = 512,
        height = 512,
        transparent_background = TRUE
    )

    expect_equal(captured_body$size, "1024x1024")
    expect_equal(captured_body$background, "transparent")
    expect_false("transparent_background" %in% names(captured_body))
    expect_equal(rawToChar(result$images[[1]]$bytes), "png-bytes")
})

test_that("AiHubMix image edit omits response_format for compatibility", {
    provider <- create_aihubmix(
        api_key = "test_key",
        base_url = "https://custom.aihubmix.com/v1"
    )
    model <- provider$image_model("gpt-image-2")
    captured_body <- NULL

    image_path <- tempfile(fileext = ".png")
    writeBin(charToRaw("fakepng"), image_path)
    on.exit(unlink(image_path), add = TRUE)

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
        image = image_path,
        prompt = "Make it cobalt blue",
        output_dir = tempdir()
    )

    expect_equal(captured_body$model, "gpt-image-2")
    expect_false("response_format" %in% names(captured_body))
    expect_true(!is.null(captured_body$image))
    expect_equal(rawToChar(result$images[[1]]$bytes), "edited-bytes")
})

test_that("create_aihubmix() falls back to default base_url and model", {
    # Unset env vars
    old_key <- Sys.getenv("AIHUBMIX_API_KEY")
    old_url <- Sys.getenv("AIHUBMIX_BASE_URL")
    old_model <- Sys.getenv("AIHUBMIX_MODEL")
    old_warn <- getOption("warn")

    Sys.unsetenv("AIHUBMIX_API_KEY")
    Sys.unsetenv("AIHUBMIX_BASE_URL")
    Sys.unsetenv("AIHUBMIX_MODEL")
    options(warn = -1)

    on.exit({
        if (old_key != "") Sys.setenv(AIHUBMIX_API_KEY = old_key)
        if (old_url != "") Sys.setenv(AIHUBMIX_BASE_URL = old_url)
        if (old_model != "") Sys.setenv(AIHUBMIX_MODEL = old_model)
        options(warn = old_warn)
    })

    expect_warning(provider <- create_aihubmix(), "AiHubMix API key not set")
    model <- provider$language_model()
    config <- model$get_config()

    expect_equal(config$base_url, "https://aihubmix.com/v1")
    expect_warning(
        model <- provider$language_model(),
        NA
    )
    expect_equal(model$model_id, "claude-3-5-sonnet-20241022")
})

test_that("AiHubMixLanguageModel parses reasoning content correctly", {
    model <- AiHubMixLanguageModel$new("test-model", list(api_key = "test", provider_name = "aihubmix"))

    # Mocking standard Chat Completions response with reasoning_content
    mock_response <- list(
        id = "chatcmpl-123",
        object = "chat.completion",
        created = 1677652288,
        model = "claude-3-5-sonnet",
        choices = list(
            list(
                index = 0,
                message = list(
                    role = "assistant",
                    content = "Final response",
                    reasoning_content = "Thinking process..."
                ),
                finish_reason = "stop"
            )
        ),
        usage = list(
            prompt_tokens = 9,
            completion_tokens = 12,
            total_tokens = 21
        )
    )

    result <- model$parse_response(mock_response)

    expect_equal(result$text, "Final response")
    expect_equal(result$reasoning, "Thinking process...")
    expect_equal(result$finish_reason, "stop")
    expect_equal(result$usage$total_tokens, 21)
})

test_that("AiHubMixLanguageModel builds payload with extra params correctly", {
    model <- AiHubMixLanguageModel$new("test-model", list(api_key = "test", provider_name = "aihubmix", base_url = "https://aihubmix.com/v1"))

    params <- list(
        messages = list(list(role = "user", content = "Hello")),
        max_tokens = 1000,
        reasoning_effort = "low",
        budget_tokens = 1024
    )

    payload <- model$build_payload(params)

    expect_equal(payload$body$reasoning_effort, "low")
    expect_equal(payload$body$budget_tokens, 1024)
    expect_equal(payload$body$max_tokens, 1000)
    expect_equal(payload$body$model, "test-model")
})

test_that("AiHubMix text generation works (online)", {
    skip_if_no_api_key("AiHubMix")
    skip_on_cran()

    old_url <- Sys.getenv("AIHUBMIX_BASE_URL")
    Sys.setenv(AIHUBMIX_BASE_URL = "https://aihubmix.com/v1")
    on.exit({
        Sys.setenv(AIHUBMIX_BASE_URL = old_url)
    })

    aihubmix <- create_aihubmix()
    model <- aihubmix$language_model("claude-3-5-sonnet-20241022") # standard model for cheap test

    result <- generate_text(model, "Reply exactly with 'PONG'")

    expect_s3_class(result, "GenerateResult")
    expect_true(nchar(result$text) > 0)
    expect_true(grepl("PONG", result$text, ignore.case = TRUE))
    expect_false(is.null(result$usage))
})

# ============================================================================
# AiHubMix Anthropic & Gemini API Tests
# ============================================================================

test_that("create_aihubmix_anthropic() initializes correctly", {
    provider <- safe_create_provider(
        create_aihubmix_anthropic,
        extended_caching = TRUE
    )

    expect_s3_class(provider, "AnthropicProvider")
    model <- provider$language_model("claude-3-5-sonnet-20241022")
    config <- model$get_config()

    expect_equal(config$base_url, "https://aihubmix.com/v1")
    expect_equal(config$provider_name, "aihubmix")
    expect_true(config$enable_caching)
    # The header should contain the caching beta string
    headers <- environment(model$do_generate)$private$get_headers()
    expect_equal(headers$`anthropic-beta`, "extended-cache-ttl-2025-04-11")
})

test_that("create_aihubmix_gemini() initializes correctly", {
    provider <- safe_create_provider(create_aihubmix_gemini)

    expect_s3_class(provider, "GeminiProvider")
    model <- provider$language_model("gemini-2.5-flash")
    config <- model$get_config()

    expect_equal(config$base_url, "https://aihubmix.com/gemini/v1beta/models")
    expect_equal(config$provider_name, "aihubmix")
})
