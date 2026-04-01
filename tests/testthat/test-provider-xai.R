# Tests for xAI Provider
library(testthat)
library(aisdk)

# Load helper functions (for environment variable handling)
# assuming it exists in the test folder
helper_path <- file.path(test_path("helper-env.R"))
if (file.exists(helper_path)) {
    source(helper_path)
}

# ============================================================================
# Offline Tests (no API key required)
# ============================================================================

test_that("create_xai() creates a provider with correct defaults", {
    # Use safe provider creation if available
    if (exists("safe_create_provider")) {
        provider <- safe_create_provider(create_xai)
    } else {
        # Fallback to direct creation, suppressing the missing key warning
        provider <- suppressWarnings(create_xai())
    }

    expect_s3_class(provider, "XAIProvider")
    expect_equal(provider$specification_version, "v1")
})

test_that("xAI provider creates language model correctly", {
    if (exists("safe_create_provider")) {
        provider <- safe_create_provider(create_xai)
    } else {
        provider <- suppressWarnings(create_xai())
    }

    model <- provider$language_model("grok-4-1-fast-reasoning")

    expect_s3_class(model, "XAILanguageModel")
    expect_equal(model$model_id, "grok-4-1-fast-reasoning")
    expect_equal(model$provider, "xai")
    expect_equal(model$specification_version, "v1")
})

test_that("xAI provider requires model_id if env not set", {
    if (exists("safe_create_provider")) {
        provider <- safe_create_provider(create_xai)
    } else {
        provider <- suppressWarnings(create_xai())
    }

    # Without XAI_MODEL env var set, should fallback to default or error
    old_model <- Sys.getenv("XAI_MODEL")
    Sys.setenv(XAI_MODEL = "")
    on.exit(Sys.setenv(XAI_MODEL = old_model))

    # According to our implementation, XAI_MODEL defaults to "grok-beta"
    model <- provider$language_model()
    expect_equal(model$model_id, "grok-beta")
})

test_that("xAI provider creates image model correctly", {
    provider <- suppressWarnings(create_xai(api_key = "test_key"))
    model <- provider$image_model("grok-2-image")

    expect_s3_class(model, "XAIImageModel")
    expect_equal(model$model_id, "grok-2-image")
    expect_equal(model$provider, "xai")
})

test_that("create_xai() warns when API key is missing", {
    # Temporarily unset API key
    old_key <- Sys.getenv("XAI_API_KEY")
    Sys.setenv(XAI_API_KEY = "")
    on.exit(Sys.setenv(XAI_API_KEY = old_key))

    expect_warning(
        create_xai(),
        "xAI API key not set"
    )
})

# ============================================================================
# Live API Tests (only run when API key is available)
# ============================================================================

test_that("xAI provider can make real API calls", {
    if (exists("skip_if_no_api_key")) {
        skip_if_no_api_key("xAI")
    } else {
        if (nchar(Sys.getenv("XAI_API_KEY")) == 0) {
            skip("No XAI_API_KEY found")
        }
    }
    skip_on_cran()

    provider <- create_xai()
    model_id <- Sys.getenv("XAI_MODEL", "grok-beta")
    model <- provider$language_model(model_id)

    # Make a simple API call
    result <- model$generate(
        messages = list(
            list(role = "user", content = "Say 'Hello, World!'")
        ),
        max_tokens = 10
    )

    # Check that we got a response
    expect_true(!is.null(result$text))
    expect_true(nchar(result$text) > 0)
})

test_that("xAI image model generates and edits images with JSON payloads", {
    provider <- suppressWarnings(create_xai(api_key = "test_key"))
    model <- provider$image_model("grok-2-image")
    captured_generation <- NULL
    captured_edit <- NULL

    local_mocked_bindings(
        post_to_api = function(url, headers, body, ...) {
            if (grepl("/images/generations$", url)) {
                captured_generation <<- body
                return(list(
                    data = list(list(
                        b64_json = base64enc::base64encode(charToRaw("xai-gen"))
                    ))
                ))
            }
            captured_edit <<- body
            list(
                data = list(list(
                    b64_json = base64enc::base64encode(charToRaw("xai-edit"))
                ))
            )
        }
    )

    generated <- generate_image(
        model = model,
        prompt = "Draw a cobalt blue mug",
        output_dir = tempdir()
    )
    edited <- edit_image(
        model = model,
        image = "https://example.com/source.png",
        prompt = "Make it watercolor",
        output_dir = tempdir()
    )

    expect_equal(captured_generation$model, "grok-2-image")
    expect_equal(captured_generation$response_format, "b64_json")
    expect_equal(captured_edit$image$type, "image_url")
    expect_equal(captured_edit$image$url, "https://example.com/source.png")
    expect_equal(rawToChar(generated$images[[1]]$bytes), "xai-gen")
    expect_equal(rawToChar(edited$images[[1]]$bytes), "xai-edit")
})
