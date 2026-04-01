# Tests for Volcengine Provider
library(testthat)
library(aisdk)

# Load helper functions (for environment variable handling)
helper_path <- file.path(test_path("helper-env.R"))
source(helper_path)

# ============================================================================
# Offline Tests (no API key required)
# ============================================================================

test_that("create_volcengine() creates a provider with correct defaults", {
    # Use safe provider creation
    provider <- safe_create_provider(create_volcengine)

    expect_s3_class(provider, "VolcengineProvider")
    expect_equal(provider$specification_version, "v1")
})

test_that("Volcengine provider creates language model correctly", {
    provider <- safe_create_provider(create_volcengine)
    model <- provider$language_model("doubao-1-5-pro-256k-250115")

    expect_s3_class(model, "VolcengineLanguageModel")
    expect_equal(model$model_id, "doubao-1-5-pro-256k-250115")
    expect_equal(model$provider, "volcengine")
    expect_equal(model$specification_version, "v1")
})

test_that("Volcengine provider creates image model correctly", {
    provider <- safe_create_provider(create_volcengine)
    model <- provider$image_model("doubao-seedream-5-0")

    expect_s3_class(model, "VolcengineImageModel")
    expect_equal(model$model_id, "doubao-seedream-5-0")
    expect_equal(model$provider, "volcengine")
})

test_that("Volcengine provider requires model_id", {
    provider <- safe_create_provider(create_volcengine)

    # Without ARK_MODEL env var set, should error
    old_model <- Sys.getenv("ARK_MODEL")
    Sys.setenv(ARK_MODEL = "")
    on.exit(Sys.setenv(ARK_MODEL = old_model))

    expect_error(provider$language_model(), "Model ID not provided")
})

test_that("create_volcengine() accepts custom base_url", {
    provider <- safe_create_provider(create_volcengine,
        base_url = "https://custom.volcengine.com/api/v3"
    )
    model <- provider$language_model("doubao-1-5-pro-256k-250115")

    # Model should be created successfully
    expect_s3_class(model, "VolcengineLanguageModel")
})

test_that("create_volcengine() warns when API key is missing", {
    # Temporarily unset API key
    old_key <- Sys.getenv("ARK_API_KEY")
    Sys.setenv(ARK_API_KEY = "")
    on.exit(Sys.setenv(ARK_API_KEY = old_key))

    expect_warning(
        create_volcengine(),
        "Volcengine API key not set"
    )
})

test_that("Volcengine provider inherits responses_model and smart_model", {
    provider <- safe_create_provider(create_volcengine)

    # responses_model should be available (inherited from OpenAIProvider)
    expect_true(!is.null(provider$responses_model))
    expect_true(is.function(provider$responses_model))

    # smart_model should be available (inherited from OpenAIProvider)
    expect_true(!is.null(provider$smart_model))
    expect_true(is.function(provider$smart_model))
})

test_that("Volcengine image model posts generation payload and parses images", {
    provider <- safe_create_provider(create_volcengine)
    model <- provider$image_model("doubao-seedream-5-0")
    captured_body <- NULL

    local_mocked_bindings(
        post_to_api = function(url, headers, body, ...) {
            captured_body <<- body
            list(
                data = list(list(
                    b64_json = base64enc::base64encode(charToRaw("seedream-bytes"))
                ))
            )
        }
    )

    result <- generate_image(
        model = model,
        prompt = "A sleek editorial mug photo",
        output_dir = tempdir()
    )

    expect_equal(captured_body$model, "doubao-seedream-5-0")
    expect_equal(captured_body$response_format, "b64_json")
    expect_equal(rawToChar(result$images[[1]]$bytes), "seedream-bytes")
})

test_that("Volcengine image edit reuses generations endpoint with image inputs", {
    provider <- safe_create_provider(create_volcengine)
    model <- provider$image_model("doubao-seedream-5-0")
    captured_body <- NULL

    local_mocked_bindings(
        post_to_api = function(url, headers, body, ...) {
            captured_body <<- body
            list(
                data = list(list(
                    b64_json = base64enc::base64encode(charToRaw("edited-seedream"))
                ))
            )
        }
    )

    result <- edit_image(
        model = model,
        image = "https://example.com/source.png",
        prompt = "Turn this into a watercolor illustration",
        output_dir = tempdir()
    )

    expect_equal(captured_body$image, "https://example.com/source.png")
    expect_equal(captured_body$prompt, "Turn this into a watercolor illustration")
    expect_equal(rawToChar(result$images[[1]]$bytes), "edited-seedream")
})

test_that("Volcengine image edit rejects mask uploads for now", {
    provider <- safe_create_provider(create_volcengine)
    model <- provider$image_model("doubao-seedream-5-0")

    expect_error(
        edit_image(
            model = model,
            image = "https://example.com/source.png",
            mask = "https://example.com/mask.png",
            prompt = "Edit this image"
        ),
        "does not support `mask` yet"
    )
})

# ============================================================================
# Live API Tests (only run when API key is available)
# ============================================================================

test_that("Volcengine provider can make real API calls", {
    skip_if_no_api_key("Volcengine")
    skip_on_cran()

    provider <- create_volcengine()
    model_id <- Sys.getenv("ARK_MODEL", "doubao-1-5-pro-256k-250115")
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

test_that("Volcengine provider handles tool calls", {
    skip_if_no_api_key("Volcengine")
    skip_on_cran()

    provider <- create_volcengine()
    model_id <- Sys.getenv("ARK_MODEL", "doubao-1-5-pro-256k-250115")
    model <- provider$language_model(model_id)

    # Create a simple test tool
    test_tool <- Tool$new(
        name = "get_time",
        description = "Get the current time",
        parameters = z_object(.dummy = z_string("Unused")),
        execute = function(args) {
            paste0("Current time: ", Sys.time())
        }
    )

    # Call model with tool
    result <- model$generate(
        messages = list(
            list(role = "user", content = "What time is it?")
        ),
        tools = list(test_tool),
        max_tokens = 50
    )

    # Check response
    expect_true(!is.null(result$text) || !is.null(result$tool_calls))
})
