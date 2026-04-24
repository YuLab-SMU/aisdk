# Tests for Stepfun Provider
library(testthat)
library(aisdk)

# Load helper functions (for environment variable handling)
helper_path <- file.path(test_path("helper-env.R"))
if (file.exists(helper_path)) {
    source(helper_path)
}

# ============================================================================
# Offline Tests (no API key required)
# ============================================================================

test_that("create_stepfun() creates a provider with correct defaults", {
    # Use safe provider creation if available
    if (exists("safe_create_provider")) {
        provider <- safe_create_provider(create_stepfun)
    } else {
        # Fallback to direct creation, suppressing the missing key warning
        provider <- suppressWarnings(create_stepfun())
    }

    expect_s3_class(provider, "StepfunProvider")
    expect_equal(provider$specification_version, "v1")
})

test_that("Stepfun provider creates language model correctly", {
    if (exists("safe_create_provider")) {
        provider <- safe_create_provider(create_stepfun)
    } else {
        provider <- suppressWarnings(create_stepfun())
    }

    model <- provider$language_model("step-1-8k")

    expect_s3_class(model, "StepfunLanguageModel")
    expect_equal(model$model_id, "step-1-8k")
    expect_equal(model$provider, "stepfun")
    expect_equal(model$specification_version, "v1")
})

test_that("Stepfun provider requires model_id if env not set", {
    if (exists("safe_create_provider")) {
        provider <- safe_create_provider(create_stepfun)
    } else {
        provider <- suppressWarnings(create_stepfun())
    }

    # Without STEPFUN_MODEL env var set, should fallback to default
    old_model <- Sys.getenv("STEPFUN_MODEL")
    Sys.setenv(STEPFUN_MODEL = "")
    on.exit(Sys.setenv(STEPFUN_MODEL = old_model))

    # According to our implementation, STEPFUN_MODEL defaults to "step-3.5-flash"
    model <- provider$language_model()
    expect_equal(model$model_id, "step-3.5-flash")
})

test_that("Stepfun provider creates image model correctly", {
    provider <- suppressWarnings(create_stepfun(api_key = "test_key"))
    model <- provider$image_model("step-1x-medium")

    expect_s3_class(model, "StepfunImageModel")
    expect_equal(model$model_id, "step-1x-medium")
    expect_equal(model$provider, "stepfun")
})

test_that("create_stepfun() warns when API key is missing", {
    # Temporarily unset API key
    old_key <- Sys.getenv("STEPFUN_API_KEY")
    Sys.setenv(STEPFUN_API_KEY = "")
    on.exit(Sys.setenv(STEPFUN_API_KEY = old_key))

    expect_warning(
        create_stepfun(),
        "Stepfun API key not set"
    )
})

# ============================================================================
# Live API Tests (only run when API key is available)
# ============================================================================

test_that("Stepfun provider can make real API calls", {
    if (exists("skip_if_no_api_key")) {
        skip_if_no_api_key("Stepfun")
    } else {
        if (nchar(Sys.getenv("STEPFUN_API_KEY")) == 0) {
            skip("No STEPFUN_API_KEY found")
        }
    }
    skip_on_cran()

    provider <- create_stepfun()
    model_id <- Sys.getenv("STEPFUN_MODEL", "step-1-8k")
    model <- provider$language_model(model_id)

    # Make a simple API call (use higher max_tokens for reasoning models like step-3.5-flash)
    result <- model$generate(
        messages = list(
            list(role = "user", content = "Say 'Hello, World!'")
        ),
        max_tokens = 200
    )

    # Check that we got a response
    expect_true(!is.null(result$text))
    expect_true(nchar(result$text) > 0)
})

test_that("Stepfun image model generates images and edits with step-1x-edit", {
    skip_on_cran()

    provider <- suppressWarnings(create_stepfun(api_key = "test_key"))
    gen_model <- provider$image_model("step-1x-medium")
    edit_model <- provider$image_model("step-1x-edit")
    captured_generation <- NULL
    captured_edit <- NULL

    input_path <- tempfile(fileext = ".png")
    writeBin(charToRaw("stepfun-image"), input_path)
    on.exit(unlink(input_path), add = TRUE)

    local_mocked_bindings(
        post_to_api = function(url, headers, body, ...) {
            captured_generation <<- body
            list(
                data = list(list(
                    image = base64enc::base64encode(charToRaw("stepfun-gen"))
                ))
            )
        },
        post_multipart_to_api = function(url, headers, body, ...) {
            captured_edit <<- body
            list(
                data = list(list(
                    image = base64enc::base64encode(charToRaw("stepfun-edit"))
                ))
            )
        }
    )

    generated <- generate_image(
        model = gen_model,
        prompt = "Draw a white mug",
        output_dir = tempdir()
    )
    edited <- edit_image(
        model = edit_model,
        image = input_path,
        prompt = "Make it cobalt blue",
        output_dir = tempdir()
    )

    expect_equal(captured_generation$model, "step-1x-medium")
    expect_equal(captured_generation$response_format, "b64_json")
    expect_true(!is.null(captured_edit$image))
    expect_equal(rawToChar(generated$images[[1]]$bytes), "stepfun-gen")
    expect_equal(rawToChar(edited$images[[1]]$bytes), "stepfun-edit")
})

test_that("Stepfun image edit rejects unsupported model and mask", {
    provider <- suppressWarnings(create_stepfun(api_key = "test_key"))

    expect_error(
        edit_image(
            model = provider$image_model("step-1x-medium"),
            image = tempfile(fileext = ".png"),
            prompt = "Edit this image"
        ),
        "requires the `step-1x-edit` model"
    )

    input_path <- tempfile(fileext = ".png")
    writeBin(charToRaw("stepfun-image"), input_path)
    on.exit(unlink(input_path), add = TRUE)

    expect_error(
        edit_image(
            model = provider$image_model("step-1x-edit"),
            image = input_path,
            mask = input_path,
            prompt = "Edit this image"
        ),
        "does not support `mask` yet"
    )
})
