# Tests for Bailian (Alibaba Cloud DashScope) Provider
library(testthat)
library(aisdk)

# Load helper functions (for environment variable handling)
helper_path <- file.path(test_path("helper-env.R"))
source(helper_path)

# ============================================================================
# Offline Tests (no API key required)
# ============================================================================

test_that("create_bailian() creates a provider with correct defaults", {
    provider <- safe_create_provider(create_bailian)

    expect_s3_class(provider, "BailianProvider")
    expect_equal(provider$specification_version, "v1")
})

test_that("Bailian provider creates language model correctly", {
    provider <- safe_create_provider(create_bailian)
    model <- provider$language_model("qwen-plus")

    expect_s3_class(model, "BailianLanguageModel")
    expect_equal(model$model_id, "qwen-plus")
    expect_equal(model$provider, "bailian")
    expect_equal(model$specification_version, "v1")
})

test_that("Bailian provider uses default model when none specified", {
    provider <- safe_create_provider(create_bailian)
    model <- provider$language_model()

    expect_s3_class(model, "BailianLanguageModel")
    # Default is qwen-plus
    expect_equal(model$model_id, "qwen-plus")
})

test_that("create_bailian() accepts custom base_url", {
    provider <- safe_create_provider(create_bailian,
        base_url = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1"
    )
    model <- provider$language_model("qwen-plus")

    expect_s3_class(model, "BailianLanguageModel")
})

test_that("create_bailian() warns when API key is missing", {
    old_key <- Sys.getenv("DASHSCOPE_API_KEY")
    Sys.setenv(DASHSCOPE_API_KEY = "")
    on.exit(Sys.setenv(DASHSCOPE_API_KEY = old_key))

    expect_warning(
        create_bailian(),
        "DashScope API key not set"
    )
})

test_that("Bailian provider inherits responses_model and smart_model", {
    provider <- safe_create_provider(create_bailian)

    expect_true(!is.null(provider$responses_model))
    expect_true(is.function(provider$responses_model))
    expect_true(!is.null(provider$smart_model))
    expect_true(is.function(provider$smart_model))
})

# ============================================================================
# Live API Tests (only run when API key is available)
# ============================================================================

test_that("Bailian provider can make real API calls", {
    skip_if_no_api_key("Bailian")
    skip_on_cran()

    provider <- create_bailian()
    model_id <- Sys.getenv("DASHSCOPE_MODEL", "qwen-plus")
    model <- provider$language_model(model_id)

    result <- model$generate(
        messages = list(
            list(role = "user", content = "Say 'Hello, World!'")
        ),
        max_tokens = 10
    )

    expect_true(!is.null(result$text))
    expect_true(nchar(result$text) > 0)
})

test_that("Bailian provider handles tool calls", {
    skip_if_no_api_key("Bailian")
    skip_on_cran()

    provider <- create_bailian()
    model_id <- Sys.getenv("DASHSCOPE_MODEL", "qwen-plus")
    model <- provider$language_model(model_id)

    test_tool <- Tool$new(
        name = "get_time",
        description = "Get the current time",
        parameters = z_object(.dummy = z_string("Unused")),
        execute = function(args) {
            paste0("Current time: ", Sys.time())
        }
    )

    result <- model$generate(
        messages = list(
            list(role = "user", content = "What time is it?")
        ),
        tools = list(test_tool),
        max_tokens = 50
    )

    expect_true(!is.null(result$text) || !is.null(result$tool_calls))
})
