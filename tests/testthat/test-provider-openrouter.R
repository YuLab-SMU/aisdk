# Tests for OpenRouter Provider
library(testthat)
library(aisdk)

# Load helper functions (for environment variable handling)
helper_path <- file.path(test_path("helper-env.R"))
source(helper_path)

# ============================================================================
# Offline Tests (no API key required)
# ============================================================================

test_that("create_openrouter() creates a provider with correct defaults", {
    provider <- safe_create_provider(create_openrouter)

    expect_s3_class(provider, "OpenRouterProvider")
    expect_equal(provider$specification_version, "v1")
})

test_that("OpenRouter provider creates language model correctly", {
    provider <- safe_create_provider(create_openrouter)
    model <- provider$language_model("openai/gpt-4o")

    expect_s3_class(model, "OpenRouterLanguageModel")
    expect_equal(model$model_id, "openai/gpt-4o")
    expect_equal(model$provider, "openrouter")
    expect_equal(model$specification_version, "v1")
})

test_that("OpenRouter provider requires model_id", {
    provider <- safe_create_provider(create_openrouter)

    old_model <- Sys.getenv("OPENROUTER_MODEL")
    Sys.setenv(OPENROUTER_MODEL = "")
    on.exit(Sys.setenv(OPENROUTER_MODEL = old_model))

    expect_error(provider$language_model(), "Model ID not provided")
})

test_that("create_openrouter() accepts custom base_url", {
    provider <- safe_create_provider(create_openrouter,
        base_url = "https://custom.openrouter.ai/api/v1"
    )
    model <- provider$language_model("openai/gpt-4o")

    expect_s3_class(model, "OpenRouterLanguageModel")
})

test_that("create_openrouter() warns when API key is missing", {
    old_key <- Sys.getenv("OPENROUTER_API_KEY")
    Sys.setenv(OPENROUTER_API_KEY = "")
    on.exit(Sys.setenv(OPENROUTER_API_KEY = old_key))

    expect_warning(
        create_openrouter(),
        "OpenRouter API key not set"
    )
})

test_that("OpenRouter provider inherits responses_model and smart_model", {
    provider <- safe_create_provider(create_openrouter)

    expect_true(!is.null(provider$responses_model))
    expect_true(is.function(provider$responses_model))
    expect_true(!is.null(provider$smart_model))
    expect_true(is.function(provider$smart_model))
})

# ============================================================================
# Live API Tests (only run when API key is available)
# ============================================================================

test_that("OpenRouter provider can make real API calls", {
    skip_if_no_api_key("OpenRouter")
    skip_on_cran()

    provider <- create_openrouter()
    model_id <- Sys.getenv("OPENROUTER_MODEL", "openai/gpt-4o-mini")
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

test_that("OpenRouter provider handles tool calls", {
    skip_if_no_api_key("OpenRouter")
    skip_on_cran()

    provider <- create_openrouter()
    model_id <- Sys.getenv("OPENROUTER_MODEL", "openai/gpt-4o-mini")
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
