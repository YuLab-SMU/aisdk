# Tests for DeepSeek Provider
library(testthat)
library(aisdk)

# Load helper functions (for environment variable handling)
helper_path <- file.path(test_path("helper-env.R"))
source(helper_path)

deepseek_model <- Sys.getenv("DEEPSEEK_MODEL", "deepseek-chat")

test_that("create_deepseek() creates a provider with correct defaults", {
    # Use safe provider creation
    provider <- safe_create_provider(create_deepseek)

    expect_s3_class(provider, "DeepSeekProvider")
    expect_equal(provider$specification_version, "v1")
})

test_that("DeepSeek provider creates language model correctly", {
    provider <- safe_create_provider(create_deepseek)
    model <- provider$language_model(deepseek_model)

    expect_s3_class(model, "DeepSeekLanguageModel")
    expect_equal(model$model_id, deepseek_model)
    expect_equal(model$provider, "deepseek")
    expect_equal(model$specification_version, "v1")
})

test_that("DeepSeek provider uses default model when none specified", {
    provider <- safe_create_provider(create_deepseek)
    model <- provider$language_model()

    expect_s3_class(model, "DeepSeekLanguageModel")
    # Default is deepseek-chat
    expect_equal(model$model_id, "deepseek-chat")
})

test_that("create_deepseek() accepts custom base_url", {
    provider <- safe_create_provider(create_deepseek,
        base_url = "https://custom.deepseek.com"
    )
    model <- provider$language_model(deepseek_model)

    # Model should be created successfully
    expect_s3_class(model, "DeepSeekLanguageModel")
})

test_that("create_deepseek() warns when API key is missing", {
    # Temporarily unset API key
    old_key <- Sys.getenv("DEEPSEEK_API_KEY")
    Sys.setenv(DEEPSEEK_API_KEY = "")
    on.exit(Sys.setenv(DEEPSEEK_API_KEY = old_key))

    expect_warning(
        create_deepseek(),
        "DeepSeek API key not set"
    )
})

# Live API tests (only run when API key is available)
test_that("DeepSeek provider can make real API calls", {
    skip_if_no_api_key("DeepSeek")
    skip_on_cran()

    provider <- create_deepseek()
    model <- provider$language_model("deepseek-chat")

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

test_that("DeepSeek reasoner model returns reasoning content", {
    skip_if_no_api_key("DeepSeek")
    skip_on_cran()

    provider <- create_deepseek()
    model <- provider$language_model("deepseek-reasoner")

    # Make a call that should trigger reasoning
    result <- model$generate(
        messages = list(
            list(role = "user", content = "What is 15 * 23? Think step by step.")
        ),
        max_tokens = 500
    )

    # Check that we got a response
    expect_true(!is.null(result$text))
    expect_true(nchar(result$text) > 0)

    # Reasoning content should be present for deepseek-reasoner
    # Note: This may be NULL if the model doesn't return reasoning for simple queries
    # So we just check that the field exists
    expect_true("reasoning" %in% names(result))
})

test_that("DeepSeek provider handles tool calls", {
    skip_if_no_api_key("DeepSeek")
    skip_on_cran()

    provider <- create_deepseek()
    model <- provider$language_model("deepseek-chat")

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

# ============================================================================
# DeepSeek Anthropic API Tests
# ============================================================================

test_that("create_deepseek_anthropic() creates an Anthropic provider with DeepSeek config", {
    # Use safe provider creation
    provider <- safe_create_provider(create_deepseek_anthropic)

    expect_s3_class(provider, "AnthropicProvider")
    expect_equal(provider$specification_version, "v1")
})

test_that("DeepSeek Anthropic provider creates language model correctly", {
    provider <- safe_create_provider(create_deepseek_anthropic)
    model <- provider$language_model("deepseek-chat")

    expect_s3_class(model, "AnthropicLanguageModel")
    expect_equal(model$model_id, "deepseek-chat")
    expect_equal(model$provider, "deepseek")
})

test_that("create_deepseek_anthropic() warns when API key is missing", {
    # Temporarily unset API key
    old_key <- Sys.getenv("DEEPSEEK_API_KEY")
    old_anthropic_key <- Sys.getenv("ANTHROPIC_API_KEY")
    Sys.setenv(DEEPSEEK_API_KEY = "")
    Sys.setenv(ANTHROPIC_API_KEY = "")
    on.exit({
        Sys.setenv(DEEPSEEK_API_KEY = old_key)
        Sys.setenv(ANTHROPIC_API_KEY = old_anthropic_key)
    })

    expect_warning(
        create_deepseek_anthropic(),
        "Anthropic API key not set"
    )
})
