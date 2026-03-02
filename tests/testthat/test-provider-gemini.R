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

test_that("create_gemini() accepts custom base_url", {
    provider <- safe_create_provider(create_gemini,
        api_key = "FAKE",
        base_url = gemini_base_url
    )
    model <- provider$language_model(gemini_model)

    # Model should be created successfully
    expect_s3_class(model, "GeminiLanguageModel")
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
