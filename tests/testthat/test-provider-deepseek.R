# Tests for DeepSeek Provider
library(testthat)
library(aisdk)
pkgload::load_all(export_all = FALSE, helpers = FALSE, quiet = TRUE)

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

test_that("DeepSeek v4 models are marked as reasoning-capable", {
    provider <- safe_create_provider(create_deepseek)

    for (model_id in c("deepseek-v4", "deepseek-v4-flash", "deepseek-v4-pro")) {
        model <- provider$language_model(model_id)
        expect_true(isTRUE(model$capabilities$is_reasoning_model), info = model_id)
        expect_true(isTRUE(model$capabilities$reasoning), info = model_id)
    }
})

test_that("DeepSeek provider forwards thinking-mode parameters", {
    provider <- suppressWarnings(create_deepseek(api_key = "test-key"))
    model <- provider$language_model("deepseek-v4")

    payload <- model$build_payload(list(
        messages = list(list(role = "user", content = "Hello")),
        thinking = TRUE,
        thinking_budget = 2048,
        reasoning_effort = "low",
        max_tokens = 1000,
        temperature = 0.2,
        top_p = 0.9,
        presence_penalty = 0.1,
        frequency_penalty = 0.2
    ))

    # logical TRUE is auto-converted to DeepSeek API format
    expect_equal(payload$body$thinking, list(type = "enabled"))
    expect_equal(payload$body$thinking_budget, 2048)
    expect_equal(payload$body$reasoning_effort, "high")
    expect_equal(payload$body$max_completion_tokens, 1000)
    expect_null(payload$body$max_tokens)
    expect_null(payload$body$temperature)
    expect_null(payload$body$top_p)
    expect_null(payload$body$presence_penalty)
    expect_null(payload$body$frequency_penalty)
})

test_that("DeepSeek reasoning_effort maps to supported thinking-mode levels", {
    provider <- suppressWarnings(create_deepseek(api_key = "test-key"))
    model <- provider$language_model("deepseek-v4")

    low_payload <- model$build_payload(list(
        messages = list(list(role = "user", content = "Hello")),
        reasoning_effort = "medium"
    ))
    max_payload <- model$build_payload(list(
        messages = list(list(role = "user", content = "Hello")),
        reasoning_effort = "xhigh"
    ))

    expect_equal(low_payload$body$reasoning_effort, "high")
    expect_equal(max_payload$body$reasoning_effort, "max")
})

test_that("DeepSeek chat model keeps sampling params unless thinking is enabled", {
    provider <- suppressWarnings(create_deepseek(api_key = "test-key"))
    model <- provider$language_model("deepseek-chat")

    plain_payload <- model$build_payload(list(
        messages = list(list(role = "user", content = "Hello")),
        temperature = 0.2
    ))
    thinking_payload <- model$build_payload(list(
        messages = list(list(role = "user", content = "Hello")),
        thinking = TRUE,
        temperature = 0.2
    ))

    expect_equal(plain_payload$body$temperature, 0.2)
    expect_null(thinking_payload$body$temperature)
})

test_that("DeepSeek thinking parameter accepts native API format", {
    provider <- suppressWarnings(create_deepseek(api_key = "test-key"))
    model <- provider$language_model("deepseek-v4")

    payload <- model$build_payload(list(
        messages = list(list(role = "user", content = "Hello")),
        thinking = list(type = "enabled"),
        max_tokens = 1000
    ))

    # native list format is passed through as-is
    expect_equal(payload$body$thinking, list(type = "enabled"))
})

test_that("DeepSeek thinking_budget does not partially match thinking", {
    provider <- suppressWarnings(create_deepseek(api_key = "test-key"))
    model <- provider$language_model("deepseek-v4")

    payload <- model$build_payload(list(
        messages = list(list(role = "user", content = "Hello")),
        thinking_budget = 2048
    ))

    expect_null(aisdk:::list_get_exact(payload$body, "thinking"))
    expect_equal(payload$body$thinking_budget, 2048)
})

test_that("DeepSeek stream payload forwards thinking-mode parameters", {
    provider <- suppressWarnings(create_deepseek(api_key = "test-key"))
    model <- provider$language_model("deepseek-v4")

    payload <- model$build_stream_payload(list(
        messages = list(list(role = "user", content = "Hello")),
        thinking = FALSE,
        thinking_budget = 512,
        reasoning_effort = "medium"
    ))

    # logical FALSE is auto-converted to DeepSeek API format
    expect_equal(payload$body$thinking, list(type = "disabled"))
    expect_equal(payload$body$thinking_budget, 512)
    expect_equal(payload$body$reasoning_effort, "high")
})

test_that("DeepSeek tool turns preserve reasoning_content when thinking is enabled", {
    provider <- suppressWarnings(create_deepseek(api_key = "test-key"))
    model <- provider$language_model("deepseek-v4-flash")

    test_tool <- Tool$new(
        name = "get_time",
        description = "Get the current time",
        parameters = z_object(.dummy = z_string("Unused")),
        execute = function(args) "12:00"
    )

    calls <- 0
    captured_bodies <- list()

    testthat::local_mocked_bindings(
        post_to_api = function(url, headers, body, ...) {
            calls <<- calls + 1
            captured_bodies[[calls]] <<- body

            if (calls == 1) {
                return(list(
                    choices = list(list(
                        message = list(
                            content = "",
                            reasoning_content = "Need the time tool.",
                            tool_calls = list(list(
                                id = "call_1",
                                type = "function",
                                `function` = list(
                                    name = "get_time",
                                    arguments = "{\".dummy\":\"unused\"}"
                                )
                            ))
                        ),
                        finish_reason = "tool_calls"
                    )),
                    usage = list(prompt_tokens = 8, completion_tokens = 4, total_tokens = 12)
                ))
            }

            list(
                choices = list(list(
                    message = list(
                        content = "The current time is 12:00.",
                        reasoning_content = NULL
                    ),
                    finish_reason = "stop"
                )),
                usage = list(prompt_tokens = 20, completion_tokens = 6, total_tokens = 26)
            )
        },
        .package = "aisdk"
    )

    result <- generate_text(
        model,
        "What time is it?",
        tools = list(test_tool),
        max_steps = 2,
        thinking = TRUE,
        max_tokens = 100
    )

    expect_equal(result$text, "The current time is 12:00.")
    expect_equal(calls, 2)

    assistant_message <- captured_bodies[[2]]$messages[[2]]
    expect_equal(assistant_message$role, "assistant")
    expect_equal(assistant_message$reasoning_content, "Need the time tool.")
    expect_length(assistant_message$tool_calls, 1)
    expect_equal(result$messages_added[[1]]$reasoning_content, "Need the time tool.")
    expect_equal(result$messages_added[[2]]$role, "tool")
    expect_equal(result$messages_added[[3]]$role, "assistant")
    expect_equal(result$messages_added[[3]]$content, "The current time is 12:00.")
})

test_that("DeepSeek ChatSession persists tool-turn reasoning_content for later replay", {
    provider <- suppressWarnings(create_deepseek(api_key = "test-key"))
    model <- provider$language_model("deepseek-v4-flash")

    test_tool <- Tool$new(
        name = "get_time",
        description = "Get the current time",
        parameters = z_object(.dummy = z_string("Unused")),
        execute = function(args) "12:00"
    )

    calls <- 0
    captured_bodies <- list()

    testthat::local_mocked_bindings(
        post_to_api = function(url, headers, body, ...) {
            calls <<- calls + 1
            captured_bodies[[calls]] <<- body

            if (calls == 1) {
                return(list(
                    choices = list(list(
                        message = list(
                            content = "",
                            reasoning_content = "Need the time tool.",
                            tool_calls = list(list(
                                id = "call_1",
                                type = "function",
                                `function` = list(
                                    name = "get_time",
                                    arguments = "{\".dummy\":\"unused\"}"
                                )
                            ))
                        ),
                        finish_reason = "tool_calls"
                    )),
                    usage = list(prompt_tokens = 8, completion_tokens = 4, total_tokens = 12)
                ))
            }

            if (calls == 2) {
                return(list(
                    choices = list(list(
                        message = list(
                            content = "The current time is 12:00.",
                            reasoning_content = NULL
                        ),
                        finish_reason = "stop"
                    )),
                    usage = list(prompt_tokens = 20, completion_tokens = 6, total_tokens = 26)
                ))
            }

            list(
                choices = list(list(
                    message = list(
                        content = "Earlier tool context is intact.",
                        reasoning_content = NULL
                    ),
                    finish_reason = "stop"
                )),
                usage = list(prompt_tokens = 30, completion_tokens = 5, total_tokens = 35)
            )
        },
        .package = "aisdk"
    )

    session <- create_chat_session(
        model = model,
        tools = list(test_tool),
        max_steps = 3
    )

    result <- session$send("What time is it?", thinking = TRUE, max_tokens = 100)
    expect_equal(result$text, "The current time is 12:00.")

    history <- session$get_history()
    expect_equal(vapply(history, `[[`, character(1), "role"), c("user", "assistant", "tool", "assistant"))
    expect_equal(history[[2]]$reasoning_content, "Need the time tool.")
    expect_length(history[[2]]$tool_calls, 1)

    session$send("Can you still answer?", thinking = TRUE, max_tokens = 100)
    replayed_assistant <- captured_bodies[[3]]$messages[[2]]
    expect_equal(replayed_assistant$role, "assistant")
    expect_equal(replayed_assistant$reasoning_content, "Need the time tool.")
    expect_length(replayed_assistant$tool_calls, 1)
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

test_that("DeepSeek provider forwards timeout_seconds to OpenAI-compatible requests", {
    provider <- suppressWarnings(create_deepseek(api_key = "test-key", timeout_seconds = 600))
    model <- provider$language_model("deepseek-chat")

    captured_timeout <- NULL

    testthat::local_mocked_bindings(
        post_to_api = function(url, headers, body, timeout_seconds = NULL, ...) {
            captured_timeout <<- timeout_seconds
            list(
                choices = list(list(
                    message = list(
                        content = "ok",
                        reasoning_content = NULL
                    ),
                    finish_reason = "stop"
                )),
                usage = list(prompt_tokens = 1, completion_tokens = 1, total_tokens = 2)
            )
        }
    )

    result <- model$do_generate(list(
        messages = list(list(role = "user", content = "Hello"))
    ))

    expect_equal(result$text, "ok")
    expect_equal(captured_timeout, 600)
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
