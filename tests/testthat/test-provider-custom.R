test_that("Custom provider validates inputs", {
    expect_error(
        create_custom_provider(provider_name = "", base_url = "https://api.example.com"),
        "`provider_name` must be a non-empty string."
    )

    expect_error(
        create_custom_provider(provider_name = "test", base_url = ""),
        "`base_url` must be a valid URL string."
    )

    expect_error(
        create_custom_provider(provider_name = "test", base_url = "https://api.example.com", api_format = "invalid_format"),
        "'arg' should be one of "
    )
})

test_that("Custom provider correctly routes to base classes", {
    # Chat Completions
    p1 <- create_custom_provider(
        "custom1",
        "https://api.test1.com",
        api_format = "chat_completions"
    )

    m1 <- p1$language_model("model-1")
    expect_s3_class(m1, "OpenAILanguageModel")

    # Responses API
    p2 <- create_custom_provider(
        "custom2",
        "https://api.test2.com",
        api_format = "responses"
    )

    m2 <- p2$language_model("model-2")
    expect_s3_class(m2, "OpenAIResponsesLanguageModel")

    # Anthropic Messages API
    p3 <- create_custom_provider(
        "custom3",
        "https://api.test3.com",
        api_format = "anthropic_messages"
    )

    m3 <- p3$language_model("model-3")
    expect_s3_class(m3, "AnthropicLanguageModel")
})

test_that("Custom provider injects capabilities correctly", {
    p_default <- create_custom_provider(
        "test",
        "https://api.example.com",
        use_max_completion_tokens = FALSE
    )
    m_default <- p_default$language_model("model-x")

    expect_false(m_default$has_capability("is_reasoning_model"))

    p_reasoning <- create_custom_provider(
        "test",
        "https://api.example.com",
        use_max_completion_tokens = TRUE
    )
    m_reasoning <- p_reasoning$language_model("model-y")

    expect_true(m_reasoning$has_capability("is_reasoning_model"))
})

test_that("Custom provider overrides endpoint correctly", {
    p <- create_custom_provider(
        "test_endpoint",
        "https://api.custom-endpoint.com/v1",
        api_key = "test-key"
    )
    m <- p$language_model("test-model")

    # Test the private configuration directly, to ensure base_url was set
    # instead of decaying to default environment-level defaults.
    # Since the underlying `config` logic is abstracted, we can check
    # the build_payload or url generation logic directly.

    payload <- m$build_payload(list(
        messages = list(list(role = "user", content = "test"))
    ))

    expect_equal(payload$url, "https://api.custom-endpoint.com/v1/chat/completions")
    expect_equal(payload$headers[["Authorization"]], "Bearer test-key")
})

test_that("Custom provider integrates with Registry", {
    registry <- ProviderRegistry$new()
    p <- create_custom_provider("my_test", "https://api.test.com")

    registry$register("my_test", p)

    m <- registry$language_model("my_test:custom-model-abc")
    expect_s3_class(m, "OpenAILanguageModel")
})
