# Tests for SSE Aggregator
library(testthat)
library(aisdk)

# ============================================================================
# SSEAggregator Core Tests
# ============================================================================

test_that("SSEAggregator accumulates text deltas", {
    chunks <- character()
    done_called <- FALSE

    agg <- SSEAggregator$new(function(text, done) {
        if (done) {
            done_called <<- TRUE
        } else {
            chunks <<- c(chunks, text)
        }
    })

    agg$on_text_delta("Hello")
    agg$on_text_delta(", ")
    agg$on_text_delta("World!")
    agg$on_done()

    result <- agg$finalize()

    expect_equal(result$text, "Hello, World!")
    expect_equal(chunks, c("Hello", ", ", "World!"))
    expect_true(done_called)
})

test_that("SSEAggregator handles reasoning transitions", {
    chunks <- character()

    agg <- SSEAggregator$new(function(text, done) {
        if (!done) chunks <<- c(chunks, text)
    })

    # Start reasoning
    agg$on_reasoning_delta("thinking...")
    # Transition to content (should auto-close reasoning)
    agg$on_text_delta("answer")
    agg$on_done()

    result <- agg$finalize()

    expect_equal(result$text, "answer")
    expect_equal(result$reasoning, "thinking...")
    # Verify callback sequence: <think> → thinking... → </think> → answer
    expect_equal(chunks[1], "<think>\n")
    expect_equal(chunks[2], "thinking...")
    expect_equal(chunks[3], "\n</think>\n\n")
    expect_equal(chunks[4], "answer")
})

test_that("SSEAggregator closes reasoning on done", {
    chunks <- character()
    done_called <- FALSE

    agg <- SSEAggregator$new(function(text, done) {
        if (done) {
            done_called <<- TRUE
        } else {
            chunks <<- c(chunks, text)
        }
    })

    agg$on_reasoning_delta("still thinking")
    agg$on_done()

    result <- agg$finalize()

    expect_true(done_called)
    # Reasoning should be closed before done callback
    expect_true("\n</think>\n\n" %in% chunks)
})

test_that("SSEAggregator handles reasoning_start and block_stop", {
    chunks <- character()

    agg <- SSEAggregator$new(function(text, done) {
        if (!done) chunks <<- c(chunks, text)
    })

    agg$on_reasoning_start()
    agg$on_reasoning_delta("deep thought")
    agg$on_block_stop() # Should close reasoning
    agg$on_text_delta("answer")
    agg$on_done()

    result <- agg$finalize()

    expect_equal(result$reasoning, "deep thought")
    expect_equal(result$text, "answer")
    # block_stop should have closed reasoning, so no double close on text_delta
    expect_equal(sum(chunks == "<think>\n"), 1)
    expect_equal(sum(chunks == "\n</think>\n\n"), 1)
})

test_that("SSEAggregator ignores empty text deltas", {
    chunks <- character()

    agg <- SSEAggregator$new(function(text, done) {
        if (!done) chunks <<- c(chunks, text)
    })

    agg$on_text_delta(NULL)
    agg$on_text_delta("")
    agg$on_reasoning_delta(NULL)
    agg$on_reasoning_delta("")
    agg$on_done()

    result <- agg$finalize()

    expect_equal(result$text, "")
    expect_equal(result$reasoning, "")
    expect_length(chunks, 0)
})

# ============================================================================
# Tool Call Tests — OpenAI Format
# ============================================================================

test_that("SSEAggregator accumulates OpenAI-format tool calls", {
    agg <- SSEAggregator$new(function(text, done) {})

    # First chunk: tool call start with id and name
    agg$on_tool_call_delta(list(
        list(
            index = 0,
            id = "call_123",
            `function` = list(name = "get_weather", arguments = '{"ci')
        )
    ))

    # Second chunk: continue arguments
    agg$on_tool_call_delta(list(
        list(
            index = 0,
            `function` = list(arguments = 'ty": "SF"}')
        )
    ))

    agg$on_finish_reason("tool_calls")
    result <- agg$finalize()

    expect_equal(length(result$tool_calls), 1)
    expect_equal(result$tool_calls[[1]]$id, "call_123")
    expect_equal(result$tool_calls[[1]]$name, "get_weather")
    expect_equal(result$tool_calls[[1]]$arguments$city, "SF")
    expect_equal(result$finish_reason, "tool_calls")
})

test_that("SSEAggregator handles multiple tool calls", {
    agg <- SSEAggregator$new(function(text, done) {})

    # Tool 1
    agg$on_tool_call_delta(list(
        list(index = 0, id = "call_1", `function` = list(name = "fn1", arguments = '{"a":1}'))
    ))
    # Tool 2
    agg$on_tool_call_delta(list(
        list(index = 1, id = "call_2", `function` = list(name = "fn2", arguments = '{"b":2}'))
    ))

    result <- agg$finalize()

    expect_equal(length(result$tool_calls), 2)
    expect_equal(result$tool_calls[[1]]$name, "fn1")
    expect_equal(result$tool_calls[[2]]$name, "fn2")
})

test_that("SSEAggregator handles tool calls with list arguments", {
    agg <- SSEAggregator$new(function(text, done) {})

    agg$on_tool_call_delta(list(
        list(
            index = 0,
            id = "call_x",
            `function` = list(name = "test_fn"),
            arguments = list(key = "value")
        )
    ))

    result <- agg$finalize()

    expect_equal(result$tool_calls[[1]]$arguments$key, "value")
})

test_that("SSEAggregator filters tool calls with empty names", {
    agg <- SSEAggregator$new(function(text, done) {})

    agg$on_tool_call_delta(list(
        list(index = 0, id = "call_1", `function` = list(name = "valid_fn", arguments = "{}"))
    ))
    # Tool with empty name
    agg$on_tool_call_delta(list(
        list(index = 1, id = "call_2", `function` = list(name = "", arguments = "{}"))
    ))

    result <- agg$finalize()

    expect_equal(length(result$tool_calls), 1)
    expect_equal(result$tool_calls[[1]]$name, "valid_fn")
})

# ============================================================================
# Tool Call Tests — Anthropic Format
# ============================================================================

test_that("SSEAggregator handles Anthropic-format tool calls", {
    agg <- SSEAggregator$new(function(text, done) {})

    # content_block_start with tool_use
    agg$on_tool_start(index = 0, id = "toolu_123", name = "get_weather")

    # input_json_delta chunks
    agg$on_tool_input_delta(0, '{"ci')
    agg$on_tool_input_delta(0, 'ty": ')
    agg$on_tool_input_delta(0, '"London"}')

    result <- agg$finalize()

    expect_equal(length(result$tool_calls), 1)
    expect_equal(result$tool_calls[[1]]$id, "toolu_123")
    expect_equal(result$tool_calls[[1]]$name, "get_weather")
    expect_equal(result$tool_calls[[1]]$arguments$city, "London")
})

# ============================================================================
# Metadata Tests
# ============================================================================

test_that("SSEAggregator stores usage and finish_reason", {
    agg <- SSEAggregator$new(function(text, done) {})

    agg$on_text_delta("Hi")
    agg$on_usage(list(prompt_tokens = 10, completion_tokens = 5, total_tokens = 15))
    agg$on_finish_reason("stop")
    agg$on_done()

    result <- agg$finalize()

    expect_equal(result$finish_reason, "stop")
    expect_equal(result$usage$prompt_tokens, 10)
    expect_equal(result$usage$total_tokens, 15)
})

test_that("SSEAggregator stores raw response", {
    agg <- SSEAggregator$new(function(text, done) {})

    raw <- list(id = "chatcmpl-123", model = "gpt-4o")
    agg$on_raw_response(raw)
    agg$on_done()

    result <- agg$finalize()

    expect_equal(result$raw_response$id, "chatcmpl-123")
})

# ============================================================================
# OpenAI Event Mapper Tests
# ============================================================================

test_that("map_openai_chunk handles text delta", {
    chunks <- character()
    agg <- SSEAggregator$new(function(text, done) {
        if (!done) chunks <<- c(chunks, text)
    })

    data <- list(
        choices = list(
            list(
                delta = list(content = "Hello"),
                finish_reason = NULL
            )
        )
    )

    map_openai_chunk(data, done = FALSE, agg)

    expect_equal(chunks, "Hello")
})

test_that("map_openai_chunk handles reasoning content", {
    chunks <- character()
    agg <- SSEAggregator$new(function(text, done) {
        if (!done) chunks <<- c(chunks, text)
    })

    data <- list(
        choices = list(
            list(
                delta = list(reasoning_content = "Let me think..."),
                finish_reason = NULL
            )
        )
    )

    map_openai_chunk(data, done = FALSE, agg)

    expect_equal(chunks[1], "<think>\n")
    expect_equal(chunks[2], "Let me think...")
})

test_that("map_openai_chunk handles done signal", {
    done_called <- FALSE
    agg <- SSEAggregator$new(function(text, done) {
        if (done) done_called <<- TRUE
    })

    map_openai_chunk(NULL, done = TRUE, agg)

    expect_true(done_called)
})

test_that("map_openai_chunk handles tool call deltas", {
    agg <- SSEAggregator$new(function(text, done) {})

    data <- list(
        choices = list(
            list(
                delta = list(
                    tool_calls = list(
                        list(index = 0, id = "call_1", `function` = list(name = "test", arguments = '{"x":1}'))
                    )
                ),
                finish_reason = "tool_calls"
            )
        ),
        usage = list(prompt_tokens = 5, completion_tokens = 3, total_tokens = 8)
    )

    map_openai_chunk(data, done = FALSE, agg)

    result <- agg$finalize()
    expect_equal(result$finish_reason, "tool_calls")
    expect_equal(result$tool_calls[[1]]$name, "test")
    expect_equal(result$usage$total_tokens, 8)
})

# ============================================================================
# Anthropic Event Mapper Tests
# ============================================================================

test_that("map_anthropic_chunk handles text_delta", {
    chunks <- character()
    agg <- SSEAggregator$new(function(text, done) {
        if (!done) chunks <<- c(chunks, text)
    })

    event_data <- list(
        delta = list(type = "text_delta", text = "Hello from Claude")
    )

    result <- map_anthropic_chunk("content_block_delta", event_data, agg)

    expect_false(result)
    expect_equal(chunks, "Hello from Claude")
})

test_that("map_anthropic_chunk handles thinking_delta", {
    chunks <- character()
    agg <- SSEAggregator$new(function(text, done) {
        if (!done) chunks <<- c(chunks, text)
    })

    event_data <- list(
        delta = list(type = "thinking_delta", thinking = "Considering...")
    )

    map_anthropic_chunk("content_block_delta", event_data, agg)

    expect_equal(chunks[1], "<think>\n")
    expect_equal(chunks[2], "Considering...")
})

test_that("map_anthropic_chunk handles input_json_delta", {
    agg <- SSEAggregator$new(function(text, done) {})

    # First: tool start
    start_data <- list(
        index = 0,
        content_block = list(type = "tool_use", id = "toolu_1", name = "search")
    )
    map_anthropic_chunk("content_block_start", start_data, agg)

    # Then: input deltas
    delta_data <- list(
        index = 0,
        delta = list(type = "input_json_delta", partial_json = '{"query": "test"}')
    )
    map_anthropic_chunk("content_block_delta", delta_data, agg)

    result <- agg$finalize()
    expect_equal(result$tool_calls[[1]]$name, "search")
    expect_equal(result$tool_calls[[1]]$arguments$query, "test")
})

test_that("map_anthropic_chunk handles thinking block lifecycle", {
    chunks <- character()
    agg <- SSEAggregator$new(function(text, done) {
        if (!done) chunks <<- c(chunks, text)
    })

    # content_block_start with thinking
    start_data <- list(
        index = 0,
        content_block = list(type = "thinking")
    )
    map_anthropic_chunk("content_block_start", start_data, agg)

    # thinking_delta
    delta_data <- list(
        delta = list(type = "thinking_delta", thinking = "Deep thought")
    )
    map_anthropic_chunk("content_block_delta", delta_data, agg)

    # content_block_stop closes thinking
    map_anthropic_chunk("content_block_stop", list(), agg)

    expect_equal(sum(chunks == "<think>\n"), 1)
    expect_true("Deep thought" %in% chunks)
    expect_equal(sum(chunks == "\n</think>\n\n"), 1)
})

test_that("map_anthropic_chunk handles message_delta with stop_reason", {
    agg <- SSEAggregator$new(function(text, done) {})

    event_data <- list(
        delta = list(stop_reason = "end_turn"),
        usage = list(output_tokens = 42)
    )

    map_anthropic_chunk("message_delta", event_data, agg)

    result <- agg$finalize()
    expect_equal(result$finish_reason, "end_turn")
    expect_equal(result$usage$completion_tokens, 42)
})

test_that("map_anthropic_chunk returns TRUE on message_stop", {
    done_called <- FALSE
    agg <- SSEAggregator$new(function(text, done) {
        if (done) done_called <<- TRUE
    })

    result <- map_anthropic_chunk("message_stop", list(), agg)

    expect_true(result)
    expect_true(done_called)
})

# ============================================================================
# Mixed Content Tests
# ============================================================================

test_that("SSEAggregator handles reasoning then text then tool calls", {
    chunks <- character()
    agg <- SSEAggregator$new(function(text, done) {
        if (!done) chunks <<- c(chunks, text)
    })

    # Reasoning phase
    agg$on_reasoning_delta("Let me think...")
    # Text phase (auto-closes reasoning)
    agg$on_text_delta("Based on my analysis, ")
    agg$on_text_delta("I need to call a tool.")
    # Tool call
    agg$on_tool_call_delta(list(
        list(index = 0, id = "call_1", `function` = list(name = "search", arguments = '{"q":"test"}'))
    ))
    agg$on_finish_reason("tool_calls")
    agg$on_done()

    result <- agg$finalize()

    expect_equal(result$reasoning, "Let me think...")
    expect_equal(result$text, "Based on my analysis, I need to call a tool.")
    expect_equal(result$finish_reason, "tool_calls")
    expect_equal(length(result$tool_calls), 1)
    expect_equal(result$tool_calls[[1]]$name, "search")
})

test_that("Full OpenAI streaming simulation via map_openai_chunk", {
    chunks <- character()
    done_called <- FALSE

    agg <- SSEAggregator$new(function(text, done) {
        if (done) {
            done_called <<- TRUE
        } else {
            chunks <<- c(chunks, text)
        }
    })

    # Simulate 3 SSE chunks
    map_openai_chunk(list(choices = list(list(delta = list(content = "Hi"), finish_reason = NULL))), FALSE, agg)
    map_openai_chunk(list(choices = list(list(delta = list(content = " there"), finish_reason = NULL))), FALSE, agg)
    map_openai_chunk(list(
        choices = list(list(delta = list(), finish_reason = "stop")),
        usage = list(prompt_tokens = 10, completion_tokens = 2, total_tokens = 12)
    ), FALSE, agg)
    map_openai_chunk(NULL, TRUE, agg)

    result <- agg$finalize()

    expect_equal(result$text, "Hi there")
    expect_equal(result$finish_reason, "stop")
    expect_equal(result$usage$total_tokens, 12)
    expect_true(done_called)
})
