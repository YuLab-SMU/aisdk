# Tests for Core API functions
library(testthat)
library(aisdk)

# Load helper functions (for environment variable handling)
helper_path <- file.path(test_path("helper-env.R"))
source(helper_path)

openai_model <- get_openai_model()
openai_model_id <- paste0("openai:", openai_model)

# === Tests for generate_text ===

test_that("generate_text accepts a model object", {
  provider <- suppressWarnings(create_openai(api_key = "test-key"))
  model <- provider$language_model(openai_model)
  captured_body <- NULL

  local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(
        choices = list(list(
          message = list(content = "Mocked hello"),
          finish_reason = "stop"
        )),
        usage = list(prompt_tokens = 1, completion_tokens = 1, total_tokens = 2)
      )
    },
    .package = "aisdk"
  )

  result <- generate_text(model = model, prompt = "Hello")

  expect_equal(result$text, "Mocked hello")
  expect_equal(captured_body$messages[[1]]$content[[1]]$type, "text")
  expect_equal(captured_body$messages[[1]]$content[[1]]$text, "Hello")
})

test_that("generate_text accepts a string model identifier", {
  registry <- ProviderRegistry$new()
  provider <- suppressWarnings(create_openai(api_key = "test-key"))
  registry$register("test-openai-fail", provider)

  model_id <- paste0("test-openai-fail:", openai_model)
  captured_body <- NULL

  local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_body <<- body
      list(
        choices = list(list(
          message = list(content = "Mocked via registry"),
          finish_reason = "stop"
        )),
        usage = list(prompt_tokens = 1, completion_tokens = 1, total_tokens = 2)
      )
    },
    .package = "aisdk"
  )

  result <- generate_text(model = model_id, prompt = "Hello", registry = registry)

  expect_equal(result$text, "Mocked via registry")
  expect_equal(captured_body$model, openai_model)
  expect_equal(captured_body$messages[[1]]$content[[1]]$type, "text")
  expect_equal(captured_body$messages[[1]]$content[[1]]$text, "Hello")
})

test_that("generate_text recovers tool calls embedded in assistant text", {
  tool_invocations <- list()
  echo_tool <- tool(
    name = "echo",
    description = "Echo a message",
    parameters = z_object(message = z_string("Message to echo")),
    execute = function(args) {
      tool_invocations[[length(tool_invocations) + 1L]] <<- args$message
      paste("Echo:", args$message)
    }
  )

  mock_model <- MockModel$new()
  mock_model$add_response(
    text = paste0(
      "<tool_call>\n",
      "{\"name\":\"echo\",\"arguments\":{\"message\":\"hello from text\"}}\n",
      "</tool_call>"
    ),
    tool_calls = NULL
  )
  mock_model$add_response(text = "Tool execution worked.")

  result <- generate_text(
    model = mock_model,
    prompt = "Say hello",
    tools = list(echo_tool),
    max_steps = 3
  )

  expect_equal(result$text, "Tool execution worked.")
  expect_equal(tool_invocations, list("hello from text"))
  expect_length(result$all_tool_calls, 1)
  expect_equal(result$all_tool_calls[[1]]$name, "echo")
  expect_equal(result$all_tool_calls[[1]]$arguments$message, "hello from text")
})

test_that("generate_text recovers plural text tool call containers", {
  tool_invocations <- list()
  echo_tool <- tool(
    name = "echo",
    description = "Echo a message",
    parameters = z_object(message = z_string("Message to echo")),
    execute = function(args) {
      tool_invocations[[length(tool_invocations) + 1L]] <<- args$message
      paste("Echo:", args$message)
    }
  )

  mock_model <- MockModel$new()
  mock_model$add_response(
    text = paste0(
      "我先跑一下。\n",
      "<tool_calls>\n",
      "[{\"name\":\"echo\",\"arguments\":{\"message\":\"plural\"}}]\n",
      "</tool_calls>"
    ),
    tool_calls = NULL
  )
  mock_model$add_response(text = "Plural fallback tools worked.")

  result <- generate_text(
    model = mock_model,
    prompt = "Use the echo tool",
    tools = list(echo_tool),
    max_steps = 3
  )

  expect_equal(result$text, "Plural fallback tools worked.")
  expect_equal(tool_invocations, list("plural"))
  expect_length(result$all_tool_calls, 1)
  expect_equal(result$all_tool_calls[[1]]$name, "echo")
})

test_that("generate_text falls back to text tool protocol when native tool calling is unavailable", {
  tool_invocations <- list()
  first_params <- NULL
  echo_tool <- tool(
    name = "echo",
    description = "Echo a message",
    parameters = z_object(message = z_string("Message to echo")),
    execute = function(args) {
      tool_invocations[[length(tool_invocations) + 1L]] <<- args$message
      paste("Echo:", args$message)
    }
  )

  mock_model <- MockModel$new()
  mock_model$capabilities <- list(native_tool_calling = FALSE)
  mock_model$responses <- list(
    function(params) {
      first_params <<- params
      list(
        text = paste0(
          "<tool_call>\n",
          "{\"name\":\"echo\",\"arguments\":{\"message\":\"fallback\"}}\n",
          "</tool_call>"
        ),
        tool_calls = NULL,
        finish_reason = "stop",
        usage = list(total_tokens = 10)
      )
    },
    list(
      text = "<final_answer>Fallback tools worked.</final_answer>",
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 10)
    )
  )

  result <- generate_text(
    model = mock_model,
    prompt = "Use the echo tool",
    tools = list(echo_tool),
    max_steps = 3
  )

  expect_equal(result$text, "Fallback tools worked.")
  expect_equal(tool_invocations, list("fallback"))
  expect_null(first_params$tools)
  expect_true(any(vapply(
    first_params$messages,
    function(msg) grepl("<tool_call>", msg$content %||% "", fixed = TRUE) ||
      grepl("Native API tool calling is unavailable", msg$content %||% "", fixed = TRUE),
    logical(1)
  )))
})

test_that("text tool fallback requires a structured post-tool response", {
  tool_invocations <- list()
  echo_tool <- tool(
    name = "echo",
    description = "Echo a message",
    parameters = z_object(message = z_string("Message to echo")),
    execute = function(args) {
      tool_invocations[[length(tool_invocations) + 1L]] <<- args$message
      paste("Echo:", args$message)
    }
  )

  correction_prompt <- NULL
  mock_model <- MockModel$new()
  mock_model$capabilities <- list(native_tool_calling = FALSE)
  mock_model$responses <- list(
    list(
      text = paste0(
        "<tool_call>\n",
        "{\"name\":\"echo\",\"arguments\":{\"message\":\"protocol\"}}\n",
        "</tool_call>"
      ),
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 10)
    ),
    function(params) {
      list(
        text = "先看看结果，没问题就继续。",
        tool_calls = NULL,
        finish_reason = "stop",
        usage = list(total_tokens = 10)
      )
    },
    function(params) {
      correction_prompt <<- params$messages[[length(params$messages)]]$content
      list(
        text = "<final_answer>Protocol tools worked.</final_answer>",
        tool_calls = NULL,
        finish_reason = "stop",
        usage = list(total_tokens = 10)
      )
    }
  )

  result <- generate_text(
    model = mock_model,
    prompt = "Use the echo tool",
    tools = list(echo_tool),
    max_steps = 4
  )

  expect_equal(result$text, "Protocol tools worked.")
  expect_equal(tool_invocations, list("protocol"))
  expect_match(correction_prompt, "did not follow the required post-tool protocol", fixed = TRUE)
  expect_match(correction_prompt, "<final_answer>", fixed = TRUE)
})

test_that("text tool fallback accepts prose around final answer tags", {
  echo_tool <- tool(
    name = "echo",
    description = "Echo a message",
    parameters = z_object(message = z_string("Message to echo")),
    execute = function(args) paste("Echo:", args$message)
  )

  mock_model <- MockModel$new()
  mock_model$capabilities <- list(native_tool_calling = FALSE)
  mock_model$responses <- list(
    list(
      text = paste0(
        "<tool_call>\n",
        "{\"name\":\"echo\",\"arguments\":{\"message\":\"strict\"}}\n",
        "</tool_call>"
      ),
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 10)
    ),
    list(
      text = "Here is the answer:\n<final_answer>Too loose.</final_answer>",
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 10)
    )
  )

  result <- generate_text(
    model = mock_model,
    prompt = "Use the echo tool",
    tools = list(echo_tool),
    max_steps = 4
  )

  expect_equal(result$text, "Too loose.")
})

test_that("post-tool protocol can require explicit final answers with native tool calling", {
  echo_tool <- tool(
    name = "echo",
    description = "Echo a message",
    parameters = z_object(message = z_string("Message to echo")),
    execute = function(args) paste("Echo:", args$message)
  )

  correction_prompt <- NULL
  mock_model <- MockModel$new()
  mock_model$capabilities <- list(native_tool_calling = TRUE)
  mock_model$responses <- list(
    list(
      text = "",
      tool_calls = list(list(
        id = "call_1",
        name = "echo",
        arguments = list(message = "native")
      )),
      finish_reason = "tool_calls",
      usage = list(total_tokens = 10)
    ),
    list(
      text = "I checked the result. Now I will continue.",
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 10)
    ),
    function(params) {
      correction_prompt <<- params$messages[[length(params$messages)]]$content
      list(
        text = "<final_answer>Native protocol worked.</final_answer>",
        tool_calls = NULL,
        finish_reason = "stop",
        usage = list(total_tokens = 10)
      )
    }
  )

  result <- generate_text(
    model = mock_model,
    prompt = "Use the echo tool",
    tools = list(echo_tool),
    max_steps = 4,
    require_post_tool_protocol = TRUE
  )

  expect_equal(result$text, "Native protocol worked.")
  expect_match(correction_prompt, "provider's native/API tool-call interface", fixed = TRUE)
  expect_match(correction_prompt, "I checked the result", fixed = TRUE)
})

test_that("stream_text shows native post-tool progress text before correcting", {
  echo_tool <- tool(
    name = "echo",
    description = "Echo a message",
    parameters = z_object(message = z_string("Message to echo")),
    execute = function(args) paste("Echo:", args$message)
  )

  mock_model <- MockModel$new()
  mock_model$capabilities <- list(native_tool_calling = TRUE)
  mock_model$responses <- list(
    list(
      text = "",
      tool_calls = list(list(
        id = "call_1",
        name = "echo",
        arguments = list(message = "native stream")
      )),
      finish_reason = "tool_calls",
      usage = list(total_tokens = 10)
    ),
    list(
      text = "Installing the missing package and rerunning.",
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 10)
    ),
    list(
      text = "<final_answer>Native stream protocol worked.</final_answer>",
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 10)
    )
  )

  chunks <- character(0)
  result <- stream_text(
    model = mock_model,
    prompt = "Use the echo tool",
    tools = list(echo_tool),
    max_steps = 4,
    require_post_tool_protocol = TRUE,
    callback = function(text, done) {
      if (nzchar(text %||% "")) {
        chunks <<- c(chunks, text)
      }
    }
  )

  expect_equal(result$text, "Native stream protocol worked.")
  expect_true(any(grepl("Installing the missing package", chunks, fixed = TRUE)))
  expect_false(any(grepl("<final_answer>", chunks, fixed = TRUE)))
})

test_that("stream_text emits typed events and keeps tool-call prose out of history", {
  echo_tool <- tool(
    name = "echo",
    description = "Echo a message",
    parameters = z_object(message = z_string("Message to echo")),
    execute = function(args) paste("Echo:", args$message)
  )

  tool_step_text <- "FINAL REPORT\nThis should not be repeated into history."
  second_call_messages <- NULL
  mock_model <- MockModel$new()
  mock_model$capabilities <- list(native_tool_calling = TRUE)
  mock_model$responses <- list(
    list(
      text = tool_step_text,
      tool_calls = list(list(
        id = "call_1",
        name = "echo",
        arguments = list(message = "native stream")
      )),
      finish_reason = "tool_calls",
      usage = list(total_tokens = 10)
    ),
    function(params) {
      second_call_messages <<- params$messages
      list(
        text = tool_step_text,
        tool_calls = NULL,
        finish_reason = "stop",
        usage = list(total_tokens = 10)
      )
    }
  )

  raw_chunks <- character()
  events <- list()
  result <- stream_text(
    model = mock_model,
    prompt = "Use the echo tool",
    tools = list(echo_tool),
    max_steps = 3,
    callback = function(text, done) {
      if (nzchar(text %||% "")) {
        raw_chunks <<- c(raw_chunks, text)
      }
    },
    .stream_event_callback = function(event) {
      events[[length(events) + 1L]] <<- event
    }
  )

  event_types <- vapply(events, `[[`, character(1), "type")
  expect_equal(result$text, tool_step_text)
  expect_true(any(event_types == "intermediate_text"))
  expect_true(any(event_types == "final_text"))
  expect_true(any(event_types == "done"))
  expect_true(any(vapply(result$stream_events, `[[`, character(1), "type") == "intermediate_text"))
  expect_length(raw_chunks, 0)

  assistant_tool_messages <- Filter(function(message) {
    identical(message$role, "assistant") &&
      length(message$tool_calls %||% list()) > 0
  }, second_call_messages)
  expect_length(assistant_tool_messages, 1)
  expect_identical(assistant_tool_messages[[1]]$content, "")
})

test_that("stream_text keeps provider thinking separate from visible answers", {
  ThinkingStreamModel <- R6::R6Class(
    "ThinkingStreamModelForCoreTests",
    inherit = aisdk:::LanguageModelV1,
    public = list(
      provider = "mock",
      model_id = "thinking-stream-mock",
      call_count = 0L,
      initialize = function() {
        self$call_count <- 0L
      },
      do_generate = function(params) {
        list(text = "Visible answer.", tool_calls = NULL, finish_reason = "stop")
      },
      do_stream = function(params, callback) {
        self$call_count <- self$call_count + 1L
        if (self$call_count == 1L) {
          callback("<think>\n", FALSE)
          callback("Reasoning only.", FALSE)
          callback("\n</think>\n\n", FALSE)
          callback(NULL, TRUE)
          return(list(
            text = "",
            reasoning = "Reasoning only.",
            tool_calls = NULL,
            finish_reason = "stop",
            usage = list(total_tokens = 10)
          ))
        }

        callback("Visible answer.", FALSE)
        callback(NULL, TRUE)
        list(
          text = "Visible answer.",
          tool_calls = NULL,
          finish_reason = "stop",
          usage = list(total_tokens = 10)
        )
      },
      format_tool_result = function(tool_call_id, tool_name, result) {
        list(role = "tool", tool_call_id = tool_call_id, name = tool_name, content = result)
      }
    )
  )

  model <- ThinkingStreamModel$new()
  events <- list()
  result <- stream_text(
    model = model,
    prompt = "Think, then answer",
    max_steps = 2,
    callback = function(text, done) NULL,
    .stream_event_callback = function(event) {
      events[[length(events) + 1L]] <<- event
    }
  )

  event_types <- vapply(events, `[[`, character(1), "type")
  final_events <- Filter(function(event) identical(event$type, "final_text"), events)
  expect_equal(result$text, "Visible answer.")
  expect_equal(model$call_count, 2L)
  expect_true(any(event_types == "thinking_text"))
  expect_equal(final_events[[1]]$text, "Visible answer.")
  expect_false(grepl("<think>", result$text, fixed = TRUE))
})

test_that("thinking markup filter handles split tags", {
  filter <- aisdk:::new_agent_runtime_thinking_markup_filter()

  first <- filter$process("visible <thi")
  second <- filter$process("nk>\nprivate</thi")
  third <- filter$process("nk>\nanswer")

  expect_equal(first$visible, "visible ")
  expect_equal(first$thinking, "")
  expect_equal(second$visible, "")
  expect_equal(second$thinking, "<think>\nprivate")
  expect_equal(third$thinking, "</think>")
  expect_equal(third$visible, "\nanswer")
})

test_that("text tool fallback does not replay prose from tool-call turns", {
  messages <- aisdk:::append_text_tool_result_messages(
    messages = list(list(role = "user", content = "Use a tool")),
    result = list(
      text = "I am checking this now.",
      tool_calls = list(list(id = "call_1", name = "echo", arguments = list()))
    ),
    tool_results = list(list(
      id = "call_1",
      name = "echo",
      result = "ok",
      is_error = FALSE
    ))
  )

  assistant_messages <- Filter(function(message) {
    identical(message$role, "assistant")
  }, messages)
  expect_length(assistant_messages, 0)
  expect_match(messages[[length(messages)]]$content, "Tool execution results:", fixed = TRUE)
})

test_that("stream_text shows text-tool post-tool progress text before correcting", {
  echo_tool <- tool(
    name = "echo",
    description = "Echo a message",
    parameters = z_object(message = z_string("Message to echo")),
    execute = function(args) paste("Echo:", args$message)
  )

  mock_model <- MockModel$new()
  mock_model$capabilities <- list(native_tool_calling = FALSE)
  mock_model$responses <- list(
    list(
      text = paste0(
        "<tool_call>\n",
        "{\"name\":\"echo\",\"arguments\":{\"message\":\"stream\"}}\n",
        "</tool_call>"
      ),
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 10)
    ),
    list(
      text = "Checking the result before answering.",
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 10)
    ),
    list(
      text = "<final_answer>Stream protocol worked.</final_answer>",
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 10)
    )
  )

  chunks <- character(0)
  result <- stream_text(
    model = mock_model,
    prompt = "Use the echo tool",
    tools = list(echo_tool),
    max_steps = 4,
    callback = function(text, done) {
      if (nzchar(text %||% "")) {
        chunks <<- c(chunks, text)
      }
    }
  )

  expect_equal(result$text, "Stream protocol worked.")
  expect_true(any(grepl("Checking the result", chunks, fixed = TRUE)))
  expect_false(any(grepl("<final_answer>", chunks, fixed = TRUE)))
})

test_that("generate_text hides tools that require unavailable model capabilities", {
  vision_tool <- tool(
    name = "inspect_image",
    description = "Inspect image pixels",
    parameters = z_object(path = z_string("Image path")),
    execute = function(path) "inspected",
    meta = list(required_model_capabilities = c("vision_input"))
  )
  echo_tool <- tool(
    name = "echo",
    description = "Echo a message",
    parameters = z_object(message = z_string("Message")),
    execute = function(message) message
  )

  mock_model <- MockModel$new()
  mock_model$capabilities <- list(vision_input = FALSE)

  result <- generate_text(
    model = mock_model,
    prompt = "Hello",
    tools = list(vision_tool, echo_tool)
  )

  tool_names <- vapply(mock_model$last_params$tools, function(t) t$name, character(1))
  expect_equal(result$text, "Mock response")
  expect_false("inspect_image" %in% tool_names)
  expect_true("echo" %in% tool_names)
})

test_that("generate_text keeps routed tools when a compatible capability model is configured", {
  old_routes <- aisdk:::get_capability_model_routes()
  withr::defer(aisdk:::store_capability_model_routes(old_routes))
  clear_capability_model()
  set_capability_model("vision.inspect", "openai:gpt-4o", type = "language")

  vision_tool <- tool(
    name = "inspect_image",
    description = "Inspect image pixels",
    parameters = z_object(path = z_string("Image path")),
    execute = function(path) "inspected",
    meta = list(
      required_model_capabilities = c("vision_input"),
      model_capability_route = "vision.inspect"
    )
  )

  mock_model <- MockModel$new()
  mock_model$capabilities <- list(vision_input = FALSE)

  result <- generate_text(
    model = mock_model,
    prompt = "Hello",
    tools = list(vision_tool)
  )

  tool_names <- vapply(mock_model$last_params$tools, function(t) t$name, character(1))
  expect_equal(result$text, "Mock response")
  expect_true("inspect_image" %in% tool_names)
})

test_that("generate_text keeps routed tools from session capability models", {
  old_routes <- aisdk:::get_capability_model_routes()
  withr::defer(aisdk:::store_capability_model_routes(old_routes))
  clear_capability_model()

  vision_tool <- tool(
    name = "inspect_image",
    description = "Inspect image pixels",
    parameters = z_object(path = z_string("Image path")),
    execute = function(path) "inspected",
    meta = list(
      required_model_capabilities = c("vision_input"),
      model_capability_route = "vision.inspect"
    )
  )

  session <- create_chat_session(model = "mock:chat")
  session$set_capability_model("vision.inspect", "openai:gpt-4o", type = "language")

  mock_model <- MockModel$new()
  mock_model$capabilities <- list(vision_input = FALSE)

  result <- generate_text(
    model = mock_model,
    prompt = "Hello",
    tools = list(vision_tool),
    session = session
  )

  tool_names <- vapply(mock_model$last_params$tools, function(t) t$name, character(1))
  expect_equal(result$text, "Mock response")
  expect_true("inspect_image" %in% tool_names)
})

# === Tests for ProviderRegistry ===

test_that("ProviderRegistry registers and retrieves providers", {
  registry <- ProviderRegistry$new()
  provider <- suppressWarnings(create_openai())

  registry$register("test-openai", provider)
  # Verify provider is registered by checking list_providers
  expect_true("test-openai" %in% registry$list_providers())
})

test_that("ProviderRegistry parses provider:model syntax", {
  registry <- ProviderRegistry$new()
  provider <- suppressWarnings(create_openai())
  registry$register("myopenai", provider)

  model <- registry$language_model(paste0("myopenai:", openai_model))

  expect_s3_class(model, "OpenAILanguageModel")
  expect_equal(model$model_id, openai_model)
})

test_that("ProviderRegistry errors on unregistered provider", {
  registry <- ProviderRegistry$new()

  expect_error(
    registry$language_model("nonexistent:model"),
    "Provider not found"
  )
})

# === Tests for get_default_registry ===

test_that("get_default_registry returns singleton", {
  reg1 <- get_default_registry()
  reg2 <- get_default_registry()

  expect_identical(reg1, reg2)
})
