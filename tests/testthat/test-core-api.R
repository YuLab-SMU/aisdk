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

test_that("text tool fallback accepts plain final text after tool results", {
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
      expect_match(params$messages[[length(params$messages)]]$content, "Tool execution results:", fixed = TRUE)
      list(
        text = "Protocol tools worked.",
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
  expect_length(mock_model$responses, 0)
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

test_that("native post-tool protocol accepts plain final text", {
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
        arguments = list(message = "native")
      )),
      finish_reason = "tool_calls",
      usage = list(total_tokens = 10)
    ),
    function(params) {
      expect_match(params$messages[[length(params$messages)]]$content, "Post-tool response protocol:", fixed = TRUE)
      list(
        text = "Native protocol worked.",
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
  expect_length(mock_model$responses, 0)
})

test_that("stream_text accepts native plain final text after tools", {
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
      text = "Native stream protocol worked.",
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
  expect_true(any(grepl("Native stream protocol worked", chunks, fixed = TRUE)))
  expect_false(any(grepl("<final_answer>", chunks, fixed = TRUE)))
  expect_length(mock_model$responses, 0)
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
  text_delta_events <- Filter(function(event) identical(event$type, "text_delta"), events)
  expect_equal(result$text, "Visible answer.")
  expect_equal(model$call_count, 2L)
  expect_true(any(event_types == "thinking_text"))
  expect_equal(paste(vapply(text_delta_events, `[[`, character(1), "text"), collapse = ""), "Visible answer.")
  expect_equal(final_events[[1]]$text, "Visible answer.")
  expect_true(isTRUE(final_events[[1]]$already_streamed))
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

test_that("stream_text accepts text-tool plain final text after tools", {
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
      text = "Stream protocol worked.",
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
  expect_true(any(grepl("Stream protocol worked", chunks, fixed = TRUE)))
  expect_false(any(grepl("<final_answer>", chunks, fixed = TRUE)))
  expect_length(mock_model$responses, 0)
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

# --- Z4: generate_text_fallback (model fallback chain) -----------------------

test_that("generate_text_fallback falls over to the next model on an API error", {
  primary <- MockModel$new(list(function(p) rlang::abort("rate limited", class = "aisdk_api_error")))
  secondary <- MockModel$new(list(list(text = "from secondary", finish_reason = "stop",
                                       usage = list(total_tokens = 5))))
  fired <- FALSE
  res <- generate_text_fallback("hi", models = list(primary, secondary),
                                on_fallback = function(f, n, e) fired <<- TRUE)
  expect_equal(res$text, "from secondary")
  expect_true(fired)
})

test_that("generate_text_fallback surfaces a non-API error immediately", {
  primary <- MockModel$new(list(function(p) stop("bug in tool")))
  secondary <- MockModel$new(list(list(text = "unreached", finish_reason = "stop")))
  # A plain R error would recur on every model, so it is raised, not masked.
  expect_error(generate_text_fallback("hi", models = list(primary, secondary)), "bug in tool")
})

test_that("generate_text_fallback errors with a dedicated class when all models fail", {
  models <- list(
    MockModel$new(list(function(p) rlang::abort("down1", class = "aisdk_api_error"))),
    MockModel$new(list(function(p) rlang::abort("down2", class = "aisdk_api_error")))
  )
  expect_error(generate_text_fallback("hi", models = models), class = "aisdk_all_models_failed")
})

test_that("generate_text_fallback requires at least one model", {
  expect_error(generate_text_fallback("hi", models = list()), "at least one model")
})

# --- Z5: generate_batch (bulk generation with per-item error isolation) -------

test_that("generate_batch runs every prompt in order", {
  model <- MockModel$new(list(
    list(text = "A", finish_reason = "stop", usage = list(total_tokens = 5)),
    list(text = "B", finish_reason = "stop", usage = list(total_tokens = 6)),
    list(text = "C", finish_reason = "stop", usage = list(total_tokens = 7))
  ))
  res <- generate_batch(c("p1", "p2", "p3"), model = model, concurrency = 1)
  expect_length(res, 3)
  expect_equal(vapply(res, function(r) r$text, character(1)), c("A", "B", "C"))
})

test_that("generate_batch isolates a failing prompt without aborting the batch", {
  model <- MockModel$new(list(
    list(text = "ok1", finish_reason = "stop"),
    function(p) stop("boom"),
    list(text = "ok3", finish_reason = "stop")
  ))
  res <- generate_batch(list("a", "b", "c"), model = model, concurrency = 1)
  expect_equal(res[[1]]$text, "ok1")
  expect_s3_class(res[[2]], "error")
  expect_equal(res[[3]]$text, "ok3")
})

test_that("generate_batch handles an empty batch and composes with cost", {
  expect_length(generate_batch(character(0), model = MockModel$new()), 0)

  withr::local_options(aisdk.model_pricing = list("mock-model" = list(input = 1, output = 2)))
  model <- MockModel$new(list(
    list(text = "A", finish_reason = "stop", usage = list(prompt_tokens = 1e6, completion_tokens = 0)),
    list(text = "B", finish_reason = "stop", usage = list(prompt_tokens = 1e6, completion_tokens = 0))
  ))
  res <- generate_batch(c("p1", "p2"), model = model, concurrency = 1)
  total <- sum(vapply(res, function(r) estimate_cost(r$usage, "mock:mock-model"), numeric(1)))
  expect_equal(total, 2) # 2 prompts x 1M input x $1
})

# --- Z9: per-generation cost on the result -----------------------------------

test_that("generate_text attaches an estimated cost_usd to the result", {
  withr::local_options(aisdk.model_pricing = list("mock-model" = list(input = 1, output = 2)))
  r <- generate_text(
    model = MockModel$new(list(list(text = "hi", finish_reason = "stop",
                                    usage = list(prompt_tokens = 1e6, completion_tokens = 1e6, total_tokens = 2e6)))),
    prompt = "q"
  )
  expect_equal(r$cost_usd, 3) # 1M in * $1 + 1M out * $2
})

test_that("result cost_usd is NA when the model's pricing is unknown", {
  withr::local_options(aisdk.model_pricing = list())
  r <- generate_text(
    model = MockModel$new(list(list(text = "x", finish_reason = "stop",
                                    usage = list(prompt_tokens = 10)))),
    prompt = "q"
  )
  expect_true(is.na(r$cost_usd))
})

test_that("per-generation cost flows through generate_batch results", {
  withr::local_options(aisdk.model_pricing = list("mock-model" = list(input = 1, output = 0)))
  model <- MockModel$new(list(
    list(text = "a", finish_reason = "stop", usage = list(prompt_tokens = 1e6, completion_tokens = 0)),
    list(text = "b", finish_reason = "stop", usage = list(prompt_tokens = 1e6, completion_tokens = 0))
  ))
  res <- generate_batch(c("a", "b"), model = model, concurrency = 1)
  total <- sum(vapply(res, function(r) r$cost_usd %||% NA_real_, numeric(1)))
  expect_equal(total, 2)
})

# --- AJ1: count_tokens (pre-flight token counting) ---------------------------

test_that("count_tokens uses the local estimate for models without a native endpoint", {
  source(test_path("helper-mock.R"))
  n <- count_tokens(MockModel$new(), prompt = "Count the tokens in this sentence, please.")
  expect_type(n, "double")
  expect_gt(n, 0)
  # A system prompt is counted (once), so it raises the total.
  n_sys <- count_tokens(MockModel$new(), prompt = "hi", system = "a longer system preamble here")
  expect_gt(n_sys, count_tokens(MockModel$new(), prompt = "hi"))
})

test_that("count_tokens errors without prompt or messages, and accepts messages=", {
  source(test_path("helper-mock.R"))
  expect_error(count_tokens(MockModel$new()), "prompt.*messages|messages.*prompt")
  n <- count_tokens(MockModel$new(),
                    messages = list(list(role = "user", content = "direct message list")))
  expect_gt(n, 0)
})

test_that("Anthropic count_tokens hits /messages/count_tokens and returns the exact count", {
  captured_url <- NULL
  captured_body <- NULL
  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) {
      captured_url <<- url
      captured_body <<- body
      list(input_tokens = 1234)
    },
    .package = "aisdk"
  )
  m <- create_anthropic(api_key = "FAKE")$language_model("claude-sonnet-5")
  n <- count_tokens(m, prompt = "count me", system = "Be brief")

  expect_identical(n, 1234L)
  expect_match(captured_url, "/messages/count_tokens")
  # The count body drops generation-only fields and keeps the counted content.
  expect_null(captured_body$max_tokens)
  expect_null(captured_body$stream)
  expect_equal(captured_body$system, "Be brief")
  expect_true(any(vapply(captured_body$messages, function(x) identical(x$role, "user"), logical(1))))
})

test_that("Anthropic count_tokens falls back to the estimate when the endpoint is unavailable", {
  testthat::local_mocked_bindings(
    post_to_api = function(url, headers, body, ...) NULL, # offline short-circuit
    .package = "aisdk"
  )
  m <- create_anthropic(api_key = "FAKE")$language_model("claude-sonnet-5")
  n <- count_tokens(m, prompt = "estimate this when offline")
  expect_gt(n, 0)
})

# --- AP1: public typed streaming events via on_event -------------------------

test_that("stream_text on_event delivers typed events with text and a done flag", {
  source(test_path("helper-mock.R"))
  events <- list()
  m <- MockModel$new(list(list(text = "Hello world", finish_reason = "stop",
                               usage = list(total_tokens = 5))))
  res <- stream_text(model = m, prompt = "hi",
                     on_event = function(e) events[[length(events) + 1L]] <<- e)

  expect_gt(length(events), 0)
  # Every event has the documented shape.
  for (e in events) {
    expect_true(is.list(e))
    expect_true(is.character(e$type))
    expect_true(is.logical(e$done))
  }
  types <- vapply(events, function(e) e$type, character(1))
  expect_true("text_delta" %in% types)              # streamed answer text
  expect_true(any(vapply(events, function(e) isTRUE(e$done), logical(1))))
  # Answer text is reconstructable from text_delta events.
  streamed <- paste0(vapply(events[types == "text_delta"],
                            function(e) e$text %||% "", character(1)), collapse = "")
  expect_match(streamed, "Hello world", fixed = TRUE)
  expect_equal(res$text, "Hello world")
})

test_that("stream_text plain callback is unchanged when on_event is not supplied", {
  source(test_path("helper-mock.R"))
  chunks <- character()
  m <- MockModel$new(list(list(text = "abc", finish_reason = "stop", usage = list(total_tokens = 3))))
  stream_text(model = m, prompt = "hi",
              callback = function(t, done) if (nzchar(t)) chunks <<- c(chunks, t))
  expect_equal(paste(chunks, collapse = ""), "abc")
})

test_that("stream_text rejects a non-function on_event", {
  source(test_path("helper-mock.R"))
  expect_error(
    stream_text(model = MockModel$new(), prompt = "x", on_event = "not a function"),
    "must be a function"
  )
})
