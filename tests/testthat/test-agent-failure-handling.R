test_that("execute_tool_calls marks returned error strings as tool errors", {
  failing_tool <- tool(
    name = "plain_error",
    description = "Returns a string error without throwing",
    parameters = z_empty_object(),
    execute = function() "Error: deliberate failure"
  )

  results <- execute_tool_calls(
    list(list(id = "call_1", name = "plain_error", arguments = list())),
    list(failing_tool)
  )

  expect_length(results, 1)
  expect_true(results[[1]]$is_error)
  expect_match(results[[1]]$result, "deliberate failure", fixed = TRUE)
})

test_that("stream_text circuit breaker counts returned error strings", {
  tool_call <- function(id) {
    list(id = id, name = "always_fails", arguments = list())
  }

  failing_tool <- tool(
    name = "always_fails",
    description = "Returns an error string without throwing",
    parameters = z_empty_object(),
    execute = function() "Error: still failing"
  )

  model <- MockModel$new(list(
    list(
      text = "",
      tool_calls = list(tool_call("call_1")),
      finish_reason = "tool_calls",
      usage = list(total_tokens = 1)
    ),
    list(
      text = "",
      tool_calls = list(tool_call("call_2")),
      finish_reason = "tool_calls",
      usage = list(total_tokens = 1)
    ),
    list(
      text = "This response should not be reached.",
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 1)
    )
  ))

  expect_warning(
    result <- stream_text(
      model = model,
      prompt = "keep trying the failing tool",
      tools = list(failing_tool),
      max_steps = 5,
      max_tool_result_errors = 2,
      callback = function(text, done) NULL
    ),
    "Circuit breaker triggered"
  )

  expect_equal(result$finish_reason, "tool_result_failure")
  expect_equal(result$run_state$status, "tool_result_failed")
  expect_true(result$run_state$recoverable)
  expect_length(result$all_tool_results, 2)
  expect_true(all(vapply(result$all_tool_results, function(x) isTRUE(x$is_error), logical(1))))
  expect_length(model$responses, 1)
})

test_that("ChatSession send_stream invisibly returns generation result", {
  model <- MockModel$new(list(list(
    text = "streamed response",
    tool_calls = NULL,
    finish_reason = "stop",
    usage = list(total_tokens = 1)
  )))
  session <- create_chat_session(model = model)

  result <- session$send_stream("hello", callback = function(text, done) NULL)

  expect_equal(result$text, "streamed response")
  expect_equal(result$finish_reason, "stop")
  expect_equal(session$get_run_state()$status, "assistant_final")
})

test_that("console streaming marks tool-result breaker turns as failed", {
  tool_call <- function(id) {
    list(id = id, name = "always_fails", arguments = list())
  }

  failing_tool <- tool(
    name = "always_fails",
    description = "Returns an error string without throwing",
    parameters = z_empty_object(),
    execute = function() "Error: still failing"
  )

  model <- MockModel$new(list(
    list(
      text = "",
      tool_calls = list(tool_call("call_1")),
      finish_reason = "tool_calls",
      usage = list(total_tokens = 1)
    ),
    list(
      text = "",
      tool_calls = list(tool_call("call_2")),
      finish_reason = "tool_calls",
      usage = list(total_tokens = 1)
    )
  ))
  session <- create_chat_session(model = model, tools = list(failing_tool), max_steps = 5)
  app_state <- aisdk:::create_console_app_state(session, view_mode = "clean")

  expect_warning(
    ok <- aisdk:::console_send_user_message(
      "keep trying the failing tool",
      session = session,
      stream = TRUE,
      app_state = app_state
    ),
    "Circuit breaker triggered"
  )

  expect_true(ok)
  expect_equal(aisdk:::console_app_get_current_turn(app_state)$phase, "error")
  expect_equal(session$get_run_state()$status, "tool_result_failed")
})

test_that("handle_user_choice returns recovery prompts", {
  action <- aisdk:::handle_user_choice(
    "1",
    "execute_r_code",
    session = NULL,
    last_error = "Error: parse failed"
  )

  expect_equal(action$action, "continue")
  expect_match(action$guidance, "Continue from the previous", fixed = TRUE)
  expect_match(action$prompt, "Continue from the previous", fixed = TRUE)
  expect_match(action$prompt, "parse failed", fixed = TRUE)

  explain <- aisdk:::handle_user_choice("3", "execute_r_code", session = NULL)
  expect_equal(explain$action, "explain")
  expect_match(explain$guidance, "Do not call the failing tool again", fixed = TRUE)
  expect_match(explain$prompt, "Do not call the failing tool again", fixed = TRUE)
})

test_that("console detects action-promising assistant text after tool use", {
  result <- list(
    text = "`ggmosaic` installed. Now installing `confuns`",
    tool_calls = NULL,
    all_tool_results = list(list(name = "bash", result = "ok", is_error = FALSE)),
    finish_reason = "stop"
  )

  expect_true(aisdk:::console_generation_looks_incomplete(result))

  prompt <- aisdk:::console_incomplete_continuation_prompt(result)
  expect_match(prompt, "ended without a tool call", fixed = TRUE)
  expect_match(prompt, "Now installing", fixed = TRUE)
})

test_that("generate_text marks max steps as recoverable run state", {
  model <- MockModel$new(list(list(
    text = "",
    tool_calls = list(list(id = "call_1", name = "noop", arguments = list())),
    finish_reason = "tool_calls",
    usage = list(total_tokens = 1)
  )))
  noop_tool <- tool(
    name = "noop",
    description = "No-op",
    parameters = z_empty_object(),
    execute = function() "ok"
  )

  expect_warning(
    result <- generate_text(
      model = model,
      prompt = "run a tool",
      tools = list(noop_tool),
      max_steps = 1
    ),
    "Maximum generation steps"
  )

  expect_equal(result$run_state$status, "max_steps")
  expect_true(result$run_state$recoverable)
  expect_equal(result$run_state$pending_action, "continue")
})

test_that("ChatSession continue_run sends structured continuation guidance", {
  model <- MockModel$new(list(
    list(
      text = "",
      tool_calls = list(list(id = "call_1", name = "always_fails", arguments = list())),
      finish_reason = "tool_calls",
      usage = list(total_tokens = 1)
    ),
    list(
      text = "",
      tool_calls = list(list(id = "call_2", name = "always_fails", arguments = list())),
      finish_reason = "tool_calls",
      usage = list(total_tokens = 1)
    ),
    function(params) {
      user_messages <- Filter(function(msg) identical(msg$role, "user"), params$messages)
      last <- tail(user_messages, 1)[[1]]$content
      expect_true(grepl("\\[continue_run_begin\\]", last))
      expect_true(grepl("Action: continue", last, fixed = TRUE))
      list(
        text = "I will stop retrying and explain.",
        tool_calls = NULL,
        finish_reason = "stop",
        usage = list(total_tokens = 1)
      )
    }
  ))
  failing_tool <- tool(
    name = "always_fails",
    description = "Returns an error string without throwing",
    parameters = z_empty_object(),
    execute = function() "Error: still failing"
  )
  session <- create_chat_session(model = model, tools = list(failing_tool), max_steps = 5)

  expect_warning(
    first <- session$send("keep trying the failing tool", max_tool_result_errors = 2),
    "Circuit breaker triggered"
  )
  expect_equal(first$run_state$status, "tool_result_failed")

  continued <- session$continue_run("continue", stream = FALSE)

  expect_equal(continued$text, "I will stop retrying and explain.")
  expect_equal(session$get_run_state()$status, "assistant_final")
})

test_that("generate_text converts network errors into blocked_network run state", {
  NetworkModel <- R6::R6Class(
    "NetworkModel",
    inherit = LanguageModelV1,
    public = list(
      initialize = function() {
        super$initialize(provider = "mock", model_id = "network")
      },
      do_generate = function(params) {
        stop("Failed to perform HTTP request: timeout")
      },
      do_stream = function(params, callback) {
        stop("Failed to perform HTTP request: timeout")
      },
      format_tool_result = function(tool_call_id, tool_name, result_content) {
        list(role = "tool", tool_call_id = tool_call_id, name = tool_name, content = result_content)
      }
    )
  )

  result <- generate_text(model = NetworkModel$new(), prompt = "hello")

  expect_equal(result$finish_reason, "blocked_network")
  expect_equal(result$run_state$status, "blocked_network")
  expect_true(result$run_state$recoverable)
  expect_equal(result$run_state$pending_action, "retry")
})

test_that("console does not continue normal final answers", {
  result <- list(
    text = "`ggmosaic` installed. Now install `confuns` with devtools::install_github().",
    tool_calls = NULL,
    all_tool_results = list(list(name = "bash", result = "ok", is_error = FALSE)),
    finish_reason = "stop"
  )

  expect_false(aisdk:::console_generation_looks_incomplete(result))
})

test_that("console continuation detects repeated incomplete action", {
  result <- list(
    text = "Now checking the package",
    tool_calls = NULL,
    all_tool_calls = list(),
    all_tool_results = list(),
    finish_reason = "stop"
  )

  expect_true(aisdk:::console_continuation_still_incomplete(result))
})
