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

test_that("stream_text records returned error strings as observations without menu breaker", {
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

  result <- stream_text(
    model = model,
    prompt = "keep trying the failing tool",
    tools = list(failing_tool),
    max_steps = 5,
    max_tool_result_errors = 2,
    callback = function(text, done) NULL
  )

  expect_equal(result$text, "This response should not be reached.")
  expect_equal(result$run_state$status, "completed")
  expect_equal(result$decision$decision, "finalize")
  expect_length(result$all_tool_results, 2)
  expect_true(all(vapply(result$all_tool_results, function(x) isTRUE(x$is_error), logical(1))))
  expect_length(result$task_state$failures, 2)
  expect_length(model$responses, 0)
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
  expect_equal(session$get_run_state()$status, "completed")
})

test_that("runtime continues when model streams only reasoning and no visible action", {
  model <- MockModel$new(list(
    list(
      text = "",
      reasoning = "thinking but no action",
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 1)
    ),
    list(
      text = "Created a small demo file.",
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 1)
    )
  ))

  result <- stream_text(
    model = model,
    prompt = "you decide",
    max_steps = 1,
    callback = function(text, done) NULL
  )

  expect_equal(result$text, "Created a small demo file.")
  expect_equal(result$run_state$status, "completed")
  expect_equal(result$run_state$budget$execution_windows, 2)
  expect_match(model$last_params$messages[[length(model$last_params$messages)]]$content, "no visible answer", fixed = TRUE)
})

test_that("console streaming finalizes tool-result observations without failure menu", {
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

  ok <- aisdk:::console_send_user_message(
    "keep trying the failing tool",
    session = session,
    stream = TRUE,
    app_state = app_state
  )

  expect_true(ok)
  expect_equal(aisdk:::console_app_get_current_turn(app_state)$phase, "done")
  expect_equal(session$get_run_state()$status, "completed")
  expect_false(aisdk:::console_should_prompt_tool_recovery(session$get_run_state()))
})

test_that("console no longer prompts tool recovery for ordinary failures", {
  recovered <- list(
    text = "Generated and opened the plot.",
    finish_reason = "stop",
    all_tool_results = list(
      list(name = "bash", result = "Error: missing file", is_error = TRUE),
      list(name = "bash", result = "Error: missing file", is_error = TRUE),
      list(name = "bash", result = "opened", is_error = FALSE)
    )
  )

  failed <- recovered
  failed$finish_reason <- "tool_result_failure"

  expect_false(aisdk:::console_should_prompt_tool_recovery(recovered))
  expect_false(aisdk:::console_should_prompt_tool_recovery(failed))
  expect_null(aisdk:::console_check_tool_failures(failed$all_tool_results, session = NULL))
})

test_that("console failure analysis ignores tool argument validation errors", {
  validation_results <- list(
    list(
      name = "r_eval",
      result = "Error: invalid arguments for tool 'r_eval': Missing required argument `code`.",
      is_error = TRUE,
      is_validation_error = TRUE
    ),
    list(
      name = "r_eval",
      result = "Error: invalid arguments for tool 'r_eval': Argument `code` must contain at least 1 character.",
      is_error = TRUE,
      is_validation_error = TRUE
    )
  )

  expect_equal(aisdk:::analyze_tool_failures(validation_results), integer(0))
})

test_that("console stream filter hides text tool call markup", {
  filter <- aisdk:::new_console_tool_call_markup_filter()
  chunks <- c(
    "我先生成图片。\n<tool_",
    "call>\n{\"name\":\"r_eval\",\"arguments\":{\"code\":\"1+1\"}}\n",
    "</tool_call>\n",
    "图片已保存。\n"
  )

  rendered <- paste0(vapply(chunks, filter$process, character(1), done = FALSE), collapse = "")
  rendered <- paste0(rendered, filter$process(NULL, done = TRUE))

  expect_match(rendered, "我先生成图片", fixed = TRUE)
  expect_match(rendered, "图片已保存", fixed = TRUE)
  expect_false(grepl("<tool_call>", rendered, fixed = TRUE))
  expect_false(grepl("r_eval", rendered, fixed = TRUE))
})

test_that("console stream filter hides plural text tool call markup", {
  filter <- aisdk:::new_console_tool_call_markup_filter()
  chunks <- c(
    "我先检查。\n<tool_",
    "calls>\n[{\"name\":\"r_eval\",\"arguments\":{\"code\":\"1+1\"}}]\n",
    "</tool_",
    "calls>\n",
    "继续处理。\n"
  )

  rendered <- paste0(vapply(chunks, filter$process, character(1), done = FALSE), collapse = "")
  rendered <- paste0(rendered, filter$process(NULL, done = TRUE))

  expect_match(rendered, "我先检查", fixed = TRUE)
  expect_match(rendered, "继续处理", fixed = TRUE)
  expect_false(grepl("<tool_calls>", rendered, fixed = TRUE))
  expect_false(grepl("r_eval", rendered, fixed = TRUE))
})

test_that("console stream filter unwraps final answer markup", {
  filter <- aisdk:::new_console_tool_call_markup_filter()
  chunks <- c(
    "<final_",
    "answer>\n最终答案",
    "在这里。\n</final_",
    "answer>"
  )

  rendered <- paste0(vapply(chunks, filter$process, character(1), done = FALSE), collapse = "")
  rendered <- paste0(rendered, filter$process(NULL, done = TRUE))

  expect_match(rendered, "最终答案", fixed = TRUE)
  expect_match(rendered, "在这里", fixed = TRUE)
  expect_false(grepl("<final_answer>", rendered, fixed = TRUE))
})

test_that("generate_text treats max_steps as an execution window and continues", {
  model <- MockModel$new(list(
    list(
      text = "",
      tool_calls = list(list(id = "call_1", name = "noop", arguments = list())),
      finish_reason = "tool_calls",
      usage = list(total_tokens = 1)
    ),
    list(
      text = "done after window",
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 1)
    )
  ))
  noop_tool <- tool(
    name = "noop",
    description = "No-op",
    parameters = z_empty_object(),
    execute = function() "ok"
  )

  result <- generate_text(
    model = model,
    prompt = "run a tool",
    tools = list(noop_tool),
    max_steps = 1
  )

  expect_equal(result$text, "done after window")
  expect_equal(result$run_state$status, "completed")
  expect_equal(result$task_state$budget$execution_windows, 2)
  expect_length(result$all_tool_results, 1)
})

test_that("ChatSession continue_run sends structured continuation guidance", {
  model <- MockModel$new(list(
    list(
      text = "Initial turn complete.",
      tool_calls = NULL,
      finish_reason = "stop",
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

  first <- session$send("hello", max_tool_result_errors = 2)
  expect_equal(first$run_state$status, "completed")

  continued <- session$continue_run("continue", stream = FALSE)

  expect_equal(continued$text, "I will stop retrying and explain.")
  expect_equal(session$get_run_state()$status, "completed")
})

test_that("tool argument validation errors are fed back for self-correction", {
  ran <- FALSE
  validating_tool <- tool(
    name = "r_eval",
    description = "Run R code",
    parameters = z_object(
      code = z_string("R code", min_length = 1),
      .required = "code"
    ),
    execute = function(args) {
      ran <<- TRUE
      paste0("value: ", args$code)
    },
    meta = list(validate_arguments = TRUE)
  )

  model <- MockModel$new(list(
    list(
      text = "",
      tool_calls = list(list(id = "call_1", name = "r_eval", arguments = list())),
      finish_reason = "tool_calls",
      usage = list(total_tokens = 1)
    ),
    function(params) {
      tool_messages <- Filter(function(msg) identical(msg$role, "tool"), params$messages)
      expect_length(tool_messages, 1)
      expect_match(tool_messages[[1]]$content, "invalid_tool_arguments", fixed = TRUE)
      list(
        text = "",
        tool_calls = list(list(id = "call_2", name = "r_eval", arguments = list(code = "1 + 1"))),
        finish_reason = "tool_calls",
        usage = list(total_tokens = 1)
      )
    },
    list(
      text = "done",
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 1)
    )
  ))

  result <- generate_text(
    model = model,
    prompt = "run code",
    tools = list(validating_tool),
    max_steps = 5,
    max_tool_result_errors = 1
  )

  expect_true(ran)
  expect_equal(result$text, "done")
  expect_equal(result$finish_reason, "stop")
  expect_equal(result$run_state$status, "completed")
  expect_length(result$all_tool_results, 2)
  expect_true(result$all_tool_results[[1]]$is_validation_error)
  expect_false(result$all_tool_results[[2]]$is_error)
})

test_that("generate_text converts network errors into blocked task state", {
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

  expect_equal(result$finish_reason, "blocked")
  expect_equal(result$run_state$status, "blocked")
  expect_equal(result$decision$decision, "blocked")
  expect_match(result$run_state$blocker, "timeout", fixed = TRUE)
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

test_that("console continuation does not infer state from action words", {
  result <- list(
    text = "Now checking the package",
    tool_calls = NULL,
    all_tool_calls = list(),
    all_tool_results = list(),
    finish_reason = "stop"
  )

  expect_false(aisdk:::console_continuation_still_incomplete(result))
})

test_that("console detects empty final text after tool execution", {
  # Tools ran (e.g. r_eval) but the model produced no visible answer for the
  # user. This is the silent-stop case from issue #26: user sees only
  # "Thinking complete (N lines)" and is dropped back to the prompt.
  result <- list(
    text = "",
    tool_calls = NULL,
    all_tool_results = list(list(name = "r_eval", result = "ok", is_error = FALSE)),
    finish_reason = "stop"
  )

  expect_true(aisdk:::console_generation_looks_incomplete(result))

  prompt <- aisdk:::console_incomplete_continuation_prompt(result)
  expect_match(prompt, "did not produce any visible answer", fixed = TRUE)
  expect_match(prompt, "Write the final answer now", fixed = TRUE)
})

test_that("runtime finalizer answers when tools succeeded but model returned empty text", {
  noop_tool <- tool(
    name = "noop",
    description = "No-op",
    parameters = z_empty_object(),
    execute = function() "created file plot.png"
  )
  model <- MockModel$new(list(
    list(
      text = "",
      tool_calls = list(list(id = "call_1", name = "noop", arguments = list())),
      finish_reason = "tool_calls",
      usage = list(total_tokens = 1)
    ),
    list(
      text = "",
      tool_calls = NULL,
      finish_reason = "stop",
      usage = list(total_tokens = 1)
    ),
    function(params) {
      expect_null(params$tools)
      finalizer_prompt <- params$messages[[length(params$messages)]]$content
      expect_match(finalizer_prompt, "did not produce a visible final answer", fixed = TRUE)
      expect_match(finalizer_prompt, "created file plot.png", fixed = TRUE)
      list(
        text = "Created `plot.png` and verified the tool completed successfully.",
        tool_calls = NULL,
        finish_reason = "stop",
        usage = list(total_tokens = 1)
      )
    }
  ))

  result <- generate_text(
    model = model,
    prompt = "run a tool and summarize",
    tools = list(noop_tool),
    max_steps = 5
  )

  expect_equal(result$run_state$status, "completed")
  expect_equal(result$decision$decision, "finalize")
  expect_match(result$text, "Created `plot.png`", fixed = TRUE)
})

test_that("console marks empty continuation as still incomplete", {
  # This helper remains for callers that need to inspect a manual continuation
  # result, but ordinary empty post-tool turns are finalized by the runtime.
  result <- list(
    text = "",
    tool_calls = NULL,
    all_tool_calls = list(),
    all_tool_results = list(),
    finish_reason = "stop"
  )

  expect_true(aisdk:::console_continuation_still_incomplete(result))
})

test_that("console does not flag whitespace-only text after tools as complete", {
  result <- list(
    text = "   \n  ",
    tool_calls = NULL,
    all_tool_results = list(list(name = "r_eval", result = "ok", is_error = FALSE)),
    finish_reason = "stop"
  )

  expect_true(aisdk:::console_generation_looks_incomplete(result))
})
