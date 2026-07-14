test_that("Telemetry captures events", {
  skip_if_not_installed("jsonlite")
  
  # Capture output
  output <- capture.output({
    tel <- create_telemetry(trace_id = "test-trace")
    tel$log_event("test_event", items = 5)
  })
  
  expect_length(output, 1)
  
  log_entry <- jsonlite::fromJSON(output)
  expect_equal(log_entry$trace_id, "test-trace")
  expect_equal(log_entry$type, "test_event")
  expect_equal(log_entry$items, 5)
})

test_that("Telemetry hooks work", {
  output <- capture.output({
    tel <- create_telemetry(trace_id = "hook-trace")
    hooks <- tel$as_hooks()
    
    # Simulate hook trigger
    hooks$trigger_tool_start(list(name = "my_tool"), list())
  })
  
  log_entry <- jsonlite::fromJSON(output)
  expect_equal(log_entry$type, "tool_start")
  expect_equal(log_entry$tool_name, "my_tool")
})

test_that("Telemetry stores structured tool outcomes in memory", {
  tel <- create_telemetry(trace_id = "memory-trace", emit = FALSE)
  hooks <- tel$as_hooks()

  hooks$trigger_tool_start(list(name = "check_design_column"), list(column = "design"))
  hooks$trigger_tool_end(
    list(name = "check_design_column"),
    NULL,
    success = FALSE,
    error = "Unknown design column 'design'.",
    args = list(column = "design")
  )

  events <- tel$get_events()
  expect_length(events, 2)
  expect_equal(events[[1]]$type, "tool_start")
  expect_equal(events[[2]]$type, "tool_end")
  expect_false(events[[2]]$success)
  expect_equal(events[[2]]$error_type, "wrong_accessor")
  expect_match(events[[2]]$argument_signature, "design")
})

# --- U1: run-trace sink + enriched per-call events (OTel-mappable) ------------

test_that("set_run_trace_sink receives per-step enriched model_response events", {
  source(test_path("helper-mock.R"))
  events <- list()
  prev <- set_run_trace_sink(function(event, run_id) {
    events[[length(events) + 1L]] <<- list(type = event$type, run_id = run_id, payload = event$payload)
  })
  on.exit(set_run_trace_sink(prev), add = TRUE)

  wx <- tool(name = "get_weather", description = "w", execute = function(city = "X") "sunny")
  model <- MockModel$new(list(
    list(text = "", tool_calls = list(list(id = "c1", name = "get_weather", arguments = list(city = "SF"))),
         finish_reason = "tool_calls", usage = list(prompt_tokens = 80, completion_tokens = 20, total_tokens = 100),
         response_id = "resp_1"),
    list(text = "Sunny.", tool_calls = NULL, finish_reason = "stop",
         usage = list(prompt_tokens = 40, completion_tokens = 10, total_tokens = 50), response_id = "resp_2")
  ))
  generate_text(model = model, prompt = "weather?", tools = list(wx), max_steps = 3)

  responses <- Filter(function(e) e$type == "model_response", events)
  # One event PER model call (not once per generate_text) — the granularity
  # hooks lack.
  expect_equal(length(responses), 2)
  first <- responses[[1]]$payload
  expect_equal(first$model, "mock-model")
  expect_equal(first$provider, "mock")
  expect_equal(first$response_id, "resp_1")
  expect_equal(first$usage$total_tokens, 100)
  expect_true(is.numeric(first$latency_ms))
  expect_false(is.null(responses[[1]]$run_id))
})

test_that("set_run_trace_sink clears cleanly and a sink error never breaks a run", {
  source(test_path("helper-mock.R"))
  set_run_trace_sink(function(event, run_id) stop("sink boom"))
  on.exit(set_run_trace_sink(NULL), add = TRUE)
  # A throwing sink must not propagate into the run.
  res <- generate_text(model = MockModel$new(), prompt = "hi")
  expect_true(nzchar(res$text))

  set_run_trace_sink(NULL)
  expect_null(getOption("aisdk.trace_sink"))
})

test_that("set_run_trace_sink rejects non-function sinks", {
  expect_error(set_run_trace_sink(42), "must be a function")
})
