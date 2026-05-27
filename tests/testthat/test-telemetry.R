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
