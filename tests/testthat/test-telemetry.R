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
