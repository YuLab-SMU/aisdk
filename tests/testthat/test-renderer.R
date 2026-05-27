# The Renderer contract decouples agent-output rendering from any specific UI.

test_that("Renderer base class is a usable no-op contract", {
  r <- create_null_renderer()
  expect_s3_class(r, "Renderer")
  expect_null(r$process_chunk("x", FALSE))
  expect_null(r$start_thinking())
  expect_null(r$stop_thinking())
  expect_null(r$render_tool_start("t", list()))
  expect_null(r$render_tool_result("t", "ok", success = TRUE))
  expect_null(r$reset_for_new_step())
})

test_that("CaptureRenderer records the agent-output event stream UI-agnostically", {
  r <- create_capture_renderer()
  expect_s3_class(r, "Renderer")

  r$start_thinking()
  r$process_chunk("Hello ", FALSE)
  r$process_chunk("world", FALSE)
  r$stop_thinking()
  r$reset_for_new_step()
  r$render_tool_start("search", list(q = "cats"))
  r$render_tool_result("search", "found 3", success = TRUE)
  r$process_chunk(NULL, TRUE)

  ev <- r$events()
  types <- vapply(ev, function(e) e$type, character(1))
  expect_identical(
    types,
    c("thinking_start", "text", "text", "thinking_stop",
      "step_reset", "tool_start", "tool_result", "text")
  )
  expect_identical(ev[[2]]$text, "Hello ")
  expect_identical(ev[[6]]$name, "search")
  expect_identical(ev[[6]]$arguments$q, "cats")
  expect_true(ev[[7]]$success)
  expect_true(ev[[8]]$done)
})

test_that("stream_text exposes an injectable renderer (any UI backend)", {
  expect_true("renderer" %in% names(formals(stream_text)))
})
