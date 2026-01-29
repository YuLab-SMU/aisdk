test_that("Hooks operate correctly", {
  # Setup mock model
  mock_model <- list(
    provider = "mock",
    do_generate = function(params) {
      list(text = "Mock response")
    },
    format_tool_result = function(id, name, res) {
      list(role = "tool", content = res)
    }
  )
  class(mock_model) <- "LanguageModelV1"
  
  # Test Generation Hooks
  start_called <- FALSE
  end_called <- FALSE
  
  hooks <- create_hooks(
    on_generation_start = function(model, prompt, tools) {
      start_called <<- TRUE
      expect_equal(prompt, "Hello")
    },
    on_generation_end = function(result) {
      end_called <<- TRUE
      expect_equal(result$text, "Mock response")
    }
  )
  
  result <- generate_text(mock_model, "Hello", hooks = hooks)
  
  expect_true(start_called)
  expect_true(end_called)
})

test_that("Tool Hooks operate correctly", {
  # Setup mock tool with a proper schema
  tool_called <- FALSE
  my_tool <- tool("test_tool", "desc", z_object(dummy = z_string()), function(args) {
    tool_called <<- TRUE
    "tool_result"
  })
  
  # Setup hooks
  tool_start_called <- FALSE
  tool_end_called <- FALSE
  
  hooks <- create_hooks(
    on_tool_start = function(t, args) {
      tool_start_called <<- TRUE
      expect_equal(t$name, "test_tool")
    },
    on_tool_end = function(t, res) {
      tool_end_called <<- TRUE
      expect_equal(res, "tool_result") # Raw result
    }
  )
  
  # Execute tool calls manually
  tool_calls <- list(list(id = "1", name = "test_tool", arguments = list()))
  results <- execute_tool_calls(tool_calls, list(my_tool), hooks = hooks)
  
  expect_true(tool_called)
  expect_true(tool_start_called)
  expect_true(tool_end_called)
})

test_that("Permission Hook (Implicit) allows execution", {
  my_tool <- tool("test_tool", "desc", z_object(x = z_string()), function(args) "ok")
  hooks <- create_permission_hook("implicit")
  
  results <- execute_tool_calls(
    list(list(id="1", name="test_tool", arguments=list())),
    list(my_tool),
    hooks = hooks
  )
  
  expect_equal(results[[1]]$result, "ok")
  expect_false(results[[1]]$is_error)
})

test_that("Permission Hook (Escalate) denies in non-interactive", {
  # In test environment, interactive() is FALSE
  my_tool <- tool("dangerous_tool", "desc", z_object(x = z_string()), function(args) "ok")
  hooks <- create_permission_hook("escalate", allowlist = c("safe_tool"))
  
  # Use suppressWarnings since it will warn about non-interactive
  results <- suppressWarnings({
    execute_tool_calls(
      list(list(id="1", name="dangerous_tool", arguments=list())),
      list(my_tool),
      hooks = hooks
    )
  })
  
  # Should have error because on_tool_approval returned FALSE -> stop() -> caught by tryCatch
  expect_true(results[[1]]$is_error)
  expect_match(results[[1]]$result, "Tool execution denied")
})

test_that("Permission Hook (Escalate) allows allowlisted tools", {
  my_tool <- tool("safe_tool", "desc", z_object(x = z_string()), function(args) "safe_result")
  hooks <- create_permission_hook("escalate", allowlist = c("safe_tool"))
  
  results <- execute_tool_calls(
    list(list(id="1", name="safe_tool", arguments=list())),
    list(my_tool),
    hooks = hooks
  )
  
  expect_equal(results[[1]]$result, "safe_result")
  expect_false(results[[1]]$is_error)
})
