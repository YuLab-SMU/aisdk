# Tests for agent evaluation assertions (V2: trajectory / end-state / budget)

source(test_path("helper-mock.R"))

# A 2-tool, multi-step mock run used across the trajectory assertions.
multi_step_result <- function() {
  wx <- tool(name = "get_weather", description = "w", execute = function(city = "X") "sunny")
  tz <- tool(name = "get_time", description = "t", execute = function(tz = "UTC") "12:00")
  model <- MockModel$new(list(
    list(text = "", tool_calls = list(list(id = "c1", name = "get_weather", arguments = list(city = "SF"))),
         finish_reason = "tool_calls", usage = list(total_tokens = 100)),
    list(text = "", tool_calls = list(list(id = "c2", name = "get_time", arguments = list(tz = "PST"))),
         finish_reason = "tool_calls", usage = list(total_tokens = 60)),
    list(text = "Done.", tool_calls = NULL, finish_reason = "stop", usage = list(total_tokens = 40))
  ))
  generate_text(model = model, prompt = "weather+time?", tools = list(wx, tz), max_steps = 5)
}

test_that("expect_tool_trajectory grades a multi-step trajectory (subset/exact/ordered)", {
  res <- multi_step_result()
  expect_silent(expect_tool_trajectory(res, c("get_weather", "get_time")))
  expect_silent(expect_tool_trajectory(res, c("get_weather", "get_time"), exact = TRUE))
  expect_silent(expect_tool_trajectory(res, c("get_weather", "get_time"), ordered = TRUE))
  # Wrong order fails under ordered.
  expect_error(expect_tool_trajectory(res, c("get_time", "get_weather"), ordered = TRUE))
  # A tool never called fails.
  expect_error(expect_tool_trajectory(res, c("get_weather", "send_email")))
})

test_that("expect_tool_args matches a call's arguments in the trajectory", {
  res <- multi_step_result()
  expect_silent(expect_tool_args(res, "get_weather", list(city = "SF")))
  expect_silent(expect_tool_args(res, "get_time", list(tz = "PST")))
  expect_error(expect_tool_args(res, "get_weather", list(city = "NYC")))
})

test_that("expect_run_within enforces token/step/tool-call budgets", {
  res <- multi_step_result()
  # Aggregated across 3 steps: 100 + 60 + 40 = 200 tokens.
  expect_silent(expect_run_within(res, max_tokens = 500, max_steps = 5, max_tool_calls = 3))
  expect_error(expect_run_within(res, max_tokens = 10))
  expect_error(expect_run_within(res, max_tool_calls = 1))
})

test_that("expect_tool_selection now sees the full multi-step trajectory", {
  res <- multi_step_result()
  # Previously read result$tool_calls (NULL on the final turn) and missed
  # everything; now reads the accumulated all_tool_calls.
  expect_silent(expect_tool_selection(res, c("get_weather", "get_time")))
  expect_silent(expect_tool_selection(res, c("get_weather", "get_time"), exact = TRUE))
})
