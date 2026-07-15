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

# --- Y3: run_eval_suite (task bank -> grade -> aggregate) ---------------------

test_that("run_eval_suite grades a task bank and aggregates pass-rate + cost", {
  wx <- tool(name = "get_weather", description = "w", execute = function(city = "X") "sunny")
  model <- MockModel$new(list(
    list(text = "Paris is the capital.", finish_reason = "stop",
         usage = list(prompt_tokens = 10, completion_tokens = 5, total_tokens = 15)),
    list(text = "wrong answer", finish_reason = "stop",
         usage = list(prompt_tokens = 8, completion_tokens = 4, total_tokens = 12)),
    list(text = "", tool_calls = list(list(id = "c1", name = "get_weather", arguments = list(city = "SF"))),
         finish_reason = "tool_calls", usage = list(prompt_tokens = 20, completion_tokens = 6, total_tokens = 26)),
    list(text = "It is sunny.", finish_reason = "stop",
         usage = list(prompt_tokens = 12, completion_tokens = 4, total_tokens = 16))
  ))
  withr::local_options(aisdk.model_pricing = list("mock-model" = list(input = 1, output = 2)))

  tasks <- list(
    eval_task("capital", "capital of France?", check = function(r) grepl("Paris", r$text)),
    eval_task("wrong", "capital of Japan?", check = function(r) grepl("Tokyo", r$text)),
    eval_task("weather-tool", "weather in SF?", tools = list(wx), max_steps = 3,
              check = function(r) { expect_tool_trajectory(r, "get_weather"); TRUE })
  )
  res <- run_eval_suite(tasks, model = model)

  expect_s3_class(res, "aisdk_eval_suite_result")
  expect_equal(res$n, 3)
  expect_equal(res$passed, 2)
  expect_equal(res$pass_rate, 2 / 3)
  expect_equal(res$total_tokens, 15 + 12 + 42) # weather task aggregates 26+16
  # 2/3 tasks named correctly with pass/fail.
  expect_true(res$tasks[[1]]$passed)
  expect_false(res$tasks[[2]]$passed)
  expect_true(res$tasks[[3]]$passed)
  expect_true(res$total_cost > 0)
})

test_that("run_eval_suite records a task that errors as a failure", {
  # A model that errors on the second task.
  model <- MockModel$new(list(
    list(text = "ok", finish_reason = "stop", usage = list(total_tokens = 5)),
    function(params) stop("boom")
  ))
  tasks <- list(
    eval_task("t1", "one", check = function(r) TRUE),
    eval_task("t2", "two", check = function(r) TRUE)
  )
  res <- run_eval_suite(tasks, model = model)
  expect_equal(res$passed, 1)
  expect_false(res$tasks[[2]]$passed)
  expect_match(res$tasks[[2]]$error, "boom")
})

test_that("eval_task validates its check argument", {
  expect_error(eval_task("t", "p", check = "not a function"), "must be a function")
})
