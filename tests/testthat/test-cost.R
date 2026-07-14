# Tests for cache-aware cost estimation (Y1)

test_that("estimate_cost prices input and output per million tokens", {
  cost <- estimate_cost(list(prompt_tokens = 1e6, completion_tokens = 1e6), "openai:gpt-4o")
  expect_equal(cost, 2.50 + 10.00)
})

test_that("estimate_cost resolves a versioned model by family prefix", {
  # claude-sonnet-4-5-... matches the "claude-sonnet" family key.
  cost <- estimate_cost(list(prompt_tokens = 1e6, completion_tokens = 0),
                        "anthropic:claude-sonnet-4-5-20250929")
  expect_equal(cost, 3.00)
})

test_that("estimate_cost prices cache tokens at their discounted rates", {
  # Anthropic reports cache read/write separately from prompt_tokens; read
  # defaults to 0.1x input, write to 1.25x input.
  cost <- estimate_cost(
    list(prompt_tokens = 0, completion_tokens = 0,
         cache_read_input_tokens = 1e6, cache_creation_input_tokens = 1e6),
    "claude-sonnet"
  )
  expect_equal(cost, 3.00 * 0.1 + 3.00 * 1.25)
})

test_that("estimate_cost returns NA for an unknown model", {
  expect_true(is.na(estimate_cost(list(prompt_tokens = 100), "mystery:unknown-model")))
  expect_true(is.na(estimate_cost(NULL, "gpt-4o")))
})

test_that("set_model_pricing overrides and resolves regardless of provider prefix", {
  withr::local_options(aisdk.model_pricing = list())
  set_model_pricing("acme:custom-1", input = 1, output = 2)
  expect_equal(
    estimate_cost(list(prompt_tokens = 1e6, completion_tokens = 1e6), "acme:custom-1"),
    3
  )
  # An override wins over the built-in table.
  set_model_pricing("gpt-4o", input = 99, output = 0)
  expect_equal(estimate_cost(list(prompt_tokens = 1e6, completion_tokens = 0), "openai:gpt-4o"), 99)
})

test_that("Telemetry$calculate_cost uses the model from the result when not given", {
  tel <- create_telemetry()
  res <- GenerateResult$new(
    text = "x",
    usage = list(prompt_tokens = 1e6, completion_tokens = 1e6),
    raw_response = list(model = "gpt-4o")
  )
  expect_equal(tel$calculate_cost(res), 12.5)
  # Unknown model -> NULL (not NA), preserving the method's contract.
  res2 <- GenerateResult$new(text = "x", usage = list(prompt_tokens = 1),
                             raw_response = list(model = "who-knows"))
  expect_null(tel$calculate_cost(res2))
})
