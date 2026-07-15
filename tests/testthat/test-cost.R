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

# --- Z1: OpenAI subset-style cached-token accounting -------------------------

test_that("estimate_cost prices OpenAI cached_tokens as a discounted subset", {
  withr::local_options(aisdk.model_pricing = list(
    "gpt-4o" = list(input = 2.5, output = 10, cache_read = 1.25)
  ))
  # 1M prompt, 800k of it cached: 200k fresh at 2.5 + 800k cached at 1.25.
  cost <- estimate_cost(
    list(prompt_tokens = 1e6, completion_tokens = 0, cached_tokens = 8e5), "gpt-4o"
  )
  expect_equal(cost, 0.2 * 2.5 + 0.8 * 1.25)
  # Cheaper than no caching (which pays full input rate on all 1M).
  no_cache <- estimate_cost(list(prompt_tokens = 1e6, completion_tokens = 0), "gpt-4o")
  expect_true(cost < no_cache)
})

test_that("openai_usage_with_cache surfaces nested cached_tokens", {
  u <- aisdk:::openai_usage_with_cache(list(
    prompt_tokens = 100, completion_tokens = 20,
    prompt_tokens_details = list(cached_tokens = 80)
  ))
  expect_equal(u$cached_tokens, 80)
  # No cache details -> field absent.
  u2 <- aisdk:::openai_usage_with_cache(list(prompt_tokens = 100, completion_tokens = 20))
  expect_null(u2$cached_tokens)
})

# --- AO1: estimate_prompt_cost (pre-flight cost) -----------------------------

test_that("estimate_prompt_cost prices counted input tokens before sending", {
  source(test_path("helper-mock.R"))
  withr::local_options(aisdk.model_pricing = NULL)
  set_model_pricing("mock:mock-model", input = 1.0, output = 3.0) # $/1M tokens

  e <- estimate_prompt_cost(MockModel$new(), prompt = "price this prompt please and count")
  expect_s3_class(e, "aisdk_cost_estimate")
  expect_gt(e$input_tokens, 0)
  expect_equal(e$projected_output_tokens, 0L)
  # input-only cost = input_tokens / 1e6 * input_rate
  expect_equal(e$cost_usd, e$input_tokens / 1e6 * 1.0)
})

test_that("estimate_prompt_cost adds a projected output at the output rate", {
  source(test_path("helper-mock.R"))
  withr::local_options(aisdk.model_pricing = NULL)
  set_model_pricing("mock:mock-model", input = 1.0, output = 3.0)

  e <- estimate_prompt_cost(MockModel$new(), prompt = "hi", max_output_tokens = 1000)
  expect_equal(e$projected_output_tokens, 1000L)
  expect_equal(e$cost_usd, e$input_tokens / 1e6 * 1.0 + 1000 / 1e6 * 3.0)
})

test_that("estimate_prompt_cost returns NA cost when pricing is unknown", {
  source(test_path("helper-mock.R"))
  withr::local_options(aisdk.model_pricing = NULL)
  m <- MockModel$new()
  m$model_id <- "definitely-unpriced-model"
  e <- estimate_prompt_cost(m, prompt = "hi")
  expect_true(is.na(e$cost_usd))
  expect_gt(e$input_tokens, 0)          # tokens are still counted
})

test_that("estimate_prompt_cost print method is human-readable", {
  source(test_path("helper-mock.R"))
  withr::local_options(aisdk.model_pricing = NULL)
  set_model_pricing("mock:mock-model", input = 1.0, output = 3.0)
  e <- estimate_prompt_cost(MockModel$new(), prompt = "hi", max_output_tokens = 500)
  expect_output(print(e), "aisdk cost estimate")
  expect_output(print(e), "estimated cost")
})
