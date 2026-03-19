load_ai_review_contract <- function() {
  env <- aisdk_test_env()
  source_local_aisdk_file(env, "ai_review_contract.R")
  env
}

test_that("normalize_ai_review_options preserves legacy defaults", {
  env <- load_ai_review_contract()

  normalized <- env$normalize_ai_review_options(list(
    code = "summary(mtcars)",
    eval = TRUE
  ))

  expect_equal(normalized$review, "none")
  expect_equal(normalized$runtime, "static")
  expect_equal(normalized$embed_session, "none")
  expect_false(normalized$defer_eval)
  expect_false(normalized$use_memory)
})

test_that("normalize_ai_review_options enables review for legacy ai metadata", {
  env <- load_ai_review_contract()

  normalized <- env$normalize_ai_review_options(list(
    code = "plot(mpg ~ wt, data = mtcars)",
    ai_agent = "analyst",
    uncertainty = "high"
  ))

  expect_equal(normalized$review, "required")
  expect_equal(normalized$runtime, "static")
  expect_equal(normalized$embed_session, "summary")
  expect_false(normalized$defer_eval)
  expect_true(normalized$use_memory)
})

test_that("normalize_ai_review_options respects explicit modern settings", {
  env <- load_ai_review_contract()

  normalized <- env$normalize_ai_review_options(list(
    code = "lm(mpg ~ wt, data = mtcars)",
    review = "inline",
    runtime = "live",
    embed_session = "full",
    defer_eval = TRUE
  ))

  expect_equal(normalized$review, "inline")
  expect_equal(normalized$runtime, "live")
  expect_equal(normalized$embed_session, "full")
  expect_true(normalized$defer_eval)
  expect_true(normalized$use_memory)
})

test_that("normalize_ai_review_options rejects invalid configuration values", {
  env <- load_ai_review_contract()

  expect_error(
    env$normalize_ai_review_options(list(review = "later")),
    "review"
  )
  expect_error(
    env$normalize_ai_review_options(list(runtime = "browser")),
    "runtime"
  )
  expect_error(
    env$normalize_ai_review_options(list(embed_session = "verbose")),
    "embed_session"
  )
})

test_that("transition_ai_review_state follows the canonical lifecycle", {
  env <- load_ai_review_contract()

  expect_equal(env$transition_ai_review_state("draft", "run"), "ran")
  expect_equal(env$transition_ai_review_state("ran", "approve"), "approved")
  expect_equal(env$transition_ai_review_state("approved", "freeze"), "frozen")
  expect_equal(env$transition_ai_review_state("error", "regenerate"), "draft")
  expect_equal(env$transition_ai_review_state("draft", "reject"), "rejected")
})

test_that("transition_ai_review_state rejects invalid transitions", {
  env <- load_ai_review_contract()

  expect_error(
    env$transition_ai_review_state("frozen", "approve"),
    "Cannot transition"
  )
  expect_error(
    env$transition_ai_review_state("draft", "freeze"),
    "Cannot transition"
  )
})
