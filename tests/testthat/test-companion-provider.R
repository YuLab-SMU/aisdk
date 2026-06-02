# Tests for companion-provider resolution helpers (aisdk.providers / aisdk.skills
# install-prompt support added in 1.4.12).

test_that("provider_not_found_message hints at the companion package", {
  msg <- aisdk:::provider_not_found_message("deepseek", "openai, anthropic")
  joined <- paste(msg, collapse = "\n")
  expect_match(joined, "Provider not found: deepseek")
  expect_match(joined, "Available providers: openai, anthropic")
  expect_match(joined, "aisdk.providers")
  expect_match(joined, "install.packages")
})

test_that("provider_not_found_message omits the hint for unknown providers", {
  msg <- aisdk:::provider_not_found_message("not_a_real_provider", "openai")
  joined <- paste(msg, collapse = "\n")
  expect_match(joined, "Provider not found: not_a_real_provider")
  expect_false(grepl("supplied by", joined))
})

test_that("provider_not_found_message handles an empty registry", {
  msg <- aisdk:::provider_not_found_message("deepseek", "")
  joined <- paste(msg, collapse = "\n")
  expect_match(joined, "No providers registered")
  expect_match(joined, "aisdk.providers")
})

test_that("ensure_companion_provider returns FALSE for non-companion ids", {
  expect_false(aisdk:::ensure_companion_provider("not_a_real_provider"))
})

test_that("deepseek and kimi are mapped to aisdk.providers", {
  expect_identical(aisdk:::.companion_providers[["deepseek"]], "aisdk.providers")
  expect_identical(aisdk:::.companion_providers[["kimi"]], "aisdk.providers")
})
