#!/usr/bin/env Rscript

# Smoke runner for the real testthat coverage. Keep this script deterministic:
# no network calls, no default model calls, and no manual interaction.

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Package 'devtools' is required to run this smoke test.")
}
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required to run this smoke test.")
}

devtools::load_all(quiet = TRUE)
testthat::test_file(
  "tests/testthat/test-agent-failure-handling.R",
  reporter = "summary"
)
