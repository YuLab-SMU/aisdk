# CI Testing Lessons

## 2026-04-24: Mocked HTTP tests accidentally hit real APIs in GitHub Actions

### Symptom
`R-CMD-check` failed on GitHub Actions with `401` errors against the OpenAI API, even though those tests were intended to be unit tests with mocked HTTP helpers.

### Root cause
Some tests in `tests/testthat/test-provider-openai.R` used `local_mocked_bindings()` without explicitly binding mocks to the `aisdk` package namespace. Under GitHub Actions `R CMD check`, those mocks did not reliably replace the functions used by package code, so unit tests called the real `post_to_api()` / `stream_from_api()` helpers with a fake key like `"test-key"`.

### Fix
- In package test files that rely on mocked package internals, call:
  - `pkgload::load_all(export_all = FALSE, helpers = FALSE, quiet = TRUE)`
- When mocking internal helpers used by package code, prefer:
  - `testthat::local_mocked_bindings(..., .package = "aisdk")`

### Prevention checklist
Before merging provider or HTTP-layer test changes:
- Keep live API tests guarded with `skip_if_no_api_key()`.
- Keep unit tests fully offline by mocking package HTTP helpers.
- For package-internal mocks, always set `.package = "aisdk"`.
- If a test file is often run standalone, load the development package explicitly with `pkgload::load_all(...)`.
- When adding a new provider test that uses `post_to_api()`, `stream_from_api()`, or `post_multipart_to_api()`, verify CI safety by grepping for fake API keys like `"test-key"` and ensuring every such test has mocked transport.
