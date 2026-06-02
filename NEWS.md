# aisdk 1.4.12

* Exported a small, stable "extension API" so that companion provider
  packages (such as `aisdk.providers`) can build on the core HTTP and
  image-handling machinery without reaching into the `aisdk` namespace:
  `api_endpoint_urls()`, `post_to_api()`, `post_multipart_to_api()`,
  `finalize_image_artifacts()`, `materialize_image_upload()`,
  `normalize_image_input_for_json()` and
  `normalize_image_input_to_url_like()`. These are documented with
  `\keyword{internal}` (kept out of the main help index) and are intended
  for package authors rather than end users; their behaviour is unchanged.
* When a model ID names a provider that lives in a companion package
  (e.g. `deepseek:...`, `kimi:...`) but that package is not installed, the
  provider registry now offers to install it in interactive sessions and,
  in non-interactive sessions, fails with a clear hint naming the package
  to install. Resolution of already-available providers is unchanged.

# aisdk 1.4.11

* Removed wall-clock elapsed-time assertions from two `r_eval`
  rejection/abort tests in `tests/testthat/test-r-introspect-tools.R`.
  The tests now rely solely on the structured output (the `REJECTED`
  status and the subprocess refusal marker), which already distinguishes
  an immediate rejection from a timeout wait. The previous
  `expect_lt(elapsed, ...)` thresholds were unreliable on loaded CRAN
  build machines (a `library(aisdk)` subprocess start alone exceeded the
  5s bound on the Fedora check farm) and are exactly the kind of timing
  test that "Writing R Extensions" advises against.
* Removed stray `Rplots*.pdf` plot artifacts that had been committed
  under `tests/testthat/`.

# aisdk 1.4.10

* Skip `r_eval` process-tree-reaping test on CRAN (it polls `pgrep`
  for `sh` + `sleep` grandchildren that the Linux kernel may reparent
  to PID 1 before `processx::kill_tree()` can finish; the TIMEOUT
  user-visible behaviour is exercised by a separate test).

# aisdk 1.4.9 (not released on CRAN)
* Layered architecture (Specification, Utilities, Providers, Core)
  with R6 classes for `Agent`, `Tool`, `Skill`, `Computer`, and
  `Telemetry`.
* Unified interface to multiple AI model providers (e.g. 'OpenAI',
  'Anthropic'), with request interception, robust error handling,
  and exponential retry delays.
* Local small language model inference, distributed 'MCP' ecosystem,
  multi-agent orchestration, progressive knowledge loading through
  skills, and a global skill store for sharing AI capabilities.
* Optional companion packages (aisdk.channels for Feishu/messaging
  integration, aisdk.skills for skill-forge tooling) are detected at
  runtime and degrade gracefully when not installed.
