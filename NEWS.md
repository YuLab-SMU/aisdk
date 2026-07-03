# aisdk 1.4.13

* Fixed an "implicit statefulness" defect in the OpenAI Responses adapter
  (`responses_model()` / `OpenAIResponsesLanguageModel`). It previously
  resent the full conversation history every turn *and* injected
  `previous_response_id`, which double-billed tokens against first-party
  OpenAI and hard-failed (HTTP 400) from the second turn onward against
  HTTP-stateless "Responses-compatible" proxies, with no way to recover
  within a session. Server-side state is now an explicit, gated policy:
  - New `responses_state_mode` argument on `create_openai()` and
    `create_custom_provider()` (and a `responses_state_mode` /
    `responses_stateful` key in `aisdk.yaml` provider configs). Values:
    `"stateless"` (resend full history, never send `previous_response_id`),
    `"server"`/`"auto"` (chain turns via `previous_response_id`, sending
    only the new turn). First-party `create_openai()` defaults to `"auto"`;
    custom/proxy providers default to `"stateless"` so they work out of the
    box.
  - When an endpoint rejects `previous_response_id`, the model now
    auto-degrades to stateless (drops the id, resends full history, and
    stays stateless for the rest of the session) instead of getting stuck.
  - The same gating is applied to multi-turn image editing on
    `OpenAIImageModel`.
  - Replayed **assistant** turns are now serialized as `output_text`
    content (the Responses API rejects assistant `input_text`, which
    HTTP-stateless proxies surface as a 5xx upstream error from the second
    turn on). User turns continue to use `input_text`.
  - The Custom API setup flow now asks for the Responses state policy and
    persists it to `.Renviron` (`AISDK_CUSTOM_RESPONSES_STATE_MODE`) or
    `aisdk.yaml` (`responses_state_mode`).
* `console_chat()` gained a `/reset` command (and
  `ChatSession$reset_model_state()`) that clears a model's server-side
  response-id state without discarding the conversation history.
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
