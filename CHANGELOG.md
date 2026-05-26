# Changelog

## 1.4.8

### Fixed
- Fixed official DeepSeek reasoning streams in `console_chat()` so assistant body text is rendered when it arrives immediately after a closing `</think>` tag.
- Wired the console stream tool-protocol filter into typed stream events so text fallback `<tool_call>...</tool_call>` blocks are not displayed as assistant prose.
- Kept tool-call turn preambles in intermediate text instead of duplicating them into the final assistant answer.

### Verification
- Ran `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-render_text.R', reporter = 'summary')"`.
- Ran `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-core-api.R', reporter = 'summary')"`.
- Ran `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-sse-aggregator.R', reporter = 'summary')"`.
- Ran `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-ui.R', reporter = 'summary')"`.
- Verified live `deepseek:deepseek-v4-pro` streaming emits `text_delta` events and replays correctly through the console renderer.

## 1.4.7

### Fixed
- Fixed OpenAI-compatible streaming when providers inline reasoning as `<think>...</think>` inside `delta$content`, as some self-hosted DeepSeek gateways do.
- Kept inline reasoning out of `GenerateResult$text` while preserving it in `GenerateResult$reasoning`, so console body text is not mistaken for thinking output.

### Verification
- Ran `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-sse-aggregator.R', reporter = 'summary')"`.
- Ran `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-core-api.R', reporter = 'summary')"`.
- Ran `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-ui.R', reporter = 'summary')"`.
- Ran `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-provider-custom.R', reporter = 'summary')"`.
- Ran `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-provider-deepseek.R', reporter = 'summary')"`.

## 1.4.6

### Fixed
- Fixed `console_chat()` typed streaming so assistant body text streams incrementally like thinking text instead of appearing only at turn completion.

### Verification
- Ran `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-core-api.R', reporter = 'summary')"`.
- Ran `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-ui.R', reporter = 'summary')"`.

## 1.4.5

### Fixed
- Fixed the agent runtime post-tool completion path so a model can finish by returning ordinary visible text after tool results.
- Kept `<final_answer>...</final_answer>` as an optional compatibility wrapper instead of requiring it as the only post-tool exit shape.
- Updated console-agent post-tool handling to stop cleanly when the agent has already produced a final answer.

### Verification
- Ran `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-core-api.R', reporter = 'summary')"`.
- Ran `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-ui.R', reporter = 'summary')"`.
- Ran `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-agent-failure-handling.R', reporter = 'summary')"`.

## 1.3.0

### Added
- Added provider-neutral multimodal content blocks via `input_text()` and `input_image()`.
- Added multimodal translation helpers for OpenAI Chat, OpenAI Responses, Gemini, and Anthropic.
- Added high-level image APIs: `analyze_image()`, `extract_from_image()`, `generate_image()`, and `edit_image()`.
- Added first-class image model abstractions: `ImageModelV1` and `GenerateImageResult`.
- Added image model support for Gemini, OpenAI, Volcengine, xAI, Stepfun, OpenRouter, and AiHubMix.
- Added multipart HTTP helper support for image editing APIs.
- Added native console-agent image tools for analysis, extraction, generation, editing, artifact recall, and local image discovery.
- Added session persistence for console image artifact memory.
- Added new vignettes for multimodal input and image generation.
- Added a semantic adapter stabilization checkpoint doc and dedicated semantic runtime session regression tests for canonical env invariants.

### Changed
- Updated `content_text()` and `content_image()` to bridge into the provider-neutral multimodal block format.
- Upgraded runtime capability validation so explicit non-vision language models fail early on image input.
- Expanded Gemini, OpenAI, and other provider documentation with image-model examples and support guidance.
- Improved `console_chat()` documentation to describe native image workflows and artifact reuse.
- Improved console-agent prompt guidance so image tasks are handled as native autonomous agent capabilities.
- Improved `list_directory` output for empty directories to keep console tool output stable.
- Updated Feishu setup messaging and documentation bindings for package-check consistency.
- Split Bioconductor semantic adapters into the new `aisdk.bioc` extension package.
- Split semantic benchmark and agent benchmark surfaces into the new `aisdk.bench` extension package.
- Changed the default semantic adapter registry so domain-specific adapters are loaded through explicit extension registrars instead of being hard-wired into `aisdk` core.

### Fixed
- Fixed OpenAI Responses multimodal payload translation for image input.
- Fixed Gemini multimodal input translation and image-output parsing.
- Fixed package documentation generation issues around Feishu setup exports.
- Fixed package-check portability issue caused by non-ASCII text in `R/channel_feishu_setup.R`.
- Fixed `Computer$execute_r_code()` so isolated execution always reports `execution_mode = "sandbox_exec"`.

### Verification
- Regenerated `NAMESPACE` and `man/` with `devtools::document()`.
- Rendered updated Quarto documentation for console chat and image-related vignettes.
- Ran targeted test files covering multimodal blocks, image APIs, provider integrations, console agent behavior, console UI, and session persistence.
- Ran `R CMD build .` and `R CMD check --as-cran` with `_R_CHECK_FORCE_SUGGESTS_=false` and TinyTeX on `PATH`.
