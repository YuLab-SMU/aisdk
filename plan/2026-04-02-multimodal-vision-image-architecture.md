# Multimodal Vision And Image Model Architecture Plan

Date: 2026-04-02

## Goal

Upgrade `aisdk` from partial image-message support to a coherent multimodal architecture that:

1. supports provider-neutral image input for language models
2. validates multimodal capabilities before provider calls
3. adds high-level image understanding APIs
4. adds first-class image generation and editing model support
5. leaves a clean extension path for future provider-specific image models

The design must support both "vision as multimodal language input" and "image model as image output" without conflating the two.

## Current Gaps

1. `build_messages()` already accepts structured message lists, but there is no provider-neutral content block IR.
2. `content_text()` and `content_image()` in `R/multimodal.R` emit OpenAI-style blocks, so helpers are coupled to one API shape.
3. OpenAI Responses does not translate image input into Responses API item types.
4. Gemini documents multimodal support, but its message formatter currently hardcodes user content as text-only parts.
5. Anthropic mostly passes blocks through, but there is no shared translation path from aisdk helpers into Claude image blocks.
6. Model metadata already exposes `vision` in `inst/extdata/models/*` and `R/utils_models.R`, but runtime capability validation is missing.
7. There is no first-class abstraction for image generation models, image outputs, or image editing.

## Core Decisions

1. Treat multimodal language input and image generation as separate model families.
2. Introduce a provider-neutral content block IR before touching provider payload builders.
3. Keep `generate_text()` and `stream_text()` as the main language APIs; layer `analyze_image()` and `extract_from_image()` on top.
4. Introduce a separate `ImageModelV1` abstraction and `generate_image()` / `edit_image()` APIs rather than overloading `LanguageModelV1`.
5. Preserve backward compatibility for existing `content_text()` / `content_image()` callers through a bridging layer and staged deprecation.

## Target Architecture

### Layer 1: Content Block IR

Add a normalized content block representation shared by all providers.

Recommended block shapes:

- `list(type = "input_text", text = "...")`
- `list(type = "input_image", source = list(kind = "file" | "url" | "data_uri" | "bytes"), value = "...", media_type = "image/png", detail = "auto")`
- reserve future block types now: `input_audio`, `input_document`, `input_video_frame`

Message content becomes:

- plain character strings for simple text-only flows
- a list of content blocks for multimodal flows

### Layer 2: Provider Translation

Each provider gets an explicit translation path from the normalized IR to its API payload shape.

- OpenAI Chat: `input_text -> text`, `input_image -> image_url`
- OpenAI Responses: `input_text -> input_text`, `input_image -> input_image`
- Gemini: `input_text -> parts$text`, `input_image -> inline_data | file_data`
- Anthropic: `input_text -> type=text`, `input_image -> type=image`

Provider translation should be localized to provider files, but reuse shared normalization helpers.

### Layer 3: Capability Validation

Upgrade model capability handling from display metadata to runtime checks.

Recommended capability flags:

- `vision_input`
- `image_output`
- `image_edit`
- `audio_input`
- `document_input`
- `video_input`

Validation rules:

- multimodal text calls with image blocks require `vision_input`
- `generate_image()` requires `image_output`
- `edit_image()` requires `image_edit`

### Layer 4: High-Level APIs

Add high-level helpers for the two highest-value vision workflows:

- `analyze_image(model, image, prompt, system = NULL, ...)`
- `extract_from_image(model, image, schema, system = NULL, ...)`

These should be thin wrappers over `generate_text()` with normalized content blocks.

### Layer 5: Image Model APIs

Add a separate image generation interface:

- `ImageModelV1`
- `GenerateImageResult`
- `generate_image(model, prompt, ..., output_dir = tempdir())`
- `edit_image(model, image, prompt, mask = NULL, ..., output_dir = tempdir())`

Provider factories should grow dedicated constructors such as:

- `provider$image_model(model_id)`
- optional later: `provider$image_edit_model(model_id)`

## File-Level Plan

### New Files

- `R/content_blocks.R`
- `R/content_translation.R`
- `R/image_api.R`
- `R/image_model.R`
- `vignettes/multimodal.qmd`
- `vignettes/image_generation.qmd`
- `tests/testthat/test-content-blocks.R`
- `tests/testthat/test-multimodal-validation.R`
- `tests/testthat/test-image-api.R`

### Existing Files To Modify

- `R/multimodal.R`
- `R/core_api.R`
- `R/spec_model.R`
- `R/provider_openai.R`
- `R/provider_gemini.R`
- `R/provider_anthropic.R`
- `R/utils_models.R`
- `R/utils_models_update.R`
- `_quarto.yml`
- `DESCRIPTION` if new docs/examples require additional suggested packages

### Optional Later Files

- provider adapters built on OpenAI-compatible APIs:
  - `R/provider_openrouter.R`
  - `R/provider_volcengine.R`
  - `R/provider_bailian.R`
  - `R/provider_stepfun.R`
  - `R/provider_xai.R`
  - `R/provider_nvidia.R`

## Parallel Workstreams

### Workstream A: Content IR And Backward Compatibility

Scope:

- add normalized block constructors
- add helpers to detect and normalize old OpenAI-style content blocks
- update `content_text()` / `content_image()` to bridge to new internals
- add validator utilities used by `generate_text()` and `stream_text()`

Primary files:

- `R/content_blocks.R`
- `R/multimodal.R`
- `R/core_api.R`

Deliverables:

- stable internal IR
- compatibility bridge for old helpers
- tests for block parsing and coercion

### Workstream B: Provider Translation For Vision Input

Scope:

- implement translation helpers in OpenAI Chat, OpenAI Responses, Gemini, Anthropic
- update Gemini response parsing to detect image output parts
- ensure tool call handling still works when message content is block-based

Primary files:

- `R/provider_openai.R`
- `R/provider_gemini.R`
- `R/provider_anthropic.R`

Deliverables:

- offline payload tests by provider
- minimal live smoke tests for image understanding

### Workstream C: High-Level Vision APIs

Scope:

- add `analyze_image()`
- add `extract_from_image()`
- connect schema-based extraction to existing structured output flows
- document common use cases

Primary files:

- `R/image_api.R`
- `R/core_api.R`
- `vignettes/multimodal.qmd`

Deliverables:

- ergonomic user-facing API for OCR/chart/UI/image analysis

### Workstream D: Image Model Abstraction

Scope:

- add `ImageModelV1` and `GenerateImageResult`
- add `generate_image()` and `edit_image()`
- add Gemini `image_model()` as first implementation target
- parse Gemini image output and persist to `tempdir()` by default

Primary files:

- `R/spec_model.R`
- `R/image_model.R`
- `R/image_api.R`
- `R/provider_gemini.R`

Deliverables:

- first-class image generation support without polluting `LanguageModelV1`

## Recommended Execution Order

### Milestone 1: Shared Multimodal Foundation

1. Add block IR and compatibility bridge.
2. Add runtime validation for image input.
3. Land provider translation for OpenAI Chat, OpenAI Responses, Gemini, Anthropic.

Exit criteria:

- one image-analysis example works end-to-end on each primary provider
- payload tests exist for all supported multimodal providers

### Milestone 2: High-Level Vision APIs

1. Add `analyze_image()`
2. Add `extract_from_image()`
3. Add `multimodal.qmd`

Exit criteria:

- users no longer need to hand-build message lists for common image tasks

### Milestone 3: Image Generation Foundation

1. Add `ImageModelV1` and `GenerateImageResult`
2. Add `generate_image()`
3. Add Gemini `image_model()` support
4. Parse image outputs and save files to `tempdir()`

Exit criteria:

- Gemini image model can generate image files through `generate_image()`

### Milestone 4: Provider Expansion

1. Add more image-model providers
2. Add image edit support where available
3. Extend capability matrix and docs

## Detailed Task Breakdown

### Task Group 1: Spec And Result Objects

- extend `R/spec_model.R` with:
  - `ImageModelV1`
  - `GenerateImageResult`
- keep `GenerateResult` for language flows only
- avoid changing public semantics of `LanguageModelV1`

### Task Group 2: Message Normalization

- add `normalize_content_blocks()`
- add `is_content_block_list()`
- add `coerce_legacy_multimodal_content()`
- update `build_messages()` comments and validation paths, not its public shape

### Task Group 3: Capability Resolution

- add helpers that inspect message content for block types
- validate against `model$capabilities`
- keep compatibility with old `vision` metadata by mapping it to `vision_input`

### Task Group 4: Provider Translation Internals

- OpenAI Chat:
  - convert normalized blocks into chat-completions `messages[*].content`
- OpenAI Responses:
  - convert normalized blocks into `input[*].content`
- Gemini:
  - stop assuming user content is text-only
  - support image input translation
  - parse image output parts
- Anthropic:
  - support normalized image blocks, not just raw pass-through lists

### Task Group 5: Image API Surface

- `analyze_image()` should accept one image first
- `extract_from_image()` should accept one image first
- support vector/list input later after the first stable release

### Task Group 6: Image Model Surface

- `generate_image()` should:
  - accept prompt
  - save images to `tempdir()` by default
  - return generated file paths and raw bytes metadata
- `edit_image()` should be scaffolded but can remain provider-limited in phase 1

## Testing Matrix

### OpenAI

- text + image input through chat completions
- text + image input through responses API
- capability validation failure on non-vision model

### Gemini

- text + image input translation
- image output parsing from response parts
- image model generation path

### Anthropic

- normalized image block translation
- cache-control interaction with content blocks

### Shared Runtime

- old helper compatibility
- `analyze_image()` wrapper behavior
- `extract_from_image()` wrapper behavior
- `generate_image()` output persistence

## Documentation Plan

1. Add `vignettes/multimodal.qmd` for image understanding workflows.
2. Add `vignettes/image_generation.qmd` for image model workflows.
3. Update:
   - `vignettes/providers-gemini.qmd`
   - `vignettes/providers-openai.qmd`
   - `vignettes/providers-anthropic.qmd`
   - `vignettes/providers.qmd`
4. Update `_quarto.yml` navbar to expose the new articles.
5. Regenerate `man/` with `devtools::document()`.

## Migration And Compatibility Strategy

### Phase 1 Compatibility

- keep `content_text()` and `content_image()`
- internally normalize them into the new IR
- accept old OpenAI-style block shapes where possible

### Phase 2 Soft Deprecation

- document `input_text()` / `input_image()` as the preferred API
- mark OpenAI-shaped helper outputs as legacy

### Phase 3 Provider Expansion

- once the IR is stable, retrofit OpenAI-compatible proxy providers
- add provider-specific `image_model()` support incrementally

## Risks

1. Breaking existing callers who directly construct provider-specific content blocks.
2. Divergent provider constraints on remote URLs, data URIs, and uploaded file references.
3. Mixing image generation concerns into language-model code paths.
4. Incomplete metadata for `vision` / `image_output` capabilities across model catalogs.
5. Documentation drift if provider vignettes mention features before the translation layer is complete.

## Risk Mitigations

1. Bridge old helper outputs through normalization rather than removing them.
2. Keep provider translation isolated and explicit.
3. Separate `ImageModelV1` from `LanguageModelV1`.
4. Map old `vision` metadata into new capability flags during transition.
5. Land docs only when the corresponding provider tests pass.

## Validation Checklist

- `Rscript -e "devtools::document()"`
- `Rscript -e "testthat::test_dir('tests/testthat')"`
- targeted provider tests while iterating:
  - `Rscript -e "testthat::test_file('tests/testthat/test-provider-openai.R')"`
  - `Rscript -e "testthat::test_file('tests/testthat/test-provider-gemini.R')"`
  - `Rscript -e "testthat::test_file('tests/testthat/test-provider-anthropic.R')"`
- `R CMD build .`
- `R CMD check --as-cran aisdk_*.tar.gz`
