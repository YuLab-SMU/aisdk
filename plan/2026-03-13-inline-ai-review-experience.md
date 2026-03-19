# Inline AI Review Experience Implementation Plan

> Status: archived. This plan documents the earlier interactive review
> direction. The current product direction has been simplified to published,
> read-only AI artifact cards with embedded trace and metadata instead of
> human-in-the-loop runtime actions.

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a static-first AI chunk review experience for `aisdk` where rendered R Markdown / Quarto documents show inline review cards, optional live author actions, and published outputs remain frozen, reproducible, and easy to inspect.

**Architecture:** Keep one chunk lifecycle and one card UI for both authoring and publishing. `eng_ai()` will emit structured review artifacts and render a consistent HTML card; `ProjectMemory` will persist review, execution, and session transcript metadata; an optional local live-review bridge will activate inline buttons for author preview without forcing all documents into `runtime: shiny`.

**Tech Stack:** R, knitr, rmarkdown, quarto, htmltools, DBI/RSQLite, Shiny/httpuv bridge for optional live actions, JavaScript/CSS assets under `inst/www`, `testthat`.

---

## Progress Tracking

**Status legend**
- `[ ]` Not started
- `[~]` In progress
- `[x]` Completed
- `[!]` Blocked

**Plan maintenance rules**
- Update this file immediately after finishing a task or a meaningful sub-step.
- Add the completion date and short verification note under the task when it moves to `[x]`.
- If scope changes, append a short `Scope Change` note instead of silently rewriting finished tasks.

**Overall progress**
- [x] Task 1: Lock the UX contract and chunk state model
- [x] Task 2: Extend persistence for review artifacts, execution, and transcripts
- [x] Task 3: Add reusable inline review card rendering for static HTML
- [x] Task 4: Add optional live-review bridge for author preview
- [x] Task 5: Refactor `eng_ai()` around the new lifecycle
- [x] Task 6: Integrate session/provenance embedding in published output
- [x] Task 7: Replace the current split review flow with a unified experience
- [x] Task 8: Add regression tests and documentation updates

---

### Task 1: Lock the UX Contract and Chunk State Model

**Status:** `[x]`

**Files:**
- Modify: `R/knitr_engine.R`
- Create: `R/ai_review_contract.R`
- Test: `tests/testthat/test-ai-review-contract.R`
- Update tracking: `plan/2026-03-13-inline-ai-review-experience.md`

**Intent**
- Define one canonical chunk lifecycle before changing rendering or storage.
- Preserve backward compatibility for existing `{ai}` chunks using `ai_agent` and `uncertainty`.

**Step checklist**
- [x] Write failing tests for chunk option normalization and state transitions.
- [x] Define canonical card states: `draft`, `ran`, `approved`, `frozen`, `rejected`, `error`.
- [x] Define normalized chunk options in `R/ai_review_contract.R`:
  - `review = c("none", "inline", "required")`
  - `runtime = c("static", "live")`
  - `embed_session = c("none", "summary", "full")`
  - `defer_eval = TRUE/FALSE`
- [x] Add compatibility mapping:
  - `ai_agent` or `uncertainty` implies review-enabled mode
  - existing chunks without review metadata keep current behavior
- [x] Update `eng_ai()` to call the normalizer without yet changing storage or UI behavior.
- [x] Run: `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-contract.R', reporter = 'summary')"`
- [x] Mark this task complete in this plan with a short verification note.

**Completed:** `2026-03-13`
**Verification:** Added `R/ai_review_contract.R`, wired `eng_ai()` through `normalize_ai_review_options()`, and passed `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-contract.R', reporter = 'summary')"` with 25 assertions.

**Expected test coverage**
- Existing chunks still normalize to current defaults.
- Review-enabled chunks get deterministic option values.
- Invalid values fail early with actionable errors.

---

### Task 2: Extend Persistence for Review Artifacts, Execution, and Transcripts

**Status:** `[x]`

**Files:**
- Modify: `R/project_memory.R`
- Modify: `man/ProjectMemory.Rd`
- Test: `tests/testthat/test-project-memory-review-artifacts.R`
- Update tracking: `plan/2026-03-13-inline-ai-review-experience.md`

**Intent**
- Move from storing only `prompt + response + status` to storing a full review artifact the UI can render and replay.

**Step checklist**
- [x] Write failing tests for storing and retrieving review artifacts, execution state, and transcript payloads.
- [x] Extend the `human_reviews` schema with fields needed for inline review:
  - `session_id`
  - `review_mode`
  - `runtime_mode`
  - `artifact_json`
  - `execution_status`
  - `execution_output`
  - `final_code`
  - `error_message`
- [x] Add migration-safe initialization so existing SQLite files continue to work.
- [x] Add helpers to `ProjectMemory`:
  - `store_review_artifact()`
  - `get_review_artifact()`
  - `update_execution_result()`
  - `append_review_event()` if needed for retries/audit trail
- [x] Ensure transcript payload is serializable JSON, not a live session object.
- [x] Run: `Rscript -e "testthat::test_file('tests/testthat/test-project-memory-review-artifacts.R', reporter = 'summary')"`
- [x] Mark this task complete in this plan with the schema version / migration note.

**Completed:** `2026-03-13`
**Verification:** Extended `human_reviews` with additive migration logic in `R/project_memory.R`, added `review_events`, implemented artifact/execution helpers, and passed `Rscript -e "testthat::test_file('tests/testthat/test-project-memory-review-artifacts.R', reporter = 'summary')"` with 14 assertions.
**Migration note:** Existing SQLite databases are upgraded in place via additive `ALTER TABLE` checks; no destructive reset or data rewrite is required.

**Expected test coverage**
- Fresh DB initialization.
- Existing DB upgrade path.
- Round-trip storage for transcript JSON and execution output.
- No data loss for already-approved chunks.

---

### Task 3: Add Reusable Inline Review Card Rendering for Static HTML

**Status:** `[x]`

**Files:**
- Create: `R/ai_review_card.R`
- Create: `inst/www/aisdk-review.css`
- Create: `inst/www/aisdk-review.js`
- Modify: `R/knitr_engine.R`
- Test: `tests/testthat/test-ai-review-card.R`
- Update tracking: `plan/2026-03-13-inline-ai-review-experience.md`

**Intent**
- Make every `{ai}` chunk render as a consistent, inspectable card in plain HTML, even when no live bridge is available.

**Step checklist**
- [x] Write failing tests for card HTML structure and read-only rendering.
- [x] Implement an `htmlDependency()` helper for review assets.
- [x] Implement a card builder that renders these sections when available:
  - Prompt
  - Draft response
  - Extracted code
  - Execution result
  - Session / provenance summary
- [x] Implement button slots with disabled/read-only fallback for static mode:
  - `Regenerate`
  - `Run Draft`
  - `Approve & Freeze`
  - `Reject`
  - `View Session`
- [x] Add compact state badges and timestamps.
- [x] Update `eng_ai()` so `results='asis'` returns the card wrapper, not plain concatenated markdown.
- [x] Run: `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-card.R', reporter = 'summary')"`
- [x] Mark this task complete in this plan with screenshots or notes if available.

**Completed:** `2026-03-13`
**Verification:** Added `R/ai_review_card.R`, static assets under `inst/www/`, updated `eng_ai()` to emit card HTML, declared `htmltools` in `DESCRIPTION`, and passed:
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-card.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-contract.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-project-memory-review-artifacts.R', reporter = 'summary')"`
**Notes:** Static mode now renders a read-only review toolbar with disabled actions and provenance/session summary when enabled; live action wiring is intentionally deferred to Task 4.

**Expected test coverage**
- Static cards render without Shiny runtime.
- HTML includes stable `data-` attributes for future JS hooks.
- Missing optional fields degrade cleanly.

---

### Task 4: Add Optional Live-Review Bridge for Author Preview

**Status:** `[x]`

**Files:**
- Create: `R/ai_review_live.R`
- Modify: `R/shiny_review.R`
- Modify: `R/knitr_engine.R`
- Test: `tests/testthat/test-ai-review-live.R`
- Update tracking: `plan/2026-03-13-inline-ai-review-experience.md`

**Intent**
- Keep published documents static while allowing authors to click inline actions during local review.

**Step checklist**
- [x] Write failing tests for live-mode action routing and disabled behavior when bridge is absent.
- [x] Define a minimal local review bridge API for:
  - approve / freeze
  - reject
  - rerun draft
  - regenerate draft
  - fetch transcript
- [x] Implement a local launcher, for example `review_document()` or equivalent, that serves the rendered HTML and action endpoints together.
- [x] Make `inst/www/aisdk-review.js` auto-detect whether the live bridge is available.
- [x] Keep static HTML safe when opened directly from disk: buttons should degrade to disabled/read-only with a clear hint.
- [x] Reuse `ProjectMemory` as the source of truth for state transitions.
- [x] Run: `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-live.R', reporter = 'summary')"`
- [x] Mark this task complete in this plan with the chosen bridge entry point.

**Completed:** `2026-03-13`
**Verification:** Added `R/ai_review_live.R` with a local HTTP bridge, request handlers, action dispatch, HTML bridge injection, and exported `review_document()`. Updated static review JS/CSS to auto-detect the bridge and enable inline actions. Passed:
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-live.R', reporter = 'summary')"` with bridge/action assertions passing and the actual TCP bind launcher test skipped in this sandboxed environment
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-card.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-contract.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-project-memory-review-artifacts.R', reporter = 'summary')"`
**Bridge entry point:** `review_document(path, memory = get_memory(), host = "127.0.0.1", port = NULL, browse = interactive())`

**Expected test coverage**
- Live mode can approve/reject a chunk.
- Static mode does not throw JS errors.
- Actions update persisted review state deterministically.

---

### Task 5: Refactor `eng_ai()` Around the New Lifecycle

**Status:** `[x]`

**Files:**
- Modify: `R/knitr_engine.R`
- Modify: `R/session.R` if transcript helpers are needed
- Test: `tests/testthat/test-knitr_engine.R`
- Test: `tests/testthat/test-ai-review-lifecycle.R`
- Update tracking: `plan/2026-03-13-inline-ai-review-experience.md`

**Intent**
- Separate generation, execution, approval, and freezing so the engine matches the user-facing workflow.

**Step checklist**
- [x] Write failing lifecycle tests for `defer_eval`, `approved -> frozen`, and retry behavior.
- [x] Split current `eng_ai()` flow into explicit phases:
  - normalize options
  - load artifact / frozen state
  - generate draft if needed
  - extract code
  - optionally execute draft
  - persist artifact
  - render card
- [x] Ensure approved chunks skip model calls and use stored final code / response.
- [x] Ensure `defer_eval = TRUE` stores a draft without executing code.
- [x] Ensure retries are captured as artifact history, not lost in plain text.
- [x] Keep current no-review path working for simple `{ai}` chunks.
- [x] Run:
  - `Rscript -e "testthat::test_file('tests/testthat/test-knitr_engine.R', reporter = 'summary')"`
  - `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-lifecycle.R', reporter = 'summary')"`
- [x] Mark this task complete in this plan with notes on backward compatibility.

**Completed:** `2026-03-13`
**Verification:** Refactored `R/knitr_engine.R` so review-enabled chunks now follow an explicit lifecycle: cached approved chunks reuse stored `final_code` without calling the model, `defer_eval = TRUE` persists a draft as `execution_status = "deferred"` without executing, retry attempts are captured in `artifact$retries` and `artifact$transcript`, and simple `review = "none"` chunks still render without touching `ProjectMemory`. Passed:
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-lifecycle.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-knitr_engine.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-contract.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-project-memory-review-artifacts.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-card.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-live.R', reporter = 'summary')"` (live bridge launcher bind test still skipped in this sandbox because local port binding is not permitted)
**Backward compatibility note:** Existing non-review `{ai}` chunks still bypass `ProjectMemory`; the only test harness adjustment was making `tests/testthat/test-knitr_engine.R` self-contained so the documented `test_file()` command works in isolation without a full package attach.

**Expected test coverage**
- Frozen chunk uses cache.
- Draft-only chunk avoids execution.
- Retry path persists latest stable artifact.
- Existing tutorial examples still parse.

---

### Task 6: Integrate Session and Provenance Embedding in Published Output

**Status:** `[x]`

**Files:**
- Modify: `R/knitr_engine.R`
- Modify: `R/artifacts.R`
- Create: `tests/testthat/test-ai-review-provenance.R`
- Update tracking: `plan/2026-03-13-inline-ai-review-experience.md`

**Intent**
- Let published HTML show how a chunk was produced without shipping a live runtime object.

**Step checklist**
- [x] Write failing tests for embedded provenance payloads in static output.
- [x] Define the provenance JSON shape:
  - prompt
  - model metadata
  - transcript summary or full transcript
  - retry count
  - approval status / timestamps
  - frozen code checksum
- [x] Add helpers in `R/artifacts.R` for serializing and embedding this payload into HTML.
- [x] Support `embed_session = "none" | "summary" | "full"`.
- [x] Ensure published output can render a read-only session viewer from embedded JSON.
- [x] Run: `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-provenance.R', reporter = 'summary')"`
- [x] Mark this task complete in this plan with payload size notes if needed.

**Completed:** `2026-03-13`
**Verification:** Added provenance builders/serializers in `R/artifacts.R`, wired `eng_ai()` to emit embedded provenance JSON for published cards, and updated static review card assets so published HTML can open a read-only session/provenance viewer without a live bridge. Passed:
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-provenance.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-card.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-lifecycle.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-knitr_engine.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-live.R', reporter = 'summary')"` (launcher bind test still skipped in this sandbox because local port binding is not permitted)
**Payload size note:** `embed_session = "summary"` stores deterministic metadata plus transcript excerpts only; `embed_session = "full"` stores the full transcript messages; `embed_session = "none"` omits the embedded JSON entirely. Published output embeds only static JSON and a code checksum, never a live `ChatSession` object.

**Expected test coverage**
- No live object serialization.
- Embedded JSON is deterministic.
- Large transcripts can be omitted or summarized.

---

### Task 7: Replace the Split Review Flow With a Unified Experience

**Status:** `[x]`

**Files:**
- Modify: `R/shiny_review.R`
- Modify: `R/shiny_ui.R`
- Modify: `R/shiny_server.R`
- Create: `tests/testthat/test-ai-review-ui-integration.R`
- Update tracking: `plan/2026-03-13-inline-ai-review-experience.md`

**Intent**
- Keep the global review panel useful, but make it consume the same artifact model and UI concepts as inline document review.

**Step checklist**
- [x] Write failing integration tests for reading the new artifact shape in the review panel.
- [x] Refactor `aiReviewPanelUI()` and `aiReviewPanelServer()` to use the same card/state terminology as inline cards.
- [x] Add deep-link support from the panel to a specific chunk or source file if feasible.
- [x] Ensure the panel can still act as a fallback queue for pending reviews across files.
- [x] Remove duplicated markdown rendering / state logic where possible.
- [x] Run: `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-ui-integration.R', reporter = 'summary')"`
- [x] Mark this task complete in this plan with a short note on any remaining follow-up.

**Completed:** `2026-03-13`
**Verification:** Refactored `R/shiny_review.R` so the global review queue now renders the same artifact-backed review cards used inline instead of duplicating prompt/response rendering. The panel now builds provenance-backed cards, exposes full transcript/session viewing via the same `View Session` affordance, adds `file://...#chunk=` deep links for source navigation, and refreshes deterministically after approve/reject actions by invalidating its pending-review query. Added dynamic card initialization in `inst/www/aisdk-review.js` so cards inserted by Shiny `renderUI()` get the same provenance/session behavior as static documents. Passed:
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-ui-integration.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-card.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-provenance.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-live.R', reporter = 'summary')"` (launcher bind test still skipped in this sandbox because local port binding is not permitted)
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-lifecycle.R', reporter = 'summary')"`
**Follow-up note:** No direct changes were needed in `R/shiny_ui.R` or `R/shiny_server.R` because the package does not currently mount a higher-level review shell there; Task 7 unified the reusable review module itself so any future host surface can consume the same artifact-backed panel without additional rendering logic.

**Expected test coverage**
- Panel renders new artifact fields.
- Approve/reject updates inline and global views consistently.
- No regression for existing panel users.

---

### Task 8: Add Regression Tests and Documentation Updates

**Status:** `[x]`

**Files:**
- Modify: `tutorial/01-basic-usage.Rmd`
- Modify: `tutorial/03-advanced-features.Rmd`
- Modify: `tutorial/05-example.Rmd`
- Modify: `tutorial/08-quarto-example.qmd`
- Modify: `tutorial/README.md`
- Modify: `vignettes/integrations.qmd`
- Create: `tests/testthat/test-tutorial-ai-engine-registration.R` if expansion is needed
- Create: `tests/testthat/test-ai-review-end-to-end.R`
- Update tracking: `plan/2026-03-13-inline-ai-review-experience.md`

**Intent**
- Make the new experience discoverable, documented, and protected from regression.

**Step checklist**
- [x] Write an end-to-end failing test for at least one document flow: generate draft -> approve/freeze -> re-render from cache.
- [x] Update tutorials to show:
  - static-first review cards
  - live author preview entry point
  - published provenance view
- [x] Document the new chunk options and defaults.
- [x] Document migration behavior for existing `{ai}` users.
- [x] Re-run targeted tutorial registration coverage.
- [x] Run:
  - `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-end-to-end.R', reporter = 'summary')"`
  - `Rscript -e "testthat::test_file('tests/testthat/test-tutorial-ai-engine-registration.R', reporter = 'summary')"`
- [x] Mark this task complete in this plan with the docs updated.

**Completed:** `2026-03-13`
**Verification:** Added `tests/testthat/test-ai-review-end-to-end.R` to lock the draft -> approve -> frozen re-render lifecycle, expanded `tests/testthat/test-tutorial-ai-engine-registration.R` to scan both `tutorial/` and `vignettes/`, and updated the tutorial/vignette sources to match the implemented static-first review flow, including:
- `tutorial/01-basic-usage.Rmd`
- `tutorial/02-review-workflow.Rmd`
- `tutorial/03-advanced-features.Rmd`
- `tutorial/05-example.Rmd`
- `tutorial/07-quick-reference.md`
- `tutorial/08-quarto-example.qmd`
- `tutorial/README.md`
- `vignettes/integrations.qmd`

Passed:
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-end-to-end.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-tutorial-ai-engine-registration.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-provenance.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-card.R', reporter = 'summary')"`

**Scope Change:** `2026-03-14` Quarto follow-up: the original `.qmd` tutorial syntax used direct ` ```{ai ...}` blocks, but local verification showed Quarto rendered those literally instead of executing the custom knitr engine. Updated Quarto docs/examples to use `{r}` + `#| engine: ai`, normalized the pending-review file path filter, and broadened the registration test to detect Quarto `engine: ai` usage.
**Scope Change:** `2026-03-14` Quarto path follow-up: Quarto passes knitr an intermediate `.rmarkdown` input path, which caused review records to be stored under the intermediate file instead of the source `.qmd`. Added input-path canonicalization in `R/knitr_engine.R` plus a regression test in `tests/testthat/test-knitr_engine.R` so Quarto review records persist against the source `.qmd` path.
**Scope Change:** `2026-03-14` Tutorial path follow-up: several executed tutorial examples filtered pending reviews using repo-root-relative paths such as `tutorial/foo.Rmd`, but knitr/Quarto executes chunks relative to the document directory. Updated executed examples to derive the current document path from `knitr::current_input()` so pending-review queries work during real renders.

**Expected test coverage**
- End-to-end freeze path.
- No tutorial file with `{ai}` chunks is missing engine registration.
- Docs match implemented chunk options.

---

## Non-Goals for This Plan

- Do not turn all AI documents into `runtime: shiny`.
- Do not embed live `ChatSession` R objects in published HTML.
- Do not redesign unrelated Shiny chat UI components outside the review workflow.

## Open Design Constraints

- Keep current `{ai}` chunk behavior working when review mode is disabled.
- Prefer additive schema migration over destructive DB resets.
- Prefer one inline card component shared by author preview and published view.
- Prefer deterministic embedded artifacts over provider-specific session internals.

## Execution Notes

- Start with Task 1 and Task 2 before touching UI. Without a stable state model and persisted artifact shape, the UI will drift.
- Update this file after every completed task. This file is the source of truth for implementation progress.
