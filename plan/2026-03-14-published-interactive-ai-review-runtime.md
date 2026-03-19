# Published Interactive AI Review Runtime Implementation Plan

> Status: archived. This plan documents the earlier attachable local-runtime
> review model. The current direction is a static, no-human-intervention
> artifact experience that keeps `ai` chunks but removes public interactive
> review workflows.

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Upgrade `aisdk` from a static-first review surface plus manual `review_document()` bridge into a publishable HTML experience that remains inspectable offline and becomes directly interactive when a local companion runtime is available.

**Architecture:** Keep the existing chunk lifecycle, `ProjectMemory` persistence model, and inline review card component. Add a second-layer runtime contract for published HTML: an embedded runtime manifest and client-side inspector that always works in `file://` mode, plus a discoverable local companion service that can attach to the page and enable approve / run / regenerate / session actions without requiring `runtime: shiny`.

**Tech Stack:** R, knitr, rmarkdown, quarto, htmltools, DBI/RSQLite, httpuv, processx/callr where needed for local helper launch, JavaScript/CSS under `inst/www`, `testthat`.

---

## Decision Summary

**Option A: Pure client-only HTML**
- Pros: simplest publish story, no local service needed.
- Cons: cannot execute R, persist approvals, or re-render/save source state.
- Decision: reject as the main architecture; keep only for read-only inspection.

**Option B: Embedded manifest + attachable local runtime**
- Pros: preserves static publishability, keeps offline inspection, enables live actions when a local helper is reachable, and stays aligned with the current `ProjectMemory` model.
- Cons: adds a second capability layer and handshake logic.
- Decision: recommend this as the next implementation target.

**Option C: Convert review documents to `runtime: shiny`**
- Pros: immediate interactivity.
- Cons: breaks the static publish contract, complicates Quarto/R Markdown support, and collapses the current static-first design.
- Decision: reject for this phase.

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
- [x] Task 1: Lock the published-runtime UX tiers and capability contract
- [x] Task 2: Add an embedded runtime manifest and offline interactive inspector
- [x] Task 3: Add local runtime discovery and handshake from published HTML
- [x] Task 4: Unify published-page actions with review lifecycle endpoints
- [x] Task 5: Add author-facing launch/open helpers that hide the bridge step
- [x] Task 6: Add publish/saveback flow and stale-state handling
- [x] Task 7: Regression tests, docs, and migration guidance

---

### Task 1: Lock the Published-Runtime UX Tiers and Capability Contract

**Status:** `[x]`

**Files:**
- Create: `R/ai_review_embedded_contract.R`
- Modify: `R/ai_review_contract.R`
- Modify: `R/knitr_engine.R`
- Test: `tests/testthat/test-ai-review-embedded-contract.R`
- Update tracking: `plan/2026-03-14-published-interactive-ai-review-runtime.md`

**Intent**
- Make the next phase explicit about which interactions work in which environment.
- Prevent the UI from overpromising behavior in `file://` mode.

**Step checklist**
- [x] Write failing tests for published runtime capability normalization.
- [x] Define capability tiers:
  - `inspect_only`
  - `attachable`
  - `connected`
  - `busy`
  - `stale`
  - `error`
- [x] Define action gating for:
  - `View Session`
  - `Copy Code`
  - `Run Draft`
  - `Regenerate`
  - `Approve & Freeze`
  - `Reject`
- [x] Define a deterministic manifest shape embedded into published HTML.
- [x] Decide which fields are required in offline mode vs connected mode.
- [x] Run: `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-embedded-contract.R', reporter = 'summary')"`
- [x] Mark this task complete in this plan with the manifest version.

**Completed:** `2026-03-14`
**Verification:** Added `R/ai_review_embedded_contract.R` with the canonical published-runtime contract helpers: capability tiers, action gating, required-field lists for offline vs connected states, and manifest builder version `1`. Updated `R/ai_review_contract.R` with a shared review action catalog and wired `R/knitr_engine.R` to persist `artifact$runtime_manifest` for review-enabled chunks so later tasks can embed and surface the same contract. Passed:
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-embedded-contract.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-knitr_engine.R', reporter = 'summary')"`

**Manifest version:** `1`

**Expected test coverage**
- Capability tiers normalize predictably.
- Invalid runtime capability declarations fail early.
- Offline and connected states expose different action sets without ambiguity.

---

### Task 2: Add an Embedded Runtime Manifest and Offline Interactive Inspector

**Status:** `[x]`

**Files:**
- Modify: `R/artifacts.R`
- Modify: `R/ai_review_card.R`
- Modify: `R/knitr_engine.R`
- Modify: `inst/www/aisdk-review.js`
- Modify: `inst/www/aisdk-review.css`
- Test: `tests/testthat/test-ai-review-embedded-viewer.R`
- Update tracking: `plan/2026-03-14-published-interactive-ai-review-runtime.md`

**Intent**
- Make the final HTML genuinely interactive even with no helper attached, at least for inspection.
- Remove the current gap where published output is technically visible but still feels inert.

**Step checklist**
- [x] Write failing tests for embedded manifest tags and client-side inspector rendering.
- [x] Extend the embedded provenance payload into a first-class runtime manifest with:
  - chunk metadata
  - action capability tier
  - review status
  - execution status
  - session/transcript summary
  - source document identity
  - optional saveback eligibility metadata
- [x] Add client-side interactions that work in plain `file://` mode:
  - expand/collapse transcript
  - view provenance metadata
  - copy extracted code
  - show clear connection state
- [x] Remove misleading static hints once the offline inspector exists.
- [x] Ensure Quarto and R Markdown produce the same HTML contract.
- [x] Run: `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-embedded-viewer.R', reporter = 'summary')"`
- [x] Mark this task complete in this plan with screenshots/notes if available.

**Completed:** `2026-03-14`
**Verification:** Published review cards now embed a dedicated runtime manifest script tag alongside provenance, expose an offline inspector panel in the rendered HTML, add `Copy Code` and runtime state UI that works in `file://` mode, and replace the old misleading “author preview only” message with contract-aware offline status text. The browser-side runtime reads the embedded manifest first, renders provenance/session details without a live helper, and only treats mutating actions as unavailable rather than pretending they can run offline. Verified with:
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-embedded-viewer.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-card.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-embedded-contract.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-provenance.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-lifecycle.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-end-to-end.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-knitr_engine.R', reporter = 'summary')"`

**Notes:** No screenshot was captured in-plan. HTML contract validation was covered through the rendered-card and engine tests for both embedded-manifest presence and offline inspector affordances.

**Expected test coverage**
- Published HTML embeds one manifest per chunk.
- Session/provenance inspection works without local server access.
- Offline actions never claim they can mutate state.

---

### Task 3: Add Local Runtime Discovery and Handshake From Published HTML

**Status:** `[x]`

**Files:**
- Modify: `R/ai_review_live.R`
- Modify: `inst/www/aisdk-review.js`
- Modify: `inst/www/aisdk-review.css`
- Test: `tests/testthat/test-ai-review-live.R`
- Test: `tests/testthat/test-ai-review-runtime-handshake.R`
- Update tracking: `plan/2026-03-14-published-interactive-ai-review-runtime.md`

**Intent**
- Let the published page discover whether a local helper is reachable and upgrade itself from inspect-only to interactive mode.

**Step checklist**
- [x] Write failing tests for runtime handshake state transitions.
- [x] Define a lightweight runtime handshake endpoint:
  - `ping`
  - capabilities
  - document identity match
  - chunk availability / stale detection
- [x] Make the client attempt attachment using deterministic local URLs and/or injected runtime URLs.
- [x] Surface connection states in-card:
  - `Offline`
  - `Connecting`
  - `Connected`
  - `Mismatch`
  - `Stale`
- [x] Prevent action buttons from enabling until document identity matches.
- [x] Run:
  - `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-live.R', reporter = 'summary')"`
  - `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-runtime-handshake.R', reporter = 'summary')"`
- [x] Mark this task complete in this plan with the chosen handshake URL scheme.

**Completed:** `2026-03-14`
**Verification:** Added a CORS-safe published-runtime handshake to the live bridge, upgraded injected bridge config from a single URL to `window.AISDK_REVIEW_RUNTIME`, and taught the browser runtime to attempt attachment via injected URLs first and then the deterministic fallback `http://127.0.0.1:39393/__aisdk_review__`. The client now posts document identity plus chunk IDs to `/__aisdk_review__/handshake`, then only enables mutating actions for cards whose document identity matches and whose chunk state is not stale. Cards explicitly surface `Offline`, `Connecting`, `Connected`, `Mismatch`, and `Stale` states instead of enabling all buttons after a blind ping. Verified with:
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-live.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-runtime-handshake.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-embedded-viewer.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-card.R', reporter = 'summary')"`
- `node --check inst/www/aisdk-review.js`

**Handshake URL scheme:** Injected bridge config uses `window.AISDK_REVIEW_RUNTIME.urls` when available; otherwise the page probes the deterministic local fallback `http://127.0.0.1:39393/__aisdk_review__`, then calls `POST /handshake` on the selected base URL.

**Notes:** The `review_document()` launcher test was skipped in this sandbox because local `httpuv` port binding is not permitted here, but the app-level handshake and config injection tests passed.

**Expected test coverage**
- Pages attach only to matching local runtimes.
- Offline mode degrades cleanly without JS errors.
- Stale HTML vs memory mismatches are detectable and visible.

---

### Task 4: Unify Published-Page Actions With Review Lifecycle Endpoints

**Status:** `[x]`

**Files:**
- Modify: `R/ai_review_live.R`
- Modify: `R/knitr_engine.R`
- Modify: `R/project_memory.R`
- Modify: `inst/www/aisdk-review.js`
- Test: `tests/testthat/test-ai-review-runtime-actions.R`
- Update tracking: `plan/2026-03-14-published-interactive-ai-review-runtime.md`

**Intent**
- Make actions triggered from the published page use the same lifecycle and persistence model as inline author preview, not a second ad hoc path.

**Step checklist**
- [x] Write failing tests for action dispatch from published HTML.
- [x] Reuse the current lifecycle operations for:
  - regenerate draft
  - run draft
  - approve/freeze
  - reject
  - fetch session transcript
- [x] Return structured action responses with:
  - updated state
  - updated timestamps
  - updated execution output
  - whether rerender is required
- [x] Add optimistic UI rules only where state transitions are deterministic.
- [x] Ensure actions mutate `ProjectMemory`, not just the DOM.
- [x] Run: `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-runtime-actions.R', reporter = 'summary')"`
- [x] Mark this task complete in this plan with notes on any intentionally deferred actions.

**Completed:** `2026-03-14`
**Verification:** Published-page actions now return structured lifecycle payloads and write back through the same `store_review` / `store_review_artifact` / `update_execution_result` persistence path used by the knitr engine. `approve-freeze` and `reject` update both review status and artifact state, `run` executes stored draft code through the existing execution helpers and persists execution output, and `regenerate` creates a fresh draft/transcript and stores it in `ProjectMemory` rather than only toggling DOM state. The browser runtime now consumes these structured payloads, updates state/output/code where safe, and only treats regenerate as rerender-required. Verified with:
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-runtime-actions.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-live.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-lifecycle.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-project-memory-review-artifacts.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-knitr_engine.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-runtime-handshake.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-embedded-viewer.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-card.R', reporter = 'summary')"`
- `node --check inst/www/aisdk-review.js`

**Deferred note:** `regenerate` currently reuses the stored prompt and local runtime session, but the published HTML still asks for a rerender to refresh all embedded draft-facing sections cleanly. Full publish/saveback refresh remains in later tasks.

**Expected test coverage**
- Published-page actions produce the same persisted states as author-preview actions.
- Transcript fetch still works after action transitions.
- Errors return explicit state and message payloads.

---

### Task 5: Add Author-Facing Launch/Open Helpers That Hide the Bridge Step

**Status:** `[x]`

**Files:**
- Modify: `R/ai_review_live.R`
- Create: `R/ai_review_launch.R`
- Modify: `man/review_document.Rd`
- Create: `man/open_review_document.Rd`
- Test: `tests/testthat/test-ai-review-open.R`
- Update tracking: `plan/2026-03-14-published-interactive-ai-review-runtime.md`

**Intent**
- Remove the UX footgun where users open `file://...html` directly and conclude the feature is broken.

**Step checklist**
- [x] Write failing tests for a higher-level open helper.
- [x] Add an opinionated helper, for example:
  - `open_review_document(path, render = FALSE, browse = interactive())`
  - or `render_and_open_review(path, format = "html")`
- [x] Make the helper:
  - resolve source vs output path
  - render if requested
  - launch the local runtime
  - open the connected URL, not the raw file
- [x] Keep `review_document()` as the lower-level API.
- [x] Document the distinction between raw HTML and connected review HTML.
- [x] Run: `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-open.R', reporter = 'summary')"`
- [x] Mark this task complete in this plan with the chosen public API.

**Completed:** `2026-03-14`
**Verification:** Added a new high-level `open_review_document()` helper that resolves source/output pairs before launch, delegates the actual live bridge to `review_document()`, and opens the connected `http://...` launcher URL rather than a raw `file://` HTML path. For source inputs with `render = FALSE`, it prefers an existing sibling `.html` output and only falls back to the source when no output exists. For `.html` inputs with `render = TRUE`, it looks for a sibling `.qmd` / `.Rmd` source and launches from source when available. `review_document()` remains the lower-level bridge API and its documentation now points authors to `open_review_document()` for the safer default. Verified with:
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-open.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-live.R', reporter = 'summary')"`

**Chosen public API:** `open_review_document(path, render = FALSE, browse = interactive())`

**Expected test coverage**
- Helper resolves `.Rmd`, `.qmd`, and `.html` correctly.
- Helper does not silently browse a disconnected `file://` URL.
- Existing `review_document()` callers remain backward compatible.

---

### Task 6: Add Publish/Saveback Flow and Stale-State Handling

**Status:** `[x]`

**Files:**
- Modify: `R/ai_review_live.R`
- Modify: `R/project_memory.R`
- Modify: `R/knitr_engine.R`
- Modify: `inst/www/aisdk-review.js`
- Create: `tests/testthat/test-ai-review-saveback.R`
- Update tracking: `plan/2026-03-14-published-interactive-ai-review-runtime.md`

**Intent**
- Close the loop from “interact in the published page” to “save a stable final artifact”.

**Step checklist**
- [x] Write failing tests for saveback and rerender-needed signaling.
- [x] Define how the runtime reports “this HTML is stale” after a mutating action.
- [x] Add a save/publish flow that can:
  - mark the in-memory review state as updated
  - optionally request a rerender of the source document
  - surface whether the current HTML no longer reflects the persisted state
- [x] Decide whether same-session rerender is in scope or explicitly deferred.
- [x] Ensure source identity and chunk IDs remain stable across saveback.
- [x] Run: `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-saveback.R', reporter = 'summary')"`
- [x] Mark this task complete in this plan with the chosen stale/rerender policy.

**Completed:** `2026-03-14`
**Verification:** Published runtime pages now embed `review.updated_at` into the runtime manifest and include it in the local-runtime handshake, so reopened HTML is marked `stale` whenever embedded review timestamps lag behind `ProjectMemory` or a chunk can no longer be matched. Added a first-class `Save & Refresh` action to the card toolbar, a dedicated `POST /__aisdk_review__/saveback` endpoint, and same-session saveback handling that rerenders the source `.Rmd` / `.qmd` when available, records saveback events in `ProjectMemory`, and returns reload metadata for the connected page. Verified with:
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-saveback.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-runtime-actions.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-runtime-handshake.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-embedded-contract.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-card.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-knitr_engine.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-live.R', reporter = 'summary')"` (`review_document()` port-binding case still skipped in this sandbox)
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-embedded-viewer.R', reporter = 'summary')"`
- `node --check inst/www/aisdk-review.js`

**Chosen stale / rerender policy:** The embedded HTML remains the source of truth for offline inspection, but it carries a manifest timestamp that lets a connected local runtime detect when the file is older than persisted review state. Freshly opened stale HTML is downgraded to `stale` and only offers non-destructive inspection plus saveback. In an already-connected page, mutating actions keep the current DOM usable and simply mark the page as needing `Save & Refresh`; the explicit saveback step rerenders the source document, preserves chunk IDs, and hands the browser a reload URL so the author lands back on the refreshed connected page rather than a disconnected `file://` artifact.

**Expected test coverage**
- Mutating actions can mark a page stale without corrupting persisted review state.
- Chunk identity survives rerender/saveback transitions.
- Users get an explicit signal when they need to regenerate the final HTML.

---

### Task 7: Regression Tests, Docs, and Migration Guidance

**Status:** `[x]`

**Files:**
- Modify: `tutorial/01-basic-usage.Rmd`
- Modify: `tutorial/02-review-workflow.Rmd`
- Modify: `tutorial/03-advanced-features.Rmd`
- Modify: `tutorial/05-example.Rmd`
- Modify: `tutorial/08-quarto-example.qmd`
- Modify: `tutorial/README.md`
- Modify: `vignettes/integrations.qmd`
- Create: `tests/testthat/test-ai-review-published-e2e.R`
- Update tracking: `plan/2026-03-14-published-interactive-ai-review-runtime.md`

**Intent**
- Make the new interaction model discoverable and prevent the same UX confusion from recurring.

**Step checklist**
- [x] Write an end-to-end failing test for “render published HTML -> attach local runtime -> run action -> stale/save signal”.
- [x] Update tutorials to distinguish:
  - offline published inspection
  - connected interactive review
  - saveback / rerender expectations
- [x] Document which features work:
  - in raw `file://` HTML
  - in connected local runtime mode
  - after publish/freeze
- [x] Add migration notes for users coming from the current `review_document()` workflow.
- [x] Run:
  - `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-published-e2e.R', reporter = 'summary')"`
  - `Rscript -e "testthat::test_file('tests/testthat/test-tutorial-ai-engine-registration.R', reporter = 'summary')"`
- [x] Mark this task complete in this plan with the docs updated.

**Completed:** `2026-03-14`
**Verification:** Added `tests/testthat/test-ai-review-published-e2e.R` to cover the connected published-page lifecycle: rendered card HTML attaches successfully to the local runtime, a mutating action updates persisted review state, the original embedded timestamp reconnects as `stale`, and `Save & Refresh` rerenders the source document while preserving chunk identity and returning reload metadata. Updated the tutorial set and integration vignette to consistently explain the three capability tiers: raw `file://` inspection, connected local review, and re-rendered frozen output. Docs now prefer `open_review_document()` as the safer public entry point while keeping `review_document()` documented as the lower-level bridge helper. Verified with:
- `Rscript -e "testthat::test_file('tests/testthat/test-ai-review-published-e2e.R', reporter = 'summary')"`
- `Rscript -e "testthat::test_file('tests/testthat/test-tutorial-ai-engine-registration.R', reporter = 'summary')"`

**Docs updated:** `tutorial/README.md`, `tutorial/01-basic-usage.Rmd`, `tutorial/02-review-workflow.Rmd`, `tutorial/03-advanced-features.Rmd`, `tutorial/05-example.Rmd`, `tutorial/08-quarto-example.qmd`, `vignettes/integrations.qmd`

**Expected test coverage**
- Connected published page exercises the same review lifecycle as author preview.
- Tutorials no longer imply that raw file HTML is fully interactive.
- Docs reflect the actual capability tiers and saveback behavior.

---

## Non-Goals for This Plan

- Do not require `runtime: shiny` for review documents.
- Do not attempt in-browser execution of arbitrary R code without a local helper.
- Do not build a browser extension or desktop app in this phase.
- Do not redesign unrelated Shiny chat UI components.

## Open Design Constraints

- Preserve the current static-first, reproducible publish story.
- Reuse the existing `ProjectMemory` review artifact schema where possible.
- Keep one chunk lifecycle, even if there are multiple runtime tiers.
- Make raw `file://` output useful instead of pretending it is fully interactive.
- Prefer explicit stale-state and rerender signals over silent background mutation.

## Execution Notes

- Start with Task 1 and Task 2 before touching action transport. Until offline inspection and capability tiers are explicit, the UI will keep feeling broken.
- Treat `review_document()` as the current low-level primitive, not the final UX.
- Update this file after every completed task. This file is the source of truth for implementation progress.
