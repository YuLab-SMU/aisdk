# Console Chat Phase 1-2 Implementation Plan

> Status: active. This plan covers only the first delivery slice for the
> modern console chat redesign and tracks the code-level rollout of app state,
> inspect mode, status rendering, and structured tool timeline capture.

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Land a stable Phase 1-2 foundation for `console_chat()` by introducing app-level UI state, first-class `inspect` mode, append-only status rendering, and structured tool timeline data without replacing the current `readline()` input model.

**Architecture:** Keep `console_chat()` usable throughout by layering an internal `ConsoleAppState` over the existing REPL. Route view mode and tool visibility through centralized state/options, capture tool events through existing CLI hooks, and render status/timeline summaries in append-only mode so the design works before any full-screen diff renderer exists.

**Tech Stack:** R, `cli`, internal streaming/tool-render helpers in `R/console.R`, `R/utils_cli.R`, `R/stream_renderer.R`, and `testthat`.

---

## Progress Tracking

**Status legend**
- `[ ]` Not started
- `[~]` In progress
- `[x]` Completed
- `[!]` Blocked

**Plan maintenance rules**
- Update this file immediately after landing a meaningful task or sub-step.
- Record the verification command or observed behavior when a task moves to `[x]`.
- If scope shifts, append a short `Scope Change` note rather than silently changing completed work.

**Overall progress**
- [x] Task 1: Introduce app state and first-class view modes
- [~] Task 2: Add append-only status bar and turn timeline summaries
- [~] Task 3: Expand focused tests for state and rendering helpers
- [x] Task 4: Close the Phase 1-2 handoff in the design doc
- [x] Task 5: Add overlay-backed inspector on top of captured state

---

### Task 1: Introduce App State and First-Class View Modes

**Status:** `[x]`

**Files**
- Create: `R/console_app.R`
- Modify: `R/console.R`
- Modify: `R/utils_cli.R`
- Test: `tests/testthat/test-console-ui.R`
- Update tracking: `plan/2026-03-19-console-chat-phase-1-2-implementation.md`

**Intent**
- Stop encoding console presentation state only through `verbose` and scattered helper options.
- Make `clean`, `inspect`, and `debug` concrete UI modes before any richer renderer lands.

**Checklist**
- [x] Add `ConsoleAppState`-style helpers for session metadata, view mode, tool state, capability flags, and current turn tracking.
- [x] Add centralized view-mode helpers for tool log mode and thinking visibility.
- [x] Update `console_chat()` to create and carry app state through the REPL.
- [x] Add `/inspect [on|off]` and route `/debug` through the same view-mode model.
- [x] Keep current `console_chat()` API compatible by deriving `debug` from the original `verbose` argument.

**Completed:** `2026-03-19`
**Verification:** Parsed `R/console_app.R`, `R/console.R`, and `R/utils_cli.R` successfully with `Rscript -e "parse(file='R/console_app.R'); parse(file='R/console.R'); parse(file='R/utils_cli.R')"` and added focused state-mode tests in `tests/testthat/test-console-ui.R`.

---

### Task 2: Add Append-Only Status Bar and Turn Timeline Summaries

**Status:** `[~]`

**Files**
- Modify: `R/console_app.R`
- Modify: `R/console.R`
- Modify: `R/utils_cli.R`
- Test: `tests/testthat/test-console-ui.R`
- Update tracking: `plan/2026-03-19-console-chat-phase-1-2-implementation.md`

**Intent**
- Make the console feel stateful before any overlay or full-frame renderer exists.
- Capture structured tool events now so later inspector work builds on real state, not log scraping.

**Checklist**
- [x] Render a status snapshot line from app state with model, sandbox, view, stream, local, and tool status.
- [x] Capture tool start/result events into the current turn via existing `cli_tool_start()` / `cli_tool_result()` hooks.
- [x] Store assistant text and per-turn timing in app state while a turn is executing.
- [x] Print a compact tool timeline summary after each turn in `inspect` mode.
- [x] Distinguish warnings/messages from tool result summaries as separate turn fields.
- [ ] Decide whether clean mode should eventually emit a compact "N tool steps" footer after tool-heavy turns.

**Verification so far**
- Tool events now populate `turn$tool_calls` with `status`, summaries, raw payloads, and elapsed time.
- `inspect` mode prints an append-only `Tool Timeline` block after the turn instead of switching the entire console into debug verbosity.
- Verified against the current workspace code with:
  `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-ui.R', reporter = 'summary')"`

---

### Task 3: Expand Focused Tests for State and Rendering Helpers

**Status:** `[~]`

**Files**
- Modify: `tests/testthat/test-console-ui.R`
- Optional: create `tests/testthat/test-console-app.R` if helper coverage grows
- Update tracking: `plan/2026-03-19-console-chat-phase-1-2-implementation.md`

**Intent**
- Make the Phase 1-2 slice regression-safe before Phase 3 starts layering overlays and inspector drill-down.

**Checklist**
- [x] Add tests for `/inspect` mode toggling and status refresh signals.
- [x] Add tests for status-line snapshots.
- [x] Add tests for tool timeline capture from CLI tool hooks.
- [x] Add tests for `with_console_chat_display()` deriving options from app state.
- [x] Verify the focused console UI test file passes against the current workspace code.
- [x] Add a focused streaming-path test that confirms assistant chunks accumulate in app state during `send_stream()`.
- [ ] Add a fallback-path test for status/timeline output when `cli` is unavailable.

---

### Task 5: Add Overlay-Backed Inspector on Top of Captured State

**Status:** `[x]`

**Files**
- Modify: `R/console_app.R`
- Modify: `R/console.R`
- Modify: `tests/testthat/test-console-ui.R`
- Update tracking: `plan/2026-03-19-console-chat-phase-1-2-implementation.md`

**Intent**
- Reuse the new turn/tool state immediately through real overlay state instead of one-off detail prints.
- Give `inspect` mode a persistent inspector surface even before a full diff renderer exists.

**Checklist**
- [x] Add overlay stack and focus target state to `ConsoleAppState`.
- [x] Add helper formatters for latest-turn detail and per-tool detail.
- [x] Make `/inspect turn` and `/inspect tool <index>` open an inspector overlay.
- [x] Add `/inspect close` to close the active inspect overlay.
- [x] Render the active overlay below the status bar in the append-only console path.
- [x] Add focused tests for overlay open/close state and boxed overlay rendering.

**Completed:** `2026-03-19`
**Verification:**
- Verified with:
  `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-ui.R', reporter = 'summary')"`

---

### Task 4: Close the Phase 1-2 Handoff in the Design Doc

**Status:** `[x]`

**Files**
- Modify: `plan/2026-03-19-console-chat-modern-tui-design.md`
- Modify: `plan/2026-03-19-console-chat-phase-1-2-implementation.md`

**Intent**
- Keep the architecture doc honest about what has shipped versus what remains design intent.

**Checklist**
- [x] Add a short implementation tracking section to the main design doc.
- [x] Mark Phase 1 as started/completed based on the current code, and Phase 2 as in progress.
- [x] Point future work to Phase 3 inspector overlay and composer work rather than re-describing Phase 1.
- [x] Add a short next-step note about warning/message capture and streaming-path verification.

**Completed:** `2026-03-19`
**Verification:** Updated `plan/2026-03-19-console-chat-modern-tui-design.md` with implementation status, phase snapshot, and a concrete follow-up list tied to the new Phase 1-2 plan.
