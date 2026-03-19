# Console Chat Phase 3 Inspector Overlay Plan

> Status: active. This plan tracks the first dedicated Phase 3 delivery slice
> for the console chat inspector overlay and its navigation model.

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Turn the current inspect surface into a real inspector workflow with overlay state, tool-by-tool navigation, and a clear path toward owned overlay regions and stronger focus handling.

**Architecture:** Build Phase 3 on top of the existing `ConsoleAppState`, turn/tool metadata, and append-only status/timeline path. Treat the overlay as a first-class state object now, then evolve rendering and focus semantics without rewriting the captured execution model again.

**Tech Stack:** R, `cli`, `R/console_app.R`, `R/console.R`, `tests/testthat/test-console-ui.R`.

---

## Progress Tracking

**Status legend**
- `[ ]` Not started
- `[~]` In progress
- `[x]` Completed

**Overall progress**
- [x] Task 1: Land overlay-backed inspector state
- [x] Task 2: Add command-driven tool navigation inside the inspector
- [ ] Task 3: Tighten focus semantics and overlay ownership
- [~] Task 4: Prepare the transition from append-only overlay rendering to frame-region ownership

---

### Task 1: Land Overlay-Backed Inspector State

**Status:** `[x]`

**Files**
- Modify: `R/console_app.R`
- Modify: `R/console.R`
- Modify: `tests/testthat/test-console-ui.R`

**Completed:** `2026-03-19`
**Verification:**
- `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-ui.R', reporter = 'summary')"`

**Notes**
- `ConsoleAppState` now carries `overlay_stack` and `focus_target`.
- `/inspect turn`, `/inspect tool <index>`, and `/inspect close` are wired to inspector overlay state.
- The active overlay renders as a boxed section below the status bar in the conservative append-only path.

---

### Task 2: Add Command-Driven Tool Navigation Inside the Inspector

**Status:** `[x]`

**Files**
- Modify: `R/console_app.R`
- Modify: `R/console.R`
- Modify: `tests/testthat/test-console-ui.R`

**Completed:** `2026-03-19`
**Verification:**
- `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-ui.R', reporter = 'summary')"`

**Notes**
- `/inspect next` and `/inspect prev` now traverse tool entries within the current overlay context.
- Turn-level overlay can promote into tool 1 on `next`, and tool-level overlay can move across sibling tools.
- Inspector lines now include explicit navigation hints so the overlay remains usable before keyboard navigation exists.

---

### Task 3: Tighten Focus Semantics and Overlay Ownership

**Status:** `[ ]`

**Files**
- Modify: `R/console_app.R`
- Modify: `R/console.R`
- Test: `tests/testthat/test-console-ui.R`

**Checklist**
- [ ] Distinguish overlay focus ownership from generic `focus_target` strings.
- [ ] Add explicit overlay dismissal aliases beyond `/inspect close` if needed.
- [ ] Prevent stale overlay payloads when the inspected turn is superseded or cleared.
- [ ] Decide whether opening inspect mode should restore the last inspector target or always start from the latest turn.

---

### Task 4: Prepare the Transition from Append-Only Overlay Rendering to Frame-Region Ownership

**Status:** `[~]`

**Files**
- Modify: `R/console_app.R`
- Modify: `R/console.R`
- Optional: create `R/console_frame.R`
- Test: `tests/testthat/test-console-ui.R`

**Checklist**
- [x] Extract a reusable frame-builder step for status, transcript, timeline, and overlay sections.
- [x] Stop treating overlay rendering as ad hoc post-status output.
- [ ] Define the first region-ownership contract for overlay content.
- [x] Add test coverage that validates the inspector overlay through frame-level data instead of just rendered strings.

**Verification so far**
- `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-ui.R', reporter = 'summary')"`

**Notes**
- Added `R/console_frame.R` with `build_console_frame()` and `render_console_frame()`.
- `console_chat()` now renders status/timeline/overlay through a shared frame path, even though the renderer is still append-only.
