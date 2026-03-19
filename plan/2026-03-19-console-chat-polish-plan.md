# Console Chat Polish Plan

> Status: active. This plan covers the next polish pass for `console_chat()`
> after the startup/model chooser and inspector overlay became functional.

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Improve the perceived quality of `console_chat()` by tightening chooser copy, clarifying visual hierarchy between timeline and overlay surfaces, and making the status bar behave gracefully in narrow terminals.

**Architecture:** Build on the existing `console_setup`, `console_app`, and `console_frame` layers instead of adding more ad hoc rendering logic. Treat these as polish changes on top of the current frame-based console path, not a new architecture branch.

**Tech Stack:** R, `cli`, `R/console_setup.R`, `R/console_app.R`, `R/console_frame.R`, `R/console.R`, `testthat`.

---

## Progress Tracking

**Status legend**
- `[ ]` Not started
- `[~]` In progress
- `[x]` Completed

**Overall progress**
- [x] Task 1: Compress chooser copy and decision flow
- [x] Task 2: Improve timeline vs overlay visual hierarchy
- [x] Task 3: Add narrow-terminal status bar folding
- [x] Task 4: Add focused regression coverage for the polish pass

---

### Task 1: Compress Chooser Copy and Decision Flow

**Status:** `[x]`

**Files**
- Modify: `R/console_setup.R`
- Test: `tests/testthat/test-console-setup.R`

**Intent**
- Make the startup and `/model` chooser feel concise and product-like instead of verbose and form-driven.

**Checklist**
- [ ] Shorten chooser prompts so they read like quick terminal actions, not setup wizard screens.
- [ ] Shorten menu option labels while keeping destructive distinctions clear.
- [ ] Remove repeated phrasing like "for next time", "configured console model", and long provider-specific wording where shorter labels suffice.
- [ ] Review edit flow text so "use", "edit", "overwrite", and "save as project/global" are easy to scan.
- [ ] Keep enough clarity that tests can still assert behavior without depending on exact long strings.
- [x] Shorten chooser prompts so they read like quick terminal actions, not setup wizard screens.
- [x] Shorten menu option labels while keeping destructive distinctions clear.
- [x] Remove repeated phrasing like "for next time", "configured console model", and long provider-specific wording where shorter labels suffice.
- [x] Review edit flow text so "use", "edit", "overwrite", and "save as project/global" are easy to scan.
- [x] Keep enough clarity that tests can still assert behavior without depending on exact long strings.

**Acceptance criteria**
- A user can move through the chooser without reading long sentences.
- Existing-profile and manual-setup branches remain unambiguous.
- `/model` and startup chooser copy stay consistent.

**Completed:** `2026-03-19`

---

### Task 2: Improve Timeline vs Overlay Visual Hierarchy

**Status:** `[x]`

**Files**
- Modify: `R/console_app.R`
- Modify: `R/console_frame.R`
- Possibly modify: `R/console.R`
- Test: `tests/testthat/test-console-ui.R`

**Intent**
- Make it visually obvious that the timeline is a lightweight summary surface and the overlay is the deeper inspection surface.

**Checklist**
- [ ] Reduce the visual weight of timeline rendering relative to the overlay.
- [ ] Make overlay title, border, and navigation affordances feel more primary than the timeline block.
- [ ] Ensure timeline and overlay headings are stylistically distinct.
- [ ] Keep the append-only renderer readable in terminals with and without Unicode.
- [ ] Avoid adding so much ornament that the console becomes noisy again.
- [x] Reduce the visual weight of timeline rendering relative to the overlay.
- [x] Make overlay title, border, and navigation affordances feel more primary than the timeline block.
- [x] Ensure timeline and overlay headings are stylistically distinct.
- [x] Keep the append-only renderer readable in terminals with and without Unicode.
- [x] Avoid adding so much ornament that the console becomes noisy again.

**Acceptance criteria**
- In inspect mode, the user can instantly tell "timeline summary" from "active inspector overlay".
- Overlay content reads as the main inspection target.
- The clean/debug modes are not visually regressed.

**Completed:** `2026-03-19`

---

### Task 3: Add Narrow-Terminal Status Bar Folding

**Status:** `[x]`

**Files**
- Modify: `R/console_app.R`
- Modify: `R/console_frame.R`
- Test: `tests/testthat/test-console-setup.R`
- Test: `tests/testthat/test-console-ui.R`

**Intent**
- Prevent the status bar from becoming a long, awkward wrapped line in narrow terminals.

**Checklist**
- [ ] Introduce width-aware status line folding rules.
- [ ] Define at least three width tiers:
- [x] Introduce width-aware status line folding rules.
- [x] Define at least three width tiers:
  - wide: single-line full status
  - medium: compact single-line or two-line folded status
  - narrow: multiple short lines with prioritized fields
- [x] Decide which fields are mandatory at narrow widths:
  - model
  - view
  - tool state
  - context usage if available
- [x] Push lower-priority fields such as local/stream/base context details into secondary folded lines when width is tight.
- [x] Ensure the folding strategy stays deterministic for tests.

**Acceptance criteria**
- Wide terminals keep the current rich one-line feel.
- Narrow terminals no longer print an awkward wrapped sentence.
- Context usage remains visible in a compact form when possible.

**Completed:** `2026-03-19`

---

### Task 4: Add Focused Regression Coverage For The Polish Pass

**Status:** `[x]`

**Files**
- Modify: `tests/testthat/test-console-setup.R`
- Modify: `tests/testthat/test-console-ui.R`

**Intent**
- Keep this polish pass from becoming subjective-only work that regresses later.

**Checklist**
- [ ] Add tests for shortened chooser labels only at the helper-output layer, not brittle full transcript snapshots.
- [ ] Add tests for overlay/timeline section styling decisions where possible via frame data or boxed output helpers.
- [ ] Add tests for status bar folding at at least two widths.
- [ ] Keep existing startup/model chooser and status-change tests passing.
- [x] Add tests for shortened chooser labels only at the helper-output layer, not brittle full transcript snapshots.
- [x] Add tests for overlay/timeline section styling decisions where possible via frame data or boxed output helpers.
- [x] Add tests for status bar folding at at least two widths.
- [x] Keep existing startup/model chooser and status-change tests passing.

**Acceptance criteria**
- The new polish behavior is locked down by helper-level tests.
- The tests avoid depending on full CLI output transcripts unless unavoidable.

**Completed:** `2026-03-19`

**Verification:**
- `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-setup.R', reporter = 'summary'); testthat::test_file('tests/testthat/test-console-ui.R', reporter = 'summary')"`

---

## Recommended Execution Order

1. Compress chooser copy first.
2. Add width-aware status folding second.
3. Adjust timeline/overlay hierarchy once the status surface is stable.
4. Finish with focused regression coverage across all three areas.
