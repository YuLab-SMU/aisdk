# Console Chat Startup And Status UX Plan

> Status: active. This plan tracks the user-facing console_chat UX fixes for
> startup configuration flow and status bar behavior.

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make `console_chat()` start with human-friendly provider/model setup and make its status surface informative without noisy repetition.

**Architecture:** Reuse existing `.Renviron` utilities, provider factories, and model metadata helpers instead of inventing a separate configuration system. Split the work into startup profile discovery/persistence and status-frame behavior/context metrics so both areas stay testable.

**Tech Stack:** R, `cli`, `R/console.R`, `R/console_setup.R`, `R/console_app.R`, `R/console_frame.R`, `R/utils_env.R`, `R/utils_models.R`, `testthat`.

---

## Progress Tracking

**Status legend**
- `[ ]` Not started
- `[~]` In progress
- `[x]` Completed

**Overall progress**
- [x] Task 1: Add provider/model startup guidance and profile persistence
- [x] Task 2: Improve status bar content and reduce repeated rendering
- [~] Task 3: Add focused regression coverage for startup/status helpers

---

### Task 1: Add Provider/Model Startup Guidance and Profile Persistence

**Status:** `[x]`

**Files**
- Create: `R/console_setup.R`
- Modify: `R/console.R`
- Test: `tests/testthat/test-console-setup.R`

**Checklist**
- [x] Discover configured provider/model profiles from project and global `.Renviron`.
- [x] List discovered profiles and let the user choose one at startup.
- [x] Fall back to provider-first setup when no saved profile is available.
- [x] Prompt for API key, base URL, and model selection/manual entry.
- [x] Save chosen provider config to project or global `.Renviron`.
- [x] Add stronger coverage for interactive startup branching.

**Notes**
- The same chooser flow is now reused by `/model`, so startup and mid-session
  model switching no longer diverge.
- Existing profiles can now be edited and overwritten in place, not only used
  or bypassed.

**Completed:** `2026-03-19`
**Verification:**
- `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-setup.R', reporter = 'summary')"`

---

### Task 2: Improve Status Bar Content and Reduce Repeated Rendering

**Status:** `[x]`

**Files**
- Modify: `R/console_app.R`
- Modify: `R/console_frame.R`
- Modify: `R/console.R`
- Test: `tests/testthat/test-console-setup.R`
- Test: `tests/testthat/test-console-ui.R`

**Checklist**
- [x] Add context-capacity fields to the status line when metadata or heuristics are available.
- [x] Add estimated used/remaining context values.
- [x] Stop re-rendering the status line after every assistant reply in the append-only path.
- [x] Keep explicit status refresh on startup and user-triggered setting changes.
- [ ] Decide whether to expose exact vs estimated capacity labels differently in later UI polish.

**Completed:** `2026-03-19`
**Verification:**
- `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-ui.R', reporter = 'summary')"`

---

### Task 3: Add Focused Regression Coverage For Startup/Status Helpers

**Status:** `[~]`

**Files**
- Create: `tests/testthat/test-console-setup.R`
- Modify: `tests/testthat/test-console-ui.R`

**Checklist**
- [x] Add tests for `.Renviron` parsing and profile discovery.
- [x] Add tests for profile activation in the current process.
- [x] Add tests for context metrics appearing in the status line.
- [x] Add a focused test that validates unchanged status sections are skipped by the frame renderer.
- [x] Add an integration-style test for the startup chooser flow via injectable prompt functions or equivalent seams.

**Verification so far**
- `Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-setup.R', reporter = 'summary'); testthat::test_file('tests/testthat/test-console-ui.R', reporter = 'summary')"`
