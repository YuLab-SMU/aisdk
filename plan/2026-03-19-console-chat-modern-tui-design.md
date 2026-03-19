# Console Chat Modern TUI Design

> Status: active. This document defines the target UI architecture for
> `console_chat()` and now tracks the first implementation slice that has
> started landing in the codebase.

**Goal:** Evolve `console_chat()` from a styled line-oriented REPL into a modern terminal UI that feels clean in normal use, inspectable during troubleshooting, and structurally capable of richer interaction patterns such as overlays, autocomplete, focus management, and capability-aware rendering.

**Primary reference:** the `pi-tui` design direction in `badlogic/pi-mono`
emphasizes differential rendering, component interfaces, overlays, focus-aware
inputs, autocomplete, and terminal-capability-aware behavior. The intent here
is not to copy that package or port TypeScript concepts directly into R, but to
adopt the same architectural constraints where they improve `aisdk`'s terminal
experience. Source: https://github.com/badlogic/pi-mono and
https://github.com/badlogic/pi-mono/tree/main/packages/tui

**Implementation tracker:** `plan/2026-03-19-console-chat-phase-1-2-implementation.md`
Phase 3 tracker: `plan/2026-03-19-console-chat-phase-3-inspector-overlay.md`

## Current Implementation Status

### Status legend

- `[ ]` Not started
- `[~]` In progress
- `[x]` Completed

### Phase snapshot

- [x] Phase 1 has started and landed as an internal app-state layer with
  first-class `clean` / `inspect` / `debug` modes plus `/inspect` support.
- [~] Phase 2 is in progress with append-only status rendering and structured
  tool timeline capture wired through the existing CLI tool hooks.
- [~] An overlay-backed inspector now exists via `/inspect turn`,
  `/inspect tool <index>`, and `/inspect close`, but it still renders in the
  conservative append-only path rather than a diffed screen region.
- [ ] Phases 3-6 remain design-only and should build on the new state/timeline
  foundation rather than revisiting mode logic again.

---

## Why This Exists

The current `console_chat()` implementation already improved one major problem:
it no longer needs to dump raw tool-call JSON and noisy execution details in
normal mode. That change solved "too much noise", but it did not solve
"too little structure".

Today the console experience is still fundamentally a print stream:

- assistant text is streamed directly to stdout
- tool execution is rendered as inline status lines
- commands mutate behavior but there is no persistent app-level state surface
- menus and confirmations are printed into the same flow as the conversation
- there is no concept of layout, focus, overlay ownership, or frame rendering

This works for a basic REPL, but it becomes fragile once we want to support:

- a stable status bar
- long-running task progress
- an inspectable tool timeline
- multi-line composition
- slash-command autocomplete
- file-path autocomplete
- modal help/settings/inspect panels
- capability-aware rendering across different terminals

The next design should therefore shift from "styled output" to a
state-driven terminal application model.

---

## Product Principles

### 1. Information hierarchy

The default experience should expose only what helps the user complete the
task. Execution details belong one level deeper unless the user explicitly asks
for them.

### 2. Hidden complexity, reversible inspection

Normal mode should be calm. Nothing important should be permanently hidden.
Every important background action should remain inspectable on demand.

### 3. Stable structure over decorative output

A professional terminal UI is defined more by layout discipline and predictable
state than by color alone. The architecture must support stable regions,
overlays, and focus, not just pretty lines.

### 4. Graceful capability degradation

The UI must adapt to terminals with weaker support for cursor movement, unicode,
bracketed paste, or image rendering. "Fancy when available, correct when not"
is the rule.

### 5. Implementation must stay R-native

The design may borrow ideas from `pi-tui`, but it must still fit `aisdk`'s R
architecture, current streaming model, and package constraints.

---

## Current Pain Points

### Presentation

- Conversation, tool status, warnings, menus, and prompts all share one output
  channel.
- There is no persistent status surface for model, sandbox, mode, or activity.
- Debug state exists, but as a print toggle rather than a first-class UI mode.

### Interaction

- Input is single-line and `readline()` based.
- Menus and confirmations interrupt the flow by printing inline lists.
- There is no focus model or overlay ownership.

### Observability

- Tool status is visible, but not navigable.
- There is no persistent per-turn timeline or per-tool detail surface.
- Long-running work relies on ephemeral spinner text only.

### Extensibility

- Rendering is spread across `console.R`, `stream_renderer.R`,
  `utils_cli.R`, and direct helper calls.
- The code has render helpers, but not a component model.
- Testing focuses on helper functions rather than screen states or frame-level
  transitions.

---

## Target UX

The target experience should have three explicit presentation modes.

### Clean mode

This is the default.

- Fixed status bar
- Stable conversation area
- Compact input area
- Minimal tool activity summaries
- No raw tool JSON
- No thinking blocks

Typical flow:

```text
Model: openai:gpt-5.2   Sandbox: permissive   View: clean   Tools: idle
──────────────────────────────────────────────────────────────────────────
You
Is my ggtree version current?

Assistant
Checking your local R environment...
✓ R code completed

Your local ggtree version is 4.0.1. It appears newer than the currently
reported Bioconductor release line you compared against.

> _
```

### Inspect mode

This is the recommended middle layer between clean and debug.

- Keeps the clean layout
- Adds a per-turn tool timeline
- Makes each tool step explorable
- Shows warnings, messages, and summarized outputs
- Hides raw transport-level payloads by default

Inspect mode answers: "What happened?" without turning the UI into a log dump.

### Debug mode

This is for package development and troubleshooting.

- Full tool start/result details
- Raw or near-raw arguments
- Thinking visibility enabled
- Internal timing and loop-step metadata where available

Debug mode answers: "Why did this fail?"

---

## Proposed App Architecture

The central shift is to introduce an app-level state model and frame renderer.

### Core idea

`console_chat()` should stop being only a REPL loop. It should become a thin
shell around a `ConsoleApp` state machine with explicit render phases.

### Proposed layers

#### 1. App state layer

This owns the durable UI state:

- session metadata
- current mode (`clean`, `inspect`, `debug`)
- connection / execution state
- current input buffer
- conversation transcript
- current tool timeline
- overlay stack
- focus target
- terminal capability snapshot

Suggested object:

- `ConsoleAppState`

This can remain an environment or R6 object. It does not need to expose a
public user-facing API initially.

#### 2. Event layer

All user input and runtime updates should become normalized events:

- key presses
- command invocations
- tool start
- tool end
- stream chunk
- warning/message captured
- overlay open/close
- resize event if available

Suggested shape:

- `dispatch_console_event(state, event)`

This gives the UI one place to reason about transitions.

#### 3. Render layer

The renderer should build a full "virtual screen" description from state, then
render using one of two strategies:

- diff rendering for capable terminals
- append-only fallback for minimal terminals / non-interactive environments

Suggested shape:

- `build_console_frame(state)`
- `render_console_frame(frame, previous_frame, capabilities)`

#### 4. Component layer

Components should not print directly. Each component should return lines or a
box model representation.

Suggested initial components:

- `StatusBar`
- `ConversationPane`
- `Composer`
- `ToolTimeline`
- `OverlayFrame`
- `HelpOverlay`
- `InspectorOverlay`
- `ConfirmOverlay`

This is the R equivalent of the `pi-tui` component discipline without forcing a
full React-style framework.

---

## Layout Model

Version 1 should use a fixed three-region layout with optional overlays.

### Persistent regions

#### Top: status bar

Always visible.

Fields:

- model id
- sandbox mode
- view mode
- stream on/off
- local execution on/off
- tool state (`idle`, `running`, `waiting`, `error`)
- optional token counts for current turn

#### Middle: main transcript pane

Scrollable if needed.

Contains:

- user turns
- assistant turns
- compact execution summaries
- optional inspect-mode timeline blocks

#### Bottom: composer

Always visible in interactive mode.

Contains:

- input buffer
- slash command hinting
- file-path autocomplete surface
- paste summary notice

### Overlay system

Overlays float above the base layout and take focus.

Initial overlays:

- help
- tool inspector
- confirmation dialog
- command palette
- settings view

Overlay design rules:

- overlays always own focus while open
- overlays never silently modify the base transcript
- closing an overlay restores previous focus target

---

## State Model

The current code already distinguishes some implicit states. The new design
should make them explicit.

### Session-level state

- `model_id`
- `agent_enabled`
- `sandbox_mode`
- `stream_enabled`
- `local_execution_enabled`
- `view_mode`
- `capabilities`

### Turn-level state

- `turn_id`
- `phase`: `idle`, `thinking`, `tool_running`, `awaiting_user`, `rendering`, `done`, `error`
- `assistant_text`
- `tool_calls`
- `tool_results`
- `warnings`
- `messages`
- `elapsed_ms`

### Tool-level state

- `tool_id`
- `name`
- `status`: `queued`, `running`, `done`, `failed`
- `start_time`
- `end_time`
- `args_summary`
- `result_summary`
- `raw_args`
- `raw_result`

### Overlay state

- `id`
- `type`
- `focus_target`
- `payload`

### Input state

- `buffer`
- `cursor_col`
- `selection` if later needed
- `history_index`
- `autocomplete_items`
- `paste_mode`

---

## Capability Detection

This should happen once at startup and be stored in app state.

### Minimum capability flags

- `interactive`
- `ansi`
- `unicode`
- `cursor_addressing`
- `bracketed_paste`
- `truecolor`
- `inline_images`
- `ime_safe_cursor` if detectable

### Behavior by capability tier

#### Tier A: rich terminal

- differential rendering
- overlays
- cursor positioning
- animated spinners
- inline autocomplete panel

#### Tier B: moderate terminal

- region redraw without full diff sophistication
- static overlays rendered as boxed sections
- reduced animation

#### Tier C: fallback

- append-only output
- no overlay redraw
- command-driven inspection only

This preserves correctness if the user runs inside RStudio terminal, plain
system terminal, CI, or a limited pseudo-terminal.

---

## Input and Focus Design

### Composer goals

The composer should eventually replace plain `readline()` behavior.

Versioned feature ladder:

- v1: multi-line buffer, send vs newline behavior
- v2: command history navigation
- v3: slash-command autocomplete
- v4: file-path autocomplete
- v5: richer inline editing

### Focus targets

The app should track focus as one of:

- `composer`
- `transcript`
- `timeline`
- `overlay:<id>`

This becomes essential once inspect panels and overlays are introduced.

### Keybinding principles

- all keybindings should be centralized, not hardcoded in handlers
- defaults should be documented
- bindings should remain overrideable later

Suggested initial bindings:

- `Enter`: send
- `Shift+Enter`: newline
- `Ctrl+J`: newline fallback when `Shift+Enter` is unavailable
- `Ctrl+L`: clear screen / rerender frame
- `Ctrl+T`: open tool inspector
- `Ctrl+/`: open help
- `Esc`: close overlay
- `Up/Down`: history or navigation depending on focus
- `Tab`: autocomplete accept / cycle

---

## Tool Inspection Model

This is the most important new surface after the status bar.

### Why it matters

The current clean/debug split is binary. Users need a middle mode that lets
them inspect the execution path without switching the entire app into raw
development output.

### Proposed inspector behavior

In clean mode:

- tool activity appears only as one-line summaries

In inspect mode:

- the active turn shows a compact timeline
- selecting a timeline item opens an inspector overlay

The inspector overlay should show:

- tool name
- start/end timestamps
- elapsed duration
- summarized arguments
- summarized results
- captured warnings/messages
- full raw args/result in expandable sections

This preserves the iceberg principle:

- surface the semantic result first
- keep the machinery available below the fold

---

## Thinking and Execution Visibility

The current code already supports `show_thinking`. The next design should bind
thinking visibility to presentation mode, not only to one boolean.

### Mode behavior

- `clean`: hidden
- `inspect`: hidden by default, available from inspector or transcript detail
- `debug`: visible inline

### Important rule

Thinking content and tool payloads are different categories:

- thinking is model-internal explanation-like output
- tool payloads are execution artifacts

They should not be merged into one visibility flag forever, even if they share
the same toggle today.

---

## Differential Rendering Strategy

This is where the `pi-tui` influence matters most.

### Current behavior

Most updates are append-oriented or line-clearing hacks.

### Target behavior

The renderer should compare:

- previous frame
- next frame

Then emit the minimum screen operations needed to converge.

### Phase plan

#### Phase 1

Simple region ownership:

- rerender status bar when status changes
- rerender composer when input changes
- append transcript lines normally

#### Phase 2

Structured region diff:

- transcript scroll management
- inspector overlay open/close without repainting whole screen
- focused component rerender only

#### Phase 3

True frame diff:

- line-by-line virtual screen comparison
- minimal cursor motion and updates
- fewer redraw artifacts

For `aisdk`, this should be treated as an optimization and UX quality layer,
not as the first implementation blocker.

---

## Error and Long-Running Task UX

### Status vocabulary

Long-running work should no longer be represented only by spinner text.

Use explicit app states:

- `idle`
- `running`
- `waiting_for_user`
- `retrying`
- `done`
- `failed`

### Presentation

Status bar should show the current high-level state.

Transcript should only record semantically meaningful milestones:

- "Checking local environment"
- "Retrying with fallback"
- "Awaiting permission"
- "R code failed"

Low-level output remains available in inspect/debug surfaces.

### Cancellation

The design should reserve room for cancellable tasks even if not implemented in
phase 1. This aligns with loader/AbortSignal patterns in modern TUIs.

---

## Proposed Implementation Plan

This document describes the target architecture. The implementation should be
incremental and keep `console_chat()` usable at every stage.

### Phase 1: Formalize UI modes and app state

Goal: stop scattering mode logic across helper functions.

**Current status:** `[x]` Initial slice landed on `2026-03-19`.

Tasks:

- introduce `ConsoleAppState`
- move `clean` / `inspect` / `debug` into first-class state
- route current `verbose` and `/debug` through app state
- add `/inspect`
- centralize mode-dependent visibility rules

### Phase 2: Add status bar and structured transcript blocks

Goal: make the UI feel stable before replacing input.

**Current status:** `[~]` Append-only status snapshots and per-turn tool
timeline summaries are now wired; warning/message capture and deeper transcript
typing are now partially wired, with a conservative overlay-backed inspector
available before diff rendering starts.

Tasks:

- add top status bar rendering
- tag transcript entries by type
- represent tool runs as structured timeline events
- preserve append-only fallback for unsupported terminals

### Phase 3: Add tool inspector overlay

Goal: create the middle layer between clean and debug.

**Current status:** `[~]` The first overlay-backed inspector state has landed,
and `next/prev` tool navigation is now wired. It still renders as an
append-only boxed section, but status/timeline/overlay now flow through a
shared frame builder. The remaining work is to turn that overlay into a true
owned screen region with stronger focus/navigation semantics.

Tasks:

- store per-tool structured metadata
- add inspector overlay
- allow keyboard or command access to the active turn's tools
- move warning/message display into inspector summaries

### Phase 4: Replace plain input with a composer

Goal: improve composition without destabilizing the transcript model.

Tasks:

- multi-line buffer
- send/newline separation
- history navigation
- basic slash-command completion

### Phase 5: Add capability-aware overlays and autocomplete

Goal: complete the modern TUI baseline.

Tasks:

- overlay stack
- command palette
- file-path autocomplete
- better menu/confirm components
- graceful fallback for weak terminals

### Phase 6: Differential rendering and frame-level tests

Goal: reduce flicker and make the UI verifiable.

Tasks:

- virtual frame model
- diff renderer
- screen snapshot tests
- event transition tests

---

## Testing Strategy

The current helper-level tests are useful but insufficient once the UI becomes
stateful.

### Needed test categories

#### 1. State transition tests

Examples:

- `/debug on` changes mode
- tool start sets app state to `running`
- overlay open transfers focus

#### 2. Frame construction tests

Examples:

- clean mode frame hides raw args
- inspect mode frame includes timeline summaries
- debug mode frame includes detailed tool sections

#### 3. Capability fallback tests

Examples:

- no ANSI disables diff renderer
- no unicode falls back to ASCII-safe icons

#### 4. Interaction tests

Examples:

- command palette opens and closes
- composer history navigation works
- paste detection collapses large pasted blocks into summaries

#### 5. Regression tests for output hygiene

Examples:

- R warnings/messages stay captured
- raw tool JSON never leaks in clean mode
- overlays do not duplicate transcript content

---

## Risks and Tradeoffs

### Risk: building a half-framework

If the component model becomes too abstract too early, the code will grow
without delivering immediate UX value.

Mitigation:

- introduce only the components needed by the next phase
- keep state and rendering plain and explicit

### Risk: terminal portability

A renderer that assumes strong ANSI support will behave poorly in weaker
terminals.

Mitigation:

- capability detection first
- append-only fallback always available

### Risk: too much divergence from current implementation

A big-bang rewrite would slow iteration and make regressions harder to locate.

Mitigation:

- phase delivery
- preserve current `console_chat()` external API where possible
- keep mode toggles backward compatible

### Risk: overexposing internals again

If inspect mode becomes too verbose, it will collapse back into debug mode.

Mitigation:

- semantic summaries first
- raw payloads only in expandable inspector sections

---

## Recommended Initial Decisions

These decisions should be treated as the current recommendation unless future
implementation evidence proves them wrong.

### Decision 1

Do not port `pi-tui` concepts literally. Adopt its architecture patterns, not
its implementation surface.

### Decision 2

Add `inspect` as a first-class mode. A binary clean/debug switch is not enough.

### Decision 3

Build a status bar and tool inspector before replacing input. Structural
clarity matters more than early composer sophistication.

### Decision 4

Use capability-aware rendering from the beginning. Do not build a rich-terminal
assumption into core state management.

### Decision 5

Treat screen/frame tests as required once overlays or region redraw land.

---

## Open Questions

These are not blockers for the design, but they should be resolved before the
corresponding implementation phase begins.

- Should `inspect` be a persistent mode or a transient overlay-only workflow?
- Should the transcript remain append-only in phase 2, or should it become a
  scrollable virtual pane immediately?
- How much composer functionality is realistic without replacing `readline()`
  entirely?
- Should RStudio console and external terminals share one renderer, or should
  RStudio get a conservative compatibility path?
- Which telemetry fields are worth exposing in the status bar without creating
  visual clutter?

---

## Success Criteria

The redesign should be considered successful when all of the following are
true:

- clean mode feels calm even during tool-heavy turns
- inspect mode makes execution understandable without raw log spam
- debug mode still supports package development and troubleshooting
- the UI has persistent structure, not just styled output
- menus, confirms, and inspectors no longer pollute the transcript flow
- the design degrades safely in weaker terminals
- future features can land as components and events instead of ad hoc prints

---

## Suggested Next Implementation Plan

The Phase 1-2 checklist now exists at
`plan/2026-03-19-console-chat-phase-1-2-implementation.md`.

The next concrete work items should be:

- finish Phase 2 by storing warnings/messages separately from tool result
  summaries
- add focused streaming-path tests for assistant chunk accumulation
- start Phase 3 by exposing the captured tool timeline through an inspector
  overlay or command-driven drill-down view

This keeps the code moving from "stateful append-only console" toward a real
TUI without jumping straight into composer replacement or full-frame diffing.
