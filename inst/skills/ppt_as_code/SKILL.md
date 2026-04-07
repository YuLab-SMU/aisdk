---
name: ppt-as-code
description: |
  Build webpage-native presentations instead of traditional PowerPoint. Use this skill whenever the user wants HTML slides, reveal.js decks, scroll-snap storytelling pages, product-launch-style demos, "PPT as code", web-based keynote pages, or asks to turn slide content into a reusable presentation asset instead of a one-off .pptx file. Also use it when the user wants prompts for Codex / Cursor / Claude Code to generate or upgrade a presentation system with slide transitions, fragments, URL sync, mobile adaptation, design systems, or presentation imagery.
aliases:
  - "PPT as Code"
  - "ppt as code"
  - "网页PPT"
  - "HTML slides"
  - "reveal.js slides"
  - "web presentation"
  - "演示文稿网页化"
  - "slides as code"
user-invocable: true
---

# PPT as Code

Use this skill to turn a slide request into a web-native presentation system rather than a disposable deck file.

This skill is for cases where the presentation should behave like a webpage asset:
- shareable by link
- iterated in code
- themeable and versionable
- reusable across talks, product demos, explainers, and landing-page-style decks

It is not the best default when the user explicitly needs a `.pptx`, a classroom template, or a locked corporate PowerPoint workflow.

## Default Stance

Do not start with fancy animation.

Start with the presentation system underneath:
1. `container`: the stage
2. `slides`: page-sized sections
3. `index`: current page state
4. `controls`: buttons, keyboard, dots, URL, history
5. `motion`: how state changes become transitions

If these five pieces are coherent, the deck exists.
Everything else is layering.

## What To Deliver

Unless the user asks otherwise, aim to deliver these in order:
1. a smallest viable presentation shell
2. one recommended implementation path
3. the exact prompt or scaffold to continue
4. only then the upgrades

Good default output formats:
- a single-file HTML prototype
- a reveal.js starter deck
- a concise architecture recommendation with tradeoffs
- a prompt pack for AI-assisted iteration

## Workflow

### 1. Classify the job first

Pick one primary mode before writing code:

- `single-file html`
  Use for the fastest editable prototype, AI-friendly iteration, quick link sharing, and lightweight custom decks.

- `enhanced vanilla deck`
  Use when the user wants full control and only a modest feature set: progress, dots, hash sync, fragments, and mobile adaptation.

- `reveal.js`
  Use when the user wants a mature presentation engine and does not benefit from rebuilding fragments, navigation, auto-animate, and deck conventions from scratch.

- `scroll-snap narrative`
  Use when the experience should feel like a scrolling story page rather than a stage-controlled slide deck.

If the choice is ambiguous, recommend one path and explain why in one short paragraph.

### 2. Default assumptions when the user is vague

If the user does not specify details, assume:
- desktop-first 16:9 composition
- mobile layout optimized for readability, not just scaled down
- restrained, modern visual direction
- keyboard navigation
- reduced-motion fallback
- transforms and opacity over `top`/`left` animation
- content and code in the user's working language

Do not default to purple cyberpunk aesthetics.

### 3. Build the smallest closed loop first

For a hand-written prototype, start with:
- one viewport
- 4 to 6 slides
- previous / next controls
- keyboard navigation
- one `currentIndex` state
- transform + opacity transitions

Keep the first version easy to edit. Do not over-engineer state.

### 4. Add upgrades only after the shell works

Recommended order:
1. progress bar and pagination dots
2. URL hash or History sync
3. fragments for intra-slide pacing
4. media preload strategy
5. mobile adaptation
6. visual system refinement
7. imagery workflow

If the user asks for many upgrades at once, still stage them in this order.

### 5. Choose the right tool boundary

- Keep `CSS transition` for simple slide motion and lightweight polish.
- Reach for `WAAPI` only when timing, chaining, or playback control actually matters.
- Consider `View Transition API` only if the state model is already stable and browser reality makes sense.
- Use `reveal.js` when the user wants a dependable framework more than bespoke mechanics.

See `references/framework-selection.md` when choosing among these.

### 6. Treat design as a system, not page repair

Before styling many slides, define:
- typography system
- color system
- spacing scale
- component rules

Do not improvise page by page.

If the user asks to “make it prettier”, first propose 3 distinct visual directions, then deepen one.

### 7. Use AI as a structuring assistant, not only a code generator

When helping with AI prompts:
- ask for the smallest runnable version first
- preserve existing structure when iterating
- separate style-layer edits from structure-layer edits
- distinguish framework choice from animation choice
- distinguish visual research from final image generation

Use the prompt templates in `references/prompt-pack.md` instead of inventing generic “make it better” prompts.

## Decision Rules

### Prefer hand-written HTML when

- the user wants a minimal, inspectable prototype
- the deck is custom and not very large
- the user expects AI to iterate directly on one file
- presentation mechanics are simple

### Prefer reveal.js when

- the user wants a real deck engine quickly
- fragments and slide navigation are core requirements
- long-term maintainability matters more than bespoke motion
- content authors will keep extending the deck

### Prefer scroll-snap when

- the presentation is meant to be browsed like a story page
- stage-style key-by-key control is not the main interaction

### Push back when

- the user asks for “many effects” but no pacing model
- the user wants to optimize animation before layout
- the user is mixing one-off business PPT needs with a code-first presentation workflow

## Design Heuristics

- Motion serves comprehension, not decoration.
- A deck should feel like one system, not many unrelated pages.
- Mobile adaptation means content reflow, not whole-page shrinking.
- Accessibility details are part of presentation quality, not afterthoughts.
- Code is the material; structure and pacing are the craft.

## Reference Files

- `references/article-summary.md`
  Read this when you need the distilled model from the source article.

- `references/framework-selection.md`
  Read this when deciding between vanilla HTML, scroll-snap, WAAPI, View Transition API, and reveal.js.

- `references/prompt-pack.md`
  Read this when the user wants high-quality prompts for Codex / Cursor / Claude Code, especially for iterative upgrades.

## Source

This skill was distilled from Russell's article/thread on X about “PPT as Code” and converted into a reusable workflow.
