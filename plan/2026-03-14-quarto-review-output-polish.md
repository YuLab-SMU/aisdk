# Quarto Review Output Polish

> Status: archived. This note belongs to the earlier interactive review-card
> rollout and is kept only as implementation history. The active UX direction
> is the simplified read-only artifact view.

> Goal: remove the empty Quarto code scaffold that still appears before
> published AI review cards in `.qmd` output, without changing the static-first
> runtime contract.

## Status Legend

- `[ ]` Not started
- `[~]` In progress
- `[x]` Completed
- `[!]` Blocked

## Overall Progress

- [x] Task 1: Suppress empty Quarto code scaffolding for AI review chunks

### Task 1: Suppress Empty Quarto Code Scaffolding for AI Review Chunks

**Status:** `[x]`

**Files**
- Modify: `R/knitr_engine.R`
- Verify: `tutorial/08-quarto-example.html`
- Update tracking: `plan/2026-03-14-quarto-review-output-polish.md`

**Checklist**
- [x] Confirm the rendered `.qmd` HTML still contains an empty cell scaffold.
- [x] Adjust `{ai}` engine output so Quarto does not emit an empty prompt/code shell ahead of the review card.
- [x] Re-render `tutorial/08-quarto-example.qmd`.
- [x] Verify the rendered HTML still contains the review card runtime manifest and no empty `sourceCode ai` scaffold.
- [x] Mark this task complete with a short verification note.

**Verification note**
- `tutorial/08-quarto-example.html` was re-rendered on 2026-03-14 against a
  temporary install of the patched local package.
- The previous empty `sourceCode ai` scaffold is no longer present ahead of the
  review card.
- The published card still embeds the runtime manifest and offline inspector,
  so the static-first review contract remains intact.
- Rendering and package installation both required unsandboxed execution in
  this environment because `processx`/`quarto` hit local sandbox restrictions;
  the resulting HTML was verified directly after render.
