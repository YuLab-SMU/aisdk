# Semantic Adapter Stabilization Checkpoint

**Date:** 2026-04-16  
**Scope:** `aisdk` semantic adapter runtime closure before expanding new object families  
**Related issues:** `#4`, `#5`, `#6`

This checkpoint is the upstream stop-gate for semantic adapter expansion work.
Core adapter authoring helpers are now public, but new object-family support
should only proceed against the stable contract summarized here.

## First-Release Stable Contract

The following surfaces are treated as stable in `aisdk` core for the first
semantic adapter release:

- identity: adapter dispatch through `supports()` plus identity metadata from
  `describe_identity()`
- schema: structured schema summaries from `describe_schema()`
- semantics: structured semantic summaries from `describe_semantics()`
- summary and inspection: user-facing renderers via `render_summary()` and
  `render_inspection()`
- cost estimate: optional runtime and budget metadata from `estimate_cost()`
- action validation: safety and feasibility checks from `validate_action()`
- optional workflow hints: workflow routing metadata registered through
  `register_semantic_workflow_hint()`

## Runtime Guarantees Closed Out In This Checkpoint

The semantic runtime boundary is now explicit enough to support downstream
extension work:

- `ChatSession$get_envir()` is the canonical live session environment.
- `.semantic_adapter_registry` is stored in that canonical environment.
- `send()` and `send_stream()` pass the same session object through generation
  and tool execution paths.
- `SharedSession` reuses the canonical session environment as its global scope.
- `Computer$execute_r_code()` is explicitly `execution_mode = "sandbox_exec"`
  and does not mutate live session state.

## Explicit Non-Goals For The First Release

The following items are not promised by this checkpoint:

- planner-grade orchestration semantics
- backend-aware slicing contracts
- cache-aware layered disclosure
- new Bioconductor object families before the runtime contract above remains
  stable under regression coverage

In practice, this means object-family expansion for classes such as
`DelayedArray`, `HDF5Array`, or `DESeqDataSet` should happen only after staying
within this contract instead of redefining it.

## Expansion Gate

Downstream packages may now build against the semantic adapter authoring API and
the runtime guarantees listed above. Changes outside this checkpoint should be
treated as internal or experimental until a later checkpoint promotes them.
