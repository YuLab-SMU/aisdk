# TODO

## In Progress

## Done

- [x] Extend retrieval system with broader task-conditioned signals (execution monitoring, system info, runtime state)

- [x] Add task-aware semantic adapter features to retrieval and context assembly
- [x] Add optional learned reranking on top of deterministic retrieval scoring
- [x] Upgrade retrieval scoring with richer deterministic semantic features
- [x] Upgrade retrieval ordering to unified cross-provider scoring
- [x] Expose public LLM-synthesis policy controls
- [x] Expose retrieval-provider controls and ranking policy through public context-management config
- [x] Extend context retrieval beyond `ProjectMemory` to session memory, documents, and compacted transcript segments
- [x] Add public context-management configuration API for `ChatSession`
- [x] Update tests to use the new public API instead of direct metadata where appropriate
- [x] Regenerate documentation and run focused regression tests
- [x] Add adaptive context budget helpers
- [x] Add `ContextState` to `ChatSession`
- [x] Add structured context state rendering
- [x] Add built-in R context inspection tools
- [x] Connect object-card deep inspection hints
- [x] Add deterministic working-memory synthesis
- [x] Add snippet-based and fix-based `ProjectMemory` recall
