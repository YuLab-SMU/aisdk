# Golden Rules for Skills

## 1. Concise is Key
`SKILL.md` is always in context when triggered. This is a public good (shared resource).
-   **Rule**: Keep `SKILL.md` lean (< 500 lines).
-   **Action**: Challenge every paragraph. Does the agent *really* need this in the initial context?

## 2. Progressive Disclosure
Move large tables, schemas, detailed policies, or long examples to `references/`.
-   **Benefit**: Saves tokens. Loaded only when needed via `read_skill_resource`.

## 3. Robust Scripts
Scripts in `scripts/` are the "hands" of the agent.
-   **Input Validation**: Always check `args`.
-   **Error Handling**: Use `tryCatch` to return meaningful error messages, not just crash.
-   **Output**: Return clear strings or structured data.

## 4. Imperative Instructions
Tell the agent *what* to do.
-   **Bad**: "You should probably try to..."
-   **Good**: "Execute script X."
