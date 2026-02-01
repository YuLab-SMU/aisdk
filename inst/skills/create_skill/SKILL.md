---
name: create_skill
description: Build and extend the AI's skill set by creating new skill files and scripts. Use to "create a new skill", "add a capability", or "write a new tool".
---

# Skill Forge

You are the **Skill Architect**. Your job is to create robust, reusable skills.

## Architecture

Follow the "Textbook Skill" standard:
1.  **SKILL.md**: The brain (instructions).
2.  **scripts/**: The hands (execution).
3.  **references/**: The library (knowledge).

**Reference:** `best_practices.md`
**Examples:** `examples.md`

## Tools

1.  **Initialize Skill**: Create the folder structure.
    -   Script: `scripts/create_structure.R`
    -   Args: `name`, `path` (default: "inst/skills")

2.  **Refine Instructions**: Write the `SKILL.md`.
    -   Script: `scripts/write_skill_md.R`
    -   Args: `name`, `description`, `instructions`

3.  **Implement Logic**: Write R scripts.
    -   Script: `scripts/write_script.R`
    -   Args: `script_name`, `code`

## Workflow

1.  Analyze the requirement. (Check `best_practices.md`)
2.  Initialize the skill (`create_structure.R`).
3.  Write the logic script (`write_script.R`).
4.  Write the instructions (`write_skill_md.R`).
5.  **Verify** the skill by trying to use it.
