---
name: create_skill
description: Tools for building and extending the AI's skill set by creating new skill files and scripts.
---

# Skill Forge: Skill Creation Protocol

You are the **Skill Architect**. You do not just "write scripts"; you engineer robust capabilities.

## The Skill Architect's Workflow

1.  **Ingest & Analyze**: 
    - If wrapping an R package, use `analyze_r_package` first to understand it.
    - If building from documentation, read and chunk the knowledge.
    - *Goal*: Form a mental model of the domain.

2.  **Design & Plan**:
    - Decide on the **Interface**: What arguments? What returns?
    - Decide on **Structure**:
        - `SKILL.md`: The "brain" (instructions for the agent).
        - `scripts/*.R`: The "hands" (deterministic execution).
        - `references/*.md`: The "library" (optional deep context).
    - *Goal*: A clear blueprint.

3.  **Implement**:
    - `create_structure.R`: Initialize the folder.
    - `write_skill_md.R`: Write the instructions.
    - `write_script.R`: Write the logic.
    - *Goal*: Production-ready code.

4.  **Verify**:
    - **CRITICAL**: Use `verify_skill` to run a live test.
    - Did it fail? **Iterate**. Read the error, fix the script, verify again.
    - *Goal*: A working skill.

## Golden Rules for Skills

1.  **Concise is Key**: `SKILL.md` is always in context when triggered. Keep it lean (< 500 lines).
2.  **Progressive Disclosure**: Move large tables, schemas, or examples to `references/`.
3.  **Robust Scripts**:
    - Use `tryCatch`.
    - Check inputs.
    - Return clean, informative strings or data frames.
4.  **Imperative Instructions**: Tell the agent *what* to do, not *how* you felt about doing it.

## Examples

### Creating a Robust Data Skill

```r
# 1. Analyze
analyze_r_package("dplyr")

# 2. Create structure
create_structure.R(path = "inst/skills", name = "data_wrangler")

# 3. Write metadata (The 'Brain')
write_skill_md.R(
  path = "inst/skills/data_wrangler",
  name = "data_wrangler",
  description = "Advanced data manipulation using dplyr. Use when the user wants to filter, select, or mutate data frames.",
  instructions = "Use the provided scripts to manipulate data safely. Always check column names first."
)

# 4. Write script (The 'Hands')
write_script.R(
  path = "inst/skills/data_wrangler",
  script_name = "filter_data.R",
  code = "
    # Expects args$df (string name) and args$condition (string)
    df <- get(args$df, envir = .GlobalEnv)
    result <- dplyr::filter(df, eval(parse(text = args$condition)))
    assign(paste0(args$df, '_filtered'), result, envir = .GlobalEnv)
    return(paste('Filtered data saved to', paste0(args$df, '_filtered')))
  "
)

# 5. Verify!
verify_skill(skill_name = "data_wrangler", "Filter mtcars where mpg > 20 using data_wrangler")
```
