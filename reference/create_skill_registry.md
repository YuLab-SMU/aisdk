# Create a Skill Registry

Convenience function to create and populate a SkillRegistry.

## Usage

``` r
create_skill_registry(path, recursive = FALSE)
```

## Arguments

- path:

  Path to scan for skills.

- recursive:

  Whether to scan subdirectories. Default FALSE.

## Value

A populated SkillRegistry object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Scan a skills directory
registry <- create_skill_registry(".aimd/skills")

# List available skills
registry$list_skills()

# Get a specific skill
skill <- registry$get_skill("seurat_analysis")
} # }
```
