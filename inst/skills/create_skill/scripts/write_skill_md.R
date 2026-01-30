# Script: write_skill_md.R
# Description: Creates or updates the SKILL.md file with frontmatter and instructions.
# Arguments:
#   - args$path: Path to the skill directory.
#   - args$name: Name of the skill (for YAML).
#   - args$description: Brief description (for YAML).
#   - args$instructions: Detailed instructions for the body.

path <- args$path
name <- args$name
description <- args$description
instructions <- args$instructions

if (is.null(path) || is.null(name) || is.null(description) || is.null(instructions)) {
  stop("All arguments (path, name, description, instructions) are required.")
}

skill_md_path <- file.path(path, "SKILL.md")

# Create content
content <- c(
  "---",
  paste0("name: ", name),
  paste0("description: ", description),
  "---",
  "",
  instructions
)

writeLines(content, skill_md_path)
cat(paste0("Wrote SKILL.md to ", skill_md_path, "\n"))
