# Script: create_structure.R
# Description: Creates the directory structure for a new skill.
# Arguments:
#   - args$path: Base path where skills are stored (e.g., "inst/skills").
#   - args$name: Name of the new skill (no spaces).

path <- args$path
name <- args$name

if (is.null(path) || is.null(name)) {
  stop("Both 'path' and 'name' are required.")
}

skill_dir <- file.path(path, name)
scripts_dir <- file.path(skill_dir, "scripts")

if (!dir.exists(skill_dir)) {
  dir.create(skill_dir, recursive = TRUE)
  cat(paste0("Created skill directory: ", skill_dir, "\n"))
} else {
  cat(paste0("Skill directory already exists: ", skill_dir, "\n"))
}

if (!dir.exists(scripts_dir)) {
  dir.create(scripts_dir, recursive = TRUE)
  cat(paste0("Created scripts directory: ", scripts_dir, "\n"))
} else {
  cat(paste0("Scripts directory already exists: ", scripts_dir, "\n"))
}

# Return the full path to the skill
skill_dir
