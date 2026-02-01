#' @title Initialize a New Skill
#' @description
#' Creates a new skill directory with the standard "textbook" structure:
#' SKILL.md, scripts/, references/, and assets/.
#' @param name Name of the skill.
#' @param path Parent directory where the skill folder will be created.
#' @return Path to the created skill directory.
#' @export
init_skill <- function(name, path = ".") {
  skill_dir <- file.path(path, name)
  
  if (dir.exists(skill_dir)) {
    rlang::abort(paste0("Skill directory already exists: ", skill_dir))
  }
  
  # Create directory structure
  dir.create(skill_dir, recursive = TRUE)
  dir.create(file.path(skill_dir, "scripts"))
  dir.create(file.path(skill_dir, "references"))
  dir.create(file.path(skill_dir, "assets"))
  
  # Create template SKILL.md
  skill_md_content <- c(
    "---",
    paste0("name: ", name),
    "description: TODO - Provide a clear description of what this skill does and when to use it.",
    "---",
    "",
    paste0("# ", name),
    "",
    "## Quick Start",
    "",
    "TODO - Add basic instructions and a simple example.",
    "",
    "## Workflows",
    "",
    "TODO - Describe common sequences of actions.",
    "",
    "## Reference Material",
    "",
    "Detailed knowledge is available in the `references/` directory. Use `list_skill_resources` to explore."
  )
  
  writeLines(skill_md_content, file.path(skill_dir, "SKILL.md"))
  
  # Create a dummy reference to demonstrate
  writeLines(
    "# Example Resource\n\nThis is a reference file for the agent to read.",
    file.path(skill_dir, "references", "example.md")
  )
  
  message("Skill '", name, "' initialized at: ", skill_dir)
  invisible(skill_dir)
}

#' @title Package a Skill
#' @description
#' Validates a skill and packages it into a .skill (zip) file.
#' @param path Path to the skill directory.
#' @param output_dir Directory to save the .skill file. Defaults to current directory.
#' @return Path to the created .skill file.
#' @export
package_skill <- function(path, output_dir = ".") {
  if (!dir.exists(path)) {
    rlang::abort(paste0("Skill directory not found: ", path))
  }
  
  # Load skill to validate
  tryCatch({
    skill <- Skill$new(path)
  }, error = function(e) {
    rlang::abort(paste0("Skill validation failed: ", conditionMessage(e)))
  })
  
  # Validate description length as a proxy for quality
  if (is.null(skill$description) || nchar(skill$description) < 20) {
    rlang::warn("Skill description is very short. A better description helps the agent trigger the skill correctly.")
  }
  
  # Package into zip
  skill_file <- file.path(output_dir, paste0(skill$name, ".skill"))
  
  # Temporarily change directory to package correctly
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(dirname(path))
  
  utils::zip(
    zipfile = skill_file,
    files = basename(path),
    extras = "-q"
  )
  
  message("Skill '", skill$name, "' packaged to: ", skill_file)
  invisible(skill_file)
}
