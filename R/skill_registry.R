#' @title Skill Registry: Scan and Manage Skills
#' @description
#' SkillRegistry class for discovering, caching, and retrieving skills.
#' Scans directories for SKILL.md files and provides access to skill metadata.
#' @name skill_registry
NULL

#' @title SkillRegistry Class
#' @description
#' R6 class that manages a collection of skills. Provides methods to:
#' \itemize{
#'   \item Scan directories for SKILL.md files
#'   \item Cache skill metadata (Level 1)
#'   \item Retrieve skills by name
#'   \item Generate prompt sections for LLM context
#' }
#' @export
SkillRegistry <- R6::R6Class(
  "SkillRegistry",
  
  public = list(
    #' @description
    #' Create a new SkillRegistry, optionally scanning a directory.
    #' @param path Optional path to scan for skills on creation.
    #' @return A new SkillRegistry object.
    initialize = function(path = NULL) {
      private$.skills <- list()
      
      if (!is.null(path)) {
        self$scan_skills(path)
      }
      
      invisible(self)
    },
    
    #' @description
    #' Scan a directory for skill folders containing SKILL.md files.
    #' @param path Path to the directory to scan.
    #' @param recursive Whether to scan subdirectories. Default FALSE.
    #' @return The registry object (invisibly), for chaining.
    scan_skills = function(path, recursive = FALSE) {
      if (!dir.exists(path)) {
        rlang::abort(paste0("Directory does not exist: ", path))
      }
      
      path <- normalizePath(path, mustWork = TRUE)
      
      # Find all SKILL.md files
      # Find all SKILL.md files
      if (recursive) {
        skill_files <- list.files(
          path,
          pattern = "^SKILL\\.md$",
          recursive = TRUE,
          full.names = TRUE,
          ignore.case = FALSE
        )
      } else {
        # If not recursive, we still want to support the standard structure:
        # skills/
        #   skill_a/SKILL.md
        #   skill_b/SKILL.md
        
        # 1. Check root
        root_files <- list.files(
          path,
          pattern = "^SKILL\\.md$",
          recursive = FALSE,
          full.names = TRUE,
          ignore.case = FALSE
        )
        
        # 2. Check immediate subdirectories
        subdirs <- list.dirs(path, recursive = FALSE, full.names = TRUE)
        subdir_files <- character()
        
        if (length(subdirs) > 0) {
          subdir_files <- unlist(lapply(subdirs, function(d) {
             list.files(
               d,
               pattern = "^SKILL\\.md$",
               recursive = FALSE,
               full.names = TRUE,
               ignore.case = FALSE
             )
          }))
        }
        
        skill_files <- c(root_files, subdir_files)
      }
      
      for (skill_file in skill_files) {
        skill_dir <- dirname(skill_file)
        
        tryCatch({
          skill <- Skill$new(skill_dir)
          private$.skills[[skill$name]] <- skill
        }, error = function(e) {
          warning(paste0("Failed to load skill at ", skill_dir, ": ", conditionMessage(e)))
        })
      }
      
      invisible(self)
    },
    
    #' @description
    #' Get a skill by name.
    #' @param name The name of the skill to retrieve.
    #' @return The Skill object, or NULL if not found.
    get_skill = function(name) {
      private$.skills[[name]]
    },
    
    #' @description
    #' Check if a skill exists in the registry.
    #' @param name The name of the skill to check.
    #' @return TRUE if the skill exists, FALSE otherwise.
    has_skill = function(name) {
      name %in% names(private$.skills)
    },
    
    #' @description
    #' List all registered skills with their names and descriptions.
    #' @return A data.frame with columns: name, description.
    list_skills = function() {
      if (length(private$.skills) == 0) {
        return(data.frame(name = character(0), description = character(0)))
      }
      
      data.frame(
        name = sapply(private$.skills, function(s) s$name),
        description = sapply(private$.skills, function(s) s$description %||% ""),
        path = sapply(private$.skills, function(s) s$path),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
    },
    
    #' @description
    #' Get the number of registered skills.
    #' @return Integer count of skills.
    count = function() {
      length(private$.skills)
    },
    
    #' @description
    #' Generate a prompt section listing available skills.
    #' This can be injected into the system prompt.
    #' @return Character string with formatted skill list.
    generate_prompt_section = function() {
      skills <- self$list_skills()
      
      if (nrow(skills) == 0) {
        return("")
      }
      
      lines <- c(
        "## Available Skills",
        "",
        "The following skills are available. Use `load_skill` to get detailed instructions.",
        ""
      )
      
      for (i in seq_len(nrow(skills))) {
        lines <- c(lines, paste0("- **", skills$name[i], "**: ", skills$description[i]))
      }
      
      paste(lines, collapse = "\n")
    },
    
    #' @description
    #' Print a summary of the registry.
    print = function() {
      cat("<SkillRegistry>\n")
      cat("  Skills:", self$count(), "registered\n")
      if (self$count() > 0) {
        skills <- self$list_skills()
        for (i in seq_len(min(5, nrow(skills)))) {
          cat("    -", skills$name[i], "\n")
        }
        if (nrow(skills) > 5) {
          cat("    ... and", nrow(skills) - 5, "more\n")
        }
      }
      invisible(self)
    }
  ),
  
  private = list(
    .skills = NULL
  )
)

#' @title Create a Skill Registry
#' @description
#' Convenience function to create and populate a SkillRegistry.
#' @param path Path to scan for skills.
#' @param recursive Whether to scan subdirectories. Default FALSE.
#' @return A populated SkillRegistry object.
#' @export
#' @examples
#' \dontrun{
#' # Scan a skills directory
#' registry <- create_skill_registry(".aimd/skills")
#' 
#' # List available skills
#' registry$list_skills()
#' 
#' # Get a specific skill
#' skill <- registry$get_skill("seurat_analysis")
#' }
create_skill_registry <- function(path, recursive = FALSE) {
  registry <- SkillRegistry$new()
  registry$scan_skills(path, recursive = recursive)
  registry
}

#' @title Scan for Skills
#' @description
#' Convenience function to scan a directory and return a SkillRegistry.
#' Alias for create_skill_registry().
#' @param path Path to scan for skills.
#' @param recursive Whether to scan subdirectories. Default FALSE.
#' @return A populated SkillRegistry object.
#' @export
scan_skills <- function(path, recursive = FALSE) {
  create_skill_registry(path, recursive = recursive)
}
