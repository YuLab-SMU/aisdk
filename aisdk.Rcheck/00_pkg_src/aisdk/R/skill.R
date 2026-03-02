#' @title Skills System: Progressive Knowledge Loading
#' @description
#' Skill class and utilities for loading specialized instructions and executing
#' R scripts dynamically. Implements a progressive disclosure pattern to save
#' context window tokens.
#' @name skill
NULL

#' @title Skill Class
#' @description
#' R6 class representing a skill with progressive loading capabilities.
#' A Skill consists of:
#' \itemize{
#'   \item Level 1: YAML frontmatter (name, description) - always loaded
#'   \item Level 2: SKILL.md body (detailed instructions) - on demand
#'   \item Level 3: R scripts (executable code) - executed by agent
#' }
#' @export
Skill <- R6::R6Class(
  "Skill",
  
  public = list(
    #' @field name The unique name of the skill (from YAML frontmatter).
    name = NULL,
    
    #' @field description A brief description of the skill (from YAML frontmatter).
    description = NULL,
    
    #' @field path The directory path containing the skill files.
    path = NULL,
    
    #' @description
    #' Create a new Skill object by parsing a SKILL.md file.
    #' @param path Path to the skill directory (containing SKILL.md).
    #' @return A new Skill object.
    initialize = function(path) {
      if (!is.character(path) || length(path) != 1) {
        rlang::abort("path must be a single character string.")
      }
      
      skill_md_path <- file.path(path, "SKILL.md")
      if (!file.exists(skill_md_path)) {
        rlang::abort(paste0("SKILL.md not found at: ", skill_md_path))
      }
      
      self$path <- normalizePath(path, mustWork = TRUE)
      
      # Parse YAML frontmatter (Level 1)
      content <- readLines(skill_md_path, warn = FALSE)
      parsed <- private$parse_frontmatter(content)
      
      self$name <- parsed$frontmatter$name
      self$description <- parsed$frontmatter$description
      private$.body <- parsed$body
      
      if (is.null(self$name) || self$name == "") {
        rlang::abort("SKILL.md must have a 'name' field in YAML frontmatter.")
      }
      
      invisible(self)
    },
    
    #' @description
    #' Load the full SKILL.md body content (Level 2).
    #' @return Character string containing the skill instructions.
    load = function() {
      private$.body
    },
    
    #' @description
    #' Execute an R script from the skill's scripts directory (Level 3).
    #' Uses callr for safe, isolated execution.
    #' @param script_name Name of the script file (e.g., "normalize.R").
    #' @param args Named list of arguments to pass to the script.
    #' @return The result from the script execution.
    execute_script = function(script_name, args = list()) {
      scripts_dir <- file.path(self$path, "scripts")
      script_path <- file.path(scripts_dir, script_name)
      
      if (!file.exists(script_path)) {
        rlang::abort(paste0("Script not found: ", script_path))
      }
      
      # Execute in isolated R process
      tryCatch({
        callr::r(
          function(script_path, args) {
            # Make args available in script environment
            env <- new.env(parent = globalenv())
            env$args <- args
            source(script_path, local = env)$value
          },
          args = list(script_path = script_path, args = args),
          show = FALSE
        )
      }, error = function(e) {
        paste0("Script execution error: ", conditionMessage(e))
      })
    },
    
    #' @description
    #' List available scripts in the skill's scripts directory.
    #' @return Character vector of script file names.
    list_scripts = function() {
      scripts_dir <- file.path(self$path, "scripts")
      if (!dir.exists(scripts_dir)) {
        return(character(0))
      }
      list.files(scripts_dir, pattern = "\\.R$", full.names = FALSE)
    },
    
    #' @description
    #' List available reference files in the skill's references directory.
    #' @return Character vector of reference file names.
    list_resources = function() {
      refs_dir <- file.path(self$path, "references")
      if (!dir.exists(refs_dir)) {
        return(character(0))
      }
      list.files(refs_dir, full.names = FALSE)
    },
    
    #' @description
    #' Read content of a reference file from the references directory.
    #' @param resource_name Name of the reference file.
    #' @return Character string containing the resource content.
    read_resource = function(resource_name) {
      refs_dir <- file.path(self$path, "references")
      resource_path <- file.path(refs_dir, resource_name)
      
      if (!file.exists(resource_path)) {
        rlang::abort(paste0("Resource not found: ", resource_path))
      }
      
      paste(readLines(resource_path, warn = FALSE), collapse = "\n")
    },
    
    #' @description
    #' Get the absolute path to an asset in the assets directory.
    #' @param asset_name Name of the asset file or directory.
    #' @return Absolute path string.
    get_asset_path = function(asset_name) {
      assets_dir <- file.path(self$path, "assets")
      asset_path <- file.path(assets_dir, asset_name)
      
      if (!file.exists(asset_path)) {
        rlang::abort(paste0("Asset not found: ", asset_path))
      }
      
      normalizePath(asset_path, mustWork = TRUE)
    },
    
    #' @description
    #' Print a summary of the skill.
    print = function() {
      cat("<Skill>\n")
      cat("  Name:", self$name, "\n")
      cat("  Description:", self$description %||% "(none)", "\n")
      cat("  Path:", self$path, "\n")
      cat("  Scripts:", length(self$list_scripts()), "available\n")
      invisible(self)
    }
  ),
  
  private = list(
    .body = NULL,
    
    # Parse YAML frontmatter from SKILL.md content
    parse_frontmatter = function(lines) {
      # Find frontmatter delimiters (---)
      delim_indices <- which(grepl("^---\\s*$", lines))
      
      if (length(delim_indices) < 2) {
        rlang::abort("SKILL.md must have YAML frontmatter delimited by ---")
      }
      
      start <- delim_indices[1]
      end <- delim_indices[2]
      
      # Parse YAML
      yaml_content <- paste(lines[(start + 1):(end - 1)], collapse = "\n")
      frontmatter <- yaml::yaml.load(yaml_content)
      
      # Extract body (everything after second ---)
      body_lines <- if (end < length(lines)) lines[(end + 1):length(lines)] else character(0)
      body <- paste(body_lines, collapse = "\n")
      body <- trimws(body)
      
      list(frontmatter = frontmatter, body = body)
    }
  )
)

#' @title Create Skill Tools
#' @description
#' Create the built-in tools for interacting with skills.
#' @param registry A SkillRegistry object.
#' @return A list of Tool objects.
#' @export
create_skill_tools <- function(registry) {
  list(
    # Tool 1: Load skill instructions
    tool(
      name = "load_skill",
      description = "Load detailed instructions for a skill. Returns the full SKILL.md body content.",
      parameters = z_object(
        skill_name = z_string(description = "Name of the skill to load")
      ),
      execute = function(args) {
        skill_name <- args$skill_name
        
        # Helper for fuzzy matching
        find_closest_skill <- function(target, available) {
          dists <- utils::adist(target, available, ignore.case = TRUE)
          min_dist <- min(dists)
          # Threshold: 3 edits or 30% of length, whichever is larger, but cap at 4
          threshold <- min(4, max(3, nchar(target) * 0.3))
          
          if (min_dist <= threshold) {
            closest <- available[which.min(dists)]
            return(closest)
          }
          return(NULL)
        }
        
        if (is.null(skill_name) || !nzchar(skill_name)) {
          skills <- registry$list_skills()
          if (nrow(skills) == 1) {
            skill_name <- skills$name[1]
          } else {
            return("Error: skill_name is required")
          }
        }
        skill <- registry$get_skill(skill_name)
        if (is.null(skill)) {
          # Try fuzzy matching
          available_skills <- registry$list_skills()$name
          if (length(available_skills) > 0) {
            suggestion <- find_closest_skill(skill_name, available_skills)
            if (!is.null(suggestion)) {
              return(paste0("Skill not found: ", skill_name, ". Did you mean: ", suggestion, "?"))
            }
          }
          return(paste0("Skill not found: ", skill_name))
        }
        body <- skill$load()
        scripts <- skill$list_scripts()
        resources <- skill$list_resources()
        
        script_info <- ""
        if (length(scripts) > 0) {
          interface_lines <- character(0)
          for (script in scripts) {
            script_path <- file.path(skill$path, "scripts", script)
            args_found <- character(0)
            if (file.exists(script_path)) {
              lines <- readLines(script_path, warn = FALSE)
              matches <- unlist(regmatches(lines, gregexpr("args\\$[A-Za-z0-9_.]+", lines, perl = TRUE)))
              if (length(matches) > 0) {
                args_found <- unique(sub("^args\\$", "", matches))
              }
            }
            if (length(args_found) > 0) {
              interface_lines <- c(interface_lines, paste0("- ", script, ": args{", paste(args_found, collapse = ", "), "}"))
            } else {
              interface_lines <- c(interface_lines, paste0("- ", script, ": args{}"))
            }
          }
          script_info <- paste0(
            "\n\n## Script Interface\n",
            paste(interface_lines, collapse = "\n"),
            "\n\nUse `execute_skill_script` with args, e.g. args = list(name = \"Alice\")."
          )
        }
        
        resource_info <- ""
        if (length(resources) > 0) {
          resource_info <- paste0(
            "\n\n## Available Resources\n",
            "- ", paste(resources, collapse = "\n- "),
            "\n\nUse `read_skill_resource` to read these files for more details."
          )
        }
        
        paste0(body, script_info, resource_info)
      }
    ),
    
    # Tool 1.1: List skill resources
    tool(
      name = "list_skill_resources",
      description = "List available reference files (Level 3) in a skill.",
      parameters = z_object(
        skill_name = z_string(description = "Name of the skill")
      ),
      execute = function(args) {
        skill_name <- args$skill_name
        skill <- registry$get_skill(skill_name)
        if (is.null(skill)) return(paste0("Skill not found: ", skill_name))
        
        resources <- skill$list_resources()
        if (length(resources) == 0) {
          "No reference resources available in this skill."
        } else {
          paste0("Available resources: ", paste(resources, collapse = ", "))
        }
      }
    ),
    
    # Tool 1.2: Read skill resource
    tool(
      name = "read_skill_resource",
      description = "Read the content of a reference file from a skill.",
      parameters = z_object(
        skill_name = z_string(description = "Name of the skill"),
        resource_name = z_string(description = "Name of the resource file to read")
      ),
      execute = function(args) {
        skill_name <- args$skill_name
        resource_name <- args$resource_name
        skill <- registry$get_skill(skill_name)
        if (is.null(skill)) return(paste0("Skill not found: ", skill_name))
        
        tryCatch({
          skill$read_resource(resource_name)
        }, error = function(e) {
          conditionMessage(e)
        })
      }
    ),
    
    # Tool 2: Execute skill script
    tool(
      name = "execute_skill_script",
      description = "Execute an R script from a skill. Scripts run in an isolated R process.",
      parameters = z_object(
        skill_name = z_string(description = "Name of the skill"),
        script_name = z_string(description = "Name of the script file (e.g., 'analyze.R')"),
        args = z_any_object(description = "Optional arguments to pass to the script"),
        .required = c("skill_name", "script_name"),
        .additional_properties = TRUE
      ),
      execute = function(args) {
        skill_name <- args$skill_name
        if (is.null(skill_name) || !nzchar(skill_name)) {
          skills <- registry$list_skills()
          if (nrow(skills) == 1) {
            skill_name <- skills$name[1]
          } else {
            return("Error: skill_name is required")
          }
        }
        skill <- registry$get_skill(skill_name)
        if (is.null(skill)) {
          # Try fuzzy matching (reusing logic if possible, but duplicating for tool isolation)
          available_skills <- registry$list_skills()$name
          if (length(available_skills) > 0) {
             # Simple fuzzy match inline
             dists <- utils::adist(skill_name, available_skills, ignore.case = TRUE)
             min_dist <- min(dists)
             threshold <- min(4, max(3, nchar(skill_name) * 0.3))
             if (min_dist <= threshold) {
               suggestion <- available_skills[which.min(dists)]
               return(paste0("Skill not found: ", skill_name, ". Did you mean: ", suggestion, "?"))
             }
          }
          return(paste0("Skill not found: ", skill_name))
        }
        script_name <- args$script_name
        if (is.null(script_name) || !nzchar(script_name)) {
          scripts <- skill$list_scripts()
          if (length(scripts) == 1) {
            script_name <- scripts[1]
          } else {
            return("Error: script_name is required")
          }
        }
        script_args <- args$args
        if (is.character(script_args)) {
          script_args <- tryCatch(
            jsonlite::fromJSON(script_args, simplifyVector = FALSE),
            error = function(e) script_args
          )
        }
        if (!is.list(script_args)) {
          script_args <- list()
        }
        extra_args <- args
        extra_args$skill_name <- NULL
        extra_args$script_name <- NULL
        extra_args$args <- NULL
        extra_args$.envir <- NULL
        if (length(extra_args) > 0) {
          if (length(script_args) == 0) {
            script_args <- extra_args
          } else {
            script_args <- utils::modifyList(script_args, extra_args)

          }
        }
        script_path <- file.path(skill$path, "scripts", script_name)
        # Scan for args usage removed to allow optional arguments
        skill$execute_script(script_name, script_args)
      }
    ),
    
    # Tool 3: List skill scripts
    tool(
      name = "list_skill_scripts",
      description = "List available R scripts in a skill.",
      parameters = z_object(
        skill_name = z_string(description = "Name of the skill")
      ),
      execute = function(args) {
        skill_name <- args$skill_name
        if (is.null(skill_name) || !nzchar(skill_name)) {
          skills <- registry$list_skills()
          if (nrow(skills) == 1) {
            skill_name <- skills$name[1]
          } else {
            return("Error: skill_name is required")
          }
        }
        skill <- registry$get_skill(skill_name)
        if (is.null(skill)) {
           # Try fuzzy matching
          available_skills <- registry$list_skills()$name
          if (length(available_skills) > 0) {
             dists <- utils::adist(skill_name, available_skills, ignore.case = TRUE)
             min_dist <- min(dists)
             threshold <- min(4, max(3, nchar(skill_name) * 0.3))
             if (min_dist <= threshold) {
               suggestion <- available_skills[which.min(dists)]
               return(paste0("Skill not found: ", skill_name, ". Did you mean: ", suggestion, "?"))
             }
          }
          return(paste0("Skill not found: ", skill_name))
        }
        scripts <- skill$list_scripts()
        if (length(scripts) == 0) {
          "No scripts available in this skill."
        } else {
          paste0("Available scripts: ", paste(scripts, collapse = ", "))
        }
      }
    )
  )
}

# Null-coalescing operator (if not already defined)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
