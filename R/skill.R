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

    #' @field aliases Optional aliases that should also trigger this skill.
    aliases = NULL,

    #' @field when_to_use Optional triggering guidance for this skill.
    when_to_use = NULL,

    #' @field paths Optional file glob patterns that make this skill relevant.
    paths = NULL,
    
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
      self$aliases <- private$normalize_aliases(
        parsed$frontmatter$aliases %||% parsed$frontmatter$alias %||% character(0)
      )
      self$when_to_use <- private$normalize_scalar_text(
        parsed$frontmatter$when_to_use %||%
          parsed$frontmatter$`when-to-use` %||%
          parsed$frontmatter$whenToUse %||%
          NULL
      )
      self$paths <- private$normalize_patterns(parsed$frontmatter$paths %||% NULL)
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
    #' Return concise metadata text used for routing and listing.
    #' @return Character scalar.
    metadata_text = function() {
      parts <- c(self$description %||% "", self$when_to_use %||% "")
      parts <- trimws(parts)
      parts <- parts[nzchar(parts)]
      paste(parts, collapse = " ")
    },

    #' @description
    #' Check whether the skill's `paths` patterns match any provided file paths.
    #' @param file_paths Character vector of file paths.
    #' @param cwd Optional working directory used to relativize absolute paths.
    #' @return Logical scalar.
    matches_paths = function(file_paths = character(0), cwd = NULL) {
      patterns <- self$paths %||% character(0)
      if (length(patterns) == 0 || length(file_paths) == 0) {
        return(FALSE)
      }

      normalized_files <- private$normalize_file_paths_for_matching(file_paths, cwd = cwd)
      if (length(normalized_files) == 0) {
        return(FALSE)
      }

      for (pattern in patterns) {
        if (!nzchar(pattern)) {
          next
        }
        if (identical(pattern, "**")) {
          return(TRUE)
        }
        pattern_rx <- utils::glob2rx(pattern)
        if (any(grepl(pattern_rx, normalized_files, perl = TRUE))) {
          return(TRUE)
        }
      }

      FALSE
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
      if (nzchar(self$when_to_use %||% "")) {
        cat("  When to use:", self$when_to_use, "\n")
      }
      if (length(self$aliases %||% character(0)) > 0) {
        cat("  Aliases:", paste(self$aliases, collapse = ", "), "\n")
      }
      if (length(self$paths %||% character(0)) > 0) {
        cat("  Paths:", paste(self$paths, collapse = ", "), "\n")
      }
      cat("  Path:", self$path, "\n")
      cat("  Scripts:", length(self$list_scripts()), "available\n")
      invisible(self)
    }
  ),
  
  private = list(
    .body = NULL,

    normalize_aliases = function(x) {
      if (is.null(x) || length(x) == 0) {
        return(character(0))
      }

      aliases <- unlist(x, use.names = FALSE)
      aliases <- as.character(aliases)
      aliases <- trimws(aliases)
      aliases <- aliases[nzchar(aliases)]
      unique(aliases)
    },

    normalize_scalar_text = function(x) {
      if (is.null(x) || length(x) == 0) {
        return(NULL)
      }

      value <- paste(as.character(unlist(x, use.names = FALSE)), collapse = "\n")
      value <- trimws(value)
      if (!nzchar(value)) {
        return(NULL)
      }
      value
    },

    normalize_patterns = function(x) {
      if (is.null(x) || length(x) == 0) {
        return(character(0))
      }

      values <- unlist(x, use.names = FALSE)
      values <- as.character(values)
      values <- trimws(values)
      values <- gsub("\\\\", "/", values)
      values <- sub("^\\./", "", values)
      values <- values[nzchar(values)]
      unique(values)
    },

    escape_regex = function(x) {
      gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
    },

    normalize_file_paths_for_matching = function(file_paths, cwd = NULL) {
      if (length(file_paths) == 0) {
        return(character(0))
      }

      cwd_norm <- NULL
      if (!is.null(cwd) && nzchar(cwd)) {
        cwd_norm <- normalizePath(cwd, winslash = "/", mustWork = FALSE)
      }

      normalized <- unique(vapply(file_paths, function(path) {
        candidate <- trimws(as.character(path %||% ""))
        if (!nzchar(candidate)) {
          return(NA_character_)
        }

        candidate <- gsub("\\\\", "/", candidate)
        if (grepl("^/|^[A-Za-z]:", candidate)) {
          candidate <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
        }

        if (!is.null(cwd_norm) && startsWith(candidate, paste0(cwd_norm, "/"))) {
          candidate <- substr(candidate, nchar(cwd_norm) + 2L, nchar(candidate))
        } else if (!is.null(cwd_norm) && identical(candidate, cwd_norm)) {
          candidate <- "."
        }

        candidate <- sub("^\\./", "", candidate)
        candidate
      }, character(1)))

      normalized <- normalized[!is.na(normalized)]
      normalized[nzchar(normalized)]
    },
    
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
        
        if (is.null(skill_name) || !nzchar(skill_name)) {
          skills <- registry$list_skills()
          if (nrow(skills) == 1) {
            skill_name <- skills$name[1]
          } else {
            return("Error: skill_name is required")
          }
        }
        resolved_name <- registry$resolve_skill_name(skill_name)
        skill <- if (!is.null(resolved_name)) registry$get_skill(resolved_name) else NULL
        if (is.null(skill)) {
          suggestion <- registry$find_closest_skill_name(skill_name)
          if (!is.null(suggestion)) {
            return(paste0("Skill not found: ", skill_name, ". Did you mean: ", suggestion, "?"))
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
        
        language_guard <- paste(
          "## Reply Language Invariant",
          "- The language used inside this skill document does not determine the language of your final reply.",
          "- You must answer in the user's language for the current turn.",
          "- Keep code, function names, package names, paths, commands, and quoted source text in their original language when needed for accuracy.",
          sep = "\n"
        )
        
        paste0(language_guard, "\n\n", body, script_info, resource_info, "\n\n", language_guard)
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
        resolved_name <- registry$resolve_skill_name(skill_name)
        skill <- if (!is.null(resolved_name)) registry$get_skill(resolved_name) else NULL
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
        resolved_name <- registry$resolve_skill_name(skill_name)
        skill <- if (!is.null(resolved_name)) registry$get_skill(resolved_name) else NULL
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
        resolved_name <- registry$resolve_skill_name(skill_name)
        skill <- if (!is.null(resolved_name)) registry$get_skill(resolved_name) else NULL
        if (is.null(skill)) {
          suggestion <- registry$find_closest_skill_name(skill_name)
          if (!is.null(suggestion)) {
            return(paste0("Skill not found: ", skill_name, ". Did you mean: ", suggestion, "?"))
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
        resolved_name <- registry$resolve_skill_name(skill_name)
        skill <- if (!is.null(resolved_name)) registry$get_skill(resolved_name) else NULL
        if (is.null(skill)) {
          suggestion <- registry$find_closest_skill_name(skill_name)
          if (!is.null(suggestion)) {
            return(paste0("Skill not found: ", skill_name, ". Did you mean: ", suggestion, "?"))
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
    ),

    # Tool 4: List currently available skills
    tool(
      name = "list_available_skills",
      description = "List currently available skills from the live skill registry.",
      parameters = z_empty_object(),
      execute = function(args) {
        skills <- registry$list_skills()
        if (nrow(skills) == 0) {
          return("No skills are currently available.")
        }

        lines <- c("Available skills:")
        for (i in seq_len(nrow(skills))) {
          extras <- character(0)
          if (nzchar(skills$aliases[[i]] %||% "")) {
            extras <- c(extras, paste0("aliases=", skills$aliases[[i]]))
          }
          if (nzchar(skills$path[[i]] %||% "")) {
            extras <- c(extras, paste0("path=", skills$path[[i]]))
          }
          suffix <- if (length(extras) > 0) paste0(" [", paste(extras, collapse = "; "), "]") else ""
          lines <- c(lines, paste0("- ", skills$name[[i]], ": ", skills$description[[i]], suffix))
        }
        paste(lines, collapse = "\n")
      }
    ),

    # Tool 5: Refresh registry from disk
    tool(
      name = "reload_skills",
      description = "Reload skill metadata from remembered skill roots so newly added or edited skills become available.",
      parameters = z_empty_object(),
      execute = function(args) {
        roots <- registry$list_roots()
        if (nrow(roots) == 0) {
          return("No skill roots are remembered for this registry. Recreate the agent with skills='auto' or an explicit skill path.")
        }

        before <- registry$count()
        registry$refresh(clear = TRUE)
        after <- registry$count()
        root_lines <- paste0("- ", roots$path, ifelse(roots$recursive, " (recursive)", ""))
        skill_names <- registry$list_skills()$name
        paste(c(
          sprintf("Reloaded skills: %d -> %d.", before, after),
          "Skill roots:",
          root_lines,
          if (length(skill_names) > 0) c("Current skills:", paste0("- ", skill_names)) else "Current skills: none"
        ), collapse = "\n")
      }
    )
  )
}

# Null-coalescing operator (if not already defined)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
