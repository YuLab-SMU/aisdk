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
    #' Resolve a skill name or alias to its canonical name.
    #' @param name Skill name or alias.
    #' @return Canonical skill name or NULL.
    resolve_skill_name = function(name) {
      if (is.null(name) || !nzchar(trimws(name))) {
        return(NULL)
      }

      target <- trimws(name)
      skill_names <- names(private$.skills)
      if (target %in% skill_names) {
        return(target)
      }

      lower_target <- tolower(target)
      lower_names <- tolower(skill_names)
      exact_idx <- which(lower_names == lower_target)
      if (length(exact_idx) > 0) {
        return(skill_names[[exact_idx[[1]]]])
      }

      for (skill_name in skill_names) {
        aliases <- private$.skills[[skill_name]]$aliases %||% character(0)
        if (length(aliases) == 0) {
          next
        }
        if (any(tolower(aliases) == lower_target)) {
          return(skill_name)
        }
      }

      NULL
    },

    #' @description
    #' Find the closest matching canonical skill name for fuzzy recovery.
    #' @param name Skill name or alias candidate.
    #' @return Canonical skill name or NULL.
    find_closest_skill_name = function(name) {
      if (is.null(name) || !nzchar(trimws(name)) || length(private$.skills) == 0) {
        return(NULL)
      }

      target <- trimws(name)
      candidates <- character(0)
      canonical <- character(0)
      for (skill_name in names(private$.skills)) {
        skill <- private$.skills[[skill_name]]
        terms <- unique(c(skill_name, skill$aliases %||% character(0)))
        candidates <- c(candidates, terms)
        canonical <- c(canonical, rep(skill_name, length(terms)))
      }

      if (length(candidates) == 0) {
        return(NULL)
      }

      dists <- utils::adist(target, candidates, ignore.case = TRUE)
      min_dist <- min(dists)
      threshold <- min(4, max(3, nchar(target) * 0.3))
      if (min_dist > threshold) {
        return(NULL)
      }

      canonical[[which.min(dists)]]
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
        return(data.frame(
          name = character(0),
          description = character(0),
          aliases = character(0),
          when_to_use = character(0),
          paths = character(0),
          path = character(0),
          stringsAsFactors = FALSE
        ))
      }
      
      data.frame(
        name = sapply(private$.skills, function(s) s$name),
        description = sapply(private$.skills, function(s) s$description %||% ""),
        aliases = sapply(private$.skills, function(s) paste(s$aliases %||% character(0), collapse = ", ")),
        when_to_use = sapply(private$.skills, function(s) s$when_to_use %||% ""),
        paths = sapply(private$.skills, function(s) paste(s$paths %||% character(0), collapse = ", ")),
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
    #' Find relevant skills for a user query and optional file paths.
    #' @param query Optional user query text.
    #' @param file_paths Optional character vector of file paths.
    #' @param cwd Optional working directory for path matching.
    #' @param limit Maximum number of results to return.
    #' @return Data frame of matching skills sorted by score.
    find_relevant_skills = function(query = NULL,
                                    file_paths = character(0),
                                    cwd = NULL,
                                    limit = 3L) {
      if (length(private$.skills) == 0) {
        return(private$empty_match_table())
      }

      query <- query %||% ""
      file_paths <- file_paths %||% character(0)

      rows <- lapply(names(private$.skills), function(skill_name) {
        skill <- private$.skills[[skill_name]]
        breakdown <- private$score_skill(skill, query = query, file_paths = file_paths, cwd = cwd)
        if (breakdown$score <= 0) {
          return(NULL)
        }

        data.frame(
          name = skill$name,
          score = breakdown$score,
          matched_by = paste(breakdown$matched_by, collapse = ", "),
          description = skill$description %||% "",
          when_to_use = skill$when_to_use %||% "",
          aliases = paste(skill$aliases %||% character(0), collapse = ", "),
          paths = paste(skill$paths %||% character(0), collapse = ", "),
          stringsAsFactors = FALSE
        )
      })

      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0) {
        return(private$empty_match_table())
      }

      matched <- do.call(rbind, rows)
      matched <- matched[order(matched$score, decreasing = TRUE, matched$name), , drop = FALSE]
      if (!is.null(limit) && is.finite(limit) && limit > 0 && nrow(matched) > limit) {
        matched <- matched[seq_len(limit), , drop = FALSE]
      }
      row.names(matched) <- NULL
      matched
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
        "The following skills are available.",
        "You must proactively choose and use a relevant skill when the user's task matches a skill description, even if the user does not know the skill name or never mentions skills explicitly.",
        "When a request involves a recognizable domain such as PDFs, OCR, files, reports, APIs, data analysis, or document handling, inspect likely matching skills before answering from memory.",
        "Use `load_skill` to read the chosen skill before acting. Then use `read_skill_resource` or `execute_skill_script` when the skill instructions call for them.",
        "If the user is asking to add a reusable new capability, teach the assistant a workflow, or an obvious repeated task is missing from the skill set, look for a skill-creation capability such as `skill-creator` and use it instead of only improvising a one-off answer.",
        ""
      )
      
      for (i in seq_len(nrow(skills))) {
        extras <- character(0)
        if (nzchar(skills$aliases[i] %||% "")) {
          extras <- c(extras, paste0("Aliases: ", skills$aliases[i]))
        }
        if (nzchar(skills$when_to_use[i] %||% "")) {
          extras <- c(extras, paste0("When to use: ", skills$when_to_use[i]))
        }
        if (nzchar(skills$paths[i] %||% "")) {
          extras <- c(extras, paste0("Paths: ", skills$paths[i]))
        }
        suffix <- if (length(extras) > 0) paste0(" ", paste(extras, collapse = " ")) else ""
        lines <- c(lines, paste0("- **", skills$name[i], "**: ", skills$description[i], suffix))
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
    .skills = NULL,

    empty_match_table = function() {
      data.frame(
        name = character(0),
        score = numeric(0),
        matched_by = character(0),
        description = character(0),
        when_to_use = character(0),
        aliases = character(0),
        paths = character(0),
        stringsAsFactors = FALSE
      )
    },

    extract_terms = function(text, max_chars = 16L) {
      if (is.null(text) || !nzchar(trimws(text))) {
        return(character(0))
      }

      matches <- regmatches(text, gregexpr("[A-Za-z][A-Za-z0-9_-]{2,}|[一-龥]{2,}", text, perl = TRUE))[[1]]
      matches <- unique(trimws(matches))
      matches <- matches[nzchar(matches)]
      matches[nchar(matches) <= max_chars]
    },

    score_skill = function(skill, query = NULL, file_paths = character(0), cwd = NULL) {
      score <- 0
      matched_by <- character(0)
      query_norm <- tolower(trimws(query %||% ""))

      if (nzchar(query_norm)) {
        terms <- unique(c(skill$name, skill$aliases %||% character(0)))
        terms <- trimws(terms)
        terms <- terms[nzchar(terms)]

        for (term in terms) {
          term_norm <- tolower(term)
          if (!nzchar(term_norm)) {
            next
          }

          if (identical(query_norm, term_norm)) {
            score <- max(score, 300L + nchar(term_norm))
            matched_by <- unique(c(matched_by, "name_or_alias_exact"))
          } else if (grepl(term_norm, query_norm, fixed = TRUE)) {
            score <- max(score, 220L + nchar(term_norm))
            matched_by <- unique(c(matched_by, "name_or_alias_substring"))
          }
        }

        when_terms <- private$extract_terms(skill$when_to_use %||% "", max_chars = 20L)
        for (term in when_terms) {
          term_norm <- tolower(term)
          if (grepl(term_norm, query_norm, fixed = TRUE)) {
            score <- max(score, 80L + nchar(term_norm))
            matched_by <- unique(c(matched_by, "when_to_use"))
          }
        }

        if (score == 0) {
          desc_terms <- private$extract_terms(skill$description %||% "", max_chars = 12L)
          for (term in desc_terms) {
            term_norm <- tolower(term)
            if (grepl(term_norm, query_norm, fixed = TRUE)) {
              score <- max(score, 30L + nchar(term_norm))
              matched_by <- unique(c(matched_by, "description"))
            }
          }
        }
      }

      if (length(file_paths %||% character(0)) > 0 && isTRUE(skill$matches_paths(file_paths, cwd = cwd))) {
        score <- max(score, 140L)
        matched_by <- unique(c(matched_by, "paths"))
      }

      list(score = score, matched_by = matched_by)
    }
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
#' \donttest{
#' if (interactive()) {
#' # Scan a skills directory
#' registry <- create_skill_registry(".aimd/skills")
#' 
#' # List available skills
#' registry$list_skills()
#' 
#' # Get a specific skill
#' skill <- registry$get_skill("seurat_analysis")
#' }
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
