#' Agent Discovery Library
#'
#' AgentLibrary provides automatic discovery and instantiation of agents
#' from skill directories. It scans SKILL.md files for agent metadata and
#' enables zero-configuration team assembly.
#'
#' @export
AgentLibrary <- R6::R6Class("AgentLibrary",
  public = list(
    #' @description Initialize a new AgentLibrary
    initialize = function() {
      private$.agent_templates <- list()
      private$.skill_to_agent_map <- list()
    },

    #' @description Scan skills directory and extract agent metadata
    #' @param skill_paths Character vector of paths to scan, or "auto" for default locations
    #' @param recursive Logical, whether to scan recursively (default: TRUE)
    #' @return Self (invisibly) for method chaining
    scan_from_skills = function(skill_paths = "auto", recursive = TRUE) {
      if (identical(skill_paths, "auto")) {
        skill_paths <- private$get_default_skill_paths()
      }

      if (length(skill_paths) == 0) {
        warning("No valid skill paths found")
        return(invisible(self))
      }

      for (path in skill_paths) {
        if (!dir.exists(path)) {
          warning(sprintf("Skill path does not exist: %s", path))
          next
        }

        # Use existing SkillRegistry to find skills
        registry <- SkillRegistry$new()
        registry$scan_skills(path, recursive)

        skills <- registry$list_skills()

        if (nrow(skills) == 0) next

        for (i in seq_len(nrow(skills))) {
          skill_name <- skills$name[i]
          skill_path <- skills$path[i]

          metadata <- private$extract_agent_metadata(skill_path)
          if (!is.null(metadata)) {
            private$register_agent_template(metadata, skill_path)
          }
        }
      }

      invisible(self)
    },

    #' @description Get agent capabilities summary for Architect
    #' @return Data frame with role, description, capabilities, skills columns
    get_capabilities_summary = function() {
      if (length(private$.agent_templates) == 0) {
        return(data.frame(
          role = character(0),
          description = character(0),
          capabilities = character(0),
          skills = character(0),
          stringsAsFactors = FALSE
        ))
      }

      data.frame(
        role = names(private$.agent_templates),
        description = sapply(private$.agent_templates, function(a) a$description),
        capabilities = sapply(private$.agent_templates, function(a) {
          if (length(a$capabilities) == 0) return("")
          paste(a$capabilities, collapse = ", ")
        }),
        skills = sapply(private$.agent_templates, function(a) {
          paste(basename(a$skill_paths), collapse = ", ")
        }),
        stringsAsFactors = FALSE
      )
    },

    #' @description Instantiate agents by role names (lazy loading)
    #' @param role_names Character vector of agent roles to instantiate
    #' @param model Model to use for agents
    #' @return Named list of Agent objects
    instantiate_agents = function(role_names, model = "claude-3-5-sonnet-20241022") {
      agents <- list()

      for (role in role_names) {
        template <- private$.agent_templates[[role]]
        if (is.null(template)) {
          warning(sprintf("Agent role '%s' not found in library", role))
          next
        }

        # Create agent with associated skills
        agent <- Agent$new(
          name = role,
          description = template$description,
          system_prompt = template$persona,
          skills = template$skill_paths,
          model = model
        )

        agents[[role]] <- agent
      }

      agents
    },

    #' @description List all available agent roles
    #' @return Character vector of role names
    list_roles = function() {
      names(private$.agent_templates)
    },

    #' @description Print summary of discovered agents
    print = function() {
      cat("AgentLibrary\n")
      cat(sprintf("Discovered agents: %d\n\n", length(private$.agent_templates)))

      if (length(private$.agent_templates) > 0) {
        summary <- self$get_capabilities_summary()
        for (i in seq_len(nrow(summary))) {
          cat(sprintf("  [%s]\n", summary$role[i]))
          cat(sprintf("    Description: %s\n", summary$description[i]))
          if (nzchar(summary$capabilities[i])) {
            cat(sprintf("    Capabilities: %s\n", summary$capabilities[i]))
          }
          cat(sprintf("    Skills: %s\n", summary$skills[i]))
          cat("\n")
        }
      }

      invisible(self)
    }
  ),

  private = list(
    .agent_templates = NULL,
    .skill_to_agent_map = NULL,

    # Extract agent metadata from SKILL.md
    extract_agent_metadata = function(skill_path) {
      skill_md_path <- file.path(skill_path, "SKILL.md")
      if (!file.exists(skill_md_path)) {
        return(NULL)
      }

      tryCatch({
        content <- readLines(skill_md_path, warn = FALSE)

        # Find YAML frontmatter
        yaml_indices <- which(content == "---")
        if (length(yaml_indices) < 2) {
          return(NULL)
        }

        yaml_start <- yaml_indices[1]
        yaml_end <- yaml_indices[2]

        yaml_content <- content[(yaml_start + 1):(yaml_end - 1)]
        yaml_text <- paste(yaml_content, collapse = "\n")

        # Parse YAML
        if (!requireNamespace("yaml", quietly = TRUE)) {
          warning("yaml package not available, skipping agent metadata extraction")
          return(NULL)
        }

        metadata <- yaml::yaml.load(yaml_text)

        # Check if agent section exists
        if (is.null(metadata$agent)) {
          return(NULL)
        }

        # Extract agent info
        list(
          role = metadata$agent$role,
          description = metadata$description %||% "No description",
          persona = metadata$agent$persona %||% "",
          capabilities = metadata$agent$capabilities %||% list(),
          skill_name = metadata$name
        )
      }, error = function(e) {
        # Silently skip files with parsing errors
        NULL
      })
    },

    # Register agent template for later instantiation
    register_agent_template = function(metadata, skill_path) {
      role <- metadata$role

      if (role %in% names(private$.agent_templates)) {
        # Agent role already exists, add skill to its collection
        existing <- private$.agent_templates[[role]]
        existing$skill_paths <- c(existing$skill_paths, skill_path)
        private$.agent_templates[[role]] <- existing
      } else {
        # New agent role
        private$.agent_templates[[role]] <- list(
          role = role,
          description = metadata$description,
          persona = metadata$persona,
          capabilities = metadata$capabilities,
          skill_paths = skill_path
        )
      }

      # Map skill to agent
      private$.skill_to_agent_map[[metadata$skill_name]] <- role
    },

    # Get default skill paths (same as Agent$initialize)
    get_default_skill_paths = function() {
      candidates <- c(
        file.path(Sys.getenv("HOME"), "aisdk", "skills"),
        file.path(getwd(), "aisdk", "skills"),
        file.path(getwd(), "skills"),
        file.path(getwd(), "inst", "skills"),
        system.file("skills", package = "aisdk")
      )

      candidates[dir.exists(candidates)]
    }
  )
)
