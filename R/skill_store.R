#' @title Global Skill Store
#' @description
#' A CRAN-like experience for sharing AI capabilities. Skills are packaged
#' with a skill.yaml manifest defining dependencies, MCP endpoints, and
#' prompt templates.
#' @name skill_store
NULL

#' @title Skill Manifest Specification
#' @description
#' The skill.yaml specification defines the structure for distributable skills.
#'
#' @section Specification:
#' ```yaml
#' # skill.yaml - Skill Manifest Specification v1.0
#'
#' # Required fields
#' name: my-skill                    # Unique skill identifier (lowercase, hyphens)
#' version: 1.0.0                    # Semantic version
#' description: Brief description    # One-line description
#'
#' # Author information
#' author:
#'   name: Author Name
#'   email: author@example.com
#'   url: https://github.com/author
#'
#' # License (SPDX identifier)
#' license: MIT
#'
#' # R package dependencies
#' dependencies:
#'   - dplyr >= 1.0.0
#'   - ggplot2
#'
#' # System requirements
#' system_requirements:
#'   - python >= 3.8              # Optional external requirements
#'
#' # MCP server configuration (optional)
#' mcp:
#'   command: npx                  # Command to start MCP server
#'   args:
#'     - -y
#'     - "@my-org/my-mcp-server"
#'   env:
#'     API_KEY: "${MY_API_KEY}"   # Environment variable substitution
#'
#' # Capabilities this skill provides
#' capabilities:
#'   - data-analysis
#'   - visualization
#'   - machine-learning
#'
#' # Prompt templates
#' prompts:
#'   system: |
#'     You are a specialized assistant for...
#'   examples:
#'     - "Analyze this dataset..."
#'     - "Create a visualization of..."
#'
#' # Entry points
#' entry:
#'   main: SKILL.md                # Main skill instructions
#'   scripts: scripts/             # Directory containing R scripts
#'
#' # Repository information
#' repository:
#'   type: github
#'   url: https://github.com/author/my-skill
#' ```
#' @name skill_manifest
NULL

#' @title Skill Store Class
#' @description
#' R6 class for managing the global skill store, including installation,
#' updates, and discovery of skills.
#' @export
SkillStore <- R6::R6Class(
  "SkillStore",

  public = list(
    #' @field registry_url URL of the skill registry.
    registry_url = NULL,

    #' @field install_path Local path for installed skills.
    install_path = NULL,

    #' @field installed List of installed skills.
    installed = NULL,

    #' @description
    #' Create a new SkillStore instance.
    #' @param registry_url URL of the skill registry.
    #' @param install_path Local installation path.
    #' @return A new SkillStore object.
    initialize = function(registry_url = NULL, install_path = NULL) {
      self$registry_url <- registry_url %||%
        getOption("aisdk.skill_registry", "https://skills.r-ai.dev")

      self$install_path <- install_path %||%
        file.path(Sys.getenv("HOME"), ".aisdk", "skills")

      # Create install directory if needed
      if (!dir.exists(self$install_path)) {
        dir.create(self$install_path, recursive = TRUE)
      }

      # Load installed skills index
      private$load_installed_index()

      invisible(self)
    },

    #' @description
    #' Install a skill from the registry or a GitHub repository.
    #' @param skill_ref Skill reference (e.g., "username/skillname" or registry name).
    #' @param version Optional specific version to install.
    #' @param force Force reinstallation even if already installed.
    #' @return The installed Skill object.
    install = function(skill_ref, version = NULL, force = FALSE) {
      # Parse skill reference
      parsed <- private$parse_skill_ref(skill_ref)

      # Check if already installed
      if (!force && !is.null(self$installed[[parsed$name]])) {
        installed_version <- self$installed[[parsed$name]]$version
        if (is.null(version) || installed_version == version) {
          message("Skill '", parsed$name, "' is already installed (v", installed_version, ")")
          return(invisible(self$get(parsed$name)))
        }
      }

      message("Installing skill: ", skill_ref)

      # Download skill
      skill_dir <- private$download_skill(parsed, version)

      # Validate skill.yaml
      manifest <- private$load_manifest(skill_dir)

      # Install R dependencies
      private$install_dependencies(manifest)

      # Register in installed index
      self$installed[[manifest$name]] <- list(
        name = manifest$name,
        version = manifest$version,
        path = skill_dir,
        installed_at = Sys.time()
      )
      private$save_installed_index()

      message("Successfully installed: ", manifest$name, " v", manifest$version)

      # Return the Skill object
      Skill$new(skill_dir)
    },

    #' @description
    #' Uninstall a skill.
    #' @param name Skill name.
    #' @return Self (invisibly).
    uninstall = function(name) {
      if (is.null(self$installed[[name]])) {
        message("Skill '", name, "' is not installed")
        return(invisible(self))
      }

      skill_path <- self$installed[[name]]$path

      # Remove skill directory
      if (dir.exists(skill_path)) {
        unlink(skill_path, recursive = TRUE)
      }

      # Remove from index
      self$installed[[name]] <- NULL
      private$save_installed_index()

      message("Uninstalled: ", name)
      invisible(self)
    },

    #' @description
    #' Get an installed skill.
    #' @param name Skill name.
    #' @return A Skill object or NULL.
    get = function(name) {
      if (is.null(self$installed[[name]])) {
        return(NULL)
      }

      Skill$new(self$installed[[name]]$path)
    },

    #' @description
    #' List installed skills.
    #' @return A data frame of installed skills.
    list_installed = function() {
      if (length(self$installed) == 0) {
        return(data.frame(
          name = character(),
          version = character(),
          installed_at = character(),
          stringsAsFactors = FALSE
        ))
      }

      do.call(rbind, lapply(self$installed, function(s) {
        data.frame(
          name = s$name,
          version = s$version,
          installed_at = as.character(s$installed_at),
          stringsAsFactors = FALSE
        )
      }))
    },

    #' @description
    #' Search the registry for skills.
    #' @param query Search query.
    #' @param capability Filter by capability.
    #' @return A data frame of matching skills.
    search = function(query = NULL, capability = NULL) {
      params <- list()
      if (!is.null(query)) params$q <- query
      if (!is.null(capability)) params$capability <- capability

      tryCatch({
        req <- httr2::request(self$registry_url)
        req <- httr2::req_url_path_append(req, "api", "skills", "search")

        if (length(params) > 0) {
          req <- httr2::req_url_query(req, !!!params)
        }

        req <- httr2::req_timeout(req, 10)
        response <- httr2::req_perform(req)

        result <- httr2::resp_body_json(response)

        if (length(result$skills) == 0) {
          return(data.frame(
            name = character(),
            description = character(),
            version = character(),
            author = character(),
            stringsAsFactors = FALSE
          ))
        }

        do.call(rbind, lapply(result$skills, function(s) {
          data.frame(
            name = s$name,
            description = s$description %||% "",
            version = s$version %||% "0.0.0",
            author = s$author$name %||% "unknown",
            stringsAsFactors = FALSE
          )
        }))
      }, error = function(e) {
        message("Registry search failed: ", conditionMessage(e))
        data.frame(
          name = character(),
          description = character(),
          version = character(),
          author = character(),
          stringsAsFactors = FALSE
        )
      })
    },

    #' @description
    #' Update all installed skills to latest versions.
    #' @return Self (invisibly).
    update_all = function() {
      for (name in names(self$installed)) {
        tryCatch({
          self$install(name, force = TRUE)
        }, error = function(e) {
          message("Failed to update ", name, ": ", conditionMessage(e))
        })
      }
      invisible(self)
    },

    #' @description
    #' Validate a skill.yaml manifest.
    #' @param path Path to skill directory or skill.yaml file.
    #' @return A list with validation results.
    validate = function(path) {
      if (file.info(path)$isdir) {
        yaml_path <- file.path(path, "skill.yaml")
      } else {
        yaml_path <- path
      }

      if (!file.exists(yaml_path)) {
        return(list(valid = FALSE, errors = "skill.yaml not found"))
      }

      manifest <- yaml::read_yaml(yaml_path)
      errors <- character()

      # Required fields
      required <- c("name", "version", "description")
      for (field in required) {
        if (is.null(manifest[[field]])) {
          errors <- c(errors, paste0("Missing required field: ", field))
        }
      }

      # Validate name format
      if (!is.null(manifest$name)) {
        if (!grepl("^[a-z][a-z0-9-]*$", manifest$name)) {
          errors <- c(errors, "Name must be lowercase with hyphens only")
        }
      }

      # Validate version format
      if (!is.null(manifest$version)) {
        if (!grepl("^\\d+\\.\\d+\\.\\d+", manifest$version)) {
          errors <- c(errors, "Version must be semantic (e.g., 1.0.0)")
        }
      }

      list(
        valid = length(errors) == 0,
        errors = errors,
        manifest = manifest
      )
    },

    #' @description
    #' Print method for SkillStore.
    print = function() {
      cat("<SkillStore>\n")
      cat("  Registry:", self$registry_url, "\n")
      cat("  Install path:", self$install_path, "\n")
      cat("  Installed skills:", length(self$installed), "\n")
      invisible(self)
    }
  ),

  private = list(
    index_file = NULL,

    load_installed_index = function() {
      private$index_file <- file.path(self$install_path, "index.json")

      if (file.exists(private$index_file)) {
        self$installed <- tryCatch(
          jsonlite::fromJSON(private$index_file, simplifyVector = FALSE),
          error = function(e) list()
        )
      } else {
        self$installed <- list()
      }
    },

    save_installed_index = function() {
      jsonlite::write_json(
        self$installed,
        private$index_file,
        auto_unbox = TRUE,
        pretty = TRUE
      )
    },

    parse_skill_ref = function(ref) {
      # Handle different reference formats:
      # - "skillname" -> registry lookup
      # - "username/skillname" -> GitHub
      # - "https://..." -> direct URL

      if (grepl("^https?://", ref)) {
        # Direct URL
        list(type = "url", url = ref, name = basename(ref))
      } else if (grepl("/", ref)) {
        # GitHub reference
        parts <- strsplit(ref, "/")[[1]]
        list(
          type = "github",
          owner = parts[1],
          repo = parts[2],
          name = parts[2]
        )
      } else {
        # Registry name
        list(type = "registry", name = ref)
      }
    },

    download_skill = function(parsed, version) {
      skill_dir <- file.path(self$install_path, parsed$name)

      # Remove existing if present
      if (dir.exists(skill_dir)) {
        unlink(skill_dir, recursive = TRUE)
      }

      switch(parsed$type,
        "github" = private$download_from_github(parsed, skill_dir, version),
        "url" = private$download_from_url(parsed$url, skill_dir),
        "registry" = private$download_from_registry(parsed$name, skill_dir, version)
      )

      skill_dir
    },

    download_from_github = function(parsed, dest_dir, version) {
      # Construct GitHub archive URL
      ref <- if (!is.null(version)) paste0("v", version) else "main"
      url <- sprintf(
        "https://github.com/%s/%s/archive/refs/heads/%s.zip",
        parsed$owner, parsed$repo, ref
      )

      # Try tags if heads fails
      tryCatch({
        private$download_and_extract(url, dest_dir, parsed$repo)
      }, error = function(e) {
        if (!is.null(version)) {
          url <- sprintf(
            "https://github.com/%s/%s/archive/refs/tags/v%s.zip",
            parsed$owner, parsed$repo, version
          )
          private$download_and_extract(url, dest_dir, parsed$repo)
        } else {
          stop(e)
        }
      })
    },

    download_from_url = function(url, dest_dir) {
      private$download_and_extract(url, dest_dir, "skill")
    },

    download_from_registry = function(name, dest_dir, version) {
      # Get skill info from registry
      req <- httr2::request(self$registry_url)
      req <- httr2::req_url_path_append(req, "api", "skills", name)
      req <- httr2::req_timeout(req, 10)
      response <- httr2::req_perform(req)

      info <- httr2::resp_body_json(response)

      if (!is.null(info$repository$url)) {
        # Download from repository
        parsed <- private$parse_skill_ref(info$repository$url)
        private$download_skill(parsed, version)
      } else if (!is.null(info$download_url)) {
        private$download_from_url(info$download_url, dest_dir)
      } else {
        rlang::abort(paste0("No download source for skill: ", name))
      }
    },

    download_and_extract = function(url, dest_dir, expected_name) {
      # Create temp file for download
      temp_zip <- tempfile(fileext = ".zip")
      on.exit(unlink(temp_zip), add = TRUE)

      # Download
      utils::download.file(url, temp_zip, mode = "wb", quiet = TRUE)

      # Extract to temp directory
      temp_extract <- tempfile()
      on.exit(unlink(temp_extract, recursive = TRUE), add = TRUE)
      utils::unzip(temp_zip, exdir = temp_extract)

      # Find the extracted directory (GitHub adds suffix)
      extracted_dirs <- list.dirs(temp_extract, recursive = FALSE)
      if (length(extracted_dirs) == 0) {
        rlang::abort("No directory found in archive")
      }

      # Move to destination
      dir.create(dirname(dest_dir), recursive = TRUE, showWarnings = FALSE)
      file.rename(extracted_dirs[1], dest_dir)
    },

    load_manifest = function(skill_dir) {
      yaml_path <- file.path(skill_dir, "skill.yaml")

      if (!file.exists(yaml_path)) {
        # Try to create from SKILL.md frontmatter
        skill_md <- file.path(skill_dir, "SKILL.md")
        if (file.exists(skill_md)) {
          return(private$manifest_from_skill_md(skill_md))
        }
        rlang::abort("No skill.yaml or SKILL.md found")
      }

      yaml::read_yaml(yaml_path)
    },

    manifest_from_skill_md = function(skill_md_path) {
      content <- readLines(skill_md_path, warn = FALSE)

      # Find YAML frontmatter
      delim_indices <- which(grepl("^---\\s*$", content))
      if (length(delim_indices) < 2) {
        rlang::abort("SKILL.md must have YAML frontmatter")
      }

      yaml_content <- paste(content[(delim_indices[1] + 1):(delim_indices[2] - 1)], collapse = "\n")
      yaml::yaml.load(yaml_content)
    },

    install_dependencies = function(manifest) {
      if (is.null(manifest$dependencies)) {
        return()
      }

      for (dep in manifest$dependencies) {
        # Parse dependency string (e.g., "dplyr >= 1.0.0")
        parts <- strsplit(dep, "\\s+")[[1]]
        pkg_name <- parts[1]

        if (!requireNamespace(pkg_name, quietly = TRUE)) {
          message("Installing dependency: ", pkg_name)
          utils::install.packages(pkg_name, quiet = TRUE)
        }
      }
    }
  )
)

#' @title Install a Skill
#' @description
#' Install a skill from the global skill store or a GitHub repository.
#' @param skill_ref Skill reference (e.g., "username/skillname").
#' @param version Optional specific version.
#' @param force Force reinstallation.
#' @return The installed Skill object.
#' @export
#' @examples
#' \dontrun{
#' # Install from GitHub
#' install_skill("aisdk/data-analysis")
#'
#' # Install specific version
#' install_skill("aisdk/visualization", version = "1.2.0")
#'
#' # Force reinstall
#' install_skill("aisdk/ml-tools", force = TRUE)
#' }
install_skill <- function(skill_ref, version = NULL, force = FALSE) {
  store <- get_skill_store()
  store$install(skill_ref, version, force)
}

#' @title Uninstall a Skill
#' @description
#' Remove an installed skill.
#' @param name Skill name.
#' @export
uninstall_skill <- function(name) {
  store <- get_skill_store()
  store$uninstall(name)
}

#' @title List Installed Skills
#' @description
#' List all installed skills.
#' @return A data frame of installed skills.
#' @export
list_skills <- function() {
  store <- get_skill_store()
  store$list_installed()
}

#' @title Search Skills
#' @description
#' Search the skill registry.
#' @param query Search query.
#' @param capability Filter by capability.
#' @return A data frame of matching skills.
#' @export
search_skills <- function(query = NULL, capability = NULL) {
  store <- get_skill_store()
  store$search(query, capability)
}

#' @title Get Skill Store
#' @description
#' Get the global skill store instance.
#' @return A SkillStore object.
#' @export
get_skill_store <- function() {
  if (is.null(.aisdk_store_env$store)) {
    .aisdk_store_env$store <- SkillStore$new()
  }
  .aisdk_store_env$store
}

#' @title Create Skill Scaffold
#' @description
#' Create a new skill project with the standard structure.
#' @param name Skill name.
#' @param path Directory to create the skill in.
#' @param author Author name.
#' @param description Brief description.
#' @return Path to the created skill directory.
#' @export
create_skill <- function(name, path = ".", author = NULL, description = NULL) {
  skill_dir <- file.path(path, name)

  if (dir.exists(skill_dir)) {
    rlang::abort(paste0("Directory already exists: ", skill_dir))
  }

  # Create directory structure
  dir.create(skill_dir, recursive = TRUE)
  dir.create(file.path(skill_dir, "scripts"))

  # Create skill.yaml
  manifest <- list(
    name = name,
    version = "0.1.0",
    description = description %||% paste("A skill for", name),
    author = list(
      name = author %||% Sys.info()["user"]
    ),
    license = "MIT",
    dependencies = list(),
    capabilities = list(),
    entry = list(
      main = "SKILL.md",
      scripts = "scripts/"
    )
  )

  yaml::write_yaml(manifest, file.path(skill_dir, "skill.yaml"))

  # Create SKILL.md
  skill_md <- sprintf('---
name: %s
description: %s
---

# %s

## Overview

Describe what this skill does.
## Usage

Explain how to use this skill.

## Examples

Provide usage examples.
', name, manifest$description, name)

  writeLines(skill_md, file.path(skill_dir, "SKILL.md"))

  # Create example script
  example_script <- '# Example script
# Access arguments via args$parameter_name

# Your code here
result <- "Hello from skill!"

# Return result
result
'
  writeLines(example_script, file.path(skill_dir, "scripts", "example.R"))

  message("Created skill scaffold at: ", skill_dir)
  skill_dir
}

# Package environment for skill store
.aisdk_store_env <- new.env(parent = emptyenv())

# Null-coalescing operator
if (!exists("%||%", mode = "function")) {
 `%||%` <- function(x, y) if (is.null(x)) y else x
}
