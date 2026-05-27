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
        source_ref = skill_ref,
        source_type = parsed$type,
        source_path = parsed$path %||% NULL,
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
      # - "username/skillname" -> GitHub repository root
      # - "username/repo/path/to/skill" -> skill subdirectory in a GitHub repository
      # - "https://github.com/owner/repo[/tree/branch/path]" -> GitHub URL
      # - "https://..." -> direct archive URL

      if (grepl("^https?://", ref)) {
        github_ref <- private$parse_github_url(ref)
        if (!is.null(github_ref)) {
          github_ref
        } else {
          # Direct URL
          list(type = "url", url = ref, name = basename(ref), path = NULL)
        }
      } else if (grepl("/", ref)) {
        # GitHub reference
        parts <- strsplit(ref, "/", fixed = TRUE)[[1]]
        if (length(parts) < 2 || !nzchar(parts[[1]]) || !nzchar(parts[[2]])) {
          rlang::abort("GitHub skill references must use 'owner/repo' or 'owner/repo/path'.")
        }
        skill_path <- NULL
        if (length(parts) > 2) {
          skill_path <- paste(parts[-c(1, 2)], collapse = "/")
        }
        list(
          type = "github",
          owner = parts[1],
          repo = parts[2],
          path = skill_path,
          branch = NULL,
          name = private$skill_name_from_source(parts[2], skill_path)
        )
      } else {
        # Registry name
        list(type = "registry", name = ref, path = NULL)
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
      ref <- parsed$branch %||% if (!is.null(version)) paste0("v", version) else "main"
      url <- sprintf(
        "https://github.com/%s/%s/archive/refs/heads/%s.zip",
        parsed$owner, parsed$repo, ref
      )

      # Try tags if heads fails
      tryCatch({
        private$download_and_extract(url, dest_dir, parsed$repo, subdir = parsed$path)
      }, error = function(e) {
        if (!is.null(version) && is.null(parsed$branch)) {
          url <- sprintf(
            "https://github.com/%s/%s/archive/refs/tags/v%s.zip",
            parsed$owner, parsed$repo, version
          )
          private$download_and_extract(url, dest_dir, parsed$repo, subdir = parsed$path)
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
        parsed$path <- info$path %||% parsed$path
        parsed$branch <- info$repository$branch %||% parsed$branch
        if (identical(parsed$type, "github")) {
          private$download_from_github(parsed, dest_dir, version)
        } else if (identical(parsed$type, "url")) {
          private$download_from_url(parsed$url, dest_dir)
        } else {
          rlang::abort(paste0("Unsupported repository source for skill: ", name))
        }
      } else if (!is.null(info$download_url)) {
        private$download_from_url(info$download_url, dest_dir)
      } else {
        rlang::abort(paste0("No download source for skill: ", name))
      }
    },

    download_and_extract = function(url, dest_dir, expected_name, subdir = NULL) {
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

      source_dir <- extracted_dirs[1]
      if (!is.null(subdir) && nzchar(subdir)) {
        subdir <- gsub("^/+|/+$", "", subdir)
        source_dir <- file.path(source_dir, subdir)
        if (!dir.exists(source_dir)) {
          rlang::abort(paste0("Skill path not found in archive: ", subdir))
        }
      }

      # Move to destination
      dir.create(dirname(dest_dir), recursive = TRUE, showWarnings = FALSE)
      if (!file.rename(source_dir, dest_dir)) {
        rlang::abort(paste0("Failed to install skill archive into: ", dest_dir))
      }
    },

    parse_github_url = function(url) {
      pattern <- "^https?://github[.]com/([^/]+)/([^/]+?)(?:[.]git)?(?:/(tree|blob)/([^/]+)(?:/(.*))?)?/?$"
      matches <- regexec(pattern, url)
      parts <- regmatches(url, matches)[[1]]
      if (length(parts) == 0) {
        return(NULL)
      }

      skill_path <- if (length(parts) >= 6 && nzchar(parts[[6]])) parts[[6]] else NULL
      repo <- parts[[3]]
      list(
        type = "github",
        owner = parts[[2]],
        repo = repo,
        path = skill_path,
        branch = if (length(parts) >= 5 && nzchar(parts[[5]])) parts[[5]] else NULL,
        name = private$skill_name_from_source(repo, skill_path)
      )
    },

    skill_name_from_source = function(repo, skill_path = NULL) {
      if (!is.null(skill_path) && nzchar(skill_path)) {
        return(basename(gsub("/+$", "", skill_path)))
      }
      sub("[.]git$", "", repo)
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
#' @param skill_ref Skill reference (e.g., `"username/skillname"`,
#'   `"username/repo/path/to/skill"`, or a public GitHub URL pointing to a
#'   skill directory such as
#'   `"https://github.com/posit-dev/skills/tree/main/r-lib/testing-r-packages"`).
#' @param version Optional specific version.
#' @param force Force reinstallation.
#' @return The installed Skill object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#' # Install from a public GitHub repository
#' install_skill("aisdk/data-analysis")
#' install_skill("posit-dev/skills/r-lib/testing-r-packages")
#'
#' # Install specific version
#' install_skill("aisdk/visualization", version = "1.2.0")
#'
#' # Force reinstall
#' install_skill("aisdk/ml-tools", force = TRUE)
#' }
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
create_skill <- function(name, path = tempdir(), author = NULL, description = NULL) {
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

#' @title Convert a Skill to a Store Record
#' @description
#' Convert a `Skill` object or skill directory into a JSON-ready record that
#' matches the skill store schema.
#' @param skill A `Skill` object or path to a skill directory.
#' @param include_body Include the full `SKILL.md` body.
#' @param include_files Include textual source files from `scripts/` and `references/`.
#' @param include_assets Include asset file names.
#' @return A named list ready for JSON serialization.
#' @export
skill_to_store_record <- function(skill,
                                  include_body = TRUE,
                                  include_files = TRUE,
                                  include_assets = FALSE) {
  skill_obj <- if (inherits(skill, "Skill")) {
    skill
  } else if (is.character(skill) && length(skill) == 1) {
    Skill$new(skill)
  } else {
    rlang::abort("skill must be a Skill object or a single directory path.")
  }

  skill_obj$to_store_record(
    include_body = include_body,
    include_files = include_files,
    include_assets = include_assets
  )
}

#' @title Publish a Skill to the Store
#' @description
#' Publish a skill record as a JSON file to a GitHub-backed skill store.
#' @param skill A `Skill` object or path to a skill directory.
#' @param github_user GitHub owner of the skill store repository.
#' @param github_token GitHub token with content write access.
#' @param repo_name Skill store repository name.
#' @param base_path Directory inside the repository where skills live.
#' @param branch Repository branch to write to.
#' @param message Commit message. Defaults to a generated message.
#' @param include_body Include the full `SKILL.md` body.
#' @param include_files Include textual source files.
#' @param include_assets Include asset file names.
#' @return A list containing the record and GitHub API response.
#' @export
publish_skill <- function(skill,
                          github_user,
                          github_token,
                          repo_name,
                          base_path = "skills/",
                          branch = "main",
                          message = NULL,
                          include_body = TRUE,
                          include_files = TRUE,
                          include_assets = FALSE) {
  record <- skill_to_store_record(
    skill,
    include_body = include_body,
    include_files = include_files,
    include_assets = include_assets
  )
  publish_skill_record(
    record = record,
    github_user = github_user,
    github_token = github_token,
    repo_name = repo_name,
    base_path = base_path,
    branch = branch,
    message = message
  )
}

#' @title Publish a Skill Record
#' @description
#' Upload a skill JSON record to a GitHub-backed skill store.
#' @param record JSON-ready skill record.
#' @param github_user GitHub owner of the skill store repository.
#' @param github_token GitHub token with content write access.
#' @param repo_name Skill store repository name.
#' @param base_path Directory inside the repository where skills live.
#' @param branch Repository branch to write to.
#' @param message Commit message. Defaults to a generated message.
#' @return A list with `record`, `path`, and `response`.
#' @export
publish_skill_record <- function(record,
                                 github_user,
                                 github_token,
                                 repo_name,
                                 base_path = "skills/",
                                 branch = "main",
                                 message = NULL) {
  if (!is.list(record)) {
    rlang::abort("record must be a list produced by skill_to_store_record().")
  }

  skill_id <- record$id %||% record$name
  if (is.null(skill_id) || !nzchar(skill_id)) {
    rlang::abort("record must include an id or name.")
  }

  normalized_base <- gsub("^/+|/+$", "", base_path %||% "skills/")
  file_path <- file.path(normalized_base, paste0(skill_id, ".json"))
  file_path <- gsub("\\\\", "/", file_path)
  json_content <- jsonlite::toJSON(record, auto_unbox = TRUE, pretty = TRUE, null = "null")
  encoded_content <- base64enc::base64encode(charToRaw(json_content))
  api_url <- sprintf(
    "https://api.github.com/repos/%s/%s/contents/%s",
    github_user, repo_name, file_path
  )

  get_req <- httr2::request(api_url)
  get_req <- httr2::req_headers(
    get_req,
    Authorization = paste("token", github_token),
    Accept = "application/vnd.github+json"
  )
  if (!is.null(branch) && nzchar(branch)) {
    get_req <- httr2::req_url_query(get_req, ref = branch)
  }
  get_req <- httr2::req_error(get_req, is_error = function(resp) FALSE)
  get_resp <- httr2::req_perform(get_req)
  existing_sha <- NULL
  if (httr2::resp_status(get_resp) == 200) {
    existing_sha <- tryCatch(httr2::resp_body_json(get_resp)$sha, error = function(e) NULL)
  }

  commit_message <- message %||% sprintf("Publish skill: %s", record$name %||% skill_id)
  body <- list(
    message = commit_message,
    content = encoded_content,
    branch = branch
  )
  if (!is.null(existing_sha)) {
    body$sha <- existing_sha
  }

  put_req <- httr2::request(api_url)
  put_req <- httr2::req_headers(
    put_req,
    Authorization = paste("token", github_token),
    Accept = "application/vnd.github+json"
  )
  put_req <- httr2::req_method(put_req, "PUT")
  put_req <- httr2::req_body_json(put_req, body)
  put_resp <- httr2::req_perform(put_req)

  list(
    record = record,
    path = file_path,
    response = httr2::resp_body_json(put_resp)
  )
}

#' @title Record a Skill Iteration
#' @description
#' Persist a local optimization or evaluation iteration for a skill.
#' @param skill A `Skill` object or path to a skill directory.
#' @param iteration A named list with iteration metadata, eval results, notes, or artifacts.
#' @param out_dir Output directory. Defaults to `<skill>/.aisdk/iterations`.
#' @param filename Optional filename. Defaults to a timestamped slug.
#' @param include_record Include the serialized skill record in the output.
#' @return Path to the written JSON file.
#' @export
record_skill_iteration <- function(skill,
                                   iteration = list(),
                                   out_dir = NULL,
                                   filename = NULL,
                                   include_record = TRUE) {
  skill_obj <- if (inherits(skill, "Skill")) {
    skill
  } else if (is.character(skill) && length(skill) == 1) {
    Skill$new(skill)
  } else {
    rlang::abort("skill must be a Skill object or a single directory path.")
  }

  record <- if (isTRUE(include_record)) {
    skill_to_store_record(skill_obj, include_body = TRUE, include_files = TRUE, include_assets = TRUE)
  } else {
    NULL
  }

  output_dir <- out_dir %||% file.path(skill_obj$path, ".aisdk", "iterations")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC")
  safe_name <- gsub("[^A-Za-z0-9._-]+", "-", skill_obj$name %||% "skill")
  output_file <- filename %||% paste0(timestamp, "-", safe_name, ".json")
  output_path <- file.path(output_dir, output_file)

  payload <- list(
    skill = list(
      name = skill_obj$name,
      path = skill_obj$path,
      version = skill_obj$manifest$version %||% NULL
    ),
    iteration = iteration,
    record = record,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  jsonlite::write_json(payload, output_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  output_path
}

#' @title Use a Skill from the Store
#' @description
#' Install a skill from the store and return the loaded `Skill` object.
#' @param skill_ref Skill reference.
#' @param version Optional version.
#' @param force Force reinstallation.
#' @return A `Skill` object.
#' @export
use_skill <- function(skill_ref, version = NULL, force = FALSE) {
  install_skill(skill_ref, version = version, force = force)
}

#' @title Sync a Skill to the Store
#' @description
#' Publish a skill to the store and optionally record the iteration locally.
#' @param skill A `Skill` object or path to a skill directory.
#' @param github_user GitHub owner of the skill store repository.
#' @param github_token GitHub token with content write access.
#' @param repo_name Skill store repository name.
#' @param base_path Directory inside the repository where skills live.
#' @param branch Repository branch to write to.
#' @param message Commit message for the store publish.
#' @param iteration Named list describing the current optimization iteration.
#' @param out_dir Optional local iteration log directory.
#' @param record_iteration Write a local iteration log.
#' @param include_body Include the full `SKILL.md` body in the published record.
#' @param include_files Include textual source files in the published record.
#' @param include_assets Include asset file names in the published record.
#' @param publish_fun Function used to publish the skill. Defaults to [publish_skill()].
#' @param record_fun Function used to record the iteration. Defaults to [record_skill_iteration()].
#' @return A list with `publish` and `iteration_path`.
#' @export
sync_skill <- function(skill,
                       github_user,
                       github_token,
                       repo_name,
                       base_path = "skills/",
                       branch = "main",
                       message = NULL,
                       iteration = list(),
                       out_dir = NULL,
                       record_iteration = TRUE,
                       include_body = TRUE,
                       include_files = TRUE,
                       include_assets = FALSE,
                       publish_fun = publish_skill,
                       record_fun = record_skill_iteration) {
  publish_result <- publish_fun(
    skill = skill,
    github_user = github_user,
    github_token = github_token,
    repo_name = repo_name,
    base_path = base_path,
    branch = branch,
    message = message,
    include_body = include_body,
    include_files = include_files,
    include_assets = include_assets
  )

  iteration_path <- NULL
  if (isTRUE(record_iteration)) {
    iteration_path <- record_fun(
      skill = skill,
      iteration = iteration,
      out_dir = out_dir,
      include_record = TRUE
    )
  }

  list(
    publish = publish_result,
    iteration_path = iteration_path
  )
}

# Package environment for skill store
.aisdk_store_env <- new.env(parent = emptyenv())

# Null-coalescing operator
if (!exists("%||%", mode = "function")) {
 `%||%` <- function(x, y) if (is.null(x)) y else x
}
