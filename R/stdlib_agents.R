#' @title Standard Agent Library: Built-in Specialist Agents
#' @description
#' Factory functions for creating standard library agents with scoped tools
#' and safety guardrails. These agents form the foundation of the multi-agent
#' orchestration system.
#' @name stdlib_agents
NULL

# =============================================================================
# DataAgent: Data Manipulation Specialist (dplyr/tidyr)
# =============================================================================

#' @title Create a DataAgent
#' @description
#' Creates an agent specialized in data manipulation using dplyr and tidyr.
#' The agent can filter, transform, summarize, and reshape data frames in
#' the session environment.
#'
#' @param name Agent name. Default "DataAgent".
#' @param safe_mode If TRUE (default), restricts operations to data manipulation only.
#' @return An Agent object configured for data manipulation.
#' @export
#' @examples
#' \dontrun{
#' data_agent <- create_data_agent()
#' session <- create_shared_session(model = "openai:gpt-4o")
#' session$set_var("sales", data.frame(
#'   product = c("A", "B", "C"),
#'   revenue = c(100, 200, 150)
#' ))
#' result <- data_agent$run(
#'   "Calculate total revenue and find the top product",
#'   session = session,
#'   model = "openai:gpt-4o"
#' )
#' }
create_data_agent <- function(name = "DataAgent", safe_mode = TRUE) {

  # Tool: Execute dplyr/tidyr operations
  transform_data_tool <- Tool$new(
    name = "transform_data",
    description = paste0(
      "Execute dplyr/tidyr data transformation on a data frame. ",
      "Supports: filter, select, mutate, arrange, group_by, summarize, ",
      "pivot_longer, pivot_wider, left_join, and more. ",
      "The result is stored back in the session."
    ),
    parameters = z_object(
      input_var = z_string("Name of the input data frame variable"),
      output_var = z_string("Name for the output variable (can be same as input)"),
      operations = z_string(paste0(
        "dplyr/tidyr pipeline code (without the data argument). ",
        "Example: 'filter(x > 10) %>% group_by(category) %>% summarize(total = sum(value))'"
      ))
    ),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) {
        return("Error: No session environment available.")
      }

      input_var <- args$input_var
      output_var <- args$output_var
      operations <- args$operations

      # Check input exists
      if (!exists(input_var, envir = env, inherits = FALSE)) {
        return(paste0("Error: Variable '", input_var, "' not found in session."))
      }

      input_data <- get(input_var, envir = env)
      if (!is.data.frame(input_data)) {
        return(paste0("Error: '", input_var, "' is not a data frame."))
      }

      # Check for required packages
      if (!requireNamespace("dplyr", quietly = TRUE)) {
        return("Error: dplyr package is not installed.")
      }

      # Safe mode checks
      if (safe_mode) {
        dangerous <- c("system", "shell", "eval", "parse", "source",
                      "write", "save", "download", "url", "file\\.")
        for (pattern in dangerous) {
          if (grepl(pattern, operations, ignore.case = TRUE)) {
            return(paste0("Error: Unsafe operation detected: ", pattern))
          }
        }
      }

      # Execute transformation
      result <- tryCatch({
        # Build and execute the pipeline
        code <- paste0("input_data %>% ", operations)
        output <- eval(parse(text = code), envir = list(
          input_data = input_data,
          `%>%` = dplyr::`%>%`
        ), enclos = asNamespace("dplyr"))

        # Store result
        assign(output_var, output, envir = env)

        # Return summary
        paste0(
          "Transformation complete. '", output_var, "' now has ",
          nrow(output), " rows x ", ncol(output), " columns.\n",
          "Columns: ", paste(names(output), collapse = ", ")
        )
      }, error = function(e) {
        paste0("Error in transformation: ", conditionMessage(e))
      })

      result
    }
  )

  # Tool: Summarize data
  summarize_data_tool <- Tool$new(
    name = "summarize_data",
    description = paste0(
      "Get a statistical summary of a data frame. ",
      "Returns column types, missing values, and basic statistics."
    ),
    parameters = z_object(
      var_name = z_string("Name of the data frame variable to summarize"),
      columns = z_string("Comma-separated column names to focus on (optional, empty for all)")
    ),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) {
        return("Error: No session environment available.")
      }

      var_name <- args$var_name
      columns <- args$columns

      if (!exists(var_name, envir = env, inherits = FALSE)) {
        return(paste0("Error: Variable '", var_name, "' not found."))
      }

      df <- get(var_name, envir = env)
      if (!is.data.frame(df)) {
        return(paste0("Error: '", var_name, "' is not a data frame."))
      }

      # Filter columns if specified
      if (!is.null(columns) && nzchar(columns)) {
        cols <- trimws(strsplit(columns, ",")[[1]])
        cols <- cols[cols %in% names(df)]
        if (length(cols) > 0) {
          df <- df[, cols, drop = FALSE]
        }
      }

      # Build summary
      lines <- c(
        paste0("Data frame: ", var_name),
        paste0("Dimensions: ", nrow(df), " rows x ", ncol(df), " columns"),
        ""
      )

      for (col in names(df)) {
        col_data <- df[[col]]
        col_type <- class(col_data)[1]
        n_missing <- sum(is.na(col_data))

        if (is.numeric(col_data)) {
          stats <- sprintf(
            "  %s (%s): min=%.2f, max=%.2f, mean=%.2f, median=%.2f, NA=%d",
            col, col_type,
            min(col_data, na.rm = TRUE),
            max(col_data, na.rm = TRUE),
            mean(col_data, na.rm = TRUE),
            stats::median(col_data, na.rm = TRUE),
            n_missing
          )
        } else if (is.factor(col_data) || is.character(col_data)) {
          n_unique <- length(unique(col_data))
          top_values <- names(sort(table(col_data), decreasing = TRUE))[1:min(3, n_unique)]
          stats <- sprintf(
            "  %s (%s): %d unique values, top: %s, NA=%d",
            col, col_type, n_unique,
            paste(top_values, collapse = ", "),
            n_missing
          )
        } else {
          stats <- sprintf("  %s (%s): NA=%d", col, col_type, n_missing)
        }

        lines <- c(lines, stats)
      }

      paste(lines, collapse = "\n")
    }
  )

  # Tool: Join data frames
  join_data_tool <- Tool$new(
    name = "join_data",
    description = paste0(
      "Join two data frames together. ",
      "Supports left_join, right_join, inner_join, full_join."
    ),
    parameters = z_object(
      left_var = z_string("Name of the left data frame"),
      right_var = z_string("Name of the right data frame"),
      output_var = z_string("Name for the output variable"),
      join_type = z_enum(
        c("left", "right", "inner", "full"),
        description = "Type of join to perform"
      ),
      by_columns = z_string("Comma-separated column names to join by")
    ),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) {
        return("Error: No session environment available.")
      }

      if (!requireNamespace("dplyr", quietly = TRUE)) {
        return("Error: dplyr package is not installed.")
      }

      left_var <- args$left_var
      right_var <- args$right_var
      output_var <- args$output_var
      join_type <- args$join_type
      by_columns <- trimws(strsplit(args$by_columns, ",")[[1]])

      # Get data frames
      if (!exists(left_var, envir = env)) {
        return(paste0("Error: '", left_var, "' not found."))
      }
      if (!exists(right_var, envir = env)) {
        return(paste0("Error: '", right_var, "' not found."))
      }

      left_df <- get(left_var, envir = env)
      right_df <- get(right_var, envir = env)

      # Perform join
      result <- tryCatch({
        join_fn <- switch(join_type,
          "left" = dplyr::left_join,
          "right" = dplyr::right_join,
          "inner" = dplyr::inner_join,
          "full" = dplyr::full_join
        )

        output <- join_fn(left_df, right_df, by = by_columns)
        assign(output_var, output, envir = env)

        paste0(
          "Join complete. '", output_var, "' has ",
          nrow(output), " rows x ", ncol(output), " columns."
        )
      }, error = function(e) {
        paste0("Error in join: ", conditionMessage(e))
      })

      result
    }
  )

  # Tool: List available data
  list_data_tool <- Tool$new(
    name = "list_data",
    description = "List all data frames available in the session environment.",
    parameters = z_empty_object(description = "No parameters required"),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) {
        return("Error: No session environment available.")
      }

      vars <- ls(env)
      if (length(vars) == 0) {
        return("No variables in session.")
      }

      data_frames <- character(0)
      for (v in vars) {
        obj <- get(v, envir = env)
        if (is.data.frame(obj)) {
          data_frames <- c(data_frames, sprintf(
            "- %s: %d rows x %d cols (%s)",
            v, nrow(obj), ncol(obj),
            paste(names(obj)[1:min(5, ncol(obj))], collapse = ", ")
          ))
        }
      }

      if (length(data_frames) == 0) {
        return("No data frames found in session.")
      }

      paste(c("Available data frames:", data_frames), collapse = "\n")
    }
  )

  system_prompt <- paste0(
    "You are a data manipulation specialist using dplyr and tidyr.\n\n",
    "Your expertise includes:\n",
    "- Filtering and selecting data (filter, select, slice)\n",
    "- Transforming columns (mutate, transmute, across)\n",
    "- Grouping and summarizing (group_by, summarize, count)\n",
    "- Reshaping data (pivot_longer, pivot_wider)\n",
    "- Joining data frames (left_join, inner_join, etc.)\n",
    "- Handling missing values and data cleaning\n\n",
    "Guidelines:\n",
    "1. Always check available data first with list_data\n",
    "2. Understand the data structure with summarize_data before transforming\n",
    "3. Use meaningful output variable names\n",
    "4. Chain operations efficiently with %>%\n",
    "5. Explain what each transformation does\n",
    if (safe_mode) "6. You are in safe mode: only data manipulation is allowed\n" else ""
  )

  Agent$new(
    name = name,
    description = paste0(
      "Data manipulation specialist using dplyr/tidyr. Can filter, transform, ",
      "summarize, reshape, and join data frames. Expert in tidyverse workflows."
    ),
    system_prompt = system_prompt,
    tools = list(transform_data_tool, summarize_data_tool, join_data_tool, list_data_tool)
  )
}

# =============================================================================
# FileAgent: File System Specialist (fs/readr)
# =============================================================================

#' @title Create a FileAgent
#' @description
#' Creates an agent specialized in file system operations using fs and readr.
#' The agent can read, write, and manage files with safety guardrails.
#'
#' @param name Agent name. Default "FileAgent".
#' @param allowed_dirs Character vector of allowed directories. Default current dir.
#' @param allowed_extensions Character vector of allowed file extensions.
#' @return An Agent object configured for file operations.
#' @export
#' @examples
#' \dontrun{
#' file_agent <- create_file_agent(
#'   allowed_dirs = c("./data", "./output"),
#'   allowed_extensions = c("csv", "json", "txt", "rds")
#' )
#' result <- file_agent$run(
#'   "Read the sales.csv file and store it as 'sales_data'",
#'   session = session,
#'   model = "openai:gpt-4o"
#' )
#' }
create_file_agent <- function(name = "FileAgent",
                               allowed_dirs = ".",
                               allowed_extensions = c("csv", "tsv", "txt", "json",
                                                     "rds", "rda", "xlsx", "xls")) {

  # Helper to check path safety
  check_path_safety <- function(path, allowed_dirs) {
    # Normalize path
    norm_path <- normalizePath(path, mustWork = FALSE)
    norm_dirs <- sapply(allowed_dirs, function(d) {
      normalizePath(d, mustWork = FALSE)
    })

    # Check if path is within allowed directories
    for (dir in norm_dirs) {
      if (startsWith(norm_path, dir)) {
        return(list(safe = TRUE, reason = NULL))
      }
    }

    list(
      safe = FALSE,
      reason = paste0("Path '", path, "' is outside allowed directories: ",
                     paste(allowed_dirs, collapse = ", "))
    )
  }

  # Helper to check extension
  check_extension <- function(path, allowed_extensions) {
    ext <- tools::file_ext(path)
    if (tolower(ext) %in% tolower(allowed_extensions)) {
      return(list(safe = TRUE, reason = NULL))
    }
    list(
      safe = FALSE,
      reason = paste0("Extension '.", ext, "' not allowed. Allowed: ",
                     paste0(".", allowed_extensions, collapse = ", "))
    )
  }

  # Tool: Read file
  read_file_tool <- Tool$new(
    name = "read_file",
    description = paste0(
      "Read a file into the session environment. ",
      "Supports CSV, TSV, JSON, RDS, and Excel files. ",
      "Automatically detects format from extension."
    ),
    parameters = z_object(
      path = z_string("Path to the file to read"),
      var_name = z_string("Name for the variable in session"),
      options = z_string("Optional: comma-separated read options (e.g., 'skip=1,na=NA')")
    ),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) {
        return("Error: No session environment available.")
      }

      path <- args$path
      var_name <- args$var_name

      # Safety checks
      path_check <- check_path_safety(path, allowed_dirs)
      if (!path_check$safe) {
        return(paste0("Error: ", path_check$reason))
      }

      ext_check <- check_extension(path, allowed_extensions)
      if (!ext_check$safe) {
        return(paste0("Error: ", ext_check$reason))
      }

      if (!file.exists(path)) {
        return(paste0("Error: File '", path, "' does not exist."))
      }

      # Read based on extension
      ext <- tolower(tools::file_ext(path))
      result <- tryCatch({
        data <- switch(ext,
          "csv" = {
            if (requireNamespace("readr", quietly = TRUE)) {
              readr::read_csv(path, show_col_types = FALSE)
            } else {
              utils::read.csv(path, stringsAsFactors = FALSE)
            }
          },
          "tsv" = {
            if (requireNamespace("readr", quietly = TRUE)) {
              readr::read_tsv(path, show_col_types = FALSE)
            } else {
              utils::read.delim(path, stringsAsFactors = FALSE)
            }
          },
          "txt" = readLines(path),
          "json" = jsonlite::fromJSON(path),
          "rds" = readRDS(path),
          "rda" = {
            load(path, envir = env)
            return(paste0("Loaded objects from '", path, "' into session."))
          },
          "xlsx" = , "xls" = {
            if (!requireNamespace("readxl", quietly = TRUE)) {
              return("Error: readxl package required for Excel files.")
            }
            readxl::read_excel(path)
          },
          return(paste0("Error: Unsupported file type: .", ext))
        )

        assign(var_name, data, envir = env)

        if (is.data.frame(data)) {
          paste0(
            "Read '", path, "' into '", var_name, "': ",
            nrow(data), " rows x ", ncol(data), " columns"
          )
        } else {
          paste0("Read '", path, "' into '", var_name, "'")
        }
      }, error = function(e) {
        paste0("Error reading file: ", conditionMessage(e))
      })

      result
    }
  )

  # Tool: Write file
  write_file_tool <- Tool$new(
    name = "write_file",
    description = paste0(
      "Write data from session to a file. ",
      "Supports CSV, TSV, JSON, and RDS formats."
    ),
    parameters = z_object(
      var_name = z_string("Name of the variable to write"),
      path = z_string("Path for the output file"),
      format = z_enum(
        c("csv", "tsv", "json", "rds"),
        description = "Output format (auto-detected from extension if not specified)"
      )
    ),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) {
        return("Error: No session environment available.")
      }

      var_name <- args$var_name
      path <- args$path
      format <- args$format

      # Safety checks
      path_check <- check_path_safety(path, allowed_dirs)
      if (!path_check$safe) {
        return(paste0("Error: ", path_check$reason))
      }

      if (!exists(var_name, envir = env)) {
        return(paste0("Error: Variable '", var_name, "' not found."))
      }

      data <- get(var_name, envir = env)

      # Auto-detect format from extension if not specified
      if (is.null(format) || format == "") {
        format <- tolower(tools::file_ext(path))
      }

      result <- tryCatch({
        switch(format,
          "csv" = {
            if (requireNamespace("readr", quietly = TRUE)) {
              readr::write_csv(data, path)
            } else {
              utils::write.csv(data, path, row.names = FALSE)
            }
          },
          "tsv" = {
            if (requireNamespace("readr", quietly = TRUE)) {
              readr::write_tsv(data, path)
            } else {
              utils::write.table(data, path, sep = "\t", row.names = FALSE)
            }
          },
          "json" = jsonlite::write_json(data, path, auto_unbox = TRUE, pretty = TRUE),
          "rds" = saveRDS(data, path),
          return(paste0("Error: Unsupported format: ", format))
        )

        paste0("Wrote '", var_name, "' to '", path, "'")
      }, error = function(e) {
        paste0("Error writing file: ", conditionMessage(e))
      })

      result
    }
  )

  # Tool: List files
  list_files_tool <- Tool$new(
    name = "list_files",
    description = "List files in a directory with optional pattern filtering.",
    parameters = z_object(
      path = z_string("Directory path to list"),
      pattern = z_string("Optional: regex pattern to filter files"),
      recursive = z_boolean("Search subdirectories? Default FALSE")
    ),
    execute = function(args) {
      path <- args$path %||% "."
      pattern <- args$pattern
      recursive <- args$recursive %||% FALSE

      # Safety check
      path_check <- check_path_safety(path, allowed_dirs)
      if (!path_check$safe) {
        return(paste0("Error: ", path_check$reason))
      }

      if (!dir.exists(path)) {
        return(paste0("Error: Directory '", path, "' does not exist."))
      }

      files <- list.files(path, pattern = pattern, recursive = recursive,
                         full.names = TRUE)

      if (length(files) == 0) {
        return("No files found matching criteria.")
      }

      # Get file info
      info <- file.info(files)
      summaries <- mapply(function(f, size, mtime) {
        sprintf("- %s (%.1f KB, %s)",
                basename(f),
                size / 1024,
                format(mtime, "%Y-%m-%d %H:%M"))
      }, files, info$size, info$mtime, SIMPLIFY = TRUE)

      paste(c(paste0("Files in '", path, "':"), summaries), collapse = "\n")
    }
  )

  # Tool: File info
  file_info_tool <- Tool$new(
    name = "file_info",
    description = "Get detailed information about a file.",
    parameters = z_object(
      path = z_string("Path to the file")
    ),
    execute = function(args) {
      path <- args$path

      path_check <- check_path_safety(path, allowed_dirs)
      if (!path_check$safe) {
        return(paste0("Error: ", path_check$reason))
      }

      if (!file.exists(path)) {
        return(paste0("Error: File '", path, "' does not exist."))
      }

      info <- file.info(path)
      paste0(
        "File: ", basename(path), "\n",
        "Path: ", normalizePath(path), "\n",
        "Size: ", format(info$size, big.mark = ","), " bytes\n",
        "Modified: ", format(info$mtime, "%Y-%m-%d %H:%M:%S"), "\n",
        "Type: ", if (info$isdir) "directory" else tools::file_ext(path)
      )
    }
  )

  system_prompt <- paste0(
    "You are a file system specialist for data files.\n\n",
    "Your capabilities:\n",
    "- Reading various file formats (CSV, TSV, JSON, RDS, Excel)\n",
    "- Writing data to files in different formats\n",
    "- Listing and exploring directory contents\n",
    "- Getting file metadata and information\n\n",
    "Safety constraints:\n",
    "- Allowed directories: ", paste(allowed_dirs, collapse = ", "), "\n",
    "- Allowed extensions: ", paste0(".", allowed_extensions, collapse = ", "), "\n\n",
    "Guidelines:\n",
    "1. Always verify file paths before operations\n",
    "2. Use appropriate formats for the data type\n",
    "3. Report file sizes and row counts after reading\n",
    "4. Suggest appropriate variable names"
  )

  Agent$new(
    name = name,
    description = paste0(
      "File system specialist for data files. Can read CSV, JSON, Excel, RDS files ",
      "and write data to various formats. Operates within allowed directories only."
    ),
    system_prompt = system_prompt,
    tools = list(read_file_tool, write_file_tool, list_files_tool, file_info_tool)
  )
}

# =============================================================================
# EnvAgent: Environment/Package Management Specialist
# =============================================================================

#' @title Create an EnvAgent
#' @description
#' Creates an agent specialized in R environment and package management.
#' The agent can check, install, and manage R packages with safety controls.
#'
#' @param name Agent name. Default "EnvAgent".
#' @param allow_install Allow package installation. Default FALSE.
#' @param allowed_repos CRAN mirror URLs for installation.
#' @return An Agent object configured for environment management.
#' @export
#' @examples
#' \dontrun{
#' env_agent <- create_env_agent(allow_install = TRUE)
#' result <- env_agent$run(
#'   "Check if tidyverse is installed and load it",
#'   session = session,
#'   model = "openai:gpt-4o"
#' )
#' }
create_env_agent <- function(name = "EnvAgent",
                              allow_install = FALSE,
                              allowed_repos = "https://cloud.r-project.org") {

  # Tool: Check package
  check_package_tool <- Tool$new(
    name = "check_package",
    description = "Check if an R package is installed and get its version.",
    parameters = z_object(
      package = z_string("Name of the package to check")
    ),
    execute = function(args) {
      pkg <- args$package

      if (requireNamespace(pkg, quietly = TRUE)) {
        version <- as.character(utils::packageVersion(pkg))
        paste0("Package '", pkg, "' is installed (version ", version, ")")
      } else {
        paste0("Package '", pkg, "' is NOT installed")
      }
    }
  )

  # Tool: Load package
  load_package_tool <- Tool$new(
    name = "load_package",
    description = "Load an R package into the session.",
    parameters = z_object(
      package = z_string("Name of the package to load"),
      quietly = z_boolean("Suppress loading messages? Default TRUE")
    ),
    execute = function(args) {
      pkg <- args$package
      quietly <- args$quietly %||% TRUE

      result <- tryCatch({
        if (quietly) {
          suppressPackageStartupMessages(library(pkg, character.only = TRUE))
        } else {
          library(pkg, character.only = TRUE)
        }
        paste0("Package '", pkg, "' loaded successfully")
      }, error = function(e) {
        paste0("Error loading '", pkg, "': ", conditionMessage(e))
      })

      result
    }
  )

  # Tool: Install package (conditional)
  install_package_tool <- Tool$new(
    name = "install_package",
    description = if (allow_install) {
      "Install an R package from CRAN."
    } else {
      "Package installation is disabled. Contact administrator to enable."
    },
    parameters = z_object(
      package = z_string("Name of the package to install"),
      dependencies = z_boolean("Install dependencies? Default TRUE")
    ),
    execute = function(args) {
      if (!allow_install) {
        return("Error: Package installation is disabled for safety.")
      }

      pkg <- args$package
      deps <- args$dependencies %||% TRUE

      result <- tryCatch({
        utils::install.packages(
          pkg,
          repos = allowed_repos,
          dependencies = deps,
          quiet = TRUE
        )

        if (requireNamespace(pkg, quietly = TRUE)) {
          paste0("Package '", pkg, "' installed successfully")
        } else {
          paste0("Installation completed but package '", pkg, "' not found")
        }
      }, error = function(e) {
        paste0("Error installing '", pkg, "': ", conditionMessage(e))
      })

      result
    }
  )

  # Tool: List loaded packages
  list_packages_tool <- Tool$new(
    name = "list_packages",
    description = "List currently loaded packages or search installed packages.",
    parameters = z_object(
      type = z_enum(
        c("loaded", "installed", "search"),
        description = "Type of listing: loaded (attached), installed (all), search (by pattern)"
      ),
      pattern = z_string("Search pattern (for type='search')")
    ),
    execute = function(args) {
      type <- args$type %||% "loaded"
      pattern <- args$pattern

      switch(type,
        "loaded" = {
          pkgs <- search()
          pkgs <- pkgs[grepl("^package:", pkgs)]
          pkgs <- sub("^package:", "", pkgs)
          paste(c("Loaded packages:", paste0("- ", pkgs)), collapse = "\n")
        },
        "installed" = {
          pkgs <- utils::installed.packages()[, "Package"]
          paste0("Installed packages: ", length(pkgs), " total\n",
                 "First 20: ", paste(head(pkgs, 20), collapse = ", "))
        },
        "search" = {
          if (is.null(pattern) || !nzchar(pattern)) {
            return("Error: Pattern required for search")
          }
          pkgs <- utils::installed.packages()[, "Package"]
          matches <- pkgs[grepl(pattern, pkgs, ignore.case = TRUE)]
          if (length(matches) == 0) {
            paste0("No packages matching '", pattern, "'")
          } else {
            paste(c(paste0("Packages matching '", pattern, "':"),
                   paste0("- ", matches)), collapse = "\n")
          }
        }
      )
    }
  )

  # Tool: Session info
  session_info_tool <- Tool$new(
    name = "session_info",
    description = "Get R session information including version and platform.",
    parameters = z_empty_object(description = "No parameters required"),
    execute = function(args) {
      info <- utils::sessionInfo()

      paste0(
        "R Version: ", info$R.version$version.string, "\n",
        "Platform: ", info$platform, "\n",
        "Running under: ", info$running, "\n",
        "Locale: ", paste(info$locale, collapse = ", "), "\n",
        "Base packages: ", paste(info$basePkgs, collapse = ", ")
      )
    }
  )

  # Tool: Check dependencies
  check_deps_tool <- Tool$new(
    name = "check_dependencies",
    description = "Check if all dependencies for a package are available.",
    parameters = z_object(
      package = z_string("Name of the package to check dependencies for")
    ),
    execute = function(args) {
      pkg <- args$package

      if (!requireNamespace(pkg, quietly = TRUE)) {
        return(paste0("Package '", pkg, "' is not installed."))
      }

      # Get dependencies
      deps <- tools::package_dependencies(pkg, recursive = TRUE)[[1]]

      if (is.null(deps) || length(deps) == 0) {
        return(paste0("Package '", pkg, "' has no dependencies."))
      }

      # Check each dependency
      missing <- character(0)
      for (dep in deps) {
        if (!requireNamespace(dep, quietly = TRUE)) {
          missing <- c(missing, dep)
        }
      }

      if (length(missing) == 0) {
        paste0("All ", length(deps), " dependencies for '", pkg, "' are installed.")
      } else {
        paste0(
          "Missing dependencies for '", pkg, "':\n",
          paste0("- ", missing, collapse = "\n")
        )
      }
    }
  )

  system_prompt <- paste0(
    "You are an R environment and package management specialist.\n\n",
    "Your capabilities:\n",
    "- Checking package installation status and versions\n",
    "- Loading packages into the session\n",
    if (allow_install) "- Installing packages from CRAN\n" else "",
    "- Listing loaded and installed packages\n",
    "- Checking package dependencies\n",
    "- Providing session information\n\n",
    "Guidelines:\n",
    "1. Always check if a package is installed before trying to load it\n",
    "2. Suggest alternatives if a package is not available\n",
    "3. Check dependencies before complex operations\n",
    "4. Report version information when relevant\n",
    if (!allow_install) "5. Package installation is DISABLED - suggest manual installation\n" else ""
  )

  Agent$new(
    name = name,
    description = paste0(
      "R environment and package management specialist. Can check, load, ",
      if (allow_install) "and install " else "",
      "packages, verify dependencies, and provide session information."
    ),
    system_prompt = system_prompt,
    tools = list(
      check_package_tool,
      load_package_tool,
      install_package_tool,
      list_packages_tool,
      session_info_tool,
      check_deps_tool
    )
  )
}

# Null-coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
# =============================================================================
# SkillArchitect: Advanced Skill Creator Specialist
# =============================================================================

#' @title Create a SkillArchitect Agent
#' @description
#' Creates an advanced agent specialized in creating, testing, and refining new skills.
#' It follows a rigorous "Ingest -> Design -> Implement -> Verify" workflow.
#'
#' @param name Agent name. Default "SkillArchitect".
#' @param registry Optional SkillRegistry object (defaults to creating one from inst/skills).
#' @param model The model object to use for verification (spawning a tester agent).
#' @return An Agent object configured for skill architecture.
#' @export
create_skill_architect_agent <- function(name = "SkillArchitect", registry = NULL, model = NULL) {
  # Initialize registry if not provided
  if (is.null(registry)) {
    registry <- create_skill_registry(system.file("skills", package = "aisdk"))
  }

  # Ensure the create_skill skill is available
  if (is.null(registry$get_skill("create_skill"))) {
    # If not found in package (e.g. during development), try local path
    local_skills <- file.path("inst", "skills")
    if (dir.exists(local_skills)) {
      registry <- create_skill_registry(local_skills)
    }
  }
  
  if (is.null(registry$get_skill("create_skill"))) {
     rlang::abort("The 'create_skill' skill is required but not found in the registry.")
  }

  # Get standard skill creation tools
  skill_tools <- create_skill_tools(registry)
  
  # Get Skill Forge tools (Analysis & Verification)
  # Requires model for the verification loop
  forge_tools <- list()
  if (!is.null(model)) {
    forge_tools <- create_skill_forge_tools(registry, model)
  } else {
    rlang::warn("No 'model' provided to SkillArchitect. Verification tools will be disabled.")
  }

  all_tools <- c(skill_tools, forge_tools)

  system_prompt <- paste0(
    "You are the Skill Architect, a senior AI engineer responsible for extending this system's capabilities.\n",
    "Your mission is to build robust, reusable 'Skills' that enable other agents to perform complex tasks.\n\n",
    
    "## Core Principles\n",
    "1. **Conciseness**: Don't maximize token usage. Be efficient.\n",
    "2. **Progressive Disclosure**: Detailed docs go into 'references/', core workflow in 'SKILL.md'.\n",
    "3. **Verification**: Never assume it works. Always VERIFY with a test run.\n\n",

    "## The Workflow\n",
    "1. **Ingest**: Learn the domain. Use `analyze_r_package` if wrapping a package.\n",
    "2. **Design**: Plan the skill structure. What inputs? What outputs? Draft the `SKILL.md` in your mind.\n",
    "3. **Implement**: Use the 'create_skill' tools:\n",
    "   - `create_structure.R`\n",
    "   - `write_skill_md.R`\n",
    "   - `write_script.R`\n",
    "4. **Verify**: Use `verify_skill` to spawn a tester. If it fails, iterate and fix.\n\n",

    "## Skill Structure\n",
    "- `SKILL.md`: YAML metadata + High-level instructions (Imperative mood).\n",
    "- `scripts/`: Self-contained R functions. Use `args` list for inputs.\n",
    "- `references/`: (Optional) Large documentation or tables.\n\n",

    "You have access to powerful tools. Use them wisely to build the next generation of capabilities."
  )
  
  Agent$new(
    name = name,
    description = "Senior Skill Architect. Designs, implements, and verifies new system capabilities.",
    system_prompt = system_prompt,
    tools = all_tools
  )
}
