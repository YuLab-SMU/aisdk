#' Artifact Tools for File Persistence
#' @keywords internal

create_artifact_dir <- function(base_dir = NULL) {
  if (is.null(base_dir) || !nzchar(base_dir)) {
    base_dir <- file.path(getwd(), "artifacts")
  }
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  dir <- file.path(base_dir, ts)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(dir, winslash = "/", mustWork = FALSE)
}

get_artifact_dir <- function(envir = NULL, fallback_dir = NULL) {
  dir <- NULL
  if (!is.null(envir) && exists(".artifact_dir", envir = envir, inherits = FALSE)) {
    dir <- get(".artifact_dir", envir = envir, inherits = FALSE)
  }
  if (is.null(dir) && !is.null(fallback_dir)) {
    dir <- fallback_dir
  }
  if (is.null(dir) || !nzchar(dir)) {
    dir <- create_artifact_dir()
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(dir, winslash = "/", mustWork = FALSE)
}

sanitize_filename <- function(name, default_ext = "txt") {
  if (is.null(name) || !nzchar(name)) {
    name <- paste0("artifact_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  name <- gsub("[^A-Za-z0-9._-]", "_", name)
  if (!grepl("\\.[A-Za-z0-9]+$", name)) {
    name <- paste0(name, ".", default_ext)
  }
  name
}

is_safe_subpath <- function(path) {
  if (is.null(path) || !nzchar(path)) return(TRUE)
  if (grepl("(^[A-Za-z]:)|(^/)|(^\\\\\\\\)", path)) return(FALSE)
  if (grepl("\\.\\.", path)) return(FALSE)
  TRUE
}

safe_join <- function(base_dir, subdir, filename) {
  if (!is_safe_subpath(subdir)) {
    rlang::abort("Invalid subdir path")
  }
  if (!is_safe_subpath(filename)) {
    rlang::abort("Invalid filename")
  }
  path <- if (!is.null(subdir) && nzchar(subdir)) {
    file.path(base_dir, subdir, filename)
  } else {
    file.path(base_dir, filename)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

create_artifact_tools <- function(default_dir = NULL) {
  save_text_tool <- Tool$new(
    name = "save_text_artifact",
    description = "Save a text report or notes to the artifact directory.",
    parameters = z_object(
      content = z_string("Text content to write"),
      filename = z_string("Optional filename (without path). Default auto-generated."),
      subdir = z_string("Optional subdirectory under artifact dir")
    ),
    execute = function(args) {
      env <- args$.envir
      dir <- get_artifact_dir(env, default_dir)
      filename <- sanitize_filename(args$filename, "txt")
      subdir <- args$subdir
      path <- safe_join(dir, subdir, filename)
      writeLines(args$content, path, useBytes = TRUE)
      paste0("Saved text artifact to ", path)
    }
  )

  save_rmd_tool <- Tool$new(
    name = "save_rmd_artifact",
    description = "Save an R Markdown report to the artifact directory.",
    parameters = z_object(
      content = z_string("R Markdown content to write"),
      filename = z_string("Optional filename (without path). Default auto-generated."),
      subdir = z_string("Optional subdirectory under artifact dir")
    ),
    execute = function(args) {
      env <- args$.envir
      dir <- get_artifact_dir(env, default_dir)
      filename <- sanitize_filename(args$filename, "Rmd")
      subdir <- args$subdir
      path <- safe_join(dir, subdir, filename)
      writeLines(args$content, path, useBytes = TRUE)
      paste0("Saved Rmd artifact to ", path)
    }
  )

  get_rmd_chunks_tool <- Tool$new(
    name = "get_rmd_chunks",
    description = "List and extract Rmd code chunks by label.",
    parameters = z_object(
      filename = z_string("Rmd filename in artifact directory")
    ),
    execute = function(args) {
      env <- args$.envir
      dir <- get_artifact_dir(env, default_dir)
      filename <- sanitize_filename(args$filename, "Rmd")
      path <- safe_join(dir, NULL, filename)

      if (!file.exists(path)) {
        return(paste0("Error: Rmd not found: ", path))
      }

      lines <- readLines(path, warn = FALSE)
      chunks <- list()
      in_chunk <- FALSE
      chunk_header <- NULL
      chunk_label <- NULL
      chunk_lines <- character(0)
      chunk_index <- 0

      parse_label <- function(header) {
        # header like ```{r label, opt=1}
        inner <- sub("^```\\{r\\s*", "", header)
        inner <- sub("\\}\\s*$", "", inner)
        inner <- trimws(inner)
        if (!nzchar(inner)) return(NULL)
        parts <- strsplit(inner, ",")[[1]]
        label <- trimws(parts[1])
        if (!nzchar(label)) return(NULL)
        label
      }

      for (line in lines) {
        if (!in_chunk && grepl("^```\\{r", line)) {
          in_chunk <- TRUE
          chunk_header <- line
          chunk_label <- parse_label(line)
          chunk_lines <- character(0)
          chunk_index <- chunk_index + 1
          next
        }
        if (in_chunk && grepl("^```\\s*$", line)) {
          label <- if (!is.null(chunk_label)) chunk_label else paste0("chunk_", chunk_index)
          chunks[[label]] <- list(
            label = label,
            header = chunk_header,
            code = paste(chunk_lines, collapse = "\n")
          )
          in_chunk <- FALSE
          chunk_header <- NULL
          chunk_label <- NULL
          chunk_lines <- character(0)
          next
        }
        if (in_chunk) {
          chunk_lines <- c(chunk_lines, line)
        }
      }

      if (length(chunks) == 0) {
        return("No Rmd chunks found.")
      }
      # Return compact summary with labels
      labels <- names(chunks)
      paste(c("Rmd chunks:", labels), collapse = "\n")
    }
  )

  update_rmd_chunk_tool <- Tool$new(
    name = "update_rmd_chunk",
    description = "Update an existing Rmd code chunk by label.",
    parameters = z_object(
      filename = z_string("Rmd filename in artifact directory"),
      label = z_string("Chunk label to update"),
      code = z_string("New R code for the chunk"),
      options = z_string("Optional chunk options (e.g. 'echo=FALSE'). Leave empty to keep.")
    ),
    execute = function(args) {
      env <- args$.envir
      dir <- get_artifact_dir(env, default_dir)
      filename <- sanitize_filename(args$filename, "Rmd")
      path <- safe_join(dir, NULL, filename)

      if (!file.exists(path)) {
        return(paste0("Error: Rmd not found: ", path))
      }

      lines <- readLines(path, warn = FALSE)
      out <- character(0)
      in_chunk <- FALSE
      updated <- FALSE
      current_label <- NULL

      parse_label <- function(header) {
        inner <- sub("^```\\{r\\s*", "", header)
        inner <- sub("\\}\\s*$", "", inner)
        inner <- trimws(inner)
        if (!nzchar(inner)) return(NULL)
        parts <- strsplit(inner, ",")[[1]]
        label <- trimws(parts[1])
        if (!nzchar(label)) return(NULL)
        label
      }

      build_header <- function(label, options, original) {
        if (!is.null(options) && nzchar(options)) {
          return(paste0("```{r ", label, ", ", options, "}"))
        }
        original
      }

      i <- 1
      while (i <= length(lines)) {
        line <- lines[i]
        if (!in_chunk && grepl("^```\\{r", line)) {
          in_chunk <- TRUE
          current_label <- parse_label(line)
          if (!is.null(current_label) && identical(current_label, args$label)) {
            header <- build_header(current_label, args$options, line)
            out <- c(out, header)
            # skip existing chunk code until end
            i <- i + 1
            while (i <= length(lines) && !grepl("^```\\s*$", lines[i])) {
              i <- i + 1
            }
            # insert new code and closing fence
            out <- c(out, args$code, "```")
            updated <- TRUE
            in_chunk <- FALSE
            current_label <- NULL
            i <- i + 1
            next
          }
          # not target chunk
          out <- c(out, line)
          i <- i + 1
          next
        }
        if (in_chunk && grepl("^```\\s*$", line)) {
          in_chunk <- FALSE
          current_label <- NULL
        }
        out <- c(out, line)
        i <- i + 1
      }

      if (!updated) {
        return(paste0("Error: chunk '", args$label, "' not found."))
      }

      writeLines(out, path, useBytes = TRUE)
      paste0("Updated chunk '", args$label, "' in ", path)
    }
  )

  append_rmd_chunk_tool <- Tool$new(
    name = "append_rmd_chunk",
    description = "Append a new R code chunk to an Rmd file.",
    parameters = z_object(
      filename = z_string("Rmd filename in artifact directory"),
      label = z_string("Chunk label to append"),
      code = z_string("R code for the chunk"),
      options = z_string("Optional chunk options (e.g. 'echo=FALSE')"),
      after_heading = z_string("Optional heading text to insert after")
    ),
    execute = function(args) {
      env <- args$.envir
      dir <- get_artifact_dir(env, default_dir)
      filename <- sanitize_filename(args$filename, "Rmd")
      path <- safe_join(dir, NULL, filename)

      if (!file.exists(path)) {
        return(paste0("Error: Rmd not found: ", path))
      }

      lines <- readLines(path, warn = FALSE)
      header <- if (!is.null(args$options) && nzchar(args$options)) {
        paste0("```{r ", args$label, ", ", args$options, "}")
      } else {
        paste0("```{r ", args$label, "}")
      }
      chunk_lines <- c(header, args$code, "```")

      if (!is.null(args$after_heading) && nzchar(args$after_heading)) {
        idx <- grep(paste0("^#+\\s+", gsub("([\\^\\$\\.|\\(\\)\\[\\]\\*\\+\\?\\\\])", "\\\\\\1", args$after_heading)),
                    lines)
        if (length(idx) > 0) {
          insert_at <- idx[1]
          lines <- c(lines[1:insert_at], "", chunk_lines, "", lines[(insert_at + 1):length(lines)])
          writeLines(lines, path, useBytes = TRUE)
          return(paste0("Appended chunk '", args$label, "' after heading in ", path))
        }
      }

      lines <- c(lines, "", chunk_lines)
      writeLines(lines, path, useBytes = TRUE)
      paste0("Appended chunk '", args$label, "' to ", path)
    }
  )

  append_rmd_text_tool <- Tool$new(
    name = "append_rmd_text",
    description = "Append markdown text to an Rmd file.",
    parameters = z_object(
      filename = z_string("Rmd filename in artifact directory"),
      text = z_string("Markdown text to append"),
      after_heading = z_string("Optional heading text to insert after")
    ),
    execute = function(args) {
      env <- args$.envir
      dir <- get_artifact_dir(env, default_dir)
      filename <- sanitize_filename(args$filename, "Rmd")
      path <- safe_join(dir, NULL, filename)

      if (!file.exists(path)) {
        return(paste0("Error: Rmd not found: ", path))
      }

      lines <- readLines(path, warn = FALSE)
      text_lines <- unlist(strsplit(args$text, "\n", fixed = TRUE))

      if (!is.null(args$after_heading) && nzchar(args$after_heading)) {
        idx <- grep(paste0("^#+\\s+", gsub("([\\^\\$\\.|\\(\\)\\[\\]\\*\\+\\?\\\\])", "\\\\\\1", args$after_heading)),
                    lines)
        if (length(idx) > 0) {
          insert_at <- idx[1]
          lines <- c(lines[1:insert_at], "", text_lines, "", lines[(insert_at + 1):length(lines)])
          writeLines(lines, path, useBytes = TRUE)
          return(paste0("Appended text after heading in ", path))
        }
      }

      lines <- c(lines, "", text_lines)
      writeLines(lines, path, useBytes = TRUE)
      paste0("Appended text to ", path)
    }
  )

  run_rmd_chunk_tool <- Tool$new(
    name = "run_rmd_chunk",
    description = "Execute a labeled Rmd chunk in the session environment for debugging.",
    parameters = z_object(
      filename = z_string("Rmd filename in artifact directory"),
      label = z_string("Chunk label to execute")
    ),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) return("Error: No session environment available.")
      dir <- get_artifact_dir(env, default_dir)
      filename <- sanitize_filename(args$filename, "Rmd")
      path <- safe_join(dir, NULL, filename)

      if (!file.exists(path)) {
        return(paste0("Error: Rmd not found: ", path))
      }

      lines <- readLines(path, warn = FALSE)
      in_chunk <- FALSE
      current_label <- NULL
      chunk_lines <- character(0)

      parse_label <- function(header) {
        inner <- sub("^```\\{r\\s*", "", header)
        inner <- sub("\\}\\s*$", "", inner)
        inner <- trimws(inner)
        if (!nzchar(inner)) return(NULL)
        parts <- strsplit(inner, ",")[[1]]
        label <- trimws(parts[1])
        if (!nzchar(label)) return(NULL)
        label
      }

      for (line in lines) {
        if (!in_chunk && grepl("^```\\{r", line)) {
          in_chunk <- TRUE
          current_label <- parse_label(line)
          chunk_lines <- character(0)
          next
        }
        if (in_chunk && grepl("^```\\s*$", line)) {
          if (!is.null(current_label) && identical(current_label, args$label)) {
            code <- paste(chunk_lines, collapse = "\n")
            # execute in session env
            output <- tryCatch({
              utils::capture.output(eval(parse(text = code), envir = env))
            }, error = function(e) {
              paste0("Error: ", conditionMessage(e))
            })
            return(paste(c("Chunk output:", output), collapse = "\n"))
          }
          in_chunk <- FALSE
          current_label <- NULL
          next
        }
        if (in_chunk) {
          chunk_lines <- c(chunk_lines, line)
        }
      }

      paste0("Error: chunk '", args$label, "' not found in ", path)
    }
  )

  render_rmd_tool <- Tool$new(
    name = "render_rmd_artifact",
    description = "Render an Rmd file from the artifact directory.",
    parameters = z_object(
      filename = z_string("Rmd filename in artifact directory"),
      output_format = z_enum(c("html_document", "pdf_document", "word_document"),
                             description = "Output format"),
      output_file = z_string("Optional output filename (without path).")
    ),
    execute = function(args) {
      env <- args$.envir
      dir <- get_artifact_dir(env, default_dir)
      filename <- sanitize_filename(args$filename, "Rmd")
      input_path <- safe_join(dir, NULL, filename)

      if (!file.exists(input_path)) {
        return(paste0("Error: Rmd not found: ", input_path))
      }
      if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        return("Error: rmarkdown package is not installed.")
      }

      output_format <- args$output_format %||% "html_document"
      output_file <- args$output_file
      if (!is.null(output_file) && nzchar(output_file)) {
        default_ext <- if (identical(output_format, "pdf_document")) {
          "pdf"
        } else if (identical(output_format, "word_document")) {
          "docx"
        } else {
          "html"
        }
        output_file <- sanitize_filename(output_file, default_ext)
      }

      result <- tryCatch({
        out <- rmarkdown::render(
          input = input_path,
          output_format = output_format,
          output_dir = dir,
          output_file = output_file,
          quiet = TRUE
        )
        paste0("Rendered Rmd to ", out)
      }, error = function(e) {
        paste0("Error rendering Rmd: ", conditionMessage(e))
      })

      result
    }
  )

  save_plot_tool <- Tool$new(
    name = "save_plot_artifact",
    description = "Save a ggplot object from the session to the artifact directory.",
    parameters = z_object(
      plot_var = z_string("Name of the ggplot object in session"),
      filename = z_string("Optional filename (without path). Default auto-generated."),
      width = z_number("Plot width in inches (default 8)"),
      height = z_number("Plot height in inches (default 6)"),
      dpi = z_integer("DPI for saved plot (default 300)"),
      subdir = z_string("Optional subdirectory under artifact dir")
    ),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) return("Error: No session environment available.")
      if (!exists(args$plot_var, envir = env, inherits = FALSE)) {
        return(paste0("Error: plot variable '", args$plot_var, "' not found."))
      }
      plot_obj <- get(args$plot_var, envir = env, inherits = FALSE)
      if (!inherits(plot_obj, "ggplot")) {
        return("Error: plot_var is not a ggplot object.")
      }
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        return("Error: ggplot2 package is required.")
      }

      dir <- get_artifact_dir(env, default_dir)
      filename <- sanitize_filename(args$filename, "png")
      subdir <- args$subdir
      path <- safe_join(dir, subdir, filename)
      width <- args$width %||% 8
      height <- args$height %||% 6
      dpi <- args$dpi %||% 300

      ggplot2::ggsave(path, plot_obj, width = width, height = height, dpi = dpi)
      paste0("Saved plot artifact to ", path)
    }
  )

  save_data_tool <- Tool$new(
    name = "save_data_artifact",
    description = "Save a data frame or object from session to the artifact directory.",
    parameters = z_object(
      var_name = z_string("Name of the variable to save"),
      filename = z_string("Optional filename (without path). Default auto-generated."),
      format = z_enum(c("csv", "tsv", "json", "rds"), description = "Output format"),
      subdir = z_string("Optional subdirectory under artifact dir")
    ),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) return("Error: No session environment available.")
      if (!exists(args$var_name, envir = env, inherits = FALSE)) {
        return(paste0("Error: variable '", args$var_name, "' not found."))
      }
      data <- get(args$var_name, envir = env, inherits = FALSE)
      dir <- get_artifact_dir(env, default_dir)

      format <- args$format %||% "csv"
      filename <- args$filename
      if (is.null(filename) || !nzchar(filename)) {
        filename <- paste0(args$var_name, ".", format)
      }
      filename <- sanitize_filename(filename, format)
      subdir <- args$subdir
      path <- safe_join(dir, subdir, filename)

      result <- tryCatch({
        switch(format,
          "csv" = utils::write.csv(data, path, row.names = FALSE),
          "tsv" = utils::write.table(data, path, sep = "\t", row.names = FALSE),
          "json" = jsonlite::write_json(data, path, auto_unbox = TRUE, pretty = TRUE),
          "rds" = saveRDS(data, path),
          stop(paste0("Unsupported format: ", format))
        )
        paste0("Saved data artifact to ", path)
      }, error = function(e) {
        paste0("Error saving data: ", conditionMessage(e))
      })

      result
    }
  )

  list_artifacts_tool <- Tool$new(
    name = "list_artifacts",
    description = "List files in the artifact directory.",
    parameters = z_object(
      subdir = z_string("Optional subdirectory under artifact dir")
    ),
    execute = function(args) {
      env <- args$.envir
      dir <- get_artifact_dir(env, default_dir)
      subdir <- args$subdir
      target <- if (!is.null(subdir) && nzchar(subdir)) file.path(dir, subdir) else dir
      if (!dir.exists(target)) {
        return(paste0("No artifact directory: ", target))
      }
      files <- list.files(target, recursive = TRUE, full.names = FALSE)
      if (length(files) == 0) {
        return("No artifacts found.")
      }
      paste(c("Artifacts:", files), collapse = "\n")
    }
  )

  list(save_text_tool, save_rmd_tool, render_rmd_tool,
       get_rmd_chunks_tool, update_rmd_chunk_tool, append_rmd_chunk_tool,
       append_rmd_text_tool, run_rmd_chunk_tool,
       save_plot_tool, save_data_tool, list_artifacts_tool)
}
