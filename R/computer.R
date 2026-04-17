#' @title Computer Abstraction Layer
#' @description
#' Implements a hierarchical action space following the 2026 agent design pattern.
#' Provides atomic tools (bash, read_file, write_file, execute_r_code) that push
#' complex actions to the computer layer instead of loading many tool definitions
#' into context.
#'
#' This reduces context window usage by 30-50% and provides more flexible action
#' space where agents can use any bash command, CLI, or script.
#'
#' @references
#' - Manus architecture (atomic tools -> virtual computer)
#' - Claude Code pattern (small set of tools -> OS layer)
#' - CodeAct paper (chain actions via code execution)
#'
#' @name computer
NULL

#' @title Computer Class
#' @description
#' R6 class providing computer abstraction with atomic tools for file operations,
#' bash execution, and R code execution.
#'
#' @export
Computer <- R6::R6Class("Computer",
  public = list(
    #' @field working_dir Current working directory
    working_dir = NULL,

    #' @field sandbox_mode Sandbox mode: "strict", "permissive", or "none"
    sandbox_mode = "permissive",

    #' @field execution_log Log of executed commands
    execution_log = NULL,

    #' @description
    #' Initialize computer abstraction
    #' @param working_dir Working directory. Defaults to `tempdir()`.
    #' @param sandbox_mode Sandbox mode: "strict", "permissive", or "none"
    initialize = function(working_dir = tempdir(), sandbox_mode = "permissive") {
      self$working_dir <- normalizePath(working_dir, mustWork = FALSE)
      self$sandbox_mode <- sandbox_mode
      self$execution_log <- list()

      # Create working directory if it doesn't exist
      if (!dir.exists(self$working_dir)) {
        dir.create(self$working_dir, recursive = TRUE)
      }
    },

    #' @description
    #' Execute bash command
    #' @param command Bash command to execute
    #' @param timeout_ms Timeout in milliseconds (default: 30000)
    #' @param capture_output Whether to capture output (default: TRUE)
    #' @return List with stdout, stderr, exit_code
    bash = function(command, timeout_ms = 30000, capture_output = TRUE) {
      # Log execution
      private$log_execution("bash", list(command = command))
      before_files <- private$list_files_snapshot()

      # Check sandbox restrictions
      if (self$sandbox_mode == "strict") {
        violation <- private$check_bash_violation(command)
        if (!is.null(violation)) {
          return(list(
            stdout = "",
            stderr = paste("Sandbox violation:", violation),
            exit_code = 1,
            error = TRUE
          ))
        }
      }

      # Execute command
      result <- tryCatch(
        {
          proc <- processx::run(
            "bash",
            args = c("-c", command),
            wd = self$working_dir,
            timeout = timeout_ms / 1000,
            error_on_status = FALSE,
            stdout = if (capture_output) "|" else NULL,
            stderr = if (capture_output) "|" else NULL
          )

          list(
            stdout = proc$stdout,
            stderr = proc$stderr,
            exit_code = proc$status,
            error = proc$status != 0,
            created_files = private$diff_created_files(before_files)
          )
        },
        error = function(e) {
          list(
            stdout = "",
            stderr = conditionMessage(e),
            exit_code = 1,
            error = TRUE,
            created_files = character(0)
          )
        }
      )

      result
    },

    #' @description
    #' Read file contents
    #' @param path File path (relative to working_dir or absolute)
    #' @param encoding File encoding (default: "UTF-8")
    #' @return File contents as character string
    read_file = function(path, encoding = "UTF-8") {
      # Log execution
      private$log_execution("read_file", list(path = path))

      # Resolve path
      full_path <- private$resolve_path(path)

      # Check if file exists
      if (!file.exists(full_path)) {
        return(list(
          content = NULL,
          error = TRUE,
          message = paste("File not found:", path)
        ))
      }

      # Read file
      tryCatch(
        {
          content <- paste(readLines(full_path, encoding = encoding, warn = FALSE), collapse = "\n")
          list(
            content = content,
            error = FALSE,
            path = full_path
          )
        },
        error = function(e) {
          list(
            content = NULL,
            error = TRUE,
            message = conditionMessage(e)
          )
        }
      )
    },

    #' @description
    #' Write file contents
    #' @param path File path (relative to working_dir or absolute)
    #' @param content Content to write
    #' @param encoding File encoding (default: "UTF-8")
    #' @return Success status
    write_file = function(path, content, encoding = "UTF-8") {
      # Log execution
      private$log_execution("write_file", list(path = path, size = nchar(content)))

      # Resolve path
      full_path <- private$resolve_path(path)

      # Check sandbox restrictions
      if (self$sandbox_mode == "strict") {
        violation <- private$check_write_violation(full_path)
        if (!is.null(violation)) {
          return(list(
            success = FALSE,
            error = TRUE,
            message = paste("Sandbox violation:", violation)
          ))
        }
      }

      # Create parent directory if needed
      parent_dir <- dirname(full_path)
      if (!dir.exists(parent_dir)) {
        dir.create(parent_dir, recursive = TRUE)
      }

      # Write file
      tryCatch(
        {
          writeLines(content, full_path, useBytes = TRUE)
          list(
            success = TRUE,
            error = FALSE,
            path = full_path,
            created_files = list(full_path)
          )
        },
        error = function(e) {
          list(
            success = FALSE,
            error = TRUE,
            message = conditionMessage(e)
          )
        }
      )
    },

    #' @description
    #' Execute R code in an isolated `callr` process
    #' @param code R code to execute
    #' @param timeout_ms Timeout in milliseconds (default: 30000)
    #' @param capture_output Whether to capture output (default: TRUE)
    #' @return List with result, output, error, and `execution_mode`.
    #'   `execution_mode` is always `"sandbox_exec"` for this computer-layer path,
    #'   which does not persist values into a live `ChatSession$get_envir()`.
    execute_r_code = function(code, timeout_ms = 30000, capture_output = TRUE) {
      # Log execution
      private$log_execution("execute_r_code", list(code_length = nchar(code)))
      before_files <- private$list_files_snapshot()

      # Check sandbox restrictions
      if (self$sandbox_mode == "strict") {
        violation <- private$check_code_violation(code)
        if (!is.null(violation)) {
          return(list(
            result = NULL,
            output = "",
            error = TRUE,
            message = paste("Sandbox violation:", violation),
            execution_mode = "sandbox_exec"
          ))
        }
      }

      # Execute in isolated process
      result <- tryCatch(
        {
          callr::r(
            function(code_str, wd, before_files) {
              setwd(wd)
              state <- new.env(parent = emptyenv())
              state$messages <- character(0)
              state$warnings <- character(0)
              value <- NULL
              visible <- FALSE

              output <- utils::capture.output(
                withCallingHandlers(
                  {
                    evaluated <- withVisible(eval(parse(text = code_str), envir = globalenv()))
                    value <- evaluated$value
                    visible <- isTRUE(evaluated$visible)
                    if (isTRUE(evaluated$visible)) {
                      print(evaluated$value)
                    }
                  },
                  message = function(m) {
                    state$messages <- c(state$messages, trimws(conditionMessage(m)))
                    invokeRestart("muffleMessage")
                  },
                  warning = function(w) {
                    state$warnings <- c(state$warnings, trimws(conditionMessage(w)))
                    invokeRestart("muffleWarning")
                  }
                ),
                type = "output"
              )

              list(
                result = value,
                visible = visible,
                output = output,
                messages = state$messages,
                warnings = state$warnings,
                created_files = setdiff(
                  {
                    files <- list.files(wd, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
                    files[file.exists(files) & !dir.exists(files)]
                  },
                  before_files %||% character(0)
                )
              )
            },
            args = list(
              code_str = code,
              wd = self$working_dir,
              before_files = before_files
            ),
            timeout = timeout_ms / 1000,
            show = FALSE,
            error = "stack"
          )
        },
        error = function(e) {
          return(list(
            result = NULL,
            output = "",
            error = TRUE,
            message = conditionMessage(e),
            execution_mode = "sandbox_exec"
          ))
        }
      )

      # Format result
      if (is.list(result) && !is.null(result$error)) {
        result
      } else {
        output_lines <- if (isTRUE(capture_output)) {
          c(
            if (!is.null(result$output)) result$output else character(0),
            if (length(result$messages)) paste0("Message: ", result$messages) else character(0),
            if (length(result$warnings)) paste0("Warning: ", result$warnings) else character(0)
          )
        } else {
          character(0)
        }

        if (!length(output_lines)) {
          output_lines <- "(Code executed successfully with no printed output)"
        }

        list(
          result = result$result,
          output = output_lines,
          error = FALSE,
          messages = result$messages %||% character(0),
          warnings = result$warnings %||% character(0),
          created_files = normalizePath(result$created_files %||% character(0), winslash = "/", mustWork = FALSE),
          execution_mode = "sandbox_exec"
        )
      }
    },

    #' @description
    #' Get execution log
    #' @return List of logged executions
    get_log = function() {
      self$execution_log
    },

    #' @description
    #' Clear execution log
    clear_log = function() {
      self$execution_log <- list()
      invisible(self)
    }
  ),
  private = list(
    list_files_snapshot = function() {
      if (!dir.exists(self$working_dir)) {
        return(character(0))
      }
      files <- list.files(self$working_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
      files[file.exists(files) & !dir.exists(files)]
    },

    diff_created_files = function(before_files) {
      created <- setdiff(private$list_files_snapshot(), before_files %||% character(0))
      if (length(created) == 0) {
        return(character(0))
      }
      normalizePath(created, winslash = "/", mustWork = FALSE)
    },

    #' Check bash command for sandbox violations
    check_bash_violation = function(command) {
      # Strict mode: block dangerous commands
      dangerous_patterns <- c(
        "rm -rf /",
        "dd if=",
        "mkfs",
        ":\\(\\)\\{\\s*:|:&\\s*\\};:", # Fork bomb (escaped for TRE)
        "curl.*\\|.*bash", # Pipe to bash
        "wget.*\\|.*bash"
      )

      for (pattern in dangerous_patterns) {
        if (grepl(pattern, command, ignore.case = TRUE)) {
          return(paste("Dangerous command pattern:", pattern))
        }
      }

      NULL
    },

    #' Log execution
    log_execution = function(operation, details) {
      entry <- list(
        timestamp = Sys.time(),
        operation = operation,
        details = details
      )
      self$execution_log <- c(self$execution_log, list(entry))
    },

    #' Resolve path (relative to working_dir or absolute)
    resolve_path = function(path) {
      if (grepl("^/|^[A-Za-z]:", path)) {
        # Absolute path
        normalizePath(path, mustWork = FALSE)
      } else {
        # Relative path
        normalizePath(file.path(self$working_dir, path), mustWork = FALSE)
      }
    },

    #' Check write path for sandbox violations
    check_write_violation = function(path) {
      # Strict mode: only allow writes within working_dir
      if (!startsWith(path, self$working_dir)) {
        return("Write outside working directory not allowed")
      }

      NULL
    },

    #' Check R code for sandbox violations
    check_code_violation = function(code) {
      # Strict mode: block dangerous R functions
      dangerous_patterns <- c(
        "system\\(",
        "system2\\(",
        "unlink\\(.*, recursive\\s*=\\s*TRUE",
        "file.remove\\(",
        "Sys.setenv\\("
      )

      for (pattern in dangerous_patterns) {
        if (grepl(pattern, code)) {
          return(paste("Dangerous code pattern:", pattern))
        }
      }

      NULL
    }
  )
)

#' @title Create Computer Tools
#' @description
#' Create atomic tools for computer abstraction layer. These tools provide
#' a small set of primitives that agents can use to perform complex actions.
#'
#' @param computer Computer instance (default: create new)
#' @param working_dir Working directory. Defaults to `tempdir()`.
#' @param sandbox_mode Sandbox mode: "strict", "permissive", or "none"
#' @return List of Tool objects
#' @export
create_computer_tools <- function(computer = NULL, working_dir = tempdir(), sandbox_mode = "permissive") {
  if (is.null(computer)) {
    computer <- Computer$new(working_dir = working_dir, sandbox_mode = sandbox_mode)
  }

  annotate_artifacts <- function(text, paths = character(0)) {
    out <- text
    if (length(paths) > 0) {
      attr(out, "aisdk_artifacts") <- lapply(paths, function(path) {
        list(path = normalizePath(path, winslash = "/", mustWork = FALSE))
      })
    }
    out
  }

  list(
    # Bash tool
    tool(
      name = "bash",
      description = paste(
        "Execute a bash command in the working directory.",
        "Use this to run shell utilities, CLIs, scripts, or any command-line tool.",
        "Returns stdout, stderr, and exit code."
      ),
      parameters = z_object(
        command = z_string("The bash command to execute")
      ),
      execute = function(command) {
        result <- computer$bash(command)
        if (result$error) {
          paste("Error (exit code", result$exit_code, "):\n", result$stderr)
        } else {
          annotate_artifacts(result$stdout, result$created_files %||% character(0))
        }
      },
      layer = "computer"
    ),

    # Read file tool
    tool(
      name = "read_file",
      description = paste(
        "Read the contents of a file.",
        "Path can be relative to working directory or absolute.",
        "Returns file contents as text."
      ),
      parameters = z_object(
        path = z_string("Path to the file to read")
      ),
      execute = function(path) {
        result <- computer$read_file(path)
        if (result$error) {
          result$message
        } else {
          result$content
        }
      },
      layer = "computer"
    ),

    # Write file tool
    tool(
      name = "write_file",
      description = paste(
        "Write content to a file.",
        "Path can be relative to working directory or absolute.",
        "Creates parent directories if needed."
      ),
      parameters = z_object(
        path = z_string("Path to the file to write"),
        content = z_string("Content to write to the file")
      ),
      execute = function(path, content) {
        result <- computer$write_file(path, content)
        if (result$error) {
          result$message
        } else {
          annotate_artifacts(
            paste("Successfully wrote to:", result$path),
            result$created_files %||% character(0)
          )
        }
      },
      layer = "computer"
    ),

    # Execute R code tool
    tool(
      name = "execute_r_code",
      description = paste(
        "Execute R code in an isolated process.",
        "This always runs as sandbox_exec and does not mutate a live ChatSession environment.",
        "Use this to run data analysis, create plots, or perform computations.",
        "Returns the result and any output."
      ),
      parameters = z_object(
        code = z_string("R code to execute")
      ),
      execute = function(code) {
        result <- computer$execute_r_code(code)
        if (result$error) {
          paste("Error:", result$message)
        } else {
          annotate_artifacts(
            structure(
            paste("Result:", paste(result$output, collapse = "\n")),
            aisdk_messages = result$messages %||% character(0),
            aisdk_warnings = result$warnings %||% character(0)
            ),
            result$created_files %||% character(0)
          )
        }
      },
      layer = "computer"
    )
  )
}
