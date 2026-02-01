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
#' - Manus architecture (atomic tools → virtual computer)
#' - Claude Code pattern (small set of tools → OS layer)
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
    #' @param working_dir Working directory (default: current directory)
    #' @param sandbox_mode Sandbox mode: "strict", "permissive", or "none"
    initialize = function(working_dir = getwd(), sandbox_mode = "permissive") {
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
      result <- tryCatch({
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
          error = proc$status != 0
        )
      }, error = function(e) {
        list(
          stdout = "",
          stderr = conditionMessage(e),
          exit_code = 1,
          error = TRUE
        )
      })

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
      tryCatch({
        content <- paste(readLines(full_path, encoding = encoding, warn = FALSE), collapse = "\n")
        list(
          content = content,
          error = FALSE,
          path = full_path
        )
      }, error = function(e) {
        list(
          content = NULL,
          error = TRUE,
          message = conditionMessage(e)
        )
      })
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
      tryCatch({
        writeLines(content, full_path, useBytes = TRUE)
        list(
          success = TRUE,
          error = FALSE,
          path = full_path
        )
      }, error = function(e) {
        list(
          success = FALSE,
          error = TRUE,
          message = conditionMessage(e)
        )
      })
    },

    #' @description
    #' Execute R code
    #' @param code R code to execute
    #' @param timeout_ms Timeout in milliseconds (default: 30000)
    #' @param capture_output Whether to capture output (default: TRUE)
    #' @return List with result, output, error
    execute_r_code = function(code, timeout_ms = 30000, capture_output = TRUE) {
      # Log execution
      private$log_execution("execute_r_code", list(code_length = nchar(code)))

      # Check sandbox restrictions
      if (self$sandbox_mode == "strict") {
        violation <- private$check_code_violation(code)
        if (!is.null(violation)) {
          return(list(
            result = NULL,
            output = "",
            error = TRUE,
            message = paste("Sandbox violation:", violation)
          ))
        }
      }

      # Execute in isolated process
      result <- tryCatch({
        callr::r(
          function(code_str, wd) {
            setwd(wd)
            eval(parse(text = code_str), envir = globalenv())
          },
          args = list(code_str = code, wd = self$working_dir),
          timeout = timeout_ms / 1000,
          show = capture_output,
          error = "stack"
        )
      }, error = function(e) {
        return(list(
          result = NULL,
          output = "",
          error = TRUE,
          message = conditionMessage(e)
        ))
      })

      # Format result
      if (is.list(result) && !is.null(result$error)) {
        result
      } else {
        list(
          result = result,
          output = capture.output(print(result)),
          error = FALSE
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

    #' Check bash command for sandbox violations
    check_bash_violation = function(command) {
      # Strict mode: block dangerous commands
      dangerous_patterns <- c(
        "rm -rf /",
        "dd if=",
        "mkfs",
        ":(){ :|:& };:",  # Fork bomb
        "curl.*\\|.*bash",  # Pipe to bash
        "wget.*\\|.*bash"
      )

      for (pattern in dangerous_patterns) {
        if (grepl(pattern, command, ignore.case = TRUE)) {
          return(paste("Dangerous command pattern:", pattern))
        }
      }

      NULL
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
#' @param working_dir Working directory (default: current directory)
#' @param sandbox_mode Sandbox mode: "strict", "permissive", or "none"
#' @return List of Tool objects
#' @export
create_computer_tools <- function(computer = NULL, working_dir = getwd(), sandbox_mode = "permissive") {
  if (is.null(computer)) {
    computer <- Computer$new(working_dir = working_dir, sandbox_mode = sandbox_mode)
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
          result$stdout
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
          paste("Successfully wrote to:", result$path)
        }
      },
      layer = "computer"
    ),

    # Execute R code tool
    tool(
      name = "execute_r_code",
      description = paste(
        "Execute R code in an isolated process.",
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
          paste("Result:", paste(result$output, collapse = "\n"))
        }
      },
      layer = "computer"
    )
  )
}
