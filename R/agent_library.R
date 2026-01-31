#' @title Agent Library: Built-in Agent Specialists
#' @description
#' Factory functions for creating standard library agents for common tasks.
#' These agents are pre-configured with appropriate system prompts and tools
#' for their respective specializations.
#' @name agent_library
NULL

# =============================================================================
# CoderAgent: R Code Execution Specialist (Enhanced)
# =============================================================================

#' @title Create a CoderAgent
#' @description
#' Creates an agent specialized in writing and executing R code. The agent can
#' execute R code in the session environment, making results available to other
#' agents. Enhanced version with better safety controls and debugging support.
#'
#' @param name Agent name. Default "CoderAgent".
#' @param safe_mode If TRUE (default), restricts file system and network access.
#' @param timeout_ms Code execution timeout in milliseconds. Default 30000.
#' @param max_output_lines Maximum output lines to return. Default 50.
#' @return An Agent object configured for R code execution.
#' @export
#' @examples
#' \dontrun{
#' coder <- create_coder_agent()
#' session <- create_shared_session(model = "openai:gpt-4o")
#' result <- coder$run(
#'   "Create a data frame with 3 rows and calculate the mean",
#'   session = session,
#'   model = "openai:gpt-4o"
#' )
#' }
create_coder_agent <- function(name = "CoderAgent",
                                safe_mode = TRUE,
                                timeout_ms = 30000,
                                max_output_lines = 200) {

  # Enhanced safety patterns
  get_blocked_patterns <- function(mode) {
    # Always blocked (critical security)
    critical <- c(
      "system\\s*\\(",
      "system2\\s*\\(",
      "shell\\s*\\(",
      "Sys\\.setenv",
      "\\bq\\s*\\(\\s*\\)",
      "\\bquit\\s*\\(",
      "<<-",
      "rm\\s*\\(\\s*list\\s*=\\s*ls\\s*\\(",
      "eval\\s*\\(\\s*parse\\s*\\(\\s*text\\s*=.*\\$"
    )

    if (mode == "strict") {
      # Additional patterns blocked in strict mode
      strict <- c(
        "file\\.remove\\s*\\(",
        "unlink\\s*\\(",
        "file\\.rename\\s*\\(",
        "download\\.file\\s*\\(",
        "source\\s*\\(",
        "writeLines\\s*\\(",
        "write\\.csv\\s*\\(",
        "write\\.table\\s*\\(",
        "saveRDS\\s*\\(",
        "save\\s*\\(",
        "httr::",
        "curl::",
        "RCurl::",
        "url\\s*\\("
      )
      c(critical, strict)
    } else {
      critical
    }
  }

  # Tool: Execute R code with enhanced safety
  execute_tool <- Tool$new(
    name = "execute_r_code",
    description = paste0(
      "Execute R code in the session environment. ",
      "Use this to create variables, compute values, manipulate data, etc. ",
      "The code is executed in the shared session environment, so results ",
      "are available to other agents. ",
      if (safe_mode) "Safe mode is enabled: file I/O and network operations are restricted." else ""
    ),
    parameters = z_object(
      code = z_string("R code to execute. Should be valid R syntax."),
      description = z_string("Brief description of what this code does (for logging)")
    ),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) {
        return("Error: No session environment available. Run with a session.")
      }

      code <- args$code
      desc <- args$description %||% "code execution"

      # Check for blocked patterns
      if (safe_mode) {
        patterns <- get_blocked_patterns("strict")
        for (pattern in patterns) {
          if (grepl(pattern, code, ignore.case = TRUE)) {
            return(paste0(
              "Error: Blocked operation detected. Pattern '", pattern,
              "' is not allowed in safe mode. ",
              "If you need this operation, ask the user to use FileAgent or EnvAgent."
            ))
          }
        }
      }

      # Execute with error handling and output capture
      start_time <- Sys.time()
      result <- tryCatch({
        # Parse first to catch syntax errors
        parsed <- tryCatch({
          parse(text = code)
        }, error = function(e) {
          return(list(
            success = FALSE,
            error = paste0("Syntax error: ", conditionMessage(e))
          ))
        })

        if (is.list(parsed) && !is.null(parsed$error)) {
          return(parsed$error)
        }

        # Capture output
        output <- utils::capture.output({
          value <- eval(parsed, envir = env)
        })

        # Truncate output if too long
        if (length(output) > max_output_lines) {
          output <- c(
            output[1:max_output_lines],
            paste0("... (", length(output) - max_output_lines, " more lines truncated)")
          )
        }

        # Build result message
        msg <- character(0)

        # Add output
        if (length(output) > 0 && any(nzchar(output))) {
          msg <- c(msg, "Output:", paste(output, collapse = "\n"))
        }

        # Add value summary
        if (!is.null(value) && !inherits(value, "environment")) {
          value_str <- utils::capture.output(utils::str(value, max.level = 2, list.len = 10))
          if (length(value_str) > 15) {
            value_str <- c(value_str[1:15], "... (truncated)")
          }
          msg <- c(msg, "", "Result structure:", paste(value_str, collapse = "\n"))
        }

        # Add execution time
        duration_ms <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000
        msg <- c(msg, "", sprintf("Execution time: %.1f ms", duration_ms))

        if (length(msg) == 0) {
          "Code executed successfully (no output)"
        } else {
          paste(msg, collapse = "\n")
        }
      }, error = function(e) {
        paste0("Error executing code: ", conditionMessage(e), "\n\n",
               "Suggestion: Check variable names, function arguments, and data types.")
      }, warning = function(w) {
        paste0("Warning: ", conditionMessage(w))
      })

      result
    }
  )

  # Tool: List variables with enhanced info
  list_vars_tool <- Tool$new(
    name = "list_session_variables",
    description = "List all variables currently in the session environment with type and size info.",
    parameters = z_object(
      pattern = z_string("Optional regex pattern to filter variable names"),
      show_hidden = z_boolean("Show hidden variables (starting with .)? Default FALSE"),
      .required = character(0)
    ),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) {
        return("Error: No session environment available.")
      }

      # Safely extract optional parameters
      pattern <- NULL
      if (!is.null(args$pattern) && is.character(args$pattern) && nzchar(args$pattern)) {
        pattern <- args$pattern
      }
      show_hidden <- isTRUE(args$show_hidden)

      vars <- if (is.null(pattern)) {
        ls(env, all.names = show_hidden)
      } else {
        ls(env, pattern = pattern, all.names = show_hidden)
      }

      if (length(vars) == 0) {
        return("No variables found in session environment.")
      }

      # Build detailed summary
      summaries <- sapply(vars, function(v) {
        obj <- get(v, envir = env)
        type <- paste(class(obj), collapse = ", ")
        size <- format(utils::object.size(obj), units = "auto")


        if (is.data.frame(obj)) {
          sprintf("  %s: data.frame [%d x %d] (%s)\n    Columns: %s",
                  v, nrow(obj), ncol(obj), size,
                  paste(head(names(obj), 5), collapse = ", "))
        } else if (is.vector(obj) && !is.list(obj)) {
          preview <- if (length(obj) <= 5) {
            paste(head(obj, 5), collapse = ", ")
          } else {
            paste0(paste(head(obj, 3), collapse = ", "), ", ...")
          }
          sprintf("  %s: %s [length %d] = %s", v, type, length(obj), preview)
        } else if (is.list(obj)) {
          sprintf("  %s: list [%d elements] (%s)", v, length(obj), size)
        } else if (inherits(obj, "ggplot")) {
          sprintf("  %s: ggplot object", v)
        } else if (inherits(obj, "lm") || inherits(obj, "glm")) {
          sprintf("  %s: %s model", v, class(obj)[1])
        } else {
          sprintf("  %s: %s (%s)", v, type, size)
        }
      })

      paste(c("Session variables:", summaries), collapse = "\n")
    }
  )

  # Tool: Get variable details
  inspect_var_tool <- Tool$new(
    name = "inspect_variable",
    description = "Get detailed information about a specific variable.",
    parameters = z_object(
      var_name = z_string("Name of the variable to inspect"),
      head_rows = z_integer("Number of rows to show for data frames. Default 6", minimum = 1, maximum = 20)
    ),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) {
        return("Error: No session environment available.")
      }

      var_name <- args$var_name
      head_rows <- args$head_rows %||% 6

      if (!exists(var_name, envir = env, inherits = FALSE)) {
        return(paste0("Error: Variable '", var_name, "' not found in session."))
      }

      obj <- get(var_name, envir = env)
      lines <- c(paste0("Variable: ", var_name))
      lines <- c(lines, paste0("Type: ", paste(class(obj), collapse = ", ")))
      lines <- c(lines, paste0("Size: ", format(utils::object.size(obj), units = "auto")))

      lines <- c(lines, "")

      if (is.data.frame(obj)) {
        lines <- c(lines, paste0("Dimensions: ", nrow(obj), " rows x ", ncol(obj), " columns"))
        lines <- c(lines, "")
        lines <- c(lines, "Column info:")
        for (col in names(obj)) {
          col_class <- class(obj[[col]])[1]
          n_na <- sum(is.na(obj[[col]]))
          lines <- c(lines, sprintf("  - %s (%s, %d NA)", col, col_class, n_na))
        }
        lines <- c(lines, "")
        lines <- c(lines, paste0("First ", min(head_rows, nrow(obj)), " rows:"))
        head_str <- utils::capture.output(print(utils::head(obj, head_rows)))
        lines <- c(lines, head_str)
      } else if (is.vector(obj) && !is.list(obj)) {
        lines <- c(lines, paste0("Length: ", length(obj)))
        if (is.numeric(obj)) {
          lines <- c(lines, sprintf("Range: [%.4g, %.4g]", min(obj, na.rm = TRUE), max(obj, na.rm = TRUE)))
          lines <- c(lines, sprintf("Mean: %.4g, SD: %.4g", mean(obj, na.rm = TRUE), stats::sd(obj, na.rm = TRUE)))
        }
        lines <- c(lines, paste0("NA count: ", sum(is.na(obj))))
        lines <- c(lines, "")
        lines <- c(lines, "Preview:")
        lines <- c(lines, paste(utils::head(obj, 10), collapse = ", "))
      } else {
        lines <- c(lines, "Structure:")
        str_output <- utils::capture.output(utils::str(obj, max.level = 3, list.len = 10))
        lines <- c(lines, str_output)
      }

      paste(lines, collapse = "\n")
    }
  )

  # Tool: Debug helper
  debug_tool <- Tool$new(
    name = "debug_error",
    description = "Analyze an error message and suggest fixes.",
    parameters = z_object(
      error_message = z_string("The error message to analyze"),
      code_context = z_string("The code that caused the error")
    ),
    execute = function(args) {
      error <- args$error_message
      code <- args$code_context

      suggestions <- character(0)

      # Common error patterns and suggestions
      if (grepl("object .* not found", error, ignore.case = TRUE)) {
        var_name <- gsub(".*object '([^']+)'.*", "\\1", error)
        suggestions <- c(suggestions,
          paste0("Variable '", var_name, "' doesn't exist."),
          "- Check spelling of variable name",
          "- Use list_session_variables to see available variables",
          "- The variable may need to be created first"
        )
      }

      if (grepl("could not find function", error, ignore.case = TRUE)) {
        fn_name <- gsub(".*function \"([^\"]+)\".*", "\\1", error)
        suggestions <- c(suggestions,
          paste0("Function '", fn_name, "' not found."),
          "- Check if the required package is loaded",
          "- Verify function name spelling",
          "- Use library() to load the package first"
        )
      }

      if (grepl("unexpected", error, ignore.case = TRUE)) {
        suggestions <- c(suggestions,
          "Syntax error detected.",
          "- Check for missing parentheses, brackets, or quotes",
          "- Verify operator usage (+, -, *, /, etc.)",
          "- Look for typos in function names"
        )
      }

      if (grepl("subscript out of bounds", error, ignore.case = TRUE)) {
        suggestions <- c(suggestions,
          "Index out of range.",
          "- Check the dimensions of your data",
          "- Verify row/column indices are within bounds",
          "- Use nrow() and ncol() to check dimensions"
        )
      }

      if (length(suggestions) == 0) {
        suggestions <- c(
          "General debugging suggestions:",
          "- Break the code into smaller parts",
          "- Check variable types with class()",
          "- Verify data dimensions with dim() or length()",
          "- Look for typos in variable and function names"
        )
      }

      paste(c("Error Analysis:", "", suggestions), collapse = "\n")
    }
  )

  system_prompt <- paste0(
    "You are an expert R programmer. Your role is to write and execute R code ",
    "to accomplish data manipulation, calculations, and analysis tasks.\n\n",
    "Guidelines:\n",
    "- Write clean, efficient R code\n",
    "- Use tidyverse style when appropriate\n",
    "- Store results in named variables so other agents can use them\n",
    "- Provide brief explanations of what your code does\n",
    "- Handle errors gracefully and use debug_error for troubleshooting\n",
    "- Always check available variables with list_session_variables first\n",
    if (safe_mode) "- You are in SAFE MODE: file I/O and network operations are restricted\n" else "",
    "\nWorkflow:\n",
    "1. Check existing variables with list_session_variables\n",
    "2. Inspect relevant data with inspect_variable\n",
    "3. Write and execute code with execute_r_code\n",
    "4. If errors occur, use debug_error to analyze\n",
    "5. Summarize what was done and what variables are available"
  )

  Agent$new(
    name = name,
    description = paste0(
      "Expert R programmer that can write and execute R code in the session ",
      "environment. Useful for data manipulation, calculations, and analysis. ",
      "Creates variables that other agents can access. ",
      if (safe_mode) "Safe mode enabled." else ""
    ),
    system_prompt = system_prompt,
    tools = list(execute_tool, list_vars_tool, inspect_var_tool, debug_tool)
  )
}

# =============================================================================
# PlannerAgent: Chain of Thought Reasoning
# =============================================================================

#' @title Create a PlannerAgent
#' @description
#' Creates an agent specialized in breaking down complex tasks into steps
#' using chain-of-thought reasoning. The planner helps decompose problems
#' and create action plans.
#'
#' @param name Agent name. Default "PlannerAgent".
#' @return An Agent object configured for planning and reasoning.
#' @export
#' @examples
#' \dontrun{
#' planner <- create_planner_agent()
#' result <- planner$run(
#'   "How should I approach building a machine learning model for customer churn?",
#'   model = "openai:gpt-4o"
#' )
#' }
create_planner_agent <- function(name = "PlannerAgent") {
  # Create a tool to save plans to session memory
  save_plan_tool <- Tool$new(
    name = "save_plan",
    description = "Save a structured plan to session memory for other agents to follow.",
    parameters = z_object(
      plan_name = z_string("Name for this plan"),
      steps = z_string("The steps of the plan as a numbered list")
    ),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) {
        return("Warning: No session environment. Plan not persisted.")
      }
      
      # Store plan in environment
      plans <- if (exists(".plans", envir = env)) get(".plans", envir = env) else list()
      plans[[args$plan_name]] <- list(
        steps = args$steps,
        created = Sys.time()
      )
      assign(".plans", plans, envir = env)
      
      paste0("Plan '", args$plan_name, "' saved successfully.")
    }
  )
  
  system_prompt <- paste0(
    "You are a strategic planning specialist that uses chain-of-thought reasoning ",
    "to break down complex problems into clear, actionable steps.\n\n",
    "When given a task or problem:\n",
    "1. First, understand the goal and constraints\n",
    "2. Identify the key sub-problems or phases\n",
    "3. Create a numbered step-by-step plan\n",
    "4. For each step, briefly explain WHY it's needed\n",
    "5. Identify dependencies between steps\n",
    "6. Note potential risks or decision points\n\n",
    "Use the save_plan tool to persist important plans for other agents.\n\n",
    "Format your plans clearly with:\n",
    "- Numbered steps\n",
    "- Brief explanations\n",
    "- Expected outcomes for each step"
  )
  
  Agent$new(
    name = name,
    description = paste0(
      "Strategic planning specialist that breaks down complex problems ",
      "into clear, actionable steps using chain-of-thought reasoning. ",
      "Creates structured plans that other agents can follow."
    ),
    system_prompt = system_prompt,
    tools = list(save_plan_tool)
  )
}

# =============================================================================
# VisualizerAgent: ggplot2 Visualization Expert (Enhanced)
# =============================================================================

#' @title Create a VisualizerAgent
#' @description
#' Creates an agent specialized in creating data visualizations using ggplot2.
#' Enhanced version with plot type recommendations, theme support, and
#' automatic data inspection.
#'
#' @param name Agent name. Default "VisualizerAgent".
#' @param output_dir Optional directory to save plots. If NULL, plots are
#'   stored in the session environment.
#' @param default_theme Default ggplot2 theme. Default "theme_minimal".
#' @param default_width Default plot width in inches. Default 8.
#' @param default_height Default plot height in inches. Default 6.
#' @return An Agent object configured for data visualization.
#' @export
#' @examples
#' \dontrun{
#' visualizer <- create_visualizer_agent()
#' session <- create_shared_session(model = "openai:gpt-4o")
#' session$set_var("df", data.frame(x = 1:10, y = (1:10)^2))
#' result <- visualizer$run(
#'   "Create a scatter plot of df showing the relationship between x and y",
#'   session = session,
#'   model = "openai:gpt-4o"
#' )
#' }
create_visualizer_agent <- function(name = "VisualizerAgent",
                                     output_dir = NULL,
                                     default_theme = "theme_minimal",
                                     default_width = 8,
                                     default_height = 6) {

  # Tool: Create ggplot visualization
  create_plot_tool <- Tool$new(
    name = "create_ggplot",
    description = paste0(
      "Create a ggplot2 visualization. ",
      "Provide complete ggplot2 code that creates a plot. ",
      "The plot will be stored in the session environment."
    ),
    parameters = z_object(
      plot_code = z_string(paste0(
        "Complete ggplot2 code to create the visualization. ",
        "Should start with ggplot() and include all necessary layers. ",
        "Data should reference variables from the session environment."
      )),
      plot_name = z_string("Name for the plot (used as variable name in session)"),
      title = z_string("Optional: Plot title to add"),
      save_file = z_boolean("Save plot to file? Default FALSE")
    ),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) {
        return("Error: No session environment available.")
      }

      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        return("Error: ggplot2 package is not installed. Use EnvAgent to install it.")
      }

      code <- args$plot_code
      plot_name <- args$plot_name
      title <- args$title
      save_file <- args$save_file %||% FALSE

      result <- tryCatch({
        # Create execution environment with ggplot2 loaded
        plot_env <- new.env(parent = env)
        eval(parse(text = "library(ggplot2)"), envir = plot_env)

        # Execute the plot code
        parsed <- parse(text = code)
        plot_obj <- eval(parsed, envir = plot_env)

        # Verify it's a ggplot
        if (!inherits(plot_obj, "ggplot")) {
          return("Error: Code did not produce a ggplot object. Make sure to use ggplot().")
        }

        # Add title if provided
        if (!is.null(title) && nzchar(title)) {
          plot_obj <- plot_obj + ggplot2::ggtitle(title)
        }

        # Apply default theme if not already themed
        if (!any(sapply(plot_obj$layers, function(l) inherits(l, "theme")))) {
          theme_fn <- get(default_theme, envir = asNamespace("ggplot2"))
          plot_obj <- plot_obj + theme_fn()
        }

        # Store the plot
        assign(plot_name, plot_obj, envir = env)
        assign(".last_plot", plot_obj, envir = env)

        # Save to file if requested
        if (save_file && !is.null(output_dir)) {
          filepath <- file.path(output_dir, paste0(plot_name, ".png"))
          ggplot2::ggsave(filepath, plot_obj, width = default_width, height = default_height)
          paste0("Plot '", plot_name, "' created and saved to ", filepath)
        } else {
          # Get plot summary
          n_layers <- length(plot_obj$layers)
          geoms <- sapply(plot_obj$layers, function(l) class(l$geom)[1])

          paste0(
            "Plot '", plot_name, "' created successfully.\n",
            "Layers: ", n_layers, " (", paste(unique(geoms), collapse = ", "), ")\n",
            "Access: session$get_var('", plot_name, "') or print(.last_plot)"
          )
        }
      }, error = function(e) {
        paste0("Error creating plot: ", conditionMessage(e), "\n",
               "Tip: Check that all referenced variables exist and column names are correct.")
      })

      result
    }
  )

  # Tool: Inspect data for plotting
  inspect_data_tool <- Tool$new(
    name = "inspect_data",
    description = paste0(
      "Inspect a data frame to understand its structure for plotting. ",
      "Returns column types, value ranges, and visualization recommendations."
    ),
    parameters = z_object(
      var_name = z_string("Name of the data frame variable to inspect")
    ),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) {
        return("Error: No session environment available.")
      }

      var_name <- args$var_name
      if (!exists(var_name, envir = env, inherits = FALSE)) {
        return(paste0("Error: Variable '", var_name, "' not found in session."))
      }

      obj <- get(var_name, envir = env)

      if (!is.data.frame(obj)) {
        return(paste0("'", var_name, "' is not a data frame. Type: ", class(obj)[1]))
      }

      # Build comprehensive summary
      lines <- c(
        paste0("Data frame: ", var_name),
        paste0("Dimensions: ", nrow(obj), " rows x ", ncol(obj), " columns"),
        ""
      )

      # Categorize columns
      numeric_cols <- character(0)
      categorical_cols <- character(0)
      date_cols <- character(0)

      lines <- c(lines, "Columns:")
      for (col in names(obj)) {
        col_data <- obj[[col]]
        col_class <- class(col_data)[1]
        n_na <- sum(is.na(col_data))
        na_pct <- round(100 * n_na / length(col_data), 1)

        if (is.numeric(col_data)) {
          numeric_cols <- c(numeric_cols, col)
          lines <- c(lines, sprintf(
            "  - %s (numeric): range [%.2g, %.2g], mean=%.2g, NA=%d (%.1f%%)",
            col, min(col_data, na.rm = TRUE), max(col_data, na.rm = TRUE),
            mean(col_data, na.rm = TRUE), n_na, na_pct
          ))
        } else if (inherits(col_data, "Date") || inherits(col_data, "POSIXt")) {
          date_cols <- c(date_cols, col)
          lines <- c(lines, sprintf(
            "  - %s (date): range [%s, %s], NA=%d",
            col, min(col_data, na.rm = TRUE), max(col_data, na.rm = TRUE), n_na
          ))
        } else if (is.factor(col_data) || is.character(col_data)) {
          categorical_cols <- c(categorical_cols, col)
          n_unique <- length(unique(col_data))
          top_vals <- names(sort(table(col_data), decreasing = TRUE))[1:min(3, n_unique)]
          lines <- c(lines, sprintf(
            "  - %s (categorical): %d unique, top: %s, NA=%d",
            col, n_unique, paste(top_vals, collapse = ", "), n_na
          ))
        } else {
          lines <- c(lines, sprintf("  - %s (%s): NA=%d", col, col_class, n_na))
        }
      }

      # Add visualization recommendations
      lines <- c(lines, "", "Visualization recommendations:")

      if (length(numeric_cols) >= 2) {
        lines <- c(lines, sprintf(
          "  - Scatter plot: geom_point() with %s vs %s",
          numeric_cols[1], numeric_cols[2]
        ))
      }

      if (length(numeric_cols) >= 1 && length(categorical_cols) >= 1) {
        lines <- c(lines, sprintf(
          "  - Box plot: geom_boxplot() with %s by %s",
          numeric_cols[1], categorical_cols[1]
        ))
        lines <- c(lines, sprintf(
          "  - Bar chart: geom_bar() or geom_col() for %s by %s",
          numeric_cols[1], categorical_cols[1]
        ))
      }

      if (length(numeric_cols) >= 1) {
        lines <- c(lines, sprintf(
          "  - Histogram: geom_histogram() for %s distribution",
          numeric_cols[1]
        ))
      }

      if (length(date_cols) >= 1 && length(numeric_cols) >= 1) {
        lines <- c(lines, sprintf(
          "  - Time series: geom_line() with %s over %s",
          numeric_cols[1], date_cols[1]
        ))
      }

      if (length(categorical_cols) >= 1) {
        lines <- c(lines, sprintf(
          "  - Count plot: geom_bar() for %s counts",
          categorical_cols[1]
        ))
      }

      paste(lines, collapse = "\n")
    }
  )

  # Tool: Recommend plot type
  recommend_plot_tool <- Tool$new(
    name = "recommend_plot",
    description = "Get plot type recommendations based on data characteristics and analysis goal.",
    parameters = z_object(
      var_name = z_string("Name of the data frame"),
      goal = z_enum(
        c("distribution", "comparison", "relationship", "composition", "trend"),
        description = paste0(
          "Analysis goal: ",
          "distribution (show spread of values), ",
          "comparison (compare groups), ",
          "relationship (show correlation), ",
          "composition (show parts of whole), ",
          "trend (show change over time)"
        )
      ),
      x_col = z_string("Column for x-axis (optional)"),
      y_col = z_string("Column for y-axis (optional)"),
      group_col = z_string("Column for grouping/coloring (optional)")
    ),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) {
        return("Error: No session environment available.")
      }

      var_name <- args$var_name
      goal <- args$goal
      x_col <- args$x_col
      y_col <- args$y_col
      group_col <- args$group_col

      if (!exists(var_name, envir = env)) {
        return(paste0("Error: Variable '", var_name, "' not found."))
      }

      df <- get(var_name, envir = env)
      if (!is.data.frame(df)) {
        return("Error: Variable is not a data frame.")
      }

      # Build recommendation
      rec <- switch(goal,
        "distribution" = {
          if (!is.null(x_col) && x_col %in% names(df)) {
            if (is.numeric(df[[x_col]])) {
              paste0(
                "For distribution of '", x_col, "':\n",
                "1. Histogram: ggplot(", var_name, ", aes(x = ", x_col, ")) + geom_histogram(bins = 30)\n",
                "2. Density: ggplot(", var_name, ", aes(x = ", x_col, ")) + geom_density(fill = 'steelblue', alpha = 0.5)\n",
                "3. Box plot: ggplot(", var_name, ", aes(y = ", x_col, ")) + geom_boxplot()"
              )
            } else {
              paste0(
                "For distribution of '", x_col, "' (categorical):\n",
                "Bar chart: ggplot(", var_name, ", aes(x = ", x_col, ")) + geom_bar()"
              )
            }
          } else {
            "Please specify x_col for distribution analysis."
          }
        },
        "comparison" = {
          if (!is.null(x_col) && !is.null(y_col)) {
            paste0(
              "For comparing '", y_col, "' across '", x_col, "':\n",
              "1. Box plot: ggplot(", var_name, ", aes(x = ", x_col, ", y = ", y_col, ")) + geom_boxplot()\n",
              "2. Violin: ggplot(", var_name, ", aes(x = ", x_col, ", y = ", y_col, ")) + geom_violin()\n",
              "3. Bar chart: ggplot(", var_name, ", aes(x = ", x_col, ", y = ", y_col, ")) + stat_summary(fun = mean, geom = 'bar')"
            )
          } else {
            "Please specify x_col (groups) and y_col (values) for comparison."
          }
        },
        "relationship" = {
          if (!is.null(x_col) && !is.null(y_col)) {
            code <- paste0("ggplot(", var_name, ", aes(x = ", x_col, ", y = ", y_col)
            if (!is.null(group_col)) {
              code <- paste0(code, ", color = ", group_col)
            }
            code <- paste0(code, "))")
            paste0(
              "For relationship between '", x_col, "' and '", y_col, "':\n",
              "1. Scatter: ", code, " + geom_point()\n",
              "2. With trend: ", code, " + geom_point() + geom_smooth(method = 'lm')\n",
              "3. Hex bins (large data): ", code, " + geom_hex()"
            )
          } else {
            "Please specify x_col and y_col for relationship analysis."
          }
        },
        "composition" = {
          if (!is.null(x_col)) {
            paste0(
              "For composition of '", x_col, "':\n",
              "1. Pie chart: ggplot(", var_name, ", aes(x = '', fill = ", x_col, ")) + geom_bar(width = 1) + coord_polar('y')\n",
              "2. Stacked bar: ggplot(", var_name, ", aes(x = 1, fill = ", x_col, ")) + geom_bar(position = 'fill')\n",
              "3. Treemap (requires treemapify): ggplot(", var_name, ", aes(area = count, fill = ", x_col, ")) + geom_treemap()"
            )
          } else {
            "Please specify x_col (category) for composition analysis."
          }
        },
        "trend" = {
          if (!is.null(x_col) && !is.null(y_col)) {
            paste0(
              "For trend of '", y_col, "' over '", x_col, "':\n",
              "1. Line: ggplot(", var_name, ", aes(x = ", x_col, ", y = ", y_col, ")) + geom_line()\n",
              "2. Area: ggplot(", var_name, ", aes(x = ", x_col, ", y = ", y_col, ")) + geom_area(alpha = 0.5)\n",
              "3. With points: ggplot(", var_name, ", aes(x = ", x_col, ", y = ", y_col, ")) + geom_line() + geom_point()"
            )
          } else {
            "Please specify x_col (time) and y_col (values) for trend analysis."
          }
        }
      )

      rec
    }
  )

  # Tool: List available plots
  list_plots_tool <- Tool$new(
    name = "list_plots",
    description = "List all ggplot objects stored in the session.",
    parameters = z_empty_object(description = "No parameters required"),
    execute = function(args) {
      env <- args$.envir
      if (is.null(env)) {
        return("Error: No session environment available.")
      }

      vars <- ls(env)
      plots <- character(0)

      for (v in vars) {
        obj <- get(v, envir = env)
        if (inherits(obj, "ggplot")) {
          n_layers <- length(obj$layers)
          geoms <- unique(sapply(obj$layers, function(l) class(l$geom)[1]))
          plots <- c(plots, sprintf("- %s: %d layers (%s)", v, n_layers, paste(geoms, collapse = ", ")))
        }
      }

      if (length(plots) == 0) {
        return("No ggplot objects found in session.")
      }

      paste(c("Available plots:", plots), collapse = "\n")
    }
  )

  system_prompt <- paste0(
    "You are a data visualization expert specializing in ggplot2.\n\n",
    "Your role is to create beautiful, informative visualizations. Follow these principles:\n",
    "1. Always inspect the data first using inspect_data to understand its structure\n",
    "2. Use recommend_plot to get suggestions based on the analysis goal\n",
    "3. Choose appropriate plot types for the data and question\n",
    "4. Use clear titles, axis labels, and legends\n",
    "5. Apply clean themes (", default_theme, " is the default)\n",
    "6. Use color effectively to highlight patterns\n\n",
    "Workflow:\n",
    "1. Inspect data with inspect_data\n",
    "2. Get recommendations with recommend_plot if unsure\n",
    "3. Create visualization with create_ggplot\n",
    "4. Describe what the plot shows and any insights\n\n",
    "When creating plots:\n",
    "- Reference data from the session environment by variable name\n",
    "- Use aes() for mapping data to aesthetics\n",
    "- Add appropriate geom_ layers\n",
    "- Include scale_ and theme_ customizations as needed\n",
    "- Consider the story the data is telling"
  )

  Agent$new(
    name = name,
    description = paste0(
      "Data visualization expert using ggplot2. Creates beautiful, informative ",
      "plots from data in the session environment. Can inspect data structure, ",
      "recommend plot types, and generate appropriate visualizations."
    ),
    system_prompt = system_prompt,
    tools = list(create_plot_tool, inspect_data_tool, recommend_plot_tool, list_plots_tool)
  )
}
