#' @title R-Native Programmatic Sandbox
#' @description
#' SandboxManager R6 class and utilities for building an R-native programmatic
#' tool sandbox. Inspired by Anthropic's Programmatic Tool Calling, this module
#' enables LLMs to write R code that batch-invokes registered tools and processes
#' data locally (using dplyr/purrr), returning only concise results to the context.
#'
#' @details
#' The core idea: instead of the LLM making N separate tool calls (each requiring
#' a round-trip), it writes a single R script that loops over inputs, calls tools
#' as ordinary R functions, filters/aggregates the results with dplyr, and
#' `print()`s only the key findings. This dramatically reduces token usage,
#' latency, and context window pressure.
#'
#' ## Architecture
#' ```
#' User Tools ──► SandboxManager ──► Isolated R Env
#'                 │                    ├── tool_a()
#'                 │                    ├── tool_b()
#'                 │                    ├── dplyr::*
#'                 │                    └── purrr::*
#'                 │
#'                 ▼
#'             create_r_code_tool() ──► Single "execute_r_code" Tool
#'                                      (registered with LLM)
#' ```
#' @name sandbox
NULL

# ============================================================================
# SandboxManager: Isolated R Execution Environment
# ============================================================================

#' @title SandboxManager Class
#' @description
#' R6 class that manages an isolated R environment for executing LLM-generated
#' R code. Tools are bound as callable functions within this environment,
#' enabling the LLM to batch-invoke and process data locally.
#' @export
SandboxManager <- R6::R6Class(
    "SandboxManager",
    public = list(
        #' @description Initialize a new SandboxManager.
        #' @param tools Optional list of Tool objects to bind into the sandbox.
        #' @param preload_packages Character vector of package names to preload
        #'   into the sandbox (their exports become available). Default: c("dplyr", "purrr").
        #' @param max_output_chars Maximum characters to capture from code output.
        #'   Prevents runaway `print()` from flooding the context. Default: 8000.
        #' @param parent_env Optional parent environment for the sandbox.
        #'   When a ChatSession is available, pass `session$get_envir()` here
        #'   to enable cross-step variable persistence.
        initialize = function(tools = list(),
                              preload_packages = c("dplyr", "purrr"),
                              max_output_chars = 8000,
                              parent_env = NULL) {
            private$.max_output_chars <- max_output_chars
            private$.tools <- list()

            # Create the isolated sandbox environment
            parent <- parent_env %||% new.env(parent = baseenv())
            private$.env <- rlang::env(parent)

            # Preload safe packages
            for (pkg in preload_packages) {
                if (requireNamespace(pkg, quietly = TRUE)) {
                    pkg_ns <- asNamespace(pkg)
                    pkg_exports <- getNamespaceExports(pkg_ns)
                    for (fn_name in pkg_exports) {
                        fn <- get(fn_name, envir = pkg_ns)
                        if (is.function(fn)) {
                            rlang::env_bind(private$.env, !!fn_name := fn)
                        }
                    }
                }
            }

            # Bind essential base functions that LLMs commonly use
            base_fns <- c(
                "print", "cat", "paste", "paste0", "sprintf", "format",
                "c", "list", "data.frame", "seq", "seq_len", "seq_along",
                "length", "nchar", "nrow", "ncol", "names", "head", "tail",
                "sum", "mean", "median", "max", "min", "range", "sd", "var",
                "abs", "round", "ceiling", "floor", "sqrt", "log", "exp",
                "grepl", "grep", "sub", "gsub", "regexpr", "regmatches",
                "toupper", "tolower", "trimws", "nchar", "substr", "substring",
                "strsplit", "startsWith", "endsWith",
                "which", "any", "all", "duplicated", "unique", "sort", "order",
                "rev", "table", "as.character", "as.numeric", "as.integer",
                "as.logical", "as.data.frame", "is.null", "is.na", "is.numeric",
                "is.character", "is.logical", "is.data.frame", "is.list",
                "ifelse", "switch", "do.call", "Reduce", "Map", "mapply",
                "lapply", "sapply", "vapply", "Sys.time", "Sys.Date",
                "tryCatch", "try", "message", "warning", "stop",
                "exists", "get", "assign", "environment", "new.env",
                "setdiff", "intersect", "union",
                "Sys.sleep",
                "identical", "match", "%in%",
                "readline", "readLines", "writeLines",
                "file.exists", "file.path",
                "structure", "attributes", "attr",
                "class", "inherits",
                "numeric", "character", "logical", "integer"
            )
            for (fn_name in base_fns) {
                fn <- tryCatch(get(fn_name, envir = baseenv()), error = function(e) {
                    tryCatch(get(fn_name, envir = globalenv()), error = function(e2) NULL)
                })
                if (is.function(fn)) {
                    rlang::env_bind(private$.env, !!fn_name := fn)
                }
            }

            # Also bind some stats functions
            stats_fns <- c("setNames")
            for (fn_name in stats_fns) {
                fn <- tryCatch(get(fn_name, envir = asNamespace("stats")),
                    error = function(e) NULL
                )
                if (is.function(fn)) {
                    rlang::env_bind(private$.env, !!fn_name := fn)
                }
            }

            # Bind tools if provided
            if (length(tools) > 0) {
                self$bind_tools(tools)
            }
        },

        #' @description Bind Tool objects into the sandbox as callable R functions.
        #' @param tools A list of Tool objects to bind.
        #' @return Invisible self (for chaining).
        bind_tools = function(tools) {
            for (t in tools) {
                if (!inherits(t, "Tool")) {
                    rlang::warn(paste0("Skipping non-Tool object: ", class(t)[1]))
                    next
                }
                private$.tools[[t$name]] <- t

                # Create a wrapper function that calls the tool's run method
                # We need to capture `t` by value using a factory
                wrapper <- private$make_tool_wrapper(t)
                rlang::env_bind(private$.env, !!t$name := wrapper)
            }
            invisible(self)
        },

        #' @description Execute R code in the sandbox environment.
        #' @param code_str A character string containing R code to execute.
        #' @return A character string with captured stdout, or an error message.
        execute = function(code_str) {
            if (!is.character(code_str) || length(code_str) != 1 || !nzchar(trimws(code_str))) {
                return("Error: code must be a non-empty string.")
            }

            result <- tryCatch(
                {
                    captured <- utils::capture.output({
                        eval(parse(text = code_str), envir = private$.env)
                    })
                    output <- paste(captured, collapse = "\n")

                    # Truncate if too long
                    if (nchar(output) > private$.max_output_chars) {
                        output <- paste0(
                            substr(output, 1, private$.max_output_chars),
                            "\n\n... [Output truncated at ", private$.max_output_chars, " chars]"
                        )
                    }

                    if (nchar(output) == 0) {
                        output <- "(Code executed successfully with no printed output)"
                    }

                    output
                },
                error = function(e) {
                    paste("Error executing R code:", conditionMessage(e))
                }
            )

            result
        },

        #' @description Get human-readable signatures for all bound tools.
        #' @return A character string with Markdown-formatted tool documentation.
        get_tool_signatures = function() {
            if (length(private$.tools) == 0) {
                return("")
            }

            sigs <- vapply(names(private$.tools), function(name) {
                t <- private$.tools[[name]]
                # Extract parameter info from schema
                params_info <- private$extract_params_info(t)
                paste0(
                    "### `", name, "(", params_info$signature, ")`\n",
                    t$description, "\n",
                    if (nzchar(params_info$details)) paste0("\n", params_info$details) else ""
                )
            }, character(1))

            paste(sigs, collapse = "\n\n")
        },

        #' @description Get the sandbox environment.
        #' @return The R environment used by the sandbox.
        get_env = function() {
            private$.env
        },

        #' @description Get list of bound tool names.
        #' @return Character vector of tool names available in the sandbox.
        list_tools = function() {
            names(private$.tools)
        },

        #' @description Reset the sandbox environment (clear all user variables).
        #'   Tool bindings and preloaded packages are preserved.
        reset = function() {
            # Remove user-created variables but keep tool bindings and package functions
            user_vars <- setdiff(ls(private$.env), c(names(private$.tools)))
            rm(list = user_vars, envir = private$.env)
            invisible(self)
        },

        #' @description Print method for SandboxManager.
        print = function() {
            cat("<SandboxManager>\n")
            cat("  Tools:", length(private$.tools), "\n")
            if (length(private$.tools) > 0) {
                cat("  Tool names:", paste(names(private$.tools), collapse = ", "), "\n")
            }
            cat("  Max output chars:", private$.max_output_chars, "\n")
            cat("  Env vars:", length(ls(private$.env)), "\n")
            invisible(self)
        }
    ),
    private = list(
        .env = NULL,
        .tools = NULL,
        .max_output_chars = 8000,

        # Factory function to create a tool wrapper that captures the tool by value
        make_tool_wrapper = function(tool_obj) {
            # Force evaluation of tool_obj to capture it
            force(tool_obj)

            function(...) {
                args <- list(...)

                # If called with a single unnamed list, unwrap it
                if (length(args) == 1 && is.list(args[[1]]) && is.null(names(args))) {
                    args <- args[[1]]
                }

                # Get parameter names from schema for positional argument matching
                if (inherits(tool_obj$parameters, "z_schema") &&
                    !is.null(tool_obj$parameters$properties)) {
                    param_names <- names(tool_obj$parameters$properties)
                    # If args are unnamed (positional), assign names from schema
                    if (is.null(names(args)) && length(args) > 0 && length(param_names) >= length(args)) {
                        names(args) <- param_names[seq_along(args)]
                    }
                }

                # Execute the tool
                result_str <- tool_obj$run(args)

                # Try to parse JSON results back to R objects for pipeline use
                if (is.character(result_str) && length(result_str) == 1) {
                    parsed <- tryCatch(
                        jsonlite::fromJSON(result_str, simplifyVector = TRUE, simplifyDataFrame = TRUE),
                        error = function(e) result_str
                    )
                    return(parsed)
                }

                result_str
            }
        },

        # Extract parameter info from a Tool's schema
        extract_params_info = function(tool_obj) {
            if (!inherits(tool_obj$parameters, "z_schema") ||
                is.null(tool_obj$parameters$properties)) {
                return(list(signature = "", details = ""))
            }

            props <- tool_obj$parameters$properties
            param_names <- names(props)
            required <- tool_obj$parameters$required %||% character(0)

            # Build signature string
            sig_parts <- vapply(param_names, function(pname) {
                if (pname %in% required) pname else paste0(pname, " = NULL")
            }, character(1))
            signature <- paste(sig_parts, collapse = ", ")

            # Build details
            detail_lines <- vapply(param_names, function(pname) {
                p <- props[[pname]]
                desc <- p$description %||% ""
                type <- p$type %||% "any"
                req_marker <- if (pname %in% required) " *(required)*" else ""
                paste0("- `", pname, "` (", type, "): ", desc, req_marker)
            }, character(1))
            details <- paste(detail_lines, collapse = "\n")

            list(signature = signature, details = details)
        }
    )
)

# ============================================================================
# R Code Tool Factory
# ============================================================================

#' @title Create R Code Interpreter Tool
#' @description
#' Creates a meta-tool (`execute_r_code`) backed by a SandboxManager.
#' This single tool replaces all individual tools for the LLM, enabling
#' batch execution, data filtering, and local computation.
#'
#' @param sandbox A SandboxManager object.
#' @return A Tool object named `execute_r_code`.
#' @export
create_r_code_tool <- function(sandbox) {
    if (!inherits(sandbox, "SandboxManager")) {
        rlang::abort("sandbox must be a SandboxManager object.")
    }

    # Build dynamic description including available tools
    tool_sigs <- sandbox$get_tool_signatures()
    tool_names <- sandbox$list_tools()

    if (length(tool_names) > 0) {
        tools_section <- paste0(
            "Available functions in this environment:\n\n",
            tool_sigs
        )
    } else {
        tools_section <- "No custom functions are currently available."
    }

    description <- paste0(
        "Execute R code in a persistent R interpreter environment. ",
        "Variables persist across calls. ",
        "You can use dplyr and purrr for data manipulation. ",
        "Use print() to output results you want to see. ",
        "Tool results are automatically parsed: data.frames stay as data.frames. ",
        "If your code has an error, you will see the error message and can fix it.\n\n",
        tools_section
    )

    tool(
        name = "execute_r_code",
        description = description,
        parameters = z_object(
            code = z_string(description = paste0(
                "R code to execute. Use print() to output results. ",
                "Available packages: dplyr, purrr. ",
                if (length(tool_names) > 0) {
                    paste0("Available functions: ", paste(tool_names, collapse = ", "), ".")
                } else {
                    ""
                }
            ))
        ),
        execute = function(args) {
            sandbox$execute(args$code)
        }
    )
}

# ============================================================================
# System Prompt Builder
# ============================================================================

#' @title Create Sandbox System Prompt
#' @description
#' Generates a system prompt section that instructs the LLM how to use the
#' R code sandbox effectively.
#'
#' @param sandbox A SandboxManager object.
#' @return A character string to append to the system prompt.
#' @export
create_sandbox_system_prompt <- function(sandbox) {
    if (!inherits(sandbox, "SandboxManager")) {
        rlang::abort("sandbox must be a SandboxManager object.")
    }

    tool_sigs <- sandbox$get_tool_signatures()
    tool_names <- sandbox$list_tools()

    prompt <- paste0(
        "## R Code Execution Environment\n\n",
        "You have access to a persistent R interpreter via the `execute_r_code` tool. ",
        "Instead of calling tools one by one, write R code to batch-process data efficiently.\n\n",
        "### Guidelines\n",
        "- **Batch operations**: Use `purrr::map()` or `lapply()` to call functions in a loop.\n",
        "- **Filter locally**: Use `dplyr::filter()`, `dplyr::select()`, `dplyr::summarise()` to reduce data before printing.\n",
        "- **Only print what matters**: Use `print()` to output only the final, condensed results.\n",
        "- **Variables persist**: Values you assign (e.g., `x <- 42`) remain available in subsequent calls.\n",
        "- **Error recovery**: If your code errors, read the error message and fix your code.\n",
        "- **Data frames**: Tool results that return tabular data are automatically converted to data.frames.\n\n"
    )

    if (length(tool_names) > 0) {
        prompt <- paste0(
            prompt,
            "### Available Functions\n\n",
            "The following functions are available in the R environment:\n\n",
            tool_sigs, "\n\n",
            "Call them directly in your R code like regular R functions.\n"
        )
    }

    prompt
}
