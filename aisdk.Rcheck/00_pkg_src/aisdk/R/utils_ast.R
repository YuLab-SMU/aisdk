#' @title AST Safety Analysis
#' @description
#' Provides static analysis of R Abstract Syntax Trees (AST) to prevent
#' evasion of sandbox restrictions.
#'
#' @name utils_ast
#' @keywords internal
NULL

#' Walk an Abstract Syntax Tree
#'
#' Recursively traverse an R expression and apply a visitor function to each node.
#' @param expr An R expression, call, or primitive type.
#' @param visitor A function taking a node as argument.
#' @export
walk_ast <- function(expr, visitor) {
    visitor(expr)

    if (is.expression(expr) || is.call(expr) || is.pairlist(expr)) {
        for (i in seq_along(expr)) {
            walk_ast(expr[[i]], visitor)
        }
    }
}

#' Check AST Safety
#'
#' Analyze R code for unsafe function calls or operations before execution.
#' @param code_str Character string containing R code.
#' @return The parsed AST if safe. Throws an error if unsafe.
#' @export
check_ast_safety <- function(code_str) {
    if (!is.character(code_str)) rlang::abort("code_str must be a string")

    # Empty or purely whitespace code is fine
    if (!nzchar(trimws(code_str))) {
        return(parse(text = code_str))
    }

    parsed <- tryCatch(
        {
            parse(text = code_str)
        },
        error = function(e) {
            rlang::abort(paste("Parse Error:", conditionMessage(e)))
        }
    )

    # Define restricted functions based on the security principles
    red_funcs <- c(
        "system", "system2", "system_call",
        "q", "quit", "Sys.setenv", "Sys.putenv", "setwd",
        "socketConnection", "serverSocket",
        "eval", "evalq", "parse", "mget"
    )

    yellow_funcs <- c(
        "unlink", "file.remove", "file.rename", "file.symlink", "file.link",
        "file.copy", "file.append", "file.create", "dir.create", "dir.remove",
        "download.file", "source", "sourceUrl", "curl", "url",
        "<<-"
    )

    visitor <- function(node) {
        if (!is.call(node)) {
            return()
        }

        # Helper to find the actual variable being modified
        get_assignment_target <- function(lhs) {
            while (is.call(lhs) && length(lhs) >= 2 && as.character(lhs[[1]]) %in% c("$", "[", "[[", "@")) {
                lhs <- lhs[[2]]
            }
            if (is.symbol(lhs)) {
                return(as.character(lhs))
            }
            NULL
        }

        # Extract function being called
        fn <- node[[1]]
        fn_name <- NULL

        if (is.symbol(fn)) {
            fn_name <- as.character(fn)
        } else if (is.call(fn) && as.character(fn[[1]]) %in% c("::", ":::")) {
            fn_name <- as.character(fn[[3]])
        }

        if (!is.null(fn_name)) {
            # Check for protected variable shadowing
            if (fn_name %in% c("<-", "=", "<<-", "assign")) {
                target <- NULL
                if (fn_name %in% c("<-", "=", "<<-") && length(node) > 1) {
                    target <- get_assignment_target(node[[2]])
                } else if (fn_name == "assign" && length(node) > 1 && is.character(node[[2]])) {
                    target <- node[[2]]
                }

                if (!is.null(target) && exists("sdk_is_var_locked", mode = "function") && sdk_is_var_locked(target)) {
                    meta <- sdk_get_var_metadata(target)
                    rlang::abort(sprintf("AST Safety Violation: Variable '%s' is protected (Cost: %s). Modifying it is not allowed. Please assign your result to a new variable.", target, meta$cost))
                }
            }

            if (fn_name %in% c(red_funcs, yellow_funcs, "assign")) {
                # Specific check for assign: if it modifies the global environment
                if (fn_name == "assign") {
                    args <- as.list(node)[-1]
                    arg_names <- names(args)
                    is_envir <- FALSE

                    for (i in seq_along(args)) {
                        arg <- args[[i]]
                        is_envir_arg <- FALSE

                        if (!is.null(arg_names) && arg_names[i] == "envir") {
                            is_envir_arg <- TRUE
                        } else if (i == 3 && (is.null(arg_names) || arg_names[i] == "")) {
                            is_envir_arg <- TRUE # 3rd positional is envir
                        }

                        if (is_envir_arg) {
                            if (is.call(arg) && as.character(arg[[1]]) == "globalenv") {
                                if (exists("request_authorization", mode = "function")) {
                                    request_authorization("Modify global environment using assign()", "YELLOW")
                                } else {
                                    rlang::abort("AST Safety Violation: Global environment modification using assign() is not allowed.")
                                }
                            } else if (is.symbol(arg) && as.character(arg) == ".GlobalEnv") {
                                if (exists("request_authorization", mode = "function")) {
                                    request_authorization("Modify global environment using assign()", "YELLOW")
                                } else {
                                    rlang::abort("AST Safety Violation: Global environment modification using assign() is not allowed.")
                                }
                            }
                        }
                    }
                } else if (fn_name %in% red_funcs) {
                    rlang::abort(sprintf("AST Safety Violation: Call to strictly prohibited function '%s' is not allowed.", fn_name))
                } else if (fn_name %in% yellow_funcs) {
                    if (exists("request_authorization", mode = "function")) {
                        request_authorization(sprintf("Execute potentially risky function '%s'", fn_name), "YELLOW")
                    } else {
                        rlang::abort(sprintf("AST Safety Violation: Call to restricted function '%s' requires interactive authorization.", fn_name))
                    }
                }
            }
        }
    }

    walk_ast(parsed, visitor)
    parsed
}
