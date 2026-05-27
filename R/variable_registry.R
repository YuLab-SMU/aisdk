#' @title Variable Registry
#' @description
#' Provides a mechanism to protect specific variables from being accidentally
#' modified or duplicated by the Agent within the sandbox environment.
#'
#' @name variable_registry
NULL

# Internal environment to store protected variable metadata
.sdk_protected_vars_registry <- new.env(parent = emptyenv())

#' Protect a Variable from Agent Modification
#'
#' Marks a variable as protected so that the Agent cannot accidentally overwrite,
#' shadow, or deeply copy it during sandbox execution.
#'
#' @param name Character string. The name of the variable to protect.
#' @param locked Logical. If TRUE, the variable cannot be assigned to by the Agent.
#' @param cost Character string. An indicator of the variable's computation/memory cost (e.g., "High", "Medium", "Low").
#' @return Invisible TRUE.
#' @export
sdk_protect_var <- function(name, locked = TRUE, cost = "High") {
    if (!is.character(name) || length(name) != 1) {
        rlang::abort("Variable name must be a single character string.")
    }

    .sdk_protected_vars_registry[[name]] <- list(
        locked = locked,
        cost = cost
        # Can add more metadata here like description, timestamp, etc.
    )

    invisible(TRUE)
}

#' Unprotect a Variable
#'
#' Removes protection from a previously protected variable.
#'
#' @param name Character string. The name of the variable to unprotect.
#' @return Invisible TRUE.
#' @export
sdk_unprotect_var <- function(name) {
    if (exists(name, envir = .sdk_protected_vars_registry)) {
        rm(list = name, envir = .sdk_protected_vars_registry)
    }
    invisible(TRUE)
}

#' Get Metadata for a Protected Variable
#'
#' @param name Character string. The name of the variable.
#' @return A list with metadata (locked, cost, etc.), or NULL if not protected.
#' @export
sdk_get_var_metadata <- function(name) {
    if (exists(name, envir = .sdk_protected_vars_registry)) {
        return(.sdk_protected_vars_registry[[name]])
    }
    NULL
}

#' Check if a Variable is Locked
#'
#' @param name Character string. The name of the variable.
#' @return TRUE if the variable is protected and locked, FALSE otherwise.
#' @export
sdk_is_var_locked <- function(name) {
    meta <- sdk_get_var_metadata(name)
    if (!is.null(meta)) {
        return(isTRUE(meta$locked))
    }
    FALSE
}

#' Reset the Variable Registry
#'
#' Clears all protected variables.
#' @export
sdk_clear_protected_vars <- function() {
    rm(list = ls(.sdk_protected_vars_registry), envir = .sdk_protected_vars_registry)
    invisible(TRUE)
}
