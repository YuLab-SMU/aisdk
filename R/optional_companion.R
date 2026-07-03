# Helpers for optional companion packages in the aisdk family
# (aisdk.console, aisdk.channels, aisdk.skills, aisdk.providers, ...).
# The package names are constructed dynamically so that R CMD check does
# not treat them as undeclared static dependencies. The non-dot versions
# are exported as part of the package-author extension API so companion
# packages can reach their optional siblings the same way core does.

#' Companion Package Name for a Suffix
#'
#' Maps a companion suffix (e.g. `"console"`, `"channels"`) to the package
#' name (`"aisdk.console"`, `"aisdk.channels"`).
#'
#' @param suffix Companion suffix, the part after `"aisdk."`.
#' @return The companion package name (character scalar).
#' @keywords internal
#' @export
companion_pkg_name <- function(suffix) {
  paste0("aisdk", ".", suffix)
}

#' Is a Companion Package Available?
#'
#' Quietly checks whether the companion package for `suffix` can be loaded.
#'
#' @inheritParams companion_pkg_name
#' @return `TRUE` if the companion namespace is available.
#' @keywords internal
#' @export
companion_pkg_available <- function(suffix) {
  pkg <- companion_pkg_name(suffix)
  isTRUE(suppressWarnings(requireNamespace(pkg, quietly = TRUE)))
}

#' Fetch an Object from a Companion Package
#'
#' Retrieves an object from the namespace of an available companion package.
#' Callers should check [companion_pkg_available()] (or
#' [ensure_companion_package()]) first.
#'
#' @inheritParams companion_pkg_name
#' @param name Name of the object to fetch from the companion namespace.
#' @return The requested object.
#' @keywords internal
#' @export
companion_pkg_get <- function(suffix, name) {
  pkg <- companion_pkg_name(suffix)
  get(name, envir = asNamespace(pkg), inherits = FALSE)
}

#' Install Hint for a Companion Package
#'
#' Returns the installation command shown to users when an optional
#' companion package is missing.
#'
#' @inheritParams companion_pkg_name
#' @return A character scalar with the install command.
#' @keywords internal
#' @export
companion_install_hint <- function(suffix) {
  pkg <- companion_pkg_name(suffix)
  paste0("remotes::install_github('YuLab-SMU/", pkg, "')")
}

#' Ensure a Companion Package Is Available
#'
#' Loads the companion package for `suffix` if installed. When it is missing
#' and the session is interactive, offers to install it (via
#' [rlang::check_installed()]); non-interactive sessions, scripts,
#' `R CMD check`, and CRAN runs are never interrupted and never trigger
#' installs. This is the generalized form of the provider-specific
#' companion install prompt.
#'
#' @inheritParams companion_pkg_name
#' @param reason Optional human-readable reason shown in the install prompt,
#'   e.g. `'to open the interactive console'`.
#' @return `TRUE` if the companion package is now available, else `FALSE`.
#' @keywords internal
#' @export
ensure_companion_package <- function(suffix, reason = NULL) {
  pkg <- companion_pkg_name(suffix)
  if (isTRUE(suppressWarnings(requireNamespace(pkg, quietly = TRUE)))) {
    return(TRUE)
  }
  if (!interactive()) {
    return(FALSE)
  }
  installed <- tryCatch(
    {
      rlang::check_installed(pkg, reason = reason)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!installed) {
    return(FALSE)
  }
  isTRUE(suppressWarnings(requireNamespace(pkg, quietly = TRUE)))
}

# Historical dot-prefixed aliases, kept for internal call sites.

#' @keywords internal
.companion_pkg_name <- companion_pkg_name

#' @keywords internal
.companion_pkg_available <- companion_pkg_available

#' @keywords internal
.companion_pkg_get <- companion_pkg_get

#' @keywords internal
.companion_install_hint <- companion_install_hint
