# Helpers for optional companion packages distributed outside CRAN
# (currently aisdk.channels and aisdk.skills). The package names are
# constructed dynamically so that R CMD check does not treat them as
# undeclared static dependencies.

#' @keywords internal
.companion_pkg_name <- function(suffix) {
  paste0("aisdk", ".", suffix)
}

#' @keywords internal
.companion_pkg_available <- function(suffix) {
  pkg <- .companion_pkg_name(suffix)
  isTRUE(suppressWarnings(requireNamespace(pkg, quietly = TRUE)))
}

#' @keywords internal
.companion_pkg_get <- function(suffix, name) {
  pkg <- .companion_pkg_name(suffix)
  get(name, envir = asNamespace(pkg), inherits = FALSE)
}

#' @keywords internal
.companion_install_hint <- function(suffix) {
  pkg <- .companion_pkg_name(suffix)
  paste0("remotes::install_github('YuLab-SMU/", pkg, "')")
}
