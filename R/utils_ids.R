#' @title Utilities: Stable IDs
#' @description Generic helpers for generating stable, content-derived IDs.
#' @name utils_ids
NULL

#' Generate a stable, content-derived identifier
#'
#' Generates a stable unique identifier by hashing the supplied components.
#' Part of the companion-package extension API (used by \pkg{aisdk.datatools}).
#' @param type Type of element (e.g. "layer", "guide").
#' @param ... Components to include in the ID hash.
#' @param prefix Optional prefix for the ID.
#' @return A stable ID string.
#' @keywords internal
#' @importFrom digest digest
#' @export
generate_stable_id <- function(type, ..., prefix = NULL) {
  components <- c(as.character(type), as.character(list(...)))
  hash <- substr(digest::digest(paste(components, collapse = "_")), 1, 8)
  if (!is.null(prefix)) {
    paste0(prefix, "_", hash)
  } else {
    hash
  }
}
