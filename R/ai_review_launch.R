#' @title Legacy Review Launch Helpers
#' @description Internal helpers for the older connected review runtime flow.
#' Kept only so package-internal compatibility code can still resolve review
#' source/output pairs when needed.
#' @name ai_review_launch
NULL

#' @keywords internal
normalize_open_review_document_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' @keywords internal
review_document_html_output_path <- function(path) {
  sub("\\.(Rmd|rmd|qmd)$", ".html", path, ignore.case = TRUE)
}

#' @keywords internal
find_review_document_source_path <- function(html_path) {
  stem <- sub("\\.(html|htm)$", "", html_path, ignore.case = TRUE)
  candidates <- c(
    paste0(stem, ".qmd"),
    paste0(stem, ".Rmd"),
    paste0(stem, ".rmd")
  )
  source_path <- candidates[file.exists(candidates)][1]
  if (length(source_path) == 0 || is.na(source_path) || !nzchar(source_path)) {
    return(NULL)
  }

  normalizePath(source_path, winslash = "/", mustWork = TRUE)
}

#' @keywords internal
resolve_open_review_document_target <- function(path, render = FALSE) {
  normalized <- normalize_open_review_document_path(path)
  ext <- tolower(tools::file_ext(normalized))

  if (ext %in% c("rmd", "qmd")) {
    html_path <- normalizePath(
      review_document_html_output_path(normalized),
      winslash = "/",
      mustWork = FALSE
    )
    target_path <- if (isTRUE(render) || !file.exists(html_path)) normalized else html_path

    return(list(
      requested_path = normalized,
      target_path = target_path,
      source_path = normalized,
      html_path = html_path,
      render = identical(target_path, normalized)
    ))
  }

  if (ext %in% c("html", "htm")) {
    source_path <- find_review_document_source_path(normalized)
    target_path <- normalized
    render_enabled <- FALSE

    if (isTRUE(render) && !is.null(source_path)) {
      target_path <- source_path
      render_enabled <- TRUE
    } else if (isTRUE(render) && is.null(source_path)) {
      rlang::warn(
        paste0(
          "`render = TRUE` was ignored because no matching .Rmd or .qmd source was found for ",
          basename(normalized),
          "."
        )
      )
    }

    return(list(
      requested_path = normalized,
      target_path = target_path,
      source_path = source_path,
      html_path = normalized,
      render = render_enabled
    ))
  }

  rlang::abort("`path` must point to an .html, .Rmd, or .qmd file.")
}

#' @keywords internal
open_review_document_browser <- function(url) {
  utils::browseURL(url)
}

#' @title Legacy Connected Review Entry Point
#' @description Internal legacy wrapper around `review_document()`. The
#' preferred user-facing path is now to render static AI artifact cards and
#' inspect their embedded trace/metadata directly in the published output.
#' @param path Path to an `.html`, `.Rmd`, or `.qmd` document.
#' @param render Whether to force rendering from source before launch.
#' @param memory ProjectMemory object. Defaults to `get_memory()`.
#' @param host Host interface to bind. Defaults to `"127.0.0.1"`.
#' @param port Optional port for the local runtime.
#' @param browse Open the connected review URL in a browser. Defaults to
#'   `interactive()`.
#' @return A launcher list with resolved path metadata plus the underlying live
#'   review launcher fields.
#' @keywords internal
open_review_document <- function(path, render = FALSE, memory = get_memory(),
                                 host = "127.0.0.1", port = NULL,
                                 browse = interactive()) {
  resolved <- resolve_open_review_document_target(path, render = render)

  launcher <- review_document(
    path = resolved$target_path,
    memory = memory,
    host = host,
    port = port,
    browse = FALSE
  )

  launcher$requested_path <- resolved$requested_path
  launcher$resolved_path <- resolved$target_path
  launcher$source_path <- resolved$source_path
  launcher$html_output_path <- resolved$html_path
  launcher$rendered_from_source <- resolved$render

  if (isTRUE(browse)) {
    open_review_document_browser(launcher$url)
  }

  launcher
}
