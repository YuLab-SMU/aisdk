#' @title Image Model Utilities
#' @description Internal helpers for image generation models.
#' @name image_model
#' @keywords internal
NULL

#' @keywords internal
image_extension_from_media_type <- function(media_type) {
  switch(tolower(media_type %||% ""),
    "image/png" = "png",
    "image/jpeg" = "jpg",
    "image/jpg" = "jpg",
    "image/webp" = "webp",
    "image/gif" = "gif",
    "bin"
  )
}

#' @keywords internal
ensure_output_dir <- function(output_dir) {
  if (is.null(output_dir) || !nzchar(output_dir)) {
    output_dir <- tempdir()
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir
}

#' @keywords internal
write_image_artifact <- function(bytes = NULL,
                                 media_type = "application/octet-stream",
                                 output_dir = tempdir(),
                                 prefix = "image") {
  output_dir <- ensure_output_dir(output_dir)
  ext <- image_extension_from_media_type(media_type)
  path <- tempfile(pattern = paste0(prefix, "_"), tmpdir = output_dir, fileext = paste0(".", ext))

  if (!is.null(bytes)) {
    writeBin(bytes, path)
  }

  list(
    path = path,
    media_type = media_type,
    bytes = bytes
  )
}

#' @keywords internal
finalize_image_artifacts <- function(images, output_dir = tempdir(), prefix = "image") {
  output_dir <- ensure_output_dir(output_dir)

  if (is.null(images)) {
    return(list())
  }

  lapply(seq_along(images), function(i) {
    image <- images[[i]]
    if (!is.null(image$path) && file.exists(image$path)) {
      return(image)
    }

    artifact <- write_image_artifact(
      bytes = image$bytes %||% NULL,
      media_type = image$media_type %||% "application/octet-stream",
      output_dir = output_dir,
      prefix = paste0(prefix, "_", i)
    )

    utils::modifyList(artifact, image)
  })
}

#' @keywords internal
materialize_image_upload <- function(image,
                                     output_dir = tempdir(),
                                     prefix = "upload") {
  output_dir <- ensure_output_dir(output_dir)

  block <- NULL
  if (is.list(image) && is_content_block(image)) {
    block <- coerce_legacy_content_block(image)
  } else if (is.character(image) && length(image) == 1) {
    block <- input_image(image)
  } else {
    rlang::abort("`image` must be a local file path, data URI, or input_image() block.")
  }

  if (!identical(block$type, "input_image")) {
    rlang::abort("`image` must resolve to an input_image block.")
  }

  if (identical(block$source$kind, "url")) {
    rlang::abort("This provider requires a local file path or data URI for image uploads; remote URLs are not supported here.")
  }

  if (identical(block$source$kind, "file")) {
    return(block$value)
  }

  ext <- image_extension_from_media_type(block$media_type %||% "application/octet-stream")
  path <- tempfile(pattern = paste0(prefix, "_"), tmpdir = output_dir, fileext = paste0(".", ext))

  if (!requireNamespace("base64enc", quietly = TRUE)) {
    rlang::abort("Package `base64enc` is required for local image upload support.")
  }

  writeBin(base64enc::base64decode(sub("^data:[^;]+;base64,", "", block$value)), path)
  path
}

#' @keywords internal
normalize_image_input_for_json <- function(image) {
  block <- NULL
  if (is.list(image) && is_content_block(image)) {
    block <- coerce_legacy_content_block(image)
  } else if (is.character(image) && length(image) == 1) {
    block <- input_image(image)
  } else {
    rlang::abort("`image` must be a local file path, URL, data URI, or input_image() block.")
  }

  if (!identical(block$type, "input_image")) {
    rlang::abort("`image` must resolve to an input_image block.")
  }

  if (identical(block$source$kind, "url")) {
    return(block$value)
  }

  if (!requireNamespace("base64enc", quietly = TRUE)) {
    rlang::abort("Package `base64enc` is required for local image support.")
  }

  if (identical(block$source$kind, "file")) {
    return(base64enc::base64encode(block$value))
  }

  sub("^data:[^;]+;base64,", "", block$value)
}

#' @keywords internal
normalize_image_input_to_url_like <- function(image) {
  block <- NULL
  if (is.list(image) && is_content_block(image)) {
    block <- coerce_legacy_content_block(image)
  } else if (is.character(image) && length(image) == 1) {
    block <- input_image(image)
  } else {
    rlang::abort("`image` must be a local file path, URL, data URI, or input_image() block.")
  }

  if (!identical(block$type, "input_image")) {
    rlang::abort("`image` must resolve to an input_image block.")
  }

  if (identical(block$source$kind, "url") || identical(block$source$kind, "data_uri")) {
    return(block$value)
  }

  if (!requireNamespace("base64enc", quietly = TRUE)) {
    rlang::abort("Package `base64enc` is required for local image support.")
  }

  encoded <- base64enc::base64encode(block$value)
  paste0("data:", block$media_type, ";base64,", encoded)
}

#' @keywords internal
coerce_image_inputs <- function(images, arg = "`image`") {
  if (is.null(images)) {
    rlang::abort(paste0(arg, " must not be NULL."))
  }

  if (is.list(images) && is_content_block(images)) {
    return(list(images))
  }

  if (is.character(images)) {
    if (!length(images)) {
      rlang::abort(paste0(arg, " must contain at least one image input."))
    }
    return(as.list(images))
  }

  if (is.list(images)) {
    if (!length(images)) {
      rlang::abort(paste0(arg, " must contain at least one image input."))
    }
    return(images)
  }

  rlang::abort(paste0(arg, " must be a local file path, data URI, URL, input_image() block, or a list of those values."))
}
