#' @title Provider-Neutral Content Blocks
#' @description
#' Helpers for constructing and validating provider-neutral multimodal content
#' blocks that can later be translated into provider-specific payloads.
#' @name content_blocks
NULL

#' @title Create Input Text Block
#' @description
#' Create a provider-neutral text block for multimodal message content.
#' @param text Text content.
#' @return A list representing an input text block.
#' @export
input_text <- function(text) {
  if (!is.character(text) || length(text) != 1 || is.na(text)) {
    rlang::abort("`text` must be a single non-missing character string.")
  }

  list(
    type = "input_text",
    text = text
  )
}

#' @title Create Input Image Block
#' @description
#' Create a provider-neutral image block for multimodal message content.
#' Supports local files, remote URLs, and data URIs.
#' @param x Image source as a local file path, remote URL, or data URI.
#' @param media_type MIME type of the image. If `NULL` or `"auto"`, attempts to
#'   infer from the source.
#' @param detail Optional image detail hint for providers that support it.
#' @return A list representing an input image block.
#' @export
input_image <- function(x, media_type = "auto", detail = "auto") {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    rlang::abort("`x` must be a single non-empty character string.")
  }

  source_kind <- detect_image_source_kind(x)
  resolved_media_type <- resolve_image_media_type(x, media_type)

  block <- list(
    type = "input_image",
    source = list(kind = source_kind),
    value = x,
    media_type = resolved_media_type
  )

  if (!is.null(detail) && !identical(detail, "auto")) {
    block$detail <- detail
  }

  block
}

#' @keywords internal
detect_image_source_kind <- function(x) {
  if (grepl("^https?://", x, ignore.case = TRUE)) {
    return("url")
  }
  if (grepl("^data:", x, ignore.case = TRUE)) {
    return("data_uri")
  }
  if (file.exists(x)) {
    return("file")
  }

  rlang::abort(
    paste0(
      "`x` must be a local file path, http(s) URL, or data URI. Got: ",
      x
    )
  )
}

#' @keywords internal
resolve_image_media_type <- function(x, media_type = "auto") {
  if (!is.null(media_type) && !identical(media_type, "auto")) {
    return(media_type)
  }

  if (grepl("^data:", x, ignore.case = TRUE)) {
    matched <- sub("^data:([^;]+);.*$", "\\1", x, perl = TRUE)
    if (!identical(matched, x) && nzchar(matched)) {
      return(matched)
    }
  }

  ext <- tolower(tools::file_ext(x))
  switch(ext,
    "png" = "image/png",
    "jpg" = "image/jpeg",
    "jpeg" = "image/jpeg",
    "gif" = "image/gif",
    "webp" = "image/webp",
    "bmp" = "image/bmp",
    "tif" = "image/tiff",
    "tiff" = "image/tiff",
    "image/jpeg"
  )
}

#' @keywords internal
is_content_block <- function(x) {
  is.list(x) && !is.null(x$type) && is.character(x$type) && length(x$type) == 1
}

#' @keywords internal
supported_content_block_type <- function(type) {
  type %in% c("input_text", "input_image", "text", "image_url")
}

#' @keywords internal
is_supported_content_block <- function(x) {
  is_content_block(x) && supported_content_block_type(x$type)
}

#' @keywords internal
is_content_block_list <- function(x) {
  is.list(x) && length(x) > 0 && all(vapply(x, is_supported_content_block, logical(1)))
}

#' @keywords internal
coerce_legacy_content_block <- function(block) {
  if (!is_content_block(block)) {
    rlang::abort("Invalid content block: expected a list with a `type` field.")
  }

  if (identical(block$type, "input_text") || identical(block$type, "input_image")) {
    if (identical(block$type, "input_image") && (is.null(block$value) || !nzchar(block$value %||% ""))) {
      block$value <- block$url %||% block$path %||% block$data_uri %||% NULL
    }
    if (identical(block$type, "input_image") && is.null(block$source$kind)) {
      block$source <- list(kind = block$source %||% detect_image_source_kind(block$value %||% ""))
    }
    if (identical(block$type, "input_image") && is.null(block$media_type) && !is.null(block$value)) {
      block$media_type <- resolve_image_media_type(block$value, "auto")
    }
    return(block)
  }

  if (identical(block$type, "text")) {
    return(input_text(block$text %||% ""))
  }

  if (identical(block$type, "image_url")) {
    image_url <- block$image_url %||% list()
    return(input_image(
      image_url$url %||% "",
      media_type = resolve_image_media_type(image_url$url %||% "", "auto"),
      detail = image_url$detail %||% "auto"
    ))
  }

  rlang::abort(paste0("Unsupported content block type: ", block$type))
}

#' @keywords internal
normalize_content_blocks <- function(content) {
  if (is.null(content)) {
    return(list())
  }

  if (is.character(content)) {
    if (length(content) != 1 || is.na(content)) {
      rlang::abort("Character content must be a single non-missing string.")
    }
    return(list(input_text(content)))
  }

  if (!is.list(content)) {
    rlang::abort("Content must be a character string or a list of content blocks.")
  }

  if (!length(content)) {
    return(list())
  }

  if (!all(vapply(content, is_content_block, logical(1)))) {
    rlang::abort("Content lists must contain content block objects with a `type` field.")
  }

  blocks <- lapply(content, coerce_legacy_content_block)
  validate_content_blocks(blocks)
  blocks
}

#' @keywords internal
validate_content_blocks <- function(blocks) {
  if (!is.list(blocks)) {
    rlang::abort("`blocks` must be a list.")
  }

  for (block in blocks) {
    if (!is_content_block(block)) {
      rlang::abort("Each content block must be a list with a `type` field.")
    }

    if (identical(block$type, "input_text")) {
      if (!is.character(block$text) || length(block$text) != 1 || is.na(block$text)) {
        rlang::abort("`input_text` blocks must contain a single non-missing `text` field.")
      }
    } else if (identical(block$type, "input_image")) {
      if (!is.list(block$source) || !is.character(block$source$kind) || length(block$source$kind) != 1) {
        rlang::abort("`input_image` blocks must contain `source$kind`.")
      }
      if (!block$source$kind %in% c("file", "url", "data_uri")) {
        rlang::abort("`input_image` source kind must be one of: file, url, data_uri.")
      }
      if (!is.character(block$value) || length(block$value) != 1 || is.na(block$value) || !nzchar(block$value)) {
        rlang::abort("`input_image` blocks must contain a non-empty `value`.")
      }
      if (!is.character(block$media_type) || length(block$media_type) != 1 || !nzchar(block$media_type)) {
        rlang::abort("`input_image` blocks must contain a non-empty `media_type`.")
      }
    } else {
      rlang::abort(paste0("Unsupported normalized content block type: ", block$type))
    }
  }

  invisible(blocks)
}

#' @keywords internal
content_blocks_to_text <- function(content, arg_name = "content") {
  blocks <- normalize_content_blocks(content)
  non_text <- Filter(function(block) !identical(block$type, "input_text"), blocks)
  if (length(non_text) > 0) {
    rlang::abort(paste0("`", arg_name, "` must contain only text blocks in this context."))
  }

  paste(vapply(blocks, function(block) block$text, character(1)), collapse = "\n")
}

#' @keywords internal
message_requires_capability <- function(message, capability = "vision_input") {
  content <- message$content %||% NULL
  if (is.null(content) || is.character(content)) {
    return(FALSE)
  }

  blocks <- tryCatch(
    normalize_content_blocks(content),
    error = function(e) NULL
  )
  if (is.null(blocks) || capability != "vision_input") {
    return(FALSE)
  }

  any(vapply(blocks, function(block) identical(block$type, "input_image"), logical(1)))
}

#' @keywords internal
messages_require_capability <- function(messages, capability = "vision_input") {
  if (!is.list(messages)) {
    return(FALSE)
  }

  any(vapply(messages, message_requires_capability, logical(1), capability = capability))
}

#' @keywords internal
validate_multimodal_messages <- function(messages, model) {
  if (!messages_require_capability(messages, "vision_input")) {
    return(invisible(messages))
  }

  explicit_false <- identical(model$capabilities$vision_input, FALSE) ||
    (is.null(model$capabilities$vision_input) && identical(model$capabilities$vision, FALSE))

  if (explicit_false) {
    rlang::abort(
      paste0(
        "Model `", model$provider, ":", model$model_id,
        "` does not advertise multimodal image input support."
      )
    )
  }

  invisible(messages)
}

#' @keywords internal
validate_model_messages <- function(model, messages) {
  validate_multimodal_messages(messages, model)
}
