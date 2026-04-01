#' @title Content Translation Helpers
#' @description
#' Internal helpers for translating provider-neutral content blocks into
#' provider-specific message payloads.
#' @name content_translation
#' @keywords internal
NULL

#' @keywords internal
translate_message_content <- function(content, target = c("openai_chat", "openai_responses", "gemini", "anthropic")) {
  target <- match.arg(target)

  if (is.null(content)) {
    return(content)
  }

  if (is.character(content)) {
    blocks <- normalize_content_blocks(content)
  } else if (is_content_block(content) && is_supported_content_block(content)) {
    blocks <- normalize_content_blocks(list(content))
  } else if (is_supported_block_payload(content)) {
    blocks <- normalize_content_blocks(content)
  } else {
    return(content)
  }

  switch(target,
    openai_chat = translate_blocks_openai_chat(blocks),
    openai_responses = translate_blocks_openai_responses(blocks),
    gemini = translate_blocks_gemini(blocks),
    anthropic = translate_blocks_anthropic(blocks)
  )
}

#' @keywords internal
is_supported_block_payload <- function(content) {
  is_content_block_list(content)
}

#' @keywords internal
translate_blocks_openai_chat <- function(blocks) {
  lapply(blocks, function(block) {
    if (identical(block$type, "input_text")) {
      return(list(type = "text", text = block$text))
    }

    image_payload <- list(
      type = "image_url",
      image_url = list(url = block_to_url(block))
    )
    if (!is.null(block$detail)) {
      image_payload$image_url$detail <- block$detail
    }
    image_payload
  })
}

#' @keywords internal
translate_blocks_openai_responses <- function(blocks) {
  lapply(blocks, function(block) {
    if (identical(block$type, "input_text")) {
      return(list(type = "input_text", text = block$text))
    }

    image_payload <- list(
      type = "input_image",
      image_url = block_to_url(block)
    )
    if (!is.null(block$detail)) {
      image_payload$detail <- block$detail
    }
    image_payload
  })
}

#' @keywords internal
translate_blocks_gemini <- function(blocks) {
  lapply(blocks, function(block) {
    if (identical(block$type, "input_text")) {
      return(list(text = block$text))
    }

    if (identical(block$source$kind, "url")) {
      return(list(
        fileData = list(
          mimeType = block$media_type,
          fileUri = block$value
        )
      ))
    }

    data <- block_to_base64(block)
    list(
      inlineData = list(
        mimeType = block$media_type,
        data = data
      )
    )
  })
}

#' @keywords internal
translate_blocks_anthropic <- function(blocks) {
  lapply(blocks, function(block) {
    if (identical(block$type, "input_text")) {
      return(list(type = "text", text = block$text))
    }

    if (identical(block$source$kind, "url")) {
      return(list(
        type = "image",
        source = list(
          type = "url",
          url = block$value
        )
      ))
    }

    list(
      type = "image",
      source = list(
        type = "base64",
        media_type = block$media_type,
        data = block_to_base64(block)
      )
    )
  })
}

#' @keywords internal
block_to_url <- function(block) {
  if (identical(block$source$kind, "url") || identical(block$source$kind, "data_uri")) {
    return(block$value)
  }

  paste0("data:", block$media_type, ";base64,", block_to_base64(block))
}

#' @keywords internal
block_to_base64 <- function(block) {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    rlang::abort("Package `base64enc` is required for local multimodal image support.")
  }

  if (identical(block$source$kind, "file")) {
    return(base64enc::base64encode(block$value))
  }

  if (identical(block$source$kind, "data_uri")) {
    return(sub("^data:[^;]+;base64,", "", block$value, ignore.case = TRUE))
  }

  rlang::abort(paste0("Cannot convert image source kind `", block$source$kind, "` to base64."))
}
