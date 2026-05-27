#' @title Multimodal Helpers
#' @description
#' Helper functions for constructing multimodal messages (text and images).
#' @name multimodal
NULL

#' @title Create Text Content
#' @description
#' Creates a text content object for a multimodal message.
#' This is kept for backward compatibility and returns the provider-neutral
#' text block used by multimodal providers.
#' @param text The text string.
#' @return A list representing the text content.
#' @export
content_text <- function(text) {
  input_text(text)
}

#' @title Create Image Content
#' @description
#' Creates an image content object for a multimodal message.
#' Automatically handles URLs, data URIs, raw bytes, and local files.
#' This is kept for backward compatibility and returns the provider-neutral
#' image block used by multimodal providers.
#' @param image_path Path to a local file or a URL.
#' @param media_type MIME type of the image (e.g., "image/jpeg", "image/png").
#'   If NULL, attempts to guess from the file extension.
#' @param detail Image detail suitable for some models (e.g., "auto", "low", "high").
#' @return A provider-neutral image block.
#' @export
content_image <- function(image_path, media_type = "auto", detail = "auto") {
  if (identical(media_type, "auto")) {
    media_type <- NULL
  }

  input_image(image_path, media_type = media_type, detail = detail)
}
