#' @title Multimodal Helpers
#' @description
#' Helper functions for constructing multimodal messages (text and images).
#' @name multimodal
NULL

#' @title Create Text Content
#' @description
#' Creates a text content object for a multimodal message.
#' @param text The text string.
#' @return A list representing the text content.
#' @export
content_text <- function(text) {
  list(
    type = "text",
    text = text
  )
}

#' @title Create Image Content
#' @description
#' Creates an image content object for a multimodal message.
#' automatically handles URLs and local files (converted to base64).
#' @param image_path Path to a local file or a URL.
#' @param media_type MIME type of the image (e.g., "image/jpeg", "image/png").
#'   If NULL, attempts to guess from the file extension.
#' @param detail Image detail suitable for some models (e.g., "auto", "low", "high").
#' @return A list representing the image content in OpenAI-compatible format.
#' @export
content_image <- function(image_path, media_type = "auto", detail = "auto") {
  is_url <- grepl("^https?://", image_path, ignore.case = TRUE)
  
  if (is_url) {
    url_val <- image_path
  } else {
    # Check file exists
    if (!file.exists(image_path)) {
      rlang::abort(paste0("File not found: ", image_path))
    }
    
    # Guess mime type if needed
    if (is.null(media_type) || media_type == "auto") {
      ext <- tolower(tools::file_ext(image_path))
      media_type <- switch(ext,
        "png" = "image/png",
        "jpg" = "image/jpeg",
        "jpeg" = "image/jpeg",
        "gif" = "image/gif",
        "webp" = "image/webp",
        "image/jpeg" # Default fallback
      )
    }
    
    # Read and encode
    # Check if base64enc is available
    if (!requireNamespace("base64enc", quietly = TRUE)) {
      rlang::abort("Package 'base64enc' is required for local image support. Please install it.")
    }
    
    encoded <- base64enc::base64encode(image_path)
    url_val <- paste0("data:", media_type, ";base64,", encoded)
  }
  
  res <- list(
    type = "image_url",
    image_url = list(
      url = url_val
    )
  )
  
  if (!is.null(detail) && detail != "auto") {
    res$image_url$detail <- detail
  }
  
  res
}
