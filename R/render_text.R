#' @title Render Markdown Text
#' @description
#' Render markdown-formatted text in the console with beautiful styling.
#' This function uses the same rendering engine as the streaming output,
#' supporting headers, lists, code blocks, and other markdown elements.
#'
#' @param text A character string containing markdown text, or a GenerateResult object.
#' @return NULL (invisibly)
#' @export
#' @examples
#' \dontrun{
#' # Render simple text
#' render_text("# Hello\n\nThis is **bold** text.")
#'
#' # Render with code block
#' render_text("Here is some R code:\n\n```r\nx <- 1:10\nmean(x)\n```")
#' }
render_text <- function(text) {
  # Handle GenerateResult objects
  if (inherits(text, "GenerateResult")) {
    text <- text$text
  }

  if (is.null(text) || length(text) == 0) {
    return(invisible(NULL))
  }

  # Ensure text is a single string
  if (length(text) > 1) {
    text <- paste(text, collapse = "\n")
  }

  # Create renderer
  renderer <- create_markdown_stream_renderer()

  # Process the text
  # We pass done=TRUE to flush the buffer at the end
  # Process the text
  renderer$process_chunk(text, done = FALSE)
  # Flush results
  renderer$process_chunk(NULL, done = TRUE)

  invisible(NULL)
}
