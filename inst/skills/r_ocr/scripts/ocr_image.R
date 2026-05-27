args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Usage: Rscript ocr_image.R <image_path> [language]", call. = FALSE)
}

image_path <- args[[1]]
language <- if (length(args) >= 2) args[[2]] else "eng"

if (!requireNamespace("tesseract", quietly = TRUE)) {
  stop("Package 'tesseract' is required.", call. = FALSE)
}

engine <- tesseract::tesseract(language = language)

ocr_text <- function(path, engine) {
  if (requireNamespace("magick", quietly = TRUE)) {
    image <- magick::image_read(path)
    image <- magick::image_convert(image, colorspace = "gray")
    image <- magick::image_trim(image)
    image <- magick::image_deskew(image, threshold = 40)
    return(tesseract::ocr(image, engine = engine))
  }

  tesseract::ocr(path, engine = engine)
}

cat(ocr_text(image_path, engine))
