args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Usage: Rscript ocr_pdf.R <pdf_path> [language]", call. = FALSE)
}

pdf_path <- args[[1]]
language <- if (length(args) >= 2) args[[2]] else "eng"

if (!requireNamespace("pdftools", quietly = TRUE)) {
  stop("Package 'pdftools' is required.", call. = FALSE)
}

clean_text <- function(text) {
  lines <- trimws(strsplit(text %||% "", "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  paste(lines, collapse = "\n")
}

`%||%` <- function(x, y) if (is.null(x)) y else x

pages <- tryCatch(
  pdftools::pdf_text(pdf_path),
  error = function(e) character(0)
)

clean_pages <- vapply(pages, clean_text, character(1))

if (length(clean_pages) > 0 && any(nzchar(clean_pages))) {
  for (i in seq_along(clean_pages)) {
    cat(sprintf("[page %d]\n", i))
    cat(clean_pages[[i]])
    cat("\n")
    if (i < length(clean_pages)) {
      cat("\n")
    }
  }
  quit(save = "no", status = 0)
}

if (!requireNamespace("tesseract", quietly = TRUE)) {
  stop("Package 'tesseract' is required for scanned PDF OCR fallback.", call. = FALSE)
}

png_dir <- tempfile("ocr_pdf_pages_")
dir.create(png_dir, recursive = TRUE)
on.exit(unlink(png_dir, recursive = TRUE), add = TRUE)

png_files <- pdftools::pdf_convert(
  pdf_path,
  dpi = 300,
  filenames = file.path(png_dir, "page")
)

engine <- tesseract::tesseract(language = language)

for (i in seq_along(png_files)) {
  cat(sprintf("[page %d]\n", i))
  cat(tesseract::ocr(png_files[[i]], engine = engine))
  cat("\n")
  if (i < length(png_files)) {
    cat("\n")
  }
}
