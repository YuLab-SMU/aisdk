# R OCR Package Notes

## Recommended Choice

Use `tesseract` by default. It is the standard OCR package in R and wraps the Tesseract OCR engine from Google.

## Package Matrix

| Package | Primary role | When to use | Notes |
|---|---|---|---|
| `tesseract` | OCR engine | Default image OCR and page OCR | Best default choice for new work |
| `magick` | Image preprocessing plus convenience OCR | Clean up images before OCR, or do quick OCR in one package | Often improves OCR quality when paired with `tesseract` |
| `pdftools` | PDF text extraction and page rendering | Extract text from born-digital PDFs or render scanned pages for OCR | Not itself an OCR engine |
| `rtesseract` | Legacy OCR wrapper | Only when existing code already uses it | Prefer `tesseract` for new code |

## Practical Guidance

### `tesseract`

Use for direct OCR on images.

```r
library(tesseract)
text <- ocr("image.png")
cat(text)
```

Use an explicit engine when language matters.

```r
library(tesseract)
eng <- tesseract(language = "eng")
text <- ocr("image.png", engine = eng)
```

### `magick`

Use before OCR when the image is skewed, noisy, or low-contrast.

```r
library(magick)
library(tesseract)

img <- image_read("document.jpg")
img <- image_convert(img, colorspace = "gray")
img <- image_trim(img)
text <- ocr(img)
```

Quick OCR is also possible through `magick`.

```r
library(magick)
text <- image_ocr(image_read("document.jpg"))
```

### `pdftools`

Use for regular text PDFs first.

```r
library(pdftools)
pages <- pdf_text("document.pdf")
cat(pages[[1]])
```

For scanned PDFs, render each page to an image and OCR the images.

```r
library(pdftools)
library(tesseract)

pngs <- pdf_convert("scanned.pdf", dpi = 300)
text <- vapply(pngs, ocr, character(1))
cat(text, sep = "\n\n")
```

### `rtesseract`

This is an older package. Keep it only for compatibility with legacy codebases.

## System Dependencies

- `tesseract` requires the system Tesseract engine and language data to be available.
- `magick` may require ImageMagick system libraries depending on platform.
- `pdftools` relies on Poppler.

If OCR quality is poor, say whether the likely bottleneck is image quality, missing language data, or scanned PDF rendering quality.
