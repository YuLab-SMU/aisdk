---
name: r_ocr
description: Use this skill when the user wants OCR or PDF text extraction with R only. Covers extracting text from images, scanned PDFs, and born-digital PDFs using tesseract, magick, and pdftools. Prefer this skill when avoiding Python dependencies.
---

# R OCR And PDF Workflow

Use this skill for R-first document handling.

## Package Selection

- Use `tesseract` as the default OCR engine.
- Use `magick` to preprocess images before OCR when recognition quality is weak.
- Use `pdftools` first for born-digital PDFs and for rendering scanned PDF pages to images.
- Treat `rtesseract` as legacy compatibility only. Do not choose it for new work unless the project already depends on it.

Read `references/packages.md` when you need the package comparison table or installation notes.

## Default Workflow

1. Identify the input type.
2. If the input is an image, OCR it with `tesseract`. Add `magick` preprocessing if the image is noisy, skewed, low-contrast, or padded.
3. If the input is a PDF, try `pdftools::pdf_text()` first.
4. If PDF text extraction is sparse or empty, treat it as scanned PDF: render pages with `pdftools`, then OCR page images with `tesseract`.
5. Return clean text, page-aware output, and note limitations if OCR quality is uncertain.

## Commands

- Image OCR: run `scripts/ocr_image.R`
- PDF text or OCR: run `scripts/ocr_pdf.R`

## Notes

- `magick::image_ocr()` is acceptable for quick work, but prefer `tesseract::ocr()` when you need explicit engine and language control.
- For multilingual OCR, construct a `tesseract::tesseract(language = ...)` engine explicitly.
- If system libraries are missing, say so clearly instead of pretending OCR succeeded.
