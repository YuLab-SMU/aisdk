#!/usr/bin/env python3

import json
import sys

from pypdf import PdfReader
import pdfplumber


def clean_text(text: str) -> str:
    if not text:
        return ""
    return "\n".join(line.rstrip() for line in text.splitlines()).strip()


def extract_with_pypdf(path: str):
    reader = PdfReader(path)
    pages = []
    for index, page in enumerate(reader.pages, 1):
      try:
          text = page.extract_text() or ""
      except Exception:
          text = ""
      pages.append({"page": index, "text": clean_text(text)})
    return pages


def extract_with_pdfplumber(path: str):
    pages = []
    with pdfplumber.open(path) as pdf:
        for index, page in enumerate(pdf.pages, 1):
            try:
                text = page.extract_text() or ""
            except Exception:
                text = ""
            pages.append({"page": index, "text": clean_text(text)})
    return pages


def main():
    if len(sys.argv) != 2:
        raise SystemExit("Usage: extract_pdf_text.py <input.pdf>")

    path = sys.argv[1]
    pages = extract_with_pypdf(path)
    # Fall back page-by-page when pypdf extraction is too sparse.
    if not any(page["text"] for page in pages):
        pages = extract_with_pdfplumber(path)

    print(json.dumps({
        "page_count": len(pages),
        "pages": pages,
    }, ensure_ascii=False))


if __name__ == "__main__":
    main()
