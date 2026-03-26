#' @title Channel Document Ingest
#' @description
#' Helpers for extracting, chunking, and summarizing inbound document attachments
#' before they are injected into chat context.
#' @name channel_documents
NULL

channel_chunk_text <- function(text, max_chars = 4000, max_chunks = 8) {
  if (is.null(text) || !nzchar(text)) {
    return(character(0))
  }

  paragraphs <- unlist(strsplit(text, "\n\\s*\n", perl = TRUE))
  paragraphs <- trimws(paragraphs)
  paragraphs <- paragraphs[nzchar(paragraphs)]
  if (length(paragraphs) == 0) {
    paragraphs <- trimws(unlist(strsplit(text, "\n", fixed = TRUE)))
    paragraphs <- paragraphs[nzchar(paragraphs)]
  }

  chunks <- character(0)
  current <- character(0)
  current_len <- 0L

  for (para in paragraphs) {
    para_len <- nchar(para)
    if (current_len > 0 && (current_len + para_len + 2L) > max_chars) {
      chunks <- c(chunks, paste(current, collapse = "\n\n"))
      current <- character(0)
      current_len <- 0L
      if (length(chunks) >= max_chunks) {
        break
      }
    }
    current <- c(current, para)
    current_len <- current_len + para_len + if (current_len > 0) 2L else 0L
  }

  if (length(chunks) < max_chunks && length(current) > 0) {
    chunks <- c(chunks, paste(current, collapse = "\n\n"))
  }

  chunks[seq_len(min(length(chunks), max_chunks))]
}

channel_document_summary <- function(text, max_chars = 1200) {
  if (is.null(text) || !nzchar(text)) {
    return(NULL)
  }

  lines <- trimws(unlist(strsplit(text, "\n", fixed = TRUE)))
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) {
    return(NULL)
  }

  # Prefer human-readable lines over pdf/xml noise.
  score_line <- function(line) {
    letters_ratio <- nchar(gsub("[^A-Za-z\u4e00-\u9fff]", "", line)) / max(1, nchar(line))
    penalty <- if (grepl("^(%PDF|<\\?|<rdf:|<x:|obj|endobj|stream|endstream)", line, ignore.case = TRUE)) 1 else 0
    letters_ratio - penalty
  }

  ranked <- lines[order(vapply(lines, score_line, numeric(1)), decreasing = TRUE)]
  joined <- paste(ranked, collapse = "\n")
  substr(joined, 1, max_chars)
}

channel_clean_extracted_text <- function(text) {
  text <- text %||% ""
  if (!nzchar(text)) {
    return("")
  }

  lines <- vapply(strsplit(text, "\n", fixed = TRUE)[[1]], trimws, character(1))
  lines <- lines[nzchar(lines)]
  paste(lines, collapse = "\n")
}

channel_pdf_pages_have_text <- function(pages) {
  if (length(pages %||% list()) == 0) {
    return(FALSE)
  }

  any(vapply(pages, function(page) {
    nzchar(trimws(page$text %||% ""))
  }, logical(1)))
}

channel_reflow_pdf_data_page <- function(words, line_tol = 2) {
  if (is.null(words) || !is.data.frame(words) || nrow(words) == 0) {
    return("")
  }

  required_cols <- c("x", "y", "text")
  if (!all(required_cols %in% names(words))) {
    return("")
  }

  words <- words[!is.na(words$text) & nzchar(trimws(words$text)), , drop = FALSE]
  if (nrow(words) == 0) {
    return("")
  }

  words <- words[order(words$y, words$x), , drop = FALSE]
  line_ids <- integer(nrow(words))
  current_line <- 1L
  anchor_y <- words$y[[1]]
  line_ids[[1]] <- current_line

  if (nrow(words) > 1) {
    for (i in 2:nrow(words)) {
      if (abs(words$y[[i]] - anchor_y) > line_tol) {
        current_line <- current_line + 1L
        anchor_y <- words$y[[i]]
      }
      line_ids[[i]] <- current_line
    }
  }

  lines <- lapply(split(words, line_ids), function(line_words) {
    line_words <- line_words[order(line_words$x), , drop = FALSE]
    paste(trimws(line_words$text), collapse = " ")
  })

  channel_clean_extracted_text(paste(unlist(lines, use.names = FALSE), collapse = "\n"))
}

channel_extract_pdf_pages_r <- function(path) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    return(list(page_count = 0L, pages = list(), extractor = NULL))
  }

  texts <- tryCatch(
    pdftools::pdf_text(path),
    error = function(e) character(0)
  )
  word_pages <- tryCatch(
    pdftools::pdf_data(path),
    error = function(e) list()
  )

  page_count <- max(length(texts), length(word_pages))
  if (page_count == 0) {
    return(list(page_count = 0L, pages = list(), extractor = "pdftools"))
  }

  pages <- lapply(seq_len(page_count), function(index) {
    raw_text <- if (length(texts) >= index) {
      channel_clean_extracted_text(texts[[index]])
    } else {
      ""
    }
    layout_text <- if (length(word_pages) >= index) {
      channel_reflow_pdf_data_page(word_pages[[index]])
    } else {
      ""
    }
    text <- if (nchar(layout_text, type = "chars") > nchar(raw_text, type = "chars")) {
      layout_text
    } else {
      raw_text
    }

    list(page = index, text = text)
  })

  list(page_count = length(pages), pages = pages, extractor = "pdftools")
}

channel_locate_pdf_python_script <- function() {
  pkg_name <- tryCatch(utils::packageName(), error = function(e) "")
  candidates <- c(
    if (nzchar(pkg_name)) {
      system.file("extdata", "channel_scripts", "extract_pdf_text.py", package = pkg_name)
    } else {
      ""
    },
    file.path("inst", "extdata", "channel_scripts", "extract_pdf_text.py")
  )
  candidates <- unique(candidates[nzchar(candidates)])
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) {
    return("")
  }

  hit[[1]]
}

channel_extract_pdf_pages_python <- function(path) {
  script_path <- channel_locate_pdf_python_script()
  python_bin <- Sys.which("python3")
  if (!nzchar(script_path) || !nzchar(python_bin)) {
    return(list(page_count = 0L, pages = list(), extractor = NULL))
  }

  if (!file.exists(script_path)) {
    return(list(page_count = 0L, pages = list(), extractor = NULL))
  }

  result <- tryCatch(
    system2(python_bin, c(script_path, path), stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  )
  if (length(result) == 0) {
    return(list(page_count = 0L, pages = list(), extractor = "python"))
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(paste(result, collapse = "\n"), simplifyVector = FALSE),
    error = function(e) list(page_count = 0L, pages = list(), extractor = "python")
  )
  parsed$extractor <- parsed$extractor %||% "python"
  parsed
}

channel_extract_pdf_pages <- function(path) {
  r_extracted <- channel_extract_pdf_pages_r(path)
  if (channel_pdf_pages_have_text(r_extracted$pages)) {
    return(r_extracted)
  }

  python_extracted <- channel_extract_pdf_pages_python(path)
  if (channel_pdf_pages_have_text(python_extracted$pages)) {
    return(python_extracted)
  }

  if ((r_extracted$page_count %||% 0L) > 0L) {
    return(r_extracted)
  }

  python_extracted
}

channel_build_document_record <- function(attachment, max_chunks = 6) {
  local_path <- attachment$local_path %||% NULL
  preview <- attachment$preview %||% NULL
  ext <- tolower(tools::file_ext(local_path %||% ""))

  pages <- list()
  source_text <- preview
  extractor <- NULL
  if (!is.null(local_path) && ext == "pdf") {
    extracted <- channel_extract_pdf_pages(local_path)
    pages <- extracted$pages %||% list()
    extractor <- extracted$extractor %||% NULL
    page_text <- vapply(pages, function(page) {
      page$text %||% ""
    }, character(1))
    extracted_text <- paste(page_text[nzchar(page_text)], collapse = "\n\n")
    if (nzchar(extracted_text)) {
      source_text <- extracted_text
    }
  }

  chunks <- if (!is.null(source_text)) {
    channel_chunk_text(source_text, max_chars = 1200, max_chunks = max_chunks)
  } else {
    character(0)
  }

  list(
    document_id = paste0("doc_", digest::digest(c(
      attachment$file_key %||% "",
      attachment$file_name %||% "",
      local_path %||% ""
    ), algo = "sha256")),
    type = attachment$type %||% "unknown",
    file_name = attachment$file_name %||% basename(local_path %||% "unknown"),
    local_path = local_path,
    preview = preview,
    summary = channel_document_summary(source_text),
    pages = pages,
    extractor = extractor,
    chunks = as.list(chunks),
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  )
}

channel_format_document_context <- function(documents, max_docs = 1, max_chunks = 3) {
  docs <- documents %||% list()
  if (length(docs) == 0) {
    return(NULL)
  }

  selected <- tail(docs, max_docs)
  lines <- c("[document_context_begin]")
  for (doc in selected) {
    lines <- c(
      lines,
      sprintf("[document_name: %s]", doc$file_name %||% "unknown"),
      sprintf("[document_path: %s]", doc$local_path %||% "unknown")
    )
    if (!is.null(doc$summary) && nzchar(doc$summary)) {
      lines <- c(lines, "[document_summary_begin]", doc$summary, "[document_summary_end]")
    }
    chunks <- unlist(doc$chunks %||% list(), use.names = FALSE)
    if (length(chunks) > 0) {
      lines <- c(lines, "[document_chunks_begin]")
      for (chunk in utils::head(chunks, max_chunks)) {
        lines <- c(lines, chunk, "---")
      }
      lines <- c(lines, "[document_chunks_end]")
    }
  }
  lines <- c(lines, "[document_context_end]")
  paste(lines, collapse = "\n")
}
