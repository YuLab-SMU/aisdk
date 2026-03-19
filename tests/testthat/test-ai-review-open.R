load_ai_review_open_env <- function() {
  env <- aisdk_test_env()

  files <- c(
    "project_memory.R",
    "ai_review_live.R",
    "ai_review_launch.R"
  )

  for (file in files) {
    source_local_aisdk_file(env, file)
  }

  env
}

test_that("resolve_open_review_document_target prefers existing html for source documents when render is FALSE", {
  env <- load_ai_review_open_env()
  project_root <- tempfile("open_review_source_")
  dir.create(project_root, recursive = TRUE)

  rmd_path <- file.path(project_root, "review.Rmd")
  html_path <- file.path(project_root, "review.html")
  writeLines("---\ntitle: review\n---", rmd_path)
  writeLines("<html><body>review</body></html>", html_path)

  resolved <- env$resolve_open_review_document_target(rmd_path, render = FALSE)

  expect_equal(resolved$requested_path, normalizePath(rmd_path, winslash = "/", mustWork = TRUE))
  expect_equal(resolved$target_path, normalizePath(html_path, winslash = "/", mustWork = TRUE))
  expect_equal(resolved$source_path, normalizePath(rmd_path, winslash = "/", mustWork = TRUE))
  expect_equal(resolved$html_path, normalizePath(html_path, winslash = "/", mustWork = TRUE))
  expect_false(resolved$render)
})

test_that("resolve_open_review_document_target uses the paired source when render is TRUE for html input", {
  env <- load_ai_review_open_env()
  project_root <- tempfile("open_review_html_")
  dir.create(project_root, recursive = TRUE)

  qmd_path <- file.path(project_root, "review.qmd")
  html_path <- file.path(project_root, "review.html")
  writeLines("---\ntitle: review\n---", qmd_path)
  writeLines("<html><body>review</body></html>", html_path)

  resolved <- env$resolve_open_review_document_target(html_path, render = TRUE)

  expect_equal(resolved$requested_path, normalizePath(html_path, winslash = "/", mustWork = TRUE))
  expect_equal(resolved$target_path, normalizePath(qmd_path, winslash = "/", mustWork = TRUE))
  expect_equal(resolved$source_path, normalizePath(qmd_path, winslash = "/", mustWork = TRUE))
  expect_equal(resolved$html_path, normalizePath(html_path, winslash = "/", mustWork = TRUE))
  expect_true(resolved$render)
})

test_that("open_review_document opens the connected review url instead of a raw file path", {
  env <- load_ai_review_open_env()
  project_root <- tempfile("open_review_launch_")
  dir.create(project_root, recursive = TRUE)

  html_path <- file.path(project_root, "review.html")
  writeLines("<html><body>review</body></html>", html_path)

  calls <- new.env(parent = emptyenv())
  env$review_document <- function(path, memory = NULL, host = "127.0.0.1",
                                  port = NULL, browse = FALSE) {
    calls$path <- path
    calls$browse <- browse
    structure(
      list(
        url = "http://127.0.0.1:39393/",
        base_url = "http://127.0.0.1:39393/__aisdk_review__",
        html_path = normalizePath(path, winslash = "/", mustWork = FALSE),
        stop = function() invisible(TRUE)
      ),
      class = "aisdk_review_launcher"
    )
  }
  env$open_review_document_browser <- function(url) {
    calls$url <- url
    invisible(TRUE)
  }

  launcher <- env$open_review_document(html_path, browse = TRUE)

  expect_equal(calls$path, normalizePath(html_path, winslash = "/", mustWork = TRUE))
  expect_false(isTRUE(calls$browse))
  expect_equal(calls$url, "http://127.0.0.1:39393/")
  expect_false(grepl("^file://", calls$url))
  expect_equal(launcher$resolved_path, normalizePath(html_path, winslash = "/", mustWork = TRUE))
})

test_that("open_review_document falls back to the source path when html output is missing", {
  env <- load_ai_review_open_env()
  project_root <- tempfile("open_review_missing_html_")
  dir.create(project_root, recursive = TRUE)

  rmd_path <- file.path(project_root, "review.Rmd")
  writeLines("---\ntitle: review\n---", rmd_path)

  calls <- new.env(parent = emptyenv())
  env$review_document <- function(path, memory = NULL, host = "127.0.0.1",
                                  port = NULL, browse = FALSE) {
    calls$path <- path
    structure(
      list(
        url = "http://127.0.0.1:39393/",
        base_url = "http://127.0.0.1:39393/__aisdk_review__",
        html_path = sub("\\.Rmd$", ".html", normalizePath(path, winslash = "/", mustWork = FALSE)),
        stop = function() invisible(TRUE)
      ),
      class = "aisdk_review_launcher"
    )
  }
  env$open_review_document_browser <- function(url) invisible(url)

  launcher <- env$open_review_document(rmd_path, render = FALSE, browse = FALSE)

  expect_equal(calls$path, normalizePath(rmd_path, winslash = "/", mustWork = TRUE))
  expect_equal(launcher$source_path, normalizePath(rmd_path, winslash = "/", mustWork = TRUE))
})
