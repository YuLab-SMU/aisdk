test_that("vignette documents with ai chunks register the ai engine", {
  doc_files <- list.files(
    "vignettes",
    pattern = "\\.(Rmd|qmd)$",
    full.names = TRUE,
    recursive = TRUE
  )

  ai_docs <- Filter(function(path) {
    lines <- readLines(path, warn = FALSE)
    any(grepl("^```\\{ai[ ,}]", lines)) ||
      any(grepl("^#\\|\\s*engine\\s*:\\s*ai\\s*$", lines))
  }, doc_files)

  missing_registration <- Filter(function(path) {
    !any(grepl("register_ai_engine\\s*\\(", readLines(path, warn = FALSE)))
  }, ai_docs)

  expect_equal(
    length(missing_registration),
    0,
    info = paste("Missing register_ai_engine() in:", paste(missing_registration, collapse = ", "))
  )
})
