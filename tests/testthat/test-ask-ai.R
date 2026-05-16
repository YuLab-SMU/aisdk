library(aisdk)

test_that("collect_ai_context formats explicit error context", {
  ctx <- collect_ai_context(
    script = "x <- missing_object",
    error = "object 'missing_object' not found",
    traceback = "1: eval(expr)",
    warnings = "package was built under newer R",
    include = c("error", "traceback", "warnings", "script"),
    max_context_chars = Inf
  )

  expect_s3_class(ctx, "aisdk_ai_context")
  text <- aisdk:::format_ai_context(ctx)

  expect_match(text, "[last_error_begin]", fixed = TRUE)
  expect_match(text, "missing_object", fixed = TRUE)
  expect_match(text, "[traceback_begin]", fixed = TRUE)
  expect_match(text, "[warnings_begin]", fixed = TRUE)
  expect_match(text, "[script_context_begin]", fixed = TRUE)
  expect_match(text, "x <- missing_object", fixed = TRUE)
})

test_that("collect_ai_context reads R last.warning messages", {
  assign(
    "last.warning",
    list("installation of package 'confuns' had non-zero exit status" = quote(i.p())),
    envir = globalenv()
  )
  on.exit(rm("last.warning", envir = globalenv()), add = TRUE)

  ctx <- collect_ai_context(
    include = "warnings",
    max_error_age_secs = Inf
  )

  expect_match(ctx$warnings, "confuns", fixed = TRUE)
  expect_match(ctx$warnings, "non-zero exit status", fixed = TRUE)
  expect_match(ctx$warnings, "call: i.p", fixed = TRUE)
})

test_that("package-install warnings suppress stale geterrmessage context", {
  invisible(try(stop("attempt to use zero-length variable name"), silent = TRUE))
  assign(
    "last.warning",
    list("installation of package 'confuns' had non-zero exit status" = quote(i.p())),
    envir = globalenv()
  )
  on.exit(rm("last.warning", envir = globalenv()), add = TRUE)

  ctx <- collect_ai_context(
    include = c("error", "traceback", "warnings"),
    max_error_age_secs = Inf
  )

  expect_equal(ctx$error, "")
  expect_equal(ctx$traceback, "")
  expect_match(ctx$warnings, "confuns", fixed = TRUE)
  expect_match(ctx$stale_error, "zero-length variable name", fixed = TRUE)
})

test_that("format_ai_context includes ignored stale errors and recent commands", {
  ctx <- collect_ai_context(
    error = "",
    warnings = "installation of package 'confuns' had non-zero exit status",
    include = c("error", "warnings", "history"),
    include_history = FALSE
  )
  ctx$stale_error <- "Error: attempt to use zero-length variable name"
  ctx$history <- "devtools::install_github(repo=\"kueckelj/confuns\")"

  text <- aisdk:::format_ai_context(ctx)

  expect_match(text, "[stale_error_ignored_begin]", fixed = TRUE)
  expect_match(text, "zero-length variable name", fixed = TRUE)
  expect_match(text, "devtools::install_github", fixed = TRUE)
})

test_that("clear_error_context ignores current geterrmessage without replacing it", {
  invisible(try(stop("old failure"), silent = TRUE))

  clear_error_context()
  ctx <- collect_ai_context(
    include = c("error", "warnings"),
    max_error_age_secs = Inf
  )

  expect_equal(ctx$error, "")
  expect_equal(ctx$warnings, "")
  expect_false(grepl("__clear__", geterrmessage(), fixed = TRUE))
})

test_that("collect_ai_context reads script paths", {
  script_path <- tempfile(fileext = ".R")
  writeLines(c("library(stats)", "lm(mpg ~ wt, data = mtcars)"), script_path)
  on.exit(unlink(script_path), add = TRUE)

  ctx <- collect_ai_context(
    script = script_path,
    include = "script"
  )

  expect_equal(ctx$script$source, "file")
  expect_equal(ctx$script$path, normalizePath(script_path, winslash = "/", mustWork = FALSE))
  expect_match(ctx$script$contents, "lm\\(mpg ~ wt", fixed = FALSE)
})

test_that("format_ai_context respects max_context_chars", {
  ctx <- collect_ai_context(
    script = paste(rep("x <- 1", 100), collapse = "\n"),
    include = "script",
    max_context_chars = 80
  )

  text <- aisdk:::format_ai_context(ctx)
  expect_lte(nchar(text), 100)
  expect_match(text, "[truncated]", fixed = TRUE)
})

test_that("ask_ai show_context returns initial prompt without launching chat", {
  output <- capture.output({
    preview <- ask_ai(
      prompt = "Help me diagnose this",
      skill = "biotree",
      context = "This happened while installing KEGGREST.",
      script = "BiocManager::install('KEGGREST')",
      error = "there is no package called 'BiocGenerics'",
      traceback = "1: loadNamespace(...)",
      warnings = "package 'dbplyr' was built under R version 4.5.2",
      include = c("error", "traceback", "warnings", "script"),
      show_context = TRUE
    )
  })

  expect_s3_class(preview$context, "aisdk_ai_context")
  expect_match(preview$prompt, "@biotree", fixed = TRUE)
  expect_match(preview$prompt, "BiocGenerics", fixed = TRUE)
  expect_match(preview$prompt, "This happened while installing KEGGREST.", fixed = TRUE)
  expect_true(any(grepl("BiocGenerics", output, fixed = TRUE)))
})

test_that("console_send_user_message powers initial prompt turns", {
  model <- MockModel$new(list(list(
    text = "I can diagnose that.",
    tool_calls = NULL,
    finish_reason = "stop",
    usage = list(total_tokens = 10)
  )))
  session <- create_chat_session(model = model)
  app_state <- aisdk:::create_console_app_state(session, view_mode = "clean")

  capture.output({
    ok <- aisdk:::console_send_user_message(
      input = "Diagnose this error",
      session = session,
      stream = FALSE,
      app_state = app_state
    )
  })

  expect_true(ok)
  expect_equal(session$get_history()[[1]]$role, "user")
  expect_equal(session$get_history()[[1]]$content, "Diagnose this error")
  expect_equal(session$get_last_response(), "I can diagnose that.")
})

test_that("RStudio addin is registered", {
  addins_path <- system.file("rstudio", "addins.dcf", package = "aisdk")
  expect_true(file.exists(addins_path))

  addins <- read.dcf(addins_path)
  expect_true("ask_ai" %in% addins[, "Binding"])
})
