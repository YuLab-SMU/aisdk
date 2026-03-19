load_ai_review_saveback_env <- function() {
  env <- aisdk_test_env()

  files <- c(
    "ai_review_contract.R",
    "ai_review_embedded_contract.R",
    "project_memory.R",
    "knitr_engine.R",
    "ai_review_live.R"
  )

  for (file in files) {
    source_local_aisdk_file(env, file)
  }

  env
}

seed_saveback_review <- function(memory, source_path, chunk_id = "chunk-saveback",
                                 updated_at = "2026-03-14 12:00:00") {
  memory$store_review(
    chunk_id = chunk_id,
    file_path = source_path,
    chunk_label = "saveback",
    prompt = "Create saveback_value",
    response = "Draft\n```r\nsaveback_value <- 1\n```",
    status = "pending",
    session_id = "saveback-session",
    review_mode = "required",
    runtime_mode = "static",
    execution_status = "not_run",
    final_code = "saveback_value <- 1"
  )
  memory$store_review_artifact(
    chunk_id = chunk_id,
    session_id = "saveback-session",
    review_mode = "required",
    runtime_mode = "static",
    artifact = list(
      state = "draft",
      prompt = "Create saveback_value",
      response_text = "Draft\n```r\nsaveback_value <- 1\n```",
      draft_response = "Draft",
      final_code = "saveback_value <- 1",
      execution = list(status = "not_run", output = "", error = NULL),
      transcript = list(
        list(role = "user", content = "Create saveback_value"),
        list(role = "assistant", content = "Draft")
      ),
      retries = list(),
      model = "openai:gpt-4o",
      session_id = "saveback-session",
      review_mode = "required",
      runtime_mode = "static",
      defer_eval = TRUE,
      embed_session = "summary"
    )
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), memory$db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(
    con,
    "UPDATE human_reviews SET updated_at = ? WHERE chunk_id = ?",
    params = list(updated_at, chunk_id)
  )

  invisible(chunk_id)
}

test_that("handshake marks a page stale when embedded review timestamps lag behind memory", {
  env <- load_ai_review_saveback_env()
  project_root <- tempfile("saveback_stale_")
  dir.create(project_root, recursive = TRUE)

  source_path <- file.path(project_root, "review.Rmd")
  html_path <- file.path(project_root, "review.html")
  writeLines("---\ntitle: review\n---", source_path)
  writeLines("<html><body>review</body></html>", html_path)

  memory <- env$ProjectMemory$new(project_root = project_root)
  chunk_id <- seed_saveback_review(memory, normalizePath(source_path, winslash = "/", mustWork = TRUE))

  app <- env$create_ai_review_live_app(
    html_path = html_path,
    memory = memory,
    base_url = "http://127.0.0.1:9013/__aisdk_review__"
  )

  request <- list(
    document = list(
      source_path = env$normalize_ai_review_live_path(source_path),
      identity = env$ai_review_live_document_identity(source_path)
    ),
    chunks = list(
      list(
        chunk_id = chunk_id,
        chunk_label = "saveback",
        review_updated_at = "2026-03-14 11:59:59"
      )
    )
  )

  response <- app$call(list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/__aisdk_review__/handshake",
    body = jsonlite::toJSON(request, auto_unbox = TRUE, null = "null")
  ))

  payload <- jsonlite::fromJSON(rawToChar(response$body), simplifyVector = FALSE)

  expect_equal(response$status, 200L)
  expect_equal(payload$status, "stale")
  expect_true(payload$document$match)
  expect_true(payload$chunks[[1]]$stale)
  expect_equal(payload$chunks[[1]]$stale_reason, "review_updated")
})

test_that("saveback rerenders the source document and returns reload metadata while preserving chunk ids", {
  env <- load_ai_review_saveback_env()
  project_root <- tempfile("saveback_render_")
  dir.create(project_root, recursive = TRUE)

  source_path <- file.path(project_root, "review.Rmd")
  html_path <- file.path(project_root, "review.html")
  writeLines("---\ntitle: review\n---", source_path)
  writeLines("<html><body>review</body></html>", html_path)

  memory <- env$ProjectMemory$new(project_root = project_root)
  chunk_id <- seed_saveback_review(memory, normalizePath(source_path, winslash = "/", mustWork = TRUE))

  rerendered <- new.env(parent = emptyenv())
  env$rerender_ai_review_source <- function(path) {
    rerendered$path <- path
    writeLines("<html><body>rerendered</body></html>", html_path)
    normalizePath(html_path, winslash = "/", mustWork = TRUE)
  }

  app <- env$create_ai_review_live_app(
    html_path = html_path,
    memory = memory,
    base_url = "http://127.0.0.1:9014/__aisdk_review__"
  )

  request <- list(
    document = list(
      source_path = env$normalize_ai_review_live_path(source_path),
      identity = env$ai_review_live_document_identity(source_path)
    ),
    chunks = list(
      list(
        chunk_id = chunk_id,
        chunk_label = "saveback",
        review_updated_at = "2026-03-14 11:59:59"
      )
    ),
    rerender = TRUE,
    reload_url = "http://127.0.0.1:9014/"
  )

  response <- app$call(list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/__aisdk_review__/saveback",
    body = jsonlite::toJSON(request, auto_unbox = TRUE, null = "null")
  ))

  payload <- jsonlite::fromJSON(rawToChar(response$body), simplifyVector = FALSE)

  expect_equal(response$status, 200L)
  expect_equal(rerendered$path, normalizePath(source_path, winslash = "/", mustWork = TRUE))
  expect_true(payload$ok)
  expect_equal(payload$saveback$status, "rendered")
  expect_true(payload$saveback$rerendered)
  expect_true(payload$saveback$needs_reload)
  expect_equal(payload$saveback$reload_url, "http://127.0.0.1:9014/")
  expect_equal(payload$document$identity, env$ai_review_live_document_identity(source_path))
  expect_equal(payload$chunks[[1]]$chunk_id, chunk_id)
  expect_false(payload$chunks[[1]]$stale)
  expect_false(is.null(memory$get_review(chunk_id)))
})
