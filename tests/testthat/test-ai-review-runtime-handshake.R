load_ai_review_runtime_handshake_env <- function() {
  env <- aisdk_test_env()

  files <- c(
    "ai_review_contract.R",
    "ai_review_embedded_contract.R",
    "project_memory.R",
    "ai_review_live.R"
  )

  for (file in files) {
    source_local_aisdk_file(env, file)
  }

  env
}

test_that("handshake returns connected for matching document identity and chunk set", {
  env <- load_ai_review_runtime_handshake_env()
  project_root <- tempfile("live_handshake_match_")
  dir.create(project_root, recursive = TRUE)
  html_path <- file.path(project_root, "review.html")
  writeLines("<html><head></head><body>match</body></html>", html_path)

  file_path <- normalizePath(file.path(project_root, "tutorial", "live.Rmd"), winslash = "/", mustWork = FALSE)
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  writeLines("---\ntitle: live\n---", file_path)

  memory <- env$ProjectMemory$new(project_root = project_root)
  memory$store_review(
    chunk_id = "chunk-match",
    file_path = file_path,
    chunk_label = "match",
    prompt = "summarize data",
    response = "draft response",
    status = "pending",
    session_id = "session-match",
    review_mode = "required",
    runtime_mode = "static",
    execution_status = "deferred",
    final_code = "summary(cars)"
  )
  memory$store_review_artifact(
    chunk_id = "chunk-match",
    session_id = "session-match",
    review_mode = "required",
    runtime_mode = "static",
    artifact = list(
      transcript = list(
        list(role = "user", content = "summarize data"),
        list(role = "assistant", content = "```r\nsummary(cars)\n```")
      )
    )
  )

  app <- env$create_ai_review_live_app(
    html_path = html_path,
    memory = memory,
    base_url = "http://127.0.0.1:9004/__aisdk_review__"
  )

  request <- list(
    document = list(
      source_path = env$normalize_ai_review_live_path(file_path),
      identity = env$ai_review_live_document_identity(file_path)
    ),
    chunks = list(
      list(
        chunk_id = "chunk-match",
        chunk_label = "match"
      )
    )
  )

  response <- app$call(list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/__aisdk_review__/handshake",
    body = jsonlite::toJSON(request, auto_unbox = TRUE, null = "null")
  ))

  expect_equal(response$status, 200L)
  payload <- jsonlite::fromJSON(rawToChar(response$body), simplifyVector = FALSE)
  expect_true(payload$ok)
  expect_equal(payload$status, "connected")
  expect_true(payload$document$match)
  expect_equal(payload$document$identity, request$document$identity)
  expect_true(payload$chunks[[1]]$available)
  expect_false(payload$chunks[[1]]$stale)
  expect_true(payload$chunks[[1]]$actions$run$enabled)
})

test_that("handshake returns mismatch for a different source document identity", {
  env <- load_ai_review_runtime_handshake_env()
  project_root <- tempfile("live_handshake_mismatch_")
  dir.create(project_root, recursive = TRUE)
  html_path <- file.path(project_root, "review.html")
  writeLines("<html><head></head><body>mismatch</body></html>", html_path)

  file_path <- normalizePath(file.path(project_root, "tutorial", "live.Rmd"), winslash = "/", mustWork = FALSE)
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  writeLines("---\ntitle: live\n---", file_path)

  memory <- env$ProjectMemory$new(project_root = project_root)
  memory$store_review(
    chunk_id = "chunk-mismatch",
    file_path = file_path,
    chunk_label = "mismatch",
    prompt = "summarize data",
    response = "draft response",
    status = "pending",
    review_mode = "required",
    runtime_mode = "static",
    execution_status = "deferred",
    final_code = "summary(cars)"
  )

  app <- env$create_ai_review_live_app(
    html_path = html_path,
    memory = memory,
    base_url = "http://127.0.0.1:9005/__aisdk_review__"
  )

  request <- list(
    document = list(
      source_path = "tutorial/other.Rmd",
      identity = digest::digest("tutorial/other.Rmd", algo = "sha256", serialize = FALSE)
    ),
    chunks = list(
      list(
        chunk_id = "chunk-mismatch",
        chunk_label = "mismatch"
      )
    )
  )

  response <- app$call(list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/__aisdk_review__/handshake",
    body = jsonlite::toJSON(request, auto_unbox = TRUE, null = "null")
  ))

  expect_equal(response$status, 200L)
  payload <- jsonlite::fromJSON(rawToChar(response$body), simplifyVector = FALSE)
  expect_true(payload$ok)
  expect_equal(payload$status, "mismatch")
  expect_false(payload$document$match)
  expect_true(payload$chunks[[1]]$available)
  expect_false(payload$chunks[[1]]$actions$run$enabled)
})

test_that("handshake returns stale when requested chunks are missing from local memory", {
  env <- load_ai_review_runtime_handshake_env()
  project_root <- tempfile("live_handshake_stale_")
  dir.create(project_root, recursive = TRUE)
  html_path <- file.path(project_root, "review.html")
  writeLines("<html><head></head><body>stale</body></html>", html_path)

  file_path <- normalizePath(file.path(project_root, "tutorial", "live.Rmd"), winslash = "/", mustWork = FALSE)
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  writeLines("---\ntitle: live\n---", file_path)

  memory <- env$ProjectMemory$new(project_root = project_root)

  app <- env$create_ai_review_live_app(
    html_path = html_path,
    memory = memory,
    base_url = "http://127.0.0.1:9006/__aisdk_review__"
  )

  request <- list(
    document = list(
      source_path = env$normalize_ai_review_live_path(file_path),
      identity = env$ai_review_live_document_identity(file_path)
    ),
    chunks = list(
      list(
        chunk_id = "chunk-missing",
        chunk_label = "missing"
      )
    )
  )

  response <- app$call(list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/__aisdk_review__/handshake",
    body = jsonlite::toJSON(request, auto_unbox = TRUE, null = "null")
  ))

  expect_equal(response$status, 200L)
  payload <- jsonlite::fromJSON(rawToChar(response$body), simplifyVector = FALSE)
  expect_true(payload$ok)
  expect_equal(payload$status, "stale")
  expect_true(payload$document$match)
  expect_false(payload$chunks[[1]]$available)
  expect_true(payload$chunks[[1]]$stale)
})
