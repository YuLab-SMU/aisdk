load_ai_review_published_e2e_env <- function() {
  env <- aisdk_test_env()

  files <- c(
    "ai_review_contract.R",
    "ai_review_embedded_contract.R",
    "artifacts.R",
    "ai_review_card.R",
    "project_memory.R",
    "knitr_engine.R",
    "ai_review_live.R"
  )

  for (file in files) {
    source_local_aisdk_file(env, file)
  }

  env
}

seed_published_e2e_card <- function(env, memory, source_path,
                                    chunk_id = "chunk-published-e2e",
                                    chunk_label = "published-e2e") {
  prompt <- "Create published_value"
  response <- "Draft summary\n```r\npublished_value <- 3\ncat('published ok')\n```"
  final_code <- "published_value <- 3\ncat('published ok')"
  transcript <- list(
    list(role = "user", content = prompt),
    list(role = "assistant", content = response)
  )

  memory$store_review(
    chunk_id = chunk_id,
    file_path = source_path,
    chunk_label = chunk_label,
    prompt = prompt,
    response = response,
    status = "pending",
    session_id = "published-e2e-session",
    review_mode = "required",
    runtime_mode = "static",
    execution_status = "deferred",
    final_code = final_code
  )

  review <- memory$get_review(chunk_id)
  timestamps <- list(
    created_at = review$created_at,
    updated_at = review$updated_at,
    reviewed_at = review$reviewed_at
  )

  artifact <- env$build_ai_review_artifact_record(
    state = "draft",
    prompt = prompt,
    response_text = response,
    draft_response = "Draft summary",
    final_code = final_code,
    execution_status = "deferred",
    execution_output = "",
    error_message = NULL,
    transcript = transcript,
    retries = list(),
    model_id = "openai:gpt-4o",
    session_id = "published-e2e-session",
    review_mode = "required",
    runtime_mode = "static",
    defer_eval = TRUE,
    embed_session = "summary"
  )

  artifact$runtime_manifest <- env$build_ai_review_runtime_manifest(
    chunk_id = chunk_id,
    chunk_label = chunk_label,
    file_path = source_path,
    review_mode = "required",
    runtime_mode = "static",
    review_status = "pending",
    state = "draft",
    execution_status = "deferred",
    final_code = final_code,
    embed_session = "summary",
    timestamps = timestamps
  )

  memory$store_review_artifact(
    chunk_id = chunk_id,
    session_id = "published-e2e-session",
    review_mode = "required",
    runtime_mode = "static",
    artifact = artifact
  )

  provenance <- env$build_ai_review_provenance_payload(
    chunk_id = chunk_id,
    chunk_label = chunk_label,
    file_path = source_path,
    artifact = artifact,
    review_status = "pending",
    timestamps = timestamps,
    embed_session = "summary"
  )

  card_html <- env$render_ai_review_card_html(list(
    chunk_id = chunk_id,
    chunk_label = chunk_label,
    state = "draft",
    review = "required",
    runtime = "static",
    prompt = prompt,
    response = "Draft summary",
    extracted_code = final_code,
    execution_output = "",
    session_summary = list(
      session_id = "published-e2e-session",
      model = "openai:gpt-4o",
      retries = 0
    ),
    provenance_payload = provenance,
    runtime_manifest = artifact$runtime_manifest,
    timestamps = timestamps
  ))

  list(
    chunk_id = chunk_id,
    review = memory$get_review(chunk_id),
    artifact = memory$get_review_artifact(chunk_id),
    card_html = card_html
  )
}

test_that("published html can attach, mutate review state, and then require saveback", {
  env <- load_ai_review_published_e2e_env()
  project_root <- tempfile("published_review_e2e_")
  dir.create(project_root, recursive = TRUE)

  source_path <- file.path(project_root, "published-review.Rmd")
  html_path <- file.path(project_root, "published-review.html")
  writeLines("---\ntitle: published review\n---", source_path)

  memory <- env$ProjectMemory$new(project_root = project_root)
  seeded <- seed_published_e2e_card(
    env = env,
    memory = memory,
    source_path = normalizePath(source_path, winslash = "/", mustWork = TRUE)
  )

  expect_match(seeded$card_html, "embedded-runtime-manifest")
  expect_match(seeded$card_html, "Save &amp; Refresh")

  writeLines(
    c(
      "<html><head></head><body>",
      seeded$card_html,
      "</body></html>"
    ),
    html_path
  )

  env$rerender_ai_review_source <- function(path) {
    writeLines("<html><body>rerendered published review</body></html>", html_path)
    normalizePath(html_path, winslash = "/", mustWork = TRUE)
  }

  app <- env$create_ai_review_live_app(
    html_path = html_path,
    memory = memory,
    base_url = "http://127.0.0.1:9015/__aisdk_review__"
  )

  handshake_request <- list(
    document = list(
      source_path = seeded$review$file_path,
      identity = env$ai_review_live_document_identity(seeded$review$file_path)
    ),
    chunks = list(
      list(
        chunk_id = seeded$chunk_id,
        chunk_label = "published-e2e",
        review_updated_at = seeded$review$updated_at
      )
    )
  )

  handshake_response <- app$call(list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/__aisdk_review__/handshake",
    body = jsonlite::toJSON(handshake_request, auto_unbox = TRUE, null = "null")
  ))

  handshake_payload <- jsonlite::fromJSON(
    rawToChar(handshake_response$body),
    simplifyVector = FALSE
  )

  expect_equal(handshake_response$status, 200L)
  expect_equal(handshake_payload$status, "connected")
  expect_true(handshake_payload$document$match)
  expect_false(handshake_payload$chunks[[1]]$stale)
  expect_true(handshake_payload$chunks[[1]]$actions$run$enabled)

  run_response <- app$call(list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/__aisdk_review__/action",
    body = jsonlite::toJSON(
      list(chunk_id = seeded$chunk_id, action = "run"),
      auto_unbox = TRUE,
      null = "null"
    )
  ))

  run_payload <- jsonlite::fromJSON(rawToChar(run_response$body), simplifyVector = FALSE)

  expect_equal(run_response$status, 200L)
  expect_true(run_payload$ok)
  expect_equal(run_payload$review$state, "ran")
  expect_equal(run_payload$execution$status, "completed")
  expect_true(run_payload$saveback$available)
  expect_true(run_payload$saveback$required)

  con <- DBI::dbConnect(RSQLite::SQLite(), memory$db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(
    con,
    "UPDATE human_reviews SET updated_at = ? WHERE chunk_id = ?",
    params = list("2099-01-01 00:00:00", seeded$chunk_id)
  )

  stale_response <- app$call(list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/__aisdk_review__/handshake",
    body = jsonlite::toJSON(handshake_request, auto_unbox = TRUE, null = "null")
  ))

  stale_payload <- jsonlite::fromJSON(rawToChar(stale_response$body), simplifyVector = FALSE)

  expect_equal(stale_response$status, 200L)
  expect_equal(stale_payload$status, "stale")
  expect_true(stale_payload$chunks[[1]]$stale)
  expect_equal(stale_payload$chunks[[1]]$stale_reason, "review_updated")
  expect_true(stale_payload$chunks[[1]]$actions$saveback$enabled)

  saveback_response <- app$call(list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/__aisdk_review__/saveback",
    body = jsonlite::toJSON(
      utils::modifyList(
        handshake_request,
        list(rerender = TRUE, reload_url = "http://127.0.0.1:9015/")
      ),
      auto_unbox = TRUE,
      null = "null"
    )
  ))

  saveback_payload <- jsonlite::fromJSON(
    rawToChar(saveback_response$body),
    simplifyVector = FALSE
  )

  expect_equal(saveback_response$status, 200L)
  expect_true(saveback_payload$ok)
  expect_equal(saveback_payload$saveback$status, "rendered")
  expect_true(saveback_payload$saveback$rerendered)
  expect_true(saveback_payload$saveback$needs_reload)
  expect_equal(saveback_payload$saveback$reload_url, "http://127.0.0.1:9015/")
  expect_false(saveback_payload$chunks[[1]]$stale)
  expect_equal(saveback_payload$chunks[[1]]$chunk_id, seeded$chunk_id)
})
