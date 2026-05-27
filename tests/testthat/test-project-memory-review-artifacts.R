load_project_memory_env <- function() {
  env <- aisdk_test_env()
  source_local_aisdk_file(env, "project_memory.R")
  env
}

test_that("ProjectMemory initializes review artifact columns on fresh database", {
  env <- load_project_memory_env()
  project_root <- tempfile("pmem_fresh_")
  dir.create(project_root, recursive = TRUE)

  memory <- env$ProjectMemory$new(project_root = project_root)

  con <- DBI::dbConnect(RSQLite::SQLite(), memory$db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  fields <- DBI::dbListFields(con, "human_reviews")

  expect_true(all(c(
    "session_id", "review_mode", "runtime_mode", "artifact_json",
    "execution_status", "execution_output", "final_code", "error_message"
  ) %in% fields))
})

test_that("ProjectMemory migrates legacy human_reviews schema without data loss", {
  env <- load_project_memory_env()
  project_root <- tempfile("pmem_legacy_")
  dir.create(file.path(project_root, ".aisdk"), recursive = TRUE)
  db_path <- file.path(project_root, ".aisdk", "memory.sqlite")

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE human_reviews (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      chunk_id TEXT NOT NULL UNIQUE,
      file_path TEXT NOT NULL,
      chunk_label TEXT,
      prompt TEXT NOT NULL,
      response TEXT NOT NULL,
      status TEXT DEFAULT 'pending',
      ai_agent TEXT,
      uncertainty TEXT,
      reviewed_at TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )
  ")
  DBI::dbExecute(con, "
    INSERT INTO human_reviews
    (chunk_id, file_path, chunk_label, prompt, response, status, ai_agent, uncertainty, created_at, updated_at)
    VALUES
    ('legacy-1', 'tutorial/legacy.Rmd', 'legacy-chunk', 'old prompt', 'old response', 'approved', 'analyst', 'high', '2026-03-13 20:00:00', '2026-03-13 20:00:00')
  ")
  DBI::dbDisconnect(con)
  on.exit(NULL, add = FALSE)

  memory <- env$ProjectMemory$new(project_root = project_root)
  review <- memory$get_review("legacy-1")

  con2 <- DBI::dbConnect(RSQLite::SQLite(), memory$db_path)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  fields <- DBI::dbListFields(con2, "human_reviews")

  expect_equal(review$status, "approved")
  expect_equal(review$response, "old response")
  expect_true(all(c(
    "session_id", "review_mode", "runtime_mode", "artifact_json",
    "execution_status", "execution_output", "final_code", "error_message"
  ) %in% fields))
})

test_that("ProjectMemory stores and retrieves review artifacts as JSON", {
  env <- load_project_memory_env()
  project_root <- tempfile("pmem_artifact_")
  dir.create(project_root, recursive = TRUE)

  memory <- env$ProjectMemory$new(project_root = project_root)
  memory$store_review(
    chunk_id = "chunk-artifact",
    file_path = "tutorial/doc.Rmd",
    chunk_label = "artifact",
    prompt = "generate a plot",
    response = "```r\nplot(1:3)\n```",
    status = "pending",
    ai_agent = "analyst",
    uncertainty = "medium"
  )

  memory$store_review_artifact(
    chunk_id = "chunk-artifact",
    session_id = "session-1",
    review_mode = "required",
    runtime_mode = "static",
    artifact = list(
      transcript = list(
        list(role = "user", content = "generate a plot"),
        list(role = "assistant", content = "```r\nplot(1:3)\n```")
      ),
      retries = 1,
      code = "plot(1:3)"
    )
  )

  review <- memory$get_review("chunk-artifact")
  artifact <- memory$get_review_artifact("chunk-artifact")

  expect_equal(review$session_id, "session-1")
  expect_equal(review$review_mode, "required")
  expect_equal(review$runtime_mode, "static")
  expect_equal(artifact$retries, 1)
  expect_equal(artifact$code, "plot(1:3)")
  expect_equal(artifact$transcript[[1]]$role, "user")
})

test_that("ProjectMemory updates execution result fields independently", {
  env <- load_project_memory_env()
  project_root <- tempfile("pmem_exec_")
  dir.create(project_root, recursive = TRUE)

  memory <- env$ProjectMemory$new(project_root = project_root)
  memory$store_review(
    chunk_id = "chunk-exec",
    file_path = "tutorial/doc.Rmd",
    chunk_label = "exec",
    prompt = "summarize data",
    response = "```r\nsummary(mtcars)\n```",
    status = "pending"
  )

  memory$update_execution_result(
    chunk_id = "chunk-exec",
    execution_status = "completed",
    execution_output = "Min. 10.4",
    final_code = "summary(mtcars)",
    error_message = NULL
  )

  review <- memory$get_review("chunk-exec")

  expect_equal(review$execution_status, "completed")
  expect_equal(review$execution_output, "Min. 10.4")
  expect_equal(review$final_code, "summary(mtcars)")
  expect_true(is.na(review$error_message) || is.null(review$error_message))
})
