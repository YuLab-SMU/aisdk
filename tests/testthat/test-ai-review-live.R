load_ai_review_live_env <- function() {
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

test_that("create_ai_review_live_app answers ping requests", {
  env <- load_ai_review_live_env()
  project_root <- tempfile("live_ping_")
  dir.create(project_root, recursive = TRUE)
  html_path <- file.path(project_root, "review.html")
  writeLines("<html><head></head><body>hello</body></html>", html_path)

  memory <- env$ProjectMemory$new(project_root = project_root)
  app <- env$create_ai_review_live_app(
    html_path = html_path,
    memory = memory,
    base_url = "http://127.0.0.1:9001/__aisdk_review__"
  )

  response <- app$call(list(
    REQUEST_METHOD = "GET",
    PATH_INFO = "/__aisdk_review__/ping"
  ))

  expect_equal(response$status, 200L)
  expect_match(rawToChar(response$body), "\"ok\":true")
})

test_that("perform_ai_review_live_action updates review status for approve and reject", {
  env <- load_ai_review_live_env()
  project_root <- tempfile("live_action_")
  dir.create(project_root, recursive = TRUE)

  memory <- env$ProjectMemory$new(project_root = project_root)
  memory$store_review(
    chunk_id = "chunk-live",
    file_path = "tutorial/live.Rmd",
    chunk_label = "live",
    prompt = "summarize data",
    response = "draft response",
    status = "pending"
  )

  approve <- env$perform_ai_review_live_action(memory, "chunk-live", "approve-freeze")
  expect_true(approve$ok)
  expect_equal(approve$state, "frozen")
  expect_equal(approve$review$status, "approved")
  expect_equal(memory$get_review("chunk-live")$status, "approved")

  reject <- env$perform_ai_review_live_action(memory, "chunk-live", "reject")
  expect_true(reject$ok)
  expect_equal(reject$state, "rejected")
  expect_equal(memory$get_review("chunk-live")$status, "rejected")
})

test_that("session endpoint returns stored transcript data", {
  env <- load_ai_review_live_env()
  project_root <- tempfile("live_session_")
  dir.create(project_root, recursive = TRUE)
  html_path <- file.path(project_root, "review.html")
  writeLines("<html><head></head><body>session</body></html>", html_path)

  memory <- env$ProjectMemory$new(project_root = project_root)
  memory$store_review(
    chunk_id = "chunk-session",
    file_path = "tutorial/live.Rmd",
    chunk_label = "live-session",
    prompt = "plot mtcars",
    response = "draft response",
    status = "pending",
    session_id = "session-live"
  )
  memory$store_review_artifact(
    chunk_id = "chunk-session",
    session_id = "session-live",
    review_mode = "required",
    runtime_mode = "live",
    artifact = list(
      transcript = list(
        list(role = "user", content = "plot mtcars"),
        list(role = "assistant", content = "```r\nplot(mtcars$wt, mtcars$mpg)\n```")
      )
    )
  )

  app <- env$create_ai_review_live_app(
    html_path = html_path,
    memory = memory,
    base_url = "http://127.0.0.1:9002/__aisdk_review__"
  )

  response <- app$call(list(
    REQUEST_METHOD = "GET",
    PATH_INFO = "/__aisdk_review__/session/chunk-session"
  ))

  expect_equal(response$status, 200L)
  expect_match(rawToChar(response$body), "plot mtcars")
  expect_match(rawToChar(response$body), "session-live")
})

test_that("inject_ai_review_bridge_config embeds the bridge url into html", {
  env <- load_ai_review_live_env()

  injected <- env$inject_ai_review_bridge_config(
    "<html><head><title>Doc</title></head><body>hello</body></html>",
    "http://127.0.0.1:9003/__aisdk_review__"
  )

  expect_match(injected, "AISDK_REVIEW_BRIDGE_URL")
  expect_match(injected, "AISDK_REVIEW_RUNTIME")
  expect_match(injected, "http://127.0.0.1:9003/__aisdk_review__")
  expect_match(injected, "/handshake")
})

test_that("review_document returns a launcher object that can be stopped", {
  env <- load_ai_review_live_env()
  project_root <- tempfile("live_launcher_")
  dir.create(project_root, recursive = TRUE)
  html_path <- file.path(project_root, "review.html")
  writeLines("<html><head></head><body>launcher</body></html>", html_path)

  memory <- env$ProjectMemory$new(project_root = project_root)
  launcher <- tryCatch(
    env$review_document(
      path = html_path,
      memory = memory,
      browse = FALSE
    ),
    error = function(e) {
      testthat::skip(paste("Live review launcher could not bind a local port in this environment:", conditionMessage(e)))
    }
  )

  expect_true(is.list(launcher))
  expect_true(is.function(launcher$stop))
  expect_match(launcher$url, "^http://127.0.0.1:")

  launcher$stop()
})
