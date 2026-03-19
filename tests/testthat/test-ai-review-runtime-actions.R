load_ai_review_runtime_actions_env <- function() {
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

  env$build_context <- function(...) ""
  env
}

seed_runtime_action_review <- function(memory, chunk_id, final_code,
                                       response = "Draft response\n```r\nvalue <- 1\n```",
                                       prompt = "Create value",
                                       file_path = "tutorial/live.Rmd",
                                       chunk_label = "runtime-action",
                                       status = "pending",
                                       execution_status = "deferred",
                                       artifact_state = "draft",
                                       transcript = list(
                                         list(role = "user", content = "Create value"),
                                         list(role = "assistant", content = "Draft response")
                                       )) {
  memory$store_review(
    chunk_id = chunk_id,
    file_path = file_path,
    chunk_label = chunk_label,
    prompt = prompt,
    response = response,
    status = status,
    session_id = paste0(chunk_id, "-session"),
    review_mode = "required",
    runtime_mode = "static",
    execution_status = execution_status,
    final_code = final_code
  )

  memory$store_review_artifact(
    chunk_id = chunk_id,
    session_id = paste0(chunk_id, "-session"),
    review_mode = "required",
    runtime_mode = "static",
    artifact = list(
      state = artifact_state,
      prompt = prompt,
      response_text = response,
      draft_response = trimws(gsub("(?s)```\\s*(?:\\{r[^}]*\\}|r)\\s*\\n.*?\\n?```", "", response, perl = TRUE, ignore.case = TRUE)),
      final_code = final_code,
      execution = list(
        status = execution_status,
        output = "",
        error = NULL
      ),
      transcript = transcript,
      retries = list(),
      model = "openai:gpt-4o",
      session_id = paste0(chunk_id, "-session"),
      review_mode = "required",
      runtime_mode = "static",
      defer_eval = TRUE
    )
  )
}

test_that("approve-freeze and reject return structured payloads and persist lifecycle state", {
  env <- load_ai_review_runtime_actions_env()
  project_root <- tempfile("runtime_action_status_")
  dir.create(project_root, recursive = TRUE)
  memory <- env$ProjectMemory$new(project_root = project_root)

  seed_runtime_action_review(
    memory,
    chunk_id = "chunk-approve",
    final_code = "approved_value <- 1"
  )
  seed_runtime_action_review(
    memory,
    chunk_id = "chunk-reject",
    final_code = "rejected_value <- 2"
  )

  approved <- env$perform_ai_review_live_action(memory, "chunk-approve", "approve-freeze")
  rejected <- env$perform_ai_review_live_action(memory, "chunk-reject", "reject")

  expect_true(approved$ok)
  expect_equal(approved$review$status, "approved")
  expect_equal(approved$review$state, "frozen")
  expect_false(approved$rerender_required)
  expect_equal(memory$get_review("chunk-approve")$status, "approved")
  expect_equal(memory$get_review_artifact("chunk-approve")$state, "frozen")

  expect_true(rejected$ok)
  expect_equal(rejected$review$status, "rejected")
  expect_equal(rejected$review$state, "rejected")
  expect_false(rejected$rerender_required)
  expect_equal(memory$get_review("chunk-reject")$status, "rejected")
  expect_equal(memory$get_review_artifact("chunk-reject")$state, "rejected")
})

test_that("run action executes stored code and persists execution output", {
  env <- load_ai_review_runtime_actions_env()
  project_root <- tempfile("runtime_action_run_")
  dir.create(project_root, recursive = TRUE)
  memory <- env$ProjectMemory$new(project_root = project_root)

  seed_runtime_action_review(
    memory,
    chunk_id = "chunk-run",
    final_code = "runtime_value <- 5\ncat('runtime ok')",
    response = "Run this\n```r\nruntime_value <- 5\ncat('runtime ok')\n```",
    prompt = "Run runtime_value"
  )

  exec_env <- new.env(parent = globalenv())
  result <- env$perform_ai_review_live_action(memory, "chunk-run", "run", envir = exec_env)

  review <- memory$get_review("chunk-run")
  artifact <- memory$get_review_artifact("chunk-run")

  expect_true(result$ok)
  expect_equal(result$review$state, "ran")
  expect_equal(result$execution$status, "completed")
  expect_match(result$execution$output, "runtime ok")
  expect_false(result$rerender_required)
  expect_equal(review$execution_status, "completed")
  expect_match(review$execution_output, "runtime ok")
  expect_equal(artifact$state, "ran")
  expect_equal(get("runtime_value", envir = exec_env), 5)
})

test_that("regenerate action refreshes draft content and transcript in ProjectMemory", {
  env <- load_ai_review_runtime_actions_env()
  project_root <- tempfile("runtime_action_regenerate_")
  dir.create(project_root, recursive = TRUE)
  memory <- env$ProjectMemory$new(project_root = project_root)

  seed_runtime_action_review(
    memory,
    chunk_id = "chunk-regenerate",
    final_code = "old_value <- 1",
    response = "Old draft\n```r\nold_value <- 1\n```",
    prompt = "Generate new value"
  )

  env$create_chat_session <- function(model, system_prompt) {
    history <- list()
    list(
      send = function(prompt) {
        history <<- c(
          history,
          list(list(role = "user", content = prompt)),
          list(list(role = "assistant", content = "New draft\n```r\nnew_value <- 7\n```"))
        )
        list(text = "New draft\n```r\nnew_value <- 7\n```")
      },
      get_history = function() history,
      get_model_id = function() model,
      switch_model = function(model_id) invisible(model_id)
    )
  }

  result <- env$perform_ai_review_live_action(memory, "chunk-regenerate", "regenerate")
  review <- memory$get_review("chunk-regenerate")
  artifact <- memory$get_review_artifact("chunk-regenerate")

  expect_true(result$ok)
  expect_equal(result$review$status, "pending")
  expect_equal(result$review$state, "draft")
  expect_equal(result$execution$status, "not_run")
  expect_true(result$rerender_required)
  expect_equal(review$final_code, "new_value <- 7")
  expect_match(review$response, "New draft")
  expect_equal(artifact$state, "draft")
  expect_equal(length(artifact$transcript), 2L)
  expect_equal(artifact$final_code, "new_value <- 7")
})

test_that("action endpoint returns explicit error payloads for invalid runtime actions", {
  env <- load_ai_review_runtime_actions_env()
  project_root <- tempfile("runtime_action_error_")
  dir.create(project_root, recursive = TRUE)
  html_path <- file.path(project_root, "review.html")
  writeLines("<html><head></head><body>runtime error</body></html>", html_path)

  memory <- env$ProjectMemory$new(project_root = project_root)
  app <- env$create_ai_review_live_app(
    html_path = html_path,
    memory = memory,
    base_url = "http://127.0.0.1:9012/__aisdk_review__"
  )

  response <- app$call(list(
    REQUEST_METHOD = "POST",
    PATH_INFO = "/__aisdk_review__/action",
    body = jsonlite::toJSON(
      list(chunk_id = "missing-chunk", action = "run"),
      auto_unbox = TRUE,
      null = "null"
    )
  ))

  expect_equal(response$status, 404L)
  payload <- jsonlite::fromJSON(rawToChar(response$body), simplifyVector = FALSE)
  expect_false(payload$ok)
  expect_equal(payload$action, "run")
  expect_equal(payload$chunk_id, "missing-chunk")
  expect_match(payload$message, "not found")
})
