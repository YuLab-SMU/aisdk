load_ai_review_embedded_contract_env <- function() {
  env <- aisdk_test_env()

  files <- c(
    "ai_review_contract.R",
    "ai_review_embedded_contract.R",
    "artifacts.R",
    "ai_review_card.R",
    "project_memory.R",
    "knitr_engine.R"
  )

  for (file in files) {
    source_local_aisdk_file(env, file)
  }

  env$build_context <- function(...) ""
  env$get_or_create_session <- function(options) {
    list(
      send = function(prompt) {
        list(text = "Draft\n```r\nruntime_value <- 1\n```")
      },
      get_model_id = function() "stub:model"
    )
  }
  env$ai_review_capture_transcript <- function(session) {
    list(
      list(role = "user", content = "prompt"),
      list(role = "assistant", content = "response")
    )
  }

  env
}

test_that("runtime capability tiers normalize predictably", {
  env <- load_ai_review_embedded_contract_env()

  expect_equal(
    env$normalize_ai_review_runtime_tier("attachable"),
    "attachable"
  )
  expect_equal(
    env$default_ai_review_runtime_tier("required"),
    "attachable"
  )
  expect_equal(
    env$default_ai_review_runtime_tier("none"),
    "inspect_only"
  )

  expect_error(
    env$normalize_ai_review_runtime_tier("browser"),
    "runtime tier"
  )
})

test_that("runtime action gating differs across capability tiers", {
  env <- load_ai_review_embedded_contract_env()

  inspect_only <- env$build_ai_review_runtime_actions(
    tier = "inspect_only",
    review_mode = "required",
    has_code = TRUE,
    has_session = TRUE
  )
  expect_true(inspect_only[["view-session"]]$enabled)
  expect_true(inspect_only[["copy-code"]]$enabled)
  expect_false(inspect_only[["run"]]$enabled)
  expect_false(inspect_only[["approve-freeze"]]$enabled)

  attachable <- env$build_ai_review_runtime_actions(
    tier = "attachable",
    review_mode = "required",
    has_code = TRUE,
    has_session = TRUE
  )
  expect_match(attachable[["run"]]$reason, "Connect a local review runtime")

  connected <- env$build_ai_review_runtime_actions(
    tier = "connected",
    review_mode = "required",
    has_code = TRUE,
    has_session = TRUE
  )
  expect_true(connected[["run"]]$enabled)
  expect_true(connected[["approve-freeze"]]$enabled)
  expect_true(connected[["reject"]]$enabled)

  busy <- env$build_ai_review_runtime_actions(
    tier = "busy",
    review_mode = "required",
    has_code = TRUE,
    has_session = TRUE
  )
  expect_false(busy[["regenerate"]]$enabled)
  expect_match(busy[["regenerate"]]$reason, "busy")
})

test_that("runtime manifest shape is deterministic and documents requirements", {
  env <- load_ai_review_embedded_contract_env()

  manifest <- env$build_ai_review_runtime_manifest(
    chunk_id = "chunk-embedded",
    chunk_label = "embedded",
    file_path = "/tmp/doc.Rmd",
    review_mode = "required",
    runtime_mode = "static",
    review_status = "pending",
    state = "draft",
    execution_status = "deferred",
    final_code = "runtime_value <- 1",
    embed_session = "summary"
  )

  expect_equal(manifest$version, 1L)
  expect_equal(manifest$mode, "published-review-runtime")
  expect_equal(manifest$chunk$id, "chunk-embedded")
  expect_equal(manifest$document$source_path, "/tmp/doc.Rmd")
  expect_equal(manifest$review$mode, "required")
  expect_equal(manifest$execution$status, "deferred")
  expect_equal(manifest$session$embedded, "summary")
  expect_true(manifest$code$available)
  expect_equal(manifest$runtime$tier, "attachable")
  expect_true(manifest$runtime$attachable)
  expect_false(manifest$runtime$connected)
  expect_true(manifest$runtime$actions[["copy-code"]]$enabled)
  expect_true(manifest$runtime$actions[["view-session"]]$enabled)

  expect_true(all(
    env$ai_review_runtime_required_fields("offline") %in%
      c(
        "version", "mode", "chunk.id", "chunk.label", "document.source_path",
        "document.identity", "review.mode", "review.state", "execution.status",
        "session.embedded", "code.available", "runtime.tier", "runtime.actions"
      )
  ))
  expect_true(all(
    env$ai_review_runtime_required_fields("offline") %in%
      env$ai_review_runtime_required_fields("connected")
  ))
})
