load_ai_review_embedded_viewer_env <- function() {
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
  env
}

make_embedded_viewer_stub_session_factory <- function(responses, counter_env) {
  function(model, system_prompt) {
    history <- list()
    idx <- 0L
    counter_env$model <- model
    counter_env$system_prompt <- system_prompt

    list(
      send = function(prompt) {
        idx <<- idx + 1L
        counter_env$send_calls <- (counter_env$send_calls %||% 0L) + 1L
        history <<- c(history, list(list(role = "user", content = prompt)))
        text <- responses[[idx]]
        history <<- c(history, list(list(role = "assistant", content = text)))
        list(text = text)
      },
      get_history = function() {
        history
      },
      get_model_id = function() {
        counter_env$model %||% "stub:model"
      },
      switch_model = function(model_id) {
        counter_env$model <- model_id
        invisible(TRUE)
      }
    )
  }
}

test_that("render_ai_review_runtime_manifest_tag embeds runtime manifest JSON", {
  env <- load_ai_review_embedded_viewer_env()

  manifest <- env$build_ai_review_runtime_manifest(
    chunk_id = "chunk-viewer",
    chunk_label = "viewer",
    file_path = "tutorial/viewer.Rmd",
    review_mode = "required",
    runtime_mode = "static",
    review_status = "pending",
    state = "draft",
    execution_status = "deferred",
    final_code = "viewer_value <- 1",
    embed_session = "summary"
  )

  html <- htmltools::renderTags(
    env$render_ai_review_runtime_manifest_tag("chunk-viewer", manifest)
  )$html

  expect_match(html, "application/json")
  expect_match(html, "embedded-runtime-manifest")
  expect_match(html, "\"tier\":\"attachable\"")
  expect_match(html, "\"copy-code\"")
})

test_that("render_ai_review_card_html includes read-only trace and metadata panels", {
  env <- load_ai_review_embedded_viewer_env()

  manifest <- env$build_ai_review_runtime_manifest(
    chunk_id = "chunk-inspector",
    chunk_label = "inspector",
    file_path = "tutorial/inspector.Rmd",
    review_mode = "required",
    runtime_mode = "static",
    review_status = "pending",
    state = "draft",
    execution_status = "deferred",
    final_code = "inspector_value <- 1",
    embed_session = "summary"
  )

  provenance <- env$build_ai_review_provenance_payload(
    chunk_id = "chunk-inspector",
    chunk_label = "inspector",
    file_path = "tutorial/inspector.Rmd",
    artifact = list(
      prompt = "Inspect this chunk",
      model = "openai:gpt-4o",
      session_id = "inspect-session",
      review_mode = "required",
      runtime_mode = "static",
      state = "draft",
      execution = list(status = "deferred"),
      transcript = list(
        list(role = "user", content = "Inspect this chunk"),
        list(role = "assistant", content = "Draft response", reasoning = "I should inspect the chunk state first.")
      ),
      final_code = "inspector_value <- 1"
    ),
    review_status = "pending",
    embed_session = "summary"
  )

  html <- env$render_ai_review_card_html(list(
    chunk_id = "chunk-inspector",
    chunk_label = "inspector",
    state = "draft",
    review = "required",
    runtime = "static",
    prompt = "Inspect this chunk",
    response = "Draft response",
    extracted_code = "inspector_value <- 1",
    execution_output = "",
    session_summary = list(session_id = "inspect-session", model = "openai:gpt-4o", retries = 0),
    provenance_payload = provenance,
    runtime_manifest = manifest,
    timestamps = list(created_at = "2026-03-14 12:00:00")
  ))

  expect_match(html, "embedded-runtime-manifest")
  expect_match(html, "data-runtime-tier=\"attachable\"")
  expect_match(html, "Agent Trace")
  expect_match(html, "Artifact Metadata")
  expect_match(html, "Published AI artifact")
  expect_match(html, "Reasoning:")
  expect_match(html, "data-role=\"inspector-panel\"")
  expect_match(html, "data-role=\"inspector-content\"")
  expect_match(html, "data-role=\"trace-panel\"")
  expect_match(html, "data-role=\"trace-content\"")
})
