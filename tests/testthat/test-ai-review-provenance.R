load_ai_review_provenance_env <- function() {
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

make_provenance_stub_session_factory <- function(responses, counter_env) {
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

test_that("build_ai_review_provenance_payload is deterministic and summarizes transcript", {
  env <- load_ai_review_provenance_env()

  payload <- env$build_ai_review_provenance_payload(
    chunk_id = "chunk-prov",
    chunk_label = "provenance",
    file_path = "tutorial/provenance.Rmd",
    artifact = list(
      prompt = "Plot mtcars",
      model = "openai:gpt-4o",
      session_id = "session-1",
      review_mode = "required",
      runtime_mode = "static",
      state = "frozen",
      execution = list(status = "completed"),
      retries = list(list(attempt = 1)),
      final_code = "plot(mtcars$wt, mtcars$mpg)",
      transcript = list(
        list(role = "user", content = "Plot mtcars"),
        list(role = "assistant", content = "```r\nplot(mtcars$wt, mtcars$mpg)\n```")
      )
    ),
    review_status = "approved",
    timestamps = list(
      created_at = "2026-03-13 10:00:00",
      updated_at = "2026-03-13 10:01:00",
      reviewed_at = "2026-03-13 10:02:00"
    ),
    embed_session = "summary",
    ai_agent = "analysis-bot",
    uncertainty = "high"
  )

  json_1 <- env$serialize_ai_review_provenance_payload(payload)
  json_2 <- env$serialize_ai_review_provenance_payload(payload)

  expect_equal(payload$transcript$mode, "summary")
  expect_equal(payload$transcript$message_count, 2L)
  expect_equal(payload$execution$retry_count, 1L)
  expect_true(nzchar(payload$code$checksum))
  expect_identical(json_1, json_2)
  expect_match(json_1, "\"status\":\"approved\"")
  expect_match(json_1, "\"ai_agent\":\"analysis-bot\"")
})

test_that("render_ai_review_provenance_tag embeds static JSON payload into html", {
  env <- load_ai_review_provenance_env()

  payload <- env$build_ai_review_provenance_payload(
    chunk_id = "chunk-full",
    chunk_label = "full",
    artifact = list(
      prompt = "Summarize cars",
      model = "openai:gpt-4o",
      session_id = "session-2",
      final_code = "summary(cars)",
      transcript = list(
        list(role = "user", content = "Summarize cars"),
        list(role = "assistant", content = "```r\nsummary(cars)\n```")
      )
    ),
    embed_session = "full"
  )

  html <- htmltools::renderTags(
    env$render_ai_review_provenance_tag("chunk-full", payload)
  )$html

  expect_match(html, "application/json")
  expect_match(html, "embedded-provenance")
  expect_match(html, "\"mode\":\"full\"")
  expect_match(html, "Summarize cars")
})
