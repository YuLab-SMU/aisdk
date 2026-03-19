load_ai_review_ui_env <- function() {
  env <- aisdk_test_env()

  files <- c(
    "artifacts.R",
    "ai_review_card.R",
    "project_memory.R",
    "shiny_review.R"
  )

  for (file in files) {
    source_local_aisdk_file(env, file)
  }

  env
}

test_that("render_ai_review_panel_item reuses artifact-backed review cards", {
  env <- load_ai_review_ui_env()

  review <- list(
    chunk_id = "chunk_panel",
    chunk_label = "panel-review",
    file_path = "tutorial/review.Rmd",
    prompt = "Plot mtcars",
    response = "Draft explanation",
    status = "pending",
    review_mode = "required",
    runtime_mode = "static",
    session_id = "session-panel",
    ai_agent = "analysis-bot",
    uncertainty = "medium",
    created_at = "2026-03-13 09:00:00",
    updated_at = "2026-03-13 09:01:00"
  )

  artifact <- list(
    state = "ran",
    draft_response = "Use base plotting.",
    final_code = "plot(mtcars$wt, mtcars$mpg)",
    execution = list(
      status = "completed",
      output = "plot drawn"
    ),
    model = "openai:gpt-4o",
    retries = list(list(attempt = 1L)),
    transcript = list(
      list(role = "user", content = "Plot mtcars"),
      list(role = "assistant", content = "```r\nplot(mtcars$wt, mtcars$mpg)\n```")
    )
  )

  html <- htmltools::renderTags(
    env$render_ai_review_panel_item(review, artifact, ns = identity)
  )$html

  expect_match(html, "AI Review")
  expect_match(html, "Read-only artifact view")
  expect_match(html, "Agent Trace")
  expect_match(html, "Artifact Metadata")
  expect_match(html, "Open Source")
  expect_match(html, "embedded-provenance")
  expect_match(html, "data-state=\"ran\"")
  expect_match(html, "file://")
  expect_match(html, "#chunk=panel-review")
})

test_that("aiReviewPanelServer refreshes pending reviews after actions and respects file filters", {
  env <- load_ai_review_ui_env()
  project_root <- tempfile("review_panel_")
  dir.create(project_root, recursive = TRUE)
  memory <- env$ProjectMemory$new(project_root = project_root)

  memory$store_review(
    chunk_id = "chunk_one",
    file_path = "tutorial/a.Rmd",
    chunk_label = "chunk-one",
    prompt = "Summarize cars",
    response = "Draft one",
    status = "pending",
    review_mode = "required",
    runtime_mode = "static"
  )
  memory$store_review_artifact(
    chunk_id = "chunk_one",
    artifact = list(
      state = "draft",
      draft_response = "Draft one",
      transcript = list(list(role = "assistant", content = "Draft one"))
    )
  )

  memory$store_review(
    chunk_id = "chunk_two",
    file_path = "tutorial/b.Rmd",
    chunk_label = "chunk-two",
    prompt = "Plot cars",
    response = "Draft two",
    status = "pending",
    review_mode = "required",
    runtime_mode = "static"
  )

  file_filter <- shiny::reactiveVal(NULL)

  shiny::testServer(
    env$aiReviewPanelServer,
    args = list(
      id = "review-panel",
      memory = memory,
      file_path = file_filter
    ),
    {
      returned <- session$getReturned()

      expect_equal(nrow(returned$reviews_raw()), 2L)
      expect_equal(length(returned$review_items()), 2L)

      file_filter("tutorial/a.Rmd")
      session$flushReact()
      expect_equal(nrow(returned$reviews_raw()), 1L)
      expect_equal(returned$reviews_raw()$chunk_id[[1]], "chunk_one")

      file_filter(NULL)
      session$flushReact()
      expect_equal(nrow(returned$reviews_raw()), 2L)
      expect_equal(length(returned$review_items()), 2L)
    }
  )
})
