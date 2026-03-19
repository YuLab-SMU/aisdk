load_ai_review_card_env <- function() {
  env <- aisdk_test_env()

  for (file in c("artifacts.R", "ai_review_card.R")) {
    source_local_aisdk_file(env, file)
  }

  env
}

test_that("ai_review_html_dependency exposes the static review assets", {
  env <- load_ai_review_card_env()
  dep <- env$ai_review_html_dependency()

  expect_equal(dep$name, "aisdk-review")
  expect_true(file.exists(file.path(dep$src$file, "aisdk-review.css")))
  expect_true(file.exists(file.path(dep$src$file, "aisdk-review.js")))
})

test_that("render_ai_review_card_html builds a structured review card", {
  env <- load_ai_review_card_env()

  html <- env$render_ai_review_card_html(list(
    chunk_id = "chunk-1",
    chunk_label = "analysis",
    state = "ran",
    review = "required",
    runtime = "static",
    prompt = "Create a scatter plot of mpg vs wt",
    response = "Use ggplot2 to plot the relationship.",
    extracted_code = "library(ggplot2)\nggplot(mtcars, aes(wt, mpg)) + geom_point()",
    execution_output = "<div class='plot-output'>plot</div>",
    session_summary = list(
      session_id = "session-1",
      model = "openai:gpt-4o",
      retries = 1
    ),
    timestamps = list(
      created_at = "2026-03-13 21:00:00",
      updated_at = "2026-03-13 21:05:00"
    )
  ))

  expect_match(html, "aisdk-ai-review-card")
  expect_match(html, "data-chunk-id=\"chunk-1\"")
  expect_match(html, "data-state=\"ran\"")
  expect_match(html, "Prompt")
  expect_match(html, "Draft Response")
  expect_match(html, "Extracted Code")
  expect_match(html, "Execution Result")
  expect_match(html, "Agent Summary")
  expect_match(html, "Agent Trace")
  expect_match(html, "Artifact Metadata")
  expect_match(html, "session-1")
  expect_match(html, "Published AI artifact")
  expect_no_match(html, "Approve &amp; Freeze")
  expect_no_match(html, "<pre><code>&lt;div class=\"aisdk-ai-review-card__title-group\"")
})

test_that("render_ai_review_card_html renders a read-only artifact footer for review chunks", {
  env <- load_ai_review_card_env()

  html <- env$render_ai_review_card_html(list(
    chunk_id = "chunk-static",
    chunk_label = "static-analysis",
    state = "draft",
    review = "required",
    runtime = "static",
    prompt = "Summarize mtcars",
    response = "Draft summary",
    extracted_code = "summary(mtcars)"
  ))

  expect_match(html, "Published AI artifact")
  expect_match(html, "Agent Trace")
  expect_match(html, "Artifact Metadata")
  expect_no_match(html, "Regenerate")
  expect_no_match(html, "Run Draft")
  expect_no_match(html, "Reject")
})

test_that("render_ai_review_card_html degrades cleanly when optional fields are missing", {
  env <- load_ai_review_card_env()

  html <- env$render_ai_review_card_html(list(
    chunk_id = "chunk-minimal",
    chunk_label = "minimal",
    state = "draft",
    review = "none",
    runtime = "static",
    prompt = "Return 1 + 1",
    response = "```r\n1 + 1\n```"
  ))

  expect_match(html, "chunk-minimal")
  expect_match(html, "Prompt")
  expect_no_match(html, "Execution Result")
  expect_no_match(html, "Agent Trace")
})
