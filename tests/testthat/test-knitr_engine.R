# Tests for knitr engine and context functionality

load_knitr_engine_test_env <- function() {
  env <- aisdk_test_env(parent = globalenv())

  files <- c(
    "utils_env.R",
    "context.R",
    "session.R",
    "ai_review_contract.R",
    "ai_review_embedded_contract.R",
    "artifacts.R",
    "ai_review_card.R",
    "knitr_engine.R"
  )

  for (file in files) {
    source_local_aisdk_file(env, file)
  }

  env
}

knitr_engine_test_env <- load_knitr_engine_test_env()
for (name in ls(knitr_engine_test_env, all.names = TRUE)) {
  assign(name, get(name, envir = knitr_engine_test_env), envir = environment())
}

openai_model_id <- get_openai_model_id()

# ============================================================================
# Context Tests
# ============================================================================

test_that("get_r_context returns empty string for NULL or empty vars", {
  expect_equal(get_r_context(NULL), "")
  expect_equal(get_r_context(character(0)), "")
})

test_that("get_r_context handles non-existent variables", {
  ctx <- get_r_context("nonexistent_var_xyz", envir = environment())
  expect_match(ctx, "Not found")
})

test_that("get_r_context summarizes data frames correctly", {
  df <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)
  ctx <- get_r_context("df", envir = environment())

  expect_match(ctx, "Variable: `df`")
  expect_match(ctx, "Data Frame")
  expect_match(ctx, "5 rows x 2 columns")
  expect_match(ctx, "`a`")
  expect_match(ctx, "`b`")
})

test_that("get_r_context summarizes vectors correctly", {
  nums <- c(1, 2, 3, 4, 5)
  ctx <- get_r_context("nums", envir = environment())

  expect_match(ctx, "Vector")
  expect_match(ctx, "length 5")
})

test_that("get_r_context summarizes lists correctly", {
  my_list <- list(a = 1, b = "hello", c = 1:3)
  ctx <- get_r_context("my_list", envir = environment())

  expect_match(ctx, "List")
  expect_match(ctx, "3 elements")
})

test_that("summarize_object handles NULL", {
  result <- summarize_object(NULL, "test")
  expect_match(result, "NULL")
})

test_that("summarize_object handles functions", {
  my_func <- function(x, y) x + y
  result <- summarize_object(my_func, "my_func")
  expect_match(result, "Function")
  expect_match(result, "x, y")
})

# ============================================================================
# Code Extraction Tests
# ============================================================================

test_that("extract_r_code extracts single code block", {
  text <- "Here is the code:\n```r\nprint('hello')\n```"
  code <- extract_r_code(text)
  expect_match(code, "print\\('hello'\\)")
})

test_that("extract_r_code extracts multiple code blocks", {
  text <- "First:\n```r\nx <- 1\n```\nSecond:\n```r\ny <- 2\n```"
  code <- extract_r_code(text)
  expect_match(code, "x <- 1")
  expect_match(code, "y <- 2")
})

test_that("extract_r_code handles {r} syntax", {
  text <- "Code:\n```{r}\nplot(1:10)\n```"
  code <- extract_r_code(text)
  expect_match(code, "plot\\(1:10\\)")
})

test_that("extract_r_code handles {r options} syntax", {
  text <- "Code:\n```{r echo=FALSE}\nprint(42)\n```"
  code <- extract_r_code(text)
  expect_match(code, "print\\(42\\)")
})

test_that("extract_r_code returns empty for no code blocks", {
  text <- "Just some text without code"
  code <- extract_r_code(text)
  expect_equal(code, "")
})

test_that("extract_r_code handles NULL and empty input", {
  expect_equal(extract_r_code(NULL), "")
  expect_equal(extract_r_code(""), "")
})

# ============================================================================
# Code Block Removal Tests
# ============================================================================

test_that("remove_r_code_blocks removes code", {
  text <- "Here is some code:\n```r\nprint('hello')\n```\nAnd more text."
  clean <- remove_r_code_blocks(text)
  expect_match(clean, "Here is some code:")
  expect_match(clean, "And more text")
  expect_no_match(clean, "print")
})

test_that("remove_r_code_blocks handles empty input", {
  expect_equal(remove_r_code_blocks(""), "")
  expect_equal(remove_r_code_blocks(NULL), "")
})

# ============================================================================
# Context Building Tests
# ============================================================================

test_that("build_context returns empty when disabled", {
  result <- build_context("analyze df", FALSE, environment())
  expect_equal(result, "")
})

test_that("build_context uses specified vars", {
  df <- data.frame(x = 1:3)
  result <- build_context("anything", "df", environment())
  expect_match(result, "Variable: `df`")
})

test_that("auto_detect_vars finds variables in environment", {
  my_data <- 1:10
  vars <- auto_detect_vars("analyze my_data please", environment())
  expect_true("my_data" %in% vars)
})

test_that("auto_detect_vars excludes R keywords", {
  vars <- auto_detect_vars("if TRUE then FALSE", environment())
  expect_false("TRUE" %in% vars)
  expect_false("FALSE" %in% vars)
  expect_false("if" %in% vars)
})


# ============================================================================
# Session Management Tests
# ============================================================================

test_that("get_or_create_session creates new session", {
  # Clear any existing session
  clear_ai_session()

  opts <- list(model = openai_model_id)
  session <- get_or_create_session(opts)

  expect_s3_class(session, "ChatSession")
})

test_that("get_or_create_session returns same session on second call", {
  clear_ai_session()

  opts <- list(model = openai_model_id)
  session1 <- get_or_create_session(opts)
  session2 <- get_or_create_session(opts)

  expect_identical(session1, session2)
})

test_that("get_or_create_session respects new_session option", {
  clear_ai_session()

  opts1 <- list(model = openai_model_id)
  session1 <- get_or_create_session(opts1)

  opts2 <- list(model = openai_model_id, new_session = TRUE)
  session2 <- get_or_create_session(opts2)

  expect_false(identical(session1, session2))
})

test_that("clear_ai_session clears sessions", {
  opts <- list(model = openai_model_id)
  session <- get_or_create_session(opts)

  clear_ai_session()

  expect_null(get_ai_session("default"))
})

test_that("get_ai_session returns NULL for non-existent session", {
  clear_ai_session()
  expect_null(get_ai_session("nonexistent"))
})

# ============================================================================
# Prompt Construction Tests
# ============================================================================

test_that("construct_prompt adds context when present", {
  result <- construct_prompt("analyze this", "Context info here")
  expect_match(result, "Available Data Context")
  expect_match(result, "Context info here")
  expect_match(result, "User Request")
  expect_match(result, "analyze this")
})

test_that("construct_prompt returns just prompt when no context", {
  result <- construct_prompt("analyze this", "")
  expect_equal(result, "analyze this")
})

test_that("resolve_ai_review_input_path maps Quarto intermediates back to qmd", {
  tmp_dir <- tempfile("ai-knitr-quarto-")
  dir.create(tmp_dir, recursive = TRUE)

  qmd_path <- file.path(tmp_dir, "example.qmd")
  writeLines("---\ntitle: test\n---", qmd_path)

  rmarkdown_path <- file.path(tmp_dir, "example.rmarkdown")

  expect_equal(
    resolve_ai_review_input_path(rmarkdown_path),
    normalizePath(qmd_path, winslash = "/", mustWork = FALSE)
  )

  expect_equal(
    resolve_ai_review_input_path(file.path(tmp_dir, "plain.Rmd")),
    normalizePath(file.path(tmp_dir, "plain.Rmd"), winslash = "/", mustWork = FALSE)
  )

  expect_equal(resolve_ai_review_input_path(NULL), "unknown.Rmd")
})

# ============================================================================
# Default System Prompt Test
# ============================================================================

test_that("get_default_system_prompt returns non-empty string", {
  prompt <- get_default_system_prompt()
  expect_true(nzchar(prompt))
  expect_match(prompt, "R")
})

make_simple_ai_session_factory <- function(response_text, counter_env) {
  function(model, system_prompt) {
    counter_env$model <- model
    counter_env$system_prompt <- system_prompt

    list(
      send = function(prompt) {
        counter_env$send_calls <- (counter_env$send_calls %||% 0L) + 1L
        counter_env$last_prompt <- prompt
        list(text = response_text)
      },
      get_model_id = function() model,
      switch_model = function(model_id) {
        counter_env$model <- model_id
        invisible(TRUE)
      },
      get_history = function() {
        list(
          list(role = "user", content = counter_env$last_prompt %||% ""),
          list(role = "assistant", content = response_text)
        )
      }
    )
  }
}

test_that("eng_ai returns the model response directly when eval is FALSE", {
  env <- load_knitr_engine_test_env()
  counter <- new.env(parent = emptyenv())
  env$create_chat_session <- make_simple_ai_session_factory(
    "Here is the code:\n```r\nx <- 1 + 1\n```",
    counter
  )

  exec_env <- new.env(parent = globalenv())
  output <- env$eng_ai(list(
    engine = "ai",
    label = "simple-no-eval",
    code = "Return one plus one",
    eval = FALSE,
    echo = FALSE,
    envir = exec_env
  ))

  expect_match(output, "Here is the code")
  expect_match(output, "```r")
  expect_equal(counter$send_calls, 1L)
})

test_that("eng_ai executes extracted code without invoking review infrastructure", {
  env <- load_knitr_engine_test_env()
  counter <- new.env(parent = emptyenv())
  env$create_chat_session <- make_simple_ai_session_factory(
    "Run this\n```r\nsimple_value <- 2 + 2\nprint(simple_value)\n```",
    counter
  )
  env$get_memory <- function() {
    stop("get_memory should not be called by eng_ai")
  }

  exec_env <- new.env(parent = globalenv())
  output <- env$eng_ai(list(
    engine = "ai",
    label = "simple-eval",
    code = "Calculate two plus two",
    eval = TRUE,
    echo = FALSE,
    envir = exec_env
  ))

  expect_equal(get("simple_value", envir = exec_env), 4)
  expect_match(output, "Run this")
  expect_match(output, "4")
  expect_equal(counter$send_calls, 1L)
})
