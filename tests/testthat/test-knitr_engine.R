# Tests for knitr engine and context functionality

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
  result <- aisdk:::summarize_object(NULL, "test")
  expect_match(result, "NULL")
})

test_that("summarize_object handles functions", {
  my_func <- function(x, y) x + y
  result <- aisdk:::summarize_object(my_func, "my_func")
  expect_match(result, "Function")
  expect_match(result, "x, y")
})

# ============================================================================
# Code Extraction Tests
# ============================================================================

test_that("extract_r_code extracts single code block", {
  text <- "Here is the code:\n```r\nprint('hello')\n```"
  code <- aisdk:::extract_r_code(text)
  expect_match(code, "print\\('hello'\\)")
})
  
test_that("extract_r_code extracts multiple code blocks", {
  text <- "First:\n```r\nx <- 1\n```\nSecond:\n```r\ny <- 2\n```"
  code <- aisdk:::extract_r_code(text)
  expect_match(code, "x <- 1")
  expect_match(code, "y <- 2")
})

test_that("extract_r_code handles {r} syntax", {
  text <- "Code:\n```{r}\nplot(1:10)\n```"
  code <- aisdk:::extract_r_code(text)
  expect_match(code, "plot\\(1:10\\)")
})

test_that("extract_r_code handles {r options} syntax", {
  text <- "Code:\n```{r echo=FALSE}\nprint(42)\n```"
  code <- aisdk:::extract_r_code(text)
  expect_match(code, "print\\(42\\)")
})

test_that("extract_r_code returns empty for no code blocks", {
  text <- "Just some text without code"
  code <- aisdk:::extract_r_code(text)
  expect_equal(code, "")
})

test_that("extract_r_code handles NULL and empty input", {
  expect_equal(aisdk:::extract_r_code(NULL), "")
  expect_equal(aisdk:::extract_r_code(""), "")
})

# ============================================================================
# Code Block Removal Tests
# ============================================================================

test_that("remove_r_code_blocks removes code", {
  text <- "Here is some code:\n```r\nprint('hello')\n```\nAnd more text."
  clean <- aisdk:::remove_r_code_blocks(text)
  expect_match(clean, "Here is some code:")
  expect_match(clean, "And more text")
  expect_no_match(clean, "print")
})

test_that("remove_r_code_blocks handles empty input", {
  expect_equal(aisdk:::remove_r_code_blocks(""), "")
  expect_equal(aisdk:::remove_r_code_blocks(NULL), "")
})

# ============================================================================
# Context Building Tests
# ============================================================================

test_that("build_context returns empty when disabled", {
  result <- aisdk:::build_context("analyze df", FALSE, environment())
  expect_equal(result, "")
})

test_that("build_context uses specified vars", {
  df <- data.frame(x = 1:3)
  result <- aisdk:::build_context("anything", "df", environment())
  expect_match(result, "Variable: `df`")
})

test_that("auto_detect_vars finds variables in environment", {
  my_data <- 1:10
  vars <- aisdk:::auto_detect_vars("analyze my_data please", environment())
  expect_true("my_data" %in% vars)
})

test_that("auto_detect_vars excludes R keywords", {
  vars <- aisdk:::auto_detect_vars("if TRUE then FALSE", environment())
  expect_false("TRUE" %in% vars)
  expect_false("FALSE" %in% vars)
  expect_false("if" %in% vars)
})

# ============================================================================
# Code Execution Tests
# ============================================================================

test_that("execute_code_safely runs simple code", {
  result <- aisdk:::execute_code_safely("1 + 1", environment())
  expect_match(result, "2")
})

test_that("execute_code_safely captures print output", {
  result <- aisdk:::execute_code_safely("print('hello world')", environment())
  expect_match(result, "hello world")
})

test_that("execute_code_safely handles errors gracefully", {
  result <- aisdk:::execute_code_safely("stop('test error')", environment())
  expect_match(result, "Error|error")
})

test_that("execute_code_safely modifies environment", {
  test_env <- new.env()
  aisdk:::execute_code_safely("my_var <- 42", test_env)
  expect_true(exists("my_var", envir = test_env))
  expect_equal(test_env$my_var, 42)
})

test_that("execute_code_safely handles empty code", {
  result <- aisdk:::execute_code_safely("", environment())
  expect_equal(result, "")
  
  result2 <- aisdk:::execute_code_safely("   ", environment())
  expect_equal(result2, "")
})

# ============================================================================
# Session Management Tests
# ============================================================================

test_that("get_or_create_session creates new session", {
  # Clear any existing session
  aisdk:::clear_ai_session()
  
  opts <- list(model = openai_model_id)
  session <- aisdk:::get_or_create_session(opts)
  
  expect_s3_class(session, "ChatSession")
})

test_that("get_or_create_session returns same session on second call", {
  aisdk:::clear_ai_session()
  
  opts <- list(model = openai_model_id)
  session1 <- aisdk:::get_or_create_session(opts)
  session2 <- aisdk:::get_or_create_session(opts)
  
  expect_identical(session1, session2)
})

test_that("get_or_create_session respects new_session option", {
  aisdk:::clear_ai_session()
  
  opts1 <- list(model = openai_model_id)
  session1 <- aisdk:::get_or_create_session(opts1)
  
  opts2 <- list(model = openai_model_id, new_session = TRUE)
  session2 <- aisdk:::get_or_create_session(opts2)
  
  expect_false(identical(session1, session2))
})

test_that("clear_ai_session clears sessions", {
  opts <- list(model = openai_model_id)
  session <- aisdk:::get_or_create_session(opts)
  
  aisdk:::clear_ai_session()
  
  expect_null(aisdk:::get_ai_session("default"))
})

test_that("get_ai_session returns NULL for non-existent session", {
  aisdk:::clear_ai_session()
  expect_null(get_ai_session("nonexistent"))
})

# ============================================================================
# Prompt Construction Tests
# ============================================================================

test_that("construct_prompt adds context when present", {
  result <- aisdk:::construct_prompt("analyze this", "Context info here")
  expect_match(result, "Available Data Context")
  expect_match(result, "Context info here")
  expect_match(result, "User Request")
  expect_match(result, "analyze this")
})

test_that("construct_prompt returns just prompt when no context", {
  result <- aisdk:::construct_prompt("analyze this", "")
  expect_equal(result, "analyze this")
})

# ============================================================================
# Default System Prompt Test
# ============================================================================

test_that("get_default_system_prompt returns non-empty string", {
  prompt <- aisdk:::get_default_system_prompt()
  expect_true(nzchar(prompt))
  expect_match(prompt, "R")
})
