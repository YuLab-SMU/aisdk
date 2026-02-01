# Tests for Skill class
library(testthat)
library(aisdk)

# Get path to test fixtures
fixtures_path <- testthat::test_path("fixtures", "skills")
test_skill_path <- file.path(fixtures_path, "test_skill")

# === Tests for Skill initialization ===

test_that("Skill can be created from valid SKILL.md", {
  skip_if_not(dir.exists(test_skill_path), "Test fixtures not found")
  
  skill <- Skill$new(test_skill_path)
  
  expect_s3_class(skill, "Skill")
  expect_equal(skill$name, "test_skill")
  expect_equal(skill$description, "A test skill for unit testing the Skills system")
})

test_that("Skill$path is normalized", {
  skip_if_not(dir.exists(test_skill_path), "Test fixtures not found")
  
  skill <- Skill$new(test_skill_path)
  
  expect_true(file.exists(skill$path))
  expect_false(grepl("\\.\\.", skill$path))
})

test_that("Skill validates path argument", {
  expect_error(Skill$new(123), "character string")
  expect_error(Skill$new(c("a", "b")), "character string")
})

test_that("Skill aborts if SKILL.md not found", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  
  expect_error(Skill$new(tmp_dir), "SKILL.md not found")
})

test_that("Skill aborts if YAML frontmatter missing", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  writeLines("# No frontmatter here", file.path(tmp_dir, "SKILL.md"))
  
  expect_error(Skill$new(tmp_dir), "YAML frontmatter")
})

test_that("Skill aborts if name field missing", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  writeLines(c("---", "description: test", "---"), file.path(tmp_dir, "SKILL.md"))
  
  expect_error(Skill$new(tmp_dir), "name")
})

# === Tests for Skill$load ===

test_that("Skill$load returns SKILL.md body content", {
  skip_if_not(dir.exists(test_skill_path), "Test fixtures not found")
  
  skill <- Skill$new(test_skill_path)
  body <- skill$load()
  
  expect_type(body, "character")
  expect_true(grepl("Test Skill Instructions", body))
  expect_true(grepl("Available Scripts", body))
})

# === Tests for Skill$list_scripts ===

test_that("Skill$list_scripts returns R files", {
  skip_if_not(dir.exists(test_skill_path), "Test fixtures not found")
  
  skill <- Skill$new(test_skill_path)
  scripts <- skill$list_scripts()
  
  expect_type(scripts, "character")
  expect_true("hello.R" %in% scripts)
})

test_that("Skill$list_scripts returns empty for skill without scripts dir", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))
  writeLines(c("---", "name: test", "---"), file.path(tmp_dir, "SKILL.md"))
  
  skill <- Skill$new(tmp_dir)
  scripts <- skill$list_scripts()
  
  expect_equal(length(scripts), 0)
})

# === Tests for Skill$execute_script ===

test_that("Skill$execute_script runs script and returns result", {
  skip_if_not(dir.exists(test_skill_path), "Test fixtures not found")
  
  skill <- Skill$new(test_skill_path)
  result <- skill$execute_script("hello.R", list(name = "Claude"))
  
  expect_equal(result, "Hello, Claude from test_skill!")
})

test_that("Skill$execute_script uses default when no args provided", {
  skip_if_not(dir.exists(test_skill_path), "Test fixtures not found")
  
  skill <- Skill$new(test_skill_path)
  result <- skill$execute_script("hello.R")
  
  expect_equal(result, "Hello, World from test_skill!")
})

test_that("Skill$execute_script returns error message for missing script", {
  skip_if_not(dir.exists(test_skill_path), "Test fixtures not found")
  
  skill <- Skill$new(test_skill_path)
  
  expect_error(skill$execute_script("nonexistent.R"), "Script not found")
})

# === Tests for Skill$print ===

test_that("Skill$print outputs summary", {
  skip_if_not(dir.exists(test_skill_path), "Test fixtures not found")
  
  skill <- Skill$new(test_skill_path)
  
  output <- capture.output(print(skill))
  expect_true(any(grepl("<Skill>", output)))
  expect_true(any(grepl("test_skill", output)))
})
