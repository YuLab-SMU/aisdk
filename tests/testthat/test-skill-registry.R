
# Tests for SkillRegistry class
library(testthat)
library(aisdk)

# Get path to test fixtures
fixtures_path <- testthat::test_path("fixtures", "skills")

# === Tests for SkillRegistry initialization ===

test_that("SkillRegistry can be created empty", {
  registry <- SkillRegistry$new()
  
  expect_s3_class(registry, "SkillRegistry")
  expect_equal(registry$count(), 0)
})

test_that("SkillRegistry can be created with path", {
  skip_if_not(dir.exists(fixtures_path), "Test fixtures not found")
  
  registry <- SkillRegistry$new(fixtures_path)
  
  expect_s3_class(registry, "SkillRegistry")
  expect_gte(registry$count(), 1)
})

# === Tests for SkillRegistry$scan_skills ===

test_that("SkillRegistry$scan_skills finds skills", {
  skip_if_not(dir.exists(fixtures_path), "Test fixtures not found")
  
  registry <- SkillRegistry$new()
  registry$scan_skills(fixtures_path)
  
  expect_gte(registry$count(), 1)
  expect_true(registry$has_skill("test_skill"))
})

test_that("SkillRegistry$scan_skills aborts for missing directory", {
  registry <- SkillRegistry$new()
  
  expect_error(registry$scan_skills("/nonexistent/path"), "does not exist")
})

# === Tests for SkillRegistry$get_skill ===

test_that("SkillRegistry$get_skill returns skill by name", {
  skip_if_not(dir.exists(fixtures_path), "Test fixtures not found")
  
  registry <- SkillRegistry$new(fixtures_path)
  skill <- registry$get_skill("test_skill")
  
  expect_s3_class(skill, "Skill")
  expect_equal(skill$name, "test_skill")
})

test_that("SkillRegistry$get_skill returns NULL for missing skill", {
  registry <- SkillRegistry$new()
  
  expect_null(registry$get_skill("nonexistent"))
})

# === Tests for SkillRegistry$has_skill ===

test_that("SkillRegistry$has_skill returns correct boolean", {
  skip_if_not(dir.exists(fixtures_path), "Test fixtures not found")
  
  registry <- SkillRegistry$new(fixtures_path)
  
  expect_true(registry$has_skill("test_skill"))
  expect_false(registry$has_skill("nonexistent"))
})

# === Tests for SkillRegistry$list_skills ===

test_that("SkillRegistry$list_skills returns data.frame", {
  skip_if_not(dir.exists(fixtures_path), "Test fixtures not found")
  
  registry <- SkillRegistry$new(fixtures_path)
  skills <- registry$list_skills()
  
  expect_s3_class(skills, "data.frame")
  expect_true("name" %in% names(skills))
  expect_true("description" %in% names(skills))
  expect_gte(nrow(skills), 1)
})

test_that("SkillRegistry$list_skills returns empty data.frame when empty", {
  registry <- SkillRegistry$new()
  skills <- registry$list_skills()
  
  expect_s3_class(skills, "data.frame")
  expect_equal(nrow(skills), 0)
})

# === Tests for SkillRegistry$generate_prompt_section ===

test_that("SkillRegistry$generate_prompt_section returns formatted string", {
  skip_if_not(dir.exists(fixtures_path), "Test fixtures not found")
  
  registry <- SkillRegistry$new(fixtures_path)
  prompt <- registry$generate_prompt_section()
  
  expect_type(prompt, "character")
  expect_true(grepl("Available Skills", prompt))
  expect_true(grepl("test_skill", prompt))
  expect_true(grepl("load_skill", prompt))
})

test_that("SkillRegistry$generate_prompt_section returns empty string when empty", {
  registry <- SkillRegistry$new()
  prompt <- registry$generate_prompt_section()
  
  expect_equal(prompt, "")
})

# === Tests for convenience functions ===

test_that("create_skill_registry creates populated registry", {
  skip_if_not(dir.exists(fixtures_path), "Test fixtures not found")
  
  registry <- create_skill_registry(fixtures_path)
  
  expect_s3_class(registry, "SkillRegistry")
  expect_gte(registry$count(), 1)
})

test_that("scan_skills is alias for create_skill_registry", {
  skip_if_not(dir.exists(fixtures_path), "Test fixtures not found")
  
  registry <- scan_skills(fixtures_path)
  
  expect_s3_class(registry, "SkillRegistry")
  expect_gte(registry$count(), 1)
})

# === Tests for SkillRegistry$print ===

test_that("SkillRegistry$print outputs summary", {
  skip_if_not(dir.exists(fixtures_path), "Test fixtures not found")
  
  registry <- SkillRegistry$new(fixtures_path)
  
  output <- capture.output(print(registry))
  expect_true(any(grepl("<SkillRegistry>", output)))
  expect_true(any(grepl("registered", output)))
})

# === Tests for SkillRegistry subdirectory scanning ===

test_that("SkillRegistry finds skills in subdirectories (recursive=FALSE)", {
    # Create temp structure:
    # tmp/
    #     skill_a/SKILL.md
    #     SKILL.md (in root)
    #     nested/deep/SKILL.md (should NOT be found if recursive=FALSE)
    
    temp_dir <- file.path(tempdir(), paste0("test_skills_", as.integer(Sys.time())))
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE))
    
    # Skill in root
    writeLines("---
name: root_skill
description: Root skill
---
# Root Skill", file.path(temp_dir, "SKILL.md"))
    
    # Skill in immediate subdir
    dir.create(file.path(temp_dir, "subdir_skill"))
    writeLines("---
name: subdir_skill
description: Subdir skill
---
# Subdir Skill", file.path(temp_dir, "subdir_skill", "SKILL.md"))
    
    # Skill in nested subdir
    dir.create(file.path(temp_dir, "nested", "deep"), recursive = TRUE)
    writeLines("---
name: deep_skill
description: Deep skill
---
# Deep Skill", file.path(temp_dir, "nested", "deep", "SKILL.md"))
    
    # Scan with recursive = FALSE
    registry <- SkillRegistry$new()
    registry$scan_skills(temp_dir, recursive = FALSE)
    
    # Should find root and subdir, but NOT deep
    expect_true(registry$has_skill("root_skill"))
    expect_true(registry$has_skill("subdir_skill"))
    expect_false(registry$has_skill("deep_skill"))
})