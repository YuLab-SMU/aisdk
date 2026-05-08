
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
  expect_true("aliases" %in% names(skills))
  expect_true("when_to_use" %in% names(skills))
  expect_true("paths" %in% names(skills))
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
  expect_true(grepl("even if the user does not know the skill name", prompt, fixed = TRUE))
})

test_that("SkillRegistry$generate_prompt_section returns empty string when empty", {
  registry <- SkillRegistry$new()
  prompt <- registry$generate_prompt_section()
  
  expect_equal(prompt, "")
})

test_that("SkillRegistry resolves aliases and fuzzy names", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))
  dir.create(file.path(temp_dir, "mentor-skill"))
  writeLines(c(
    "---",
    "name: mentor-skill",
    "description: mentor skill",
    "aliases:",
    "  - yshu",
    "  - Y叔",
    "---",
    "Body"
  ), file.path(temp_dir, "mentor-skill", "SKILL.md"))

  registry <- SkillRegistry$new(temp_dir)

  expect_equal(registry$resolve_skill_name("yshu"), "mentor-skill")
  expect_equal(registry$resolve_skill_name("Y叔"), "mentor-skill")
  expect_equal(registry$find_closest_skill_name("menter-skill"), "mentor-skill")
})

test_that("SkillRegistry finds relevant skills from query and file paths", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  dir.create(file.path(temp_dir, "rna-skill"))
  writeLines(c(
    "---",
    "name: rna-skill",
    "description: RNA analysis helper",
    "when_to_use: Use this when the user asks about differential expression or RNA-seq",
    "paths:",
    "  - counts/*.csv",
    "---",
    "Body"
  ), file.path(temp_dir, "rna-skill", "SKILL.md"))

  registry <- SkillRegistry$new(temp_dir)

  by_query <- registry$find_relevant_skills(query = "我想做 differential expression", limit = 1L)
  expect_equal(by_query$name[[1]], "rna-skill")
  expect_true(grepl("when_to_use", by_query$matched_by[[1]], fixed = TRUE))

  by_path <- registry$find_relevant_skills(
    query = "",
    file_paths = c("counts/sample.csv"),
    cwd = temp_dir,
    limit = 1L
  )
  expect_equal(by_path$name[[1]], "rna-skill")
  expect_true(grepl("paths", by_path$matched_by[[1]], fixed = TRUE))
})

test_that("SkillRegistry refresh picks up skill updates on disk", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  dir.create(file.path(temp_dir, "live-skill"))
  writeLines(c(
    "---",
    "name: live-skill",
    "description: First version",
    "---",
    "Body v1"
  ), file.path(temp_dir, "live-skill", "SKILL.md"))

  registry <- SkillRegistry$new()
  registry$scan_skills(temp_dir, recursive = TRUE)
  expect_equal(registry$get_skill("live-skill")$description, "First version")
  expect_equal(nrow(registry$list_roots()), 1L)

  writeLines(c(
    "---",
    "name: live-skill",
    "description: Updated version",
    "---",
    "Body v2"
  ), file.path(temp_dir, "live-skill", "SKILL.md"))

  dir.create(file.path(temp_dir, "new-skill"))
  writeLines(c(
    "---",
    "name: new-skill",
    "description: Added later",
    "---",
    "New body"
  ), file.path(temp_dir, "new-skill", "SKILL.md"))

  registry$refresh()

  expect_equal(registry$get_skill("live-skill")$description, "Updated version")
  expect_true(registry$has_skill("new-skill"))
})

test_that("default_skill_roots includes user install path and prioritizes project roots", {
  old_roots <- getOption("aisdk.skill_roots")
  old_env <- Sys.getenv("AISDK_SKILL_PATHS", unset = NA_character_)
  on.exit(options(aisdk.skill_roots = old_roots), add = TRUE)
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("AISDK_SKILL_PATHS") else Sys.setenv(AISDK_SKILL_PATHS = old_env)
  }, add = TRUE)
  options(aisdk.skill_roots = character(0))
  Sys.unsetenv("AISDK_SKILL_PATHS")

  project_dir <- tempfile()
  dir.create(project_dir)
  on.exit(unlink(project_dir, recursive = TRUE), add = TRUE)

  project_skills <- file.path(project_dir, ".aisdk", "skills")
  dir.create(project_skills, recursive = TRUE)
  project_skills <- normalizePath(project_skills, winslash = "/", mustWork = FALSE)
  dot_skills <- file.path(project_dir, ".skills")
  dir.create(dot_skills, recursive = TRUE)
  dot_skills <- normalizePath(dot_skills, winslash = "/", mustWork = FALSE)

  roots <- default_skill_roots(project_dir = project_dir)

  expect_true(dot_skills %in% roots)
  expect_true(project_skills %in% roots)
  expect_true(any(grepl("\\.skills$", roots)))
  expect_true(any(grepl("\\.aisdk/skills$", roots)))
  expect_equal(tail(roots, 1), normalizePath(project_skills, winslash = "/", mustWork = FALSE))
})

test_that("auto skill registry discovers project .skills directory", {
  project_dir <- tempfile()
  skill_dir <- file.path(project_dir, ".skills", "project-skill")
  dir.create(skill_dir, recursive = TRUE)
  on.exit(unlink(project_dir, recursive = TRUE), add = TRUE)

  writeLines(c(
    "---",
    "name: project-skill",
    "description: Project dot skills",
    "---",
    "Project-local skill body"
  ), file.path(skill_dir, "SKILL.md"))

  registry <- aisdk:::create_auto_skill_registry(project_dir = project_dir, recursive = TRUE)

  expect_true(registry$has_skill("project-skill"))
  expect_true(any(grepl("\\.skills$", registry$list_roots()$path)))
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

test_that("create_skill_registry accepts multiple roots with later roots overriding earlier skills", {
  root_a <- tempfile("skills-a-")
  root_b <- tempfile("skills-b-")
  dir.create(file.path(root_a, "shared"), recursive = TRUE)
  dir.create(file.path(root_b, "shared"), recursive = TRUE)
  on.exit(unlink(root_a, recursive = TRUE), add = TRUE)
  on.exit(unlink(root_b, recursive = TRUE), add = TRUE)

  writeLines(c(
    "---",
    "name: shared",
    "description: From A",
    "---",
    "A"
  ), file.path(root_a, "shared", "SKILL.md"))
  writeLines(c(
    "---",
    "name: shared",
    "description: From B",
    "---",
    "B"
  ), file.path(root_b, "shared", "SKILL.md"))

  registry <- create_skill_registry(c(root_a, root_b), recursive = TRUE)

  expect_equal(registry$get_skill("shared")$description, "From B")
  expect_equal(nrow(registry$list_roots()), 2L)
})
