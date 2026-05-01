
test_that("Agent initializes with skills='auto'", {
  # Mock dir.exists to satisfy "auto" logic
  # Since we can't easily mock base R functions in this script without mockery or testthat::local_mock,
  # we will use a temporary directory structure.
  
  temp_root <- tempdir()
  skill_dir <- file.path(temp_root, "inst", "skills")
  dir.create(skill_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Create a dummy skill
  dummy_skill_path <- file.path(skill_dir, "dummy_skill")
  dir.create(dummy_skill_path, recursive = TRUE)
  writeLines(c(
    "---",
    "name: dummy_skill",
    "description: A dummy skill",
    "---",
    "Instructions for dummy skill"
  ), file.path(dummy_skill_path, "SKILL.md"))
  
  # Temporarily change WD to temp_root so "auto" finds it
  withr::with_dir(temp_root, {
      # Source agent again to use the new definition? No, function is already defined.
      # But create_agent uses getwd().
      
      agent <- create_agent(
          name = "TestAgent",
          description = "Test Description",
          skills = "auto"
      )
      
      # Check if tools are added (load_skill, execute_skill_script, list_skill_scripts)
      tool_names <- sapply(agent$tools, function(t) t$name)
      expect_true("load_skill" %in% tool_names)
      expect_true("execute_skill_script" %in% tool_names)
      expect_true("list_skill_scripts" %in% tool_names)
      expect_true("reload_skills" %in% tool_names)

      # Check if system prompt contains skill info
      expect_true(grepl("Available Skills", agent$system_prompt))
      expect_true(grepl("dummy_skill", agent$system_prompt))
  })
})

test_that("Agent initializes with explicit skill path", {
  temp_root <- tempdir()
  skill_dir <- file.path(temp_root, "my_custom_skills")
  dir.create(skill_dir, recursive = TRUE, showWarnings = FALSE)
  
  dummy_skill_path <- file.path(skill_dir, "custom_skill")
  dir.create(dummy_skill_path, recursive = TRUE)
  writeLines(c(
    "---",
    "name: custom_skill",
    "description: A custom skill",
    "---",
    "Instructions"
  ), file.path(dummy_skill_path, "SKILL.md"))
  
  agent <- create_agent(
      name = "CustomAgent",
      description = "Desc",
      skills = skill_dir # Pass path directly
  )

  expect_true(grepl("custom_skill", agent$system_prompt))
  expect_gte(length(agent$tools), 7)
})

test_that("skill tools can reload updated skills from disk", {
  temp_root <- tempfile()
  skill_dir <- file.path(temp_root, "skills")
  dir.create(file.path(skill_dir, "live_skill"), recursive = TRUE)
  on.exit(unlink(temp_root, recursive = TRUE), add = TRUE)

  writeLines(c(
    "---",
    "name: live_skill",
    "description: Live skill",
    "---",
    "Version one"
  ), file.path(skill_dir, "live_skill", "SKILL.md"))

  registry <- create_skill_registry(skill_dir, recursive = TRUE)
  tools <- create_skill_tools(registry)
  reload_tool <- tools[[which(sapply(tools, function(t) t$name) == "reload_skills")]]
  load_tool <- tools[[which(sapply(tools, function(t) t$name) == "load_skill")]]

  expect_true(grepl("Version one", load_tool$run(list(skill_name = "live_skill"))))

  writeLines(c(
    "---",
    "name: live_skill",
    "description: Live skill",
    "---",
    "Version two"
  ), file.path(skill_dir, "live_skill", "SKILL.md"))

  reload_result <- reload_tool$run(list())

  expect_true(grepl("Reloaded skills", reload_result, fixed = TRUE))
  expect_true(grepl("Version two", load_tool$run(list(skill_name = "live_skill"))))
})

test_that("Agent keeps reload tools for initially empty skill roots", {
  temp_root <- tempfile()
  skill_dir <- file.path(temp_root, "skills")
  dir.create(skill_dir, recursive = TRUE)
  on.exit(unlink(temp_root, recursive = TRUE), add = TRUE)

  agent <- create_agent(
    name = "EmptySkillAgent",
    description = "Starts with an empty skill root",
    skills = skill_dir
  )

  expect_null(agent$system_prompt)
  expect_s3_class(agent$skill_registry, "SkillRegistry")
  tool_names <- sapply(agent$tools, function(t) t$name)
  expect_true("reload_skills" %in% tool_names)

  dir.create(file.path(skill_dir, "late_skill"))
  writeLines(c(
    "---",
    "name: late_skill",
    "description: Added after startup",
    "---",
    "Late body"
  ), file.path(skill_dir, "late_skill", "SKILL.md"))

  reload_tool <- agent$tools[[which(tool_names == "reload_skills")]]
  reload_tool$run(list())

  expect_true(agent$skill_registry$has_skill("late_skill"))
})

test_that("channel_resolve_agent auto-creates a skill-aware agent", {
  temp_root <- tempdir()
  skill_dir <- file.path(temp_root, "channel_skill_test")
  dir.create(skill_dir, recursive = TRUE, showWarnings = FALSE)

  dummy_skill_path <- file.path(skill_dir, "pdf_helper")
  dir.create(dummy_skill_path, recursive = TRUE)
  writeLines(c(
    "---",
    "name: pdf_helper",
    "description: Handles PDF and OCR tasks",
    "---",
    "Instructions"
  ), file.path(dummy_skill_path, "SKILL.md"))

  agent <- channel_resolve_agent(
    agent = NULL,
    skills = skill_dir,
    model = "mock:test"
  )

  expect_s3_class(agent, "Agent")
  expect_true(grepl("Users should not need to know what skills exist", agent$system_prompt, fixed = TRUE))
  expect_true(grepl("pdf_helper", agent$system_prompt, fixed = TRUE))

  tool_names <- sapply(agent$tools, function(t) t$name)
  expect_true("load_skill" %in% tool_names)
  expect_true("execute_skill_script" %in% tool_names)
  expect_true("reload_skills" %in% tool_names)
})
