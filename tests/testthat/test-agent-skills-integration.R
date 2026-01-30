
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
  expect_equal(length(agent$tools), 3) # 3 skill tools
})
