# verify_skill_creation.R
# Script to verify the Skill Creation Agent components manually.

# 1. Source the package code (simulating loading the package)
library(R6)
source("R/agent.R")
source("R/skill.R")
source("R/skill_registry.R")
source("R/stdlib_agents.R")
source("R/tool.R")
source("R/schema.R")
source("R/utils_json.R") # Likely needed for z_object/schema serialization

# Ensure we have the necessary dependencies loaded
if (!requireNamespace("callr", quietly = TRUE)) stop("callr needed")
if (!requireNamespace("yaml", quietly = TRUE)) stop("yaml needed")
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite needed")

cat("=== Verifying Skill Creation Skill Tools ===\n")

# 2. Setup Registry
# Point to local inst/skills
skills_path <- file.path(getwd(), "inst", "skills")
registry <- create_skill_registry(skills_path)

cat("Registry initialized. Skills found:\n")
print(registry$list_skills())

# 3. Get the 'create_skill' skill
skill <- registry$get_skill("create_skill")

if (is.null(skill)) {
  stop("FATAL: 'create_skill' not found in registry.")
}
cat("'create_skill' loaded successfully.\n")

# 4. Test Script: create_structure.R
test_skill_name <- "test_generated_skill"
test_skill_path <- file.path(tempdir(), "aisdk_skills")
if (!dir.exists(test_skill_path)) dir.create(test_skill_path)

cat("\nTesting create_structure.R...\n")
skill$execute_script("create_structure.R", list(
  path = test_skill_path,
  name = test_skill_name
))

expected_path <- file.path(test_skill_path, test_skill_name)
if (dir.exists(expected_path) && dir.exists(file.path(expected_path, "scripts"))) {
  cat("PASS: Directory structure created.\n")
} else {
  stop("FAIL: Directory structure not created.")
}

# 5. Test Script: write_skill_md.R
cat("\nTesting write_skill_md.R...\n")
skill$execute_script("write_skill_md.R", list(
  path = expected_path,
  name = test_skill_name,
  description = "A generated test skill",
  instructions = "This is a test."
))

if (file.exists(file.path(expected_path, "SKILL.md"))) {
  content <- readLines(file.path(expected_path, "SKILL.md"))
  if (any(grepl("name: test_generated_skill", content))) {
     cat("PASS: SKILL.md created with correct content.\n")
  } else {
     stop("FAIL: SKILL.md content incorrect.")
  }
} else {
  stop("FAIL: SKILL.md not created.")
}

# 6. Test Script: write_script.R
cat("\nTesting write_script.R...\n")
skill$execute_script("write_script.R", list(
  path = expected_path,
  script_name = "hello.R",
  code = "print('Hello World')"
))

if (file.exists(file.path(expected_path, "scripts", "hello.R"))) {
  cat("PASS: Script file created.\n")
} else {
  stop("FAIL: Script file not created.")
}

# 7. Test Agent Factory
cat("\nTesting Agent Factory...\n")
agent <- create_skill_creator_agent(registry = registry)
cat("Agent created: ", agent$name, "\n")
print(agent)

cat("\n=== Verification Complete ===\n")
