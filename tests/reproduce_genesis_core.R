
library(aisdk)

# Create a dummy skill "Calculator"
skill_dir <- tempfile()
dir.create(skill_dir)
dir.create(file.path(skill_dir, "calculator"))

# Create SKILL.md without agent definition
cat('---
name: calculator
description: Basic calculator
tools:
  - name: add
    description: Add two numbers
    parameters:
      x: number
      y: number
---
', file = file.path(skill_dir, "calculator", "SKILL.md"))

# Create tool implementation
cat('
add <- function(x, y) {
  x + y
}
', file = file.path(skill_dir, "calculator", "tools.R"))

cat("Scanning skills in:", skill_dir, "\n")

# Run genesis
# This should succeed if the Manager has access to the skill, but fail currently
tryCatch({
  result <- genesis("What is 5 + 3?", skill_paths = skill_dir, verbose = TRUE)
  print(result)
}, error = function(e) {
  cat("Error as expected (or not):", conditionMessage(e), "\n")
})
