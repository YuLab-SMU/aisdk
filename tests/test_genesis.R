# Genesis System Test Script
# This script tests the complete Genesis workflow

library(aisdk)

cat("=== Testing Project Genesis ===\n\n")

# Test 1: AgentLibrary Discovery
cat("Test 1: Agent Discovery\n")
cat("------------------------\n")

library <- AgentLibrary$new()
library$scan_from_skills("auto", recursive = TRUE)

cat("\nDiscovered agents:\n")
print(library$get_capabilities_summary())

cat("\n\n")

# Test 2: Architect Agent Creation
cat("Test 2: Architect Creation\n")
cat("--------------------------\n")

architect <- create_architect_agent(library, model = "claude-3-5-sonnet-20241022")
cat(sprintf("Architect created: %s\n", architect$name))
cat(sprintf("Description: %s\n", architect$description))

cat("\n\n")

# Test 3: Architect Response Parsing
cat("Test 3: Response Parsing\n")
cat("------------------------\n")

# Simulate an Architect response
test_response <- '{
  "task_analysis": "This task requires data analysis and visualization",
  "selected_agents": ["DataAnalyst", "Visualizer"],
  "reasoning": "Need to analyze data and create plots",
  "delegation_strategy": "DataAnalyst analyzes first, then Visualizer creates plots"
}'

plan <- parse_architect_response(test_response)
cat("Parsed plan:\n")
cat(sprintf("  Task analysis: %s\n", plan$task_analysis))
cat(sprintf("  Selected agents: %s\n", paste(plan$selected_agents, collapse = ", ")))
cat(sprintf("  Reasoning: %s\n", plan$reasoning))

cat("\n\n")

# Test 4: Genesis API (if API key is available)
cat("Test 4: Genesis API\n")
cat("-------------------\n")

if (Sys.getenv("ANTHROPIC_API_KEY") != "") {
  cat("Running Genesis with verbose mode...\n\n")

  tryCatch({
    result <- genesis(
      "Analyze the iris dataset",
      verbose = TRUE
    )

    cat("\n\nResult:\n")
    print(result)
  }, error = function(e) {
    cat(sprintf("Error: %s\n", e$message))
  })
} else {
  cat("Skipping (no API key found)\n")
  cat("Set ANTHROPIC_API_KEY environment variable to test full workflow\n")
}

cat("\n\n=== Tests Complete ===\n")
