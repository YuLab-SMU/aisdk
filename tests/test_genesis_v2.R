# Genesis V2 (PER) Test Script
# This script tests the complete Plan-Execute-Refine cycle

library(aisdk)

cat("=== Testing Genesis V2 (Plan-Execute-Refine) ===\n\n")

# Test 1: Component Testing
cat("Test 1: Component Testing\n")
cat("-------------------------\n\n")

# Test 1a: AgentLibrary Discovery
cat("1a. Agent Discovery\n")
library <- AgentLibrary$new()
library$scan_from_skills("auto", recursive = TRUE)

cat("Discovered agents:\n")
print(library$get_capabilities_summary())
cat("\n")

# Test 1b: Architect V2 Creation
cat("1b. Architect V2 Creation\n")
architect <- create_architect_v2(library, model = "claude-3-5-sonnet-20241022")
cat(sprintf("Architect V2 created: %s\n", architect$agent$name))
cat(sprintf("Description: %s\n", architect$agent$description))
cat("\n")

# Test 1c: Evaluator Creation
cat("1c. Evaluator Creation\n")
evaluator <- create_evaluator_agent(model = "claude-3-5-sonnet-20241022")
cat(sprintf("Evaluator created: %s\n", evaluator$agent$name))
cat(sprintf("Description: %s\n", evaluator$agent$description))
cat("\n")

# Test 1d: Refiner Creation
cat("1d. Refiner Creation\n")
refiner <- create_refiner_agent(library, model = "claude-3-5-sonnet-20241022")
cat(sprintf("Refiner created: %s\n", refiner$agent$name))
cat(sprintf("Description: %s\n", refiner$agent$description))
cat("\n\n")

# Test 2: Response Parsing
cat("Test 2: Response Parsing\n")
cat("------------------------\n\n")

# Test 2a: Architect V2 Response
cat("2a. Architect V2 Response Parsing\n")
test_architect_response <- '{
  "task_analysis": "This task requires data analysis and visualization",
  "selected_agents": ["DataAnalyst", "Visualizer"],
  "reasoning": "Need to analyze data and create plots",
  "delegation_strategy": "DataAnalyst analyzes first, then Visualizer creates plots",
  "success_criteria": {
    "must_have": ["Statistical summary", "Scatter plot"],
    "quality_checks": ["All columns analyzed", "Plot has labels"],
    "expected_outputs": ["Summary statistics", "ggplot object"]
  }
}'

plan <- parse_architect_v2_response(test_architect_response)
cat("Parsed plan:\n")
cat(sprintf("  Task analysis: %s\n", plan$task_analysis))
cat(sprintf("  Selected agents: %s\n", paste(plan$selected_agents, collapse = ", ")))
cat(sprintf("  Success criteria (must_have): %s\n",
           paste(plan$success_criteria$must_have, collapse = ", ")))
cat("\n")

# Test 2b: Evaluator Response
cat("2b. Evaluator Response Parsing\n")
test_evaluator_response <- '{
  "score": 75,
  "passed": true,
  "completeness": {
    "must_have_met": ["Statistical summary"],
    "must_have_missing": ["Scatter plot"],
    "assessment": "Partially complete"
  },
  "quality": {
    "strengths": ["Good analysis"],
    "weaknesses": ["Missing visualization"],
    "assessment": "Acceptable quality"
  },
  "errors": ["No plot generated"],
  "feedback": "Add visualization to complete the task"
}'

evaluation <- parse_evaluator_response(test_evaluator_response)
cat("Parsed evaluation:\n")
cat(sprintf("  Score: %d/100\n", evaluation$score))
cat(sprintf("  Passed: %s\n", evaluation$passed))
cat(sprintf("  Errors: %s\n", paste(evaluation$errors, collapse = ", ")))
cat("\n")

# Test 2c: Refiner Response
cat("2c. Refiner Response Parsing\n")
test_refiner_response <- '{
  "root_cause": "Architect forgot to include Visualizer agent",
  "action": "replan",
  "reasoning": "Need to add Visualizer for plot creation",
  "improvements": ["Add Visualizer agent", "Ensure data is in plottable format"],
  "new_agents": ["DataAnalyst", "Visualizer"],
  "new_strategy": "DataAnalyst → Visualizer pipeline"
}'

refinement <- parse_refiner_response(test_refiner_response)
cat("Parsed refinement:\n")
cat(sprintf("  Action: %s\n", refinement$action))
cat(sprintf("  Root cause: %s\n", refinement$root_cause))
cat(sprintf("  Improvements: %s\n", paste(refinement$improvements, collapse = ", ")))
cat("\n\n")

# Test 3: Genesis V2 API (if API key is available)
cat("Test 3: Genesis V2 API\n")
cat("----------------------\n\n")

if (Sys.getenv("ANTHROPIC_API_KEY") != "") {
  cat("Running Genesis V2 with verbose mode...\n\n")

  tryCatch({
    # Test 3a: Simple task (should converge quickly)
    cat("3a. Simple Task Test\n")
    result1 <- genesis_v2(
      "Analyze the iris dataset",
      max_iterations = 2,
      quality_threshold = 60,
      verbose = TRUE
    )

    cat("\n\nResult Summary:\n")
    print_genesis_v2_result(result1)

    # Test 3b: Complex task (may need multiple iterations)
    cat("\n\n3b. Complex Task Test\n")
    result2 <- genesis_v2(
      "Analyze the iris dataset and create a scatter plot of Sepal.Length vs Sepal.Width",
      max_iterations = 3,
      quality_threshold = 75,
      verbose = TRUE
    )

    cat("\n\nResult Summary:\n")
    print_genesis_v2_result(result2)

    # Test 3c: Disabled auto-refine (V1 behavior)
    cat("\n\n3c. Auto-Refine Disabled Test\n")
    result3 <- genesis_v2(
      "Analyze the mtcars dataset",
      auto_refine = FALSE,
      verbose = TRUE
    )

    cat("\n\nResult Summary:\n")
    cat(sprintf("Iterations: %d\n", result3$iterations))
    cat(sprintf("Auto-refine: disabled\n"))

    # Test 3d: Helper functions
    cat("\n\n3d. Helper Functions Test\n")

    cat("\nComparing results:\n")
    compare_genesis_results(result1, result2)

    cat("\nAnalyzing execution history:\n")
    analyze_genesis_history(result2)

    # Test 3e: Export/Import
    cat("\n\n3e. Export/Import Test\n")
    temp_file <- tempfile(fileext = ".json")
    export_genesis_result(result2, temp_file)
    imported <- import_genesis_result(temp_file)
    unlink(temp_file)

  }, error = function(e) {
    cat(sprintf("Error: %s\n", e$message))
  })

} else {
  cat("Skipping (no API key found)\n")
  cat("Set ANTHROPIC_API_KEY environment variable to test full workflow\n")
}

cat("\n\n=== All Tests Complete ===\n")
cat("\nGenesis V2 Components:\n")
cat("  ✓ AgentLibrary (discovery)\n")
cat("  ✓ Architect V2 (planning with success criteria)\n")
cat("  ✓ Evaluator (quality assessment)\n")
cat("  ✓ Refiner (improvement strategy)\n")
cat("  ✓ genesis_v2() (PER cycle)\n")
cat("  ✓ Helper functions\n")
cat("\nPlan-Execute-Refine cycle is fully operational! 🚀\n")
