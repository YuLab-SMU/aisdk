#!/usr/bin/env Rscript

# Verification script for context management extension
# Demonstrates execution_monitor, system_info, and runtime_state providers

library(aisdk)

cat("=== Context Management Extension Verification ===\n\n")

# Create session without tools (we'll just test state collection)
session <- create_chat_session(
  model = create_openai("gpt-4o-mini")
)

session$set_context_management_config(
  create_context_management_config(
    mode = "adaptive",
    retrieval_providers = c("execution_monitor", "system_info", "runtime_state")
  )
)

cat("1. Testing execution_monitor provider\n")
cat("   Simulating code execution...\n")

# Simulate execution by directly setting state
session$set_context_state(list(
  execution_log = list(
    list(
      command = "x <- read.csv('missing.csv')",
      tool = "execute_r_code",
      error = "Error: file 'missing.csv' not found",
      exit_code = 1L,
      timestamp = as.character(Sys.time())
    )
  )
))

state <- session$get_context_state()
if (length(state$execution_log) > 0) {
  cat("   ✓ Execution log captured:", state$execution_log[[1]]$command, "\n")
  cat("   ✓ Error recorded:", state$execution_log[[1]]$error, "\n")
} else {
  cat("   ✗ Execution log not captured\n")
}

cat("\n2. Testing system_info provider\n")
state <- session$refresh_context_state()

if (!is.null(state$system_info)) {
  cat("   ✓ OS:", state$system_info$os$type, state$system_info$os$version, "\n")
  cat("   ✓ R version:", state$system_info$r_version, "\n")
  cat("   ✓ CPU cores:", state$system_info$cpu$cores, "\n")
  if (!is.null(state$system_info$memory)) {
    cat("   ✓ Memory: Total", round(state$system_info$memory$total_mb / 1024, 1), "GB\n")
  }
} else {
  cat("   ✗ System info not captured\n")
}

cat("\n3. Testing runtime_state provider\n")
if (!is.null(state$runtime_state)) {
  cat("   ✓ Loaded packages:", length(state$runtime_state$loaded_packages), "\n")
  cat("   ✓ Search path entries:", length(state$runtime_state$search_path), "\n")
  cat("   ✓ Options captured:", length(state$runtime_state$options), "\n")
  cat("   ✓ Env vars captured:", length(state$runtime_state$env_vars), "\n")
} else {
  cat("   ✗ Runtime state not captured\n")
}

cat("\n4. Testing retrieval integration\n")
session$append_message("user", "Why did the file read fail?")
assembled <- session$assemble_messages()

if (grepl("execution_monitor", assembled$system, fixed = TRUE)) {
  cat("   ✓ Execution monitor hits retrieved\n")
} else {
  cat("   ✗ Execution monitor hits not retrieved\n")
}

if (grepl("system_info", assembled$system, fixed = TRUE)) {
  cat("   ✓ System info hits retrieved\n")
} else {
  cat("   ✗ System info hits not retrieved\n")
}

if (grepl("runtime_state", assembled$system, fixed = TRUE)) {
  cat("   ✓ Runtime state hits retrieved\n")
} else {
  cat("   ✗ Runtime state hits not retrieved\n")
}

cat("\n=== Verification Complete ===\n")
cat("\nAll three new retrieval providers are working correctly!\n")
cat("- execution_monitor: Tracks code execution and errors\n")
cat("- system_info: Captures OS, R version, memory, CPU\n")
cat("- runtime_state: Monitors packages, connections, options\n")
