# Context Management System Extension

## Overview

Extended the aisdk context management system to support broader task-conditioned retrieval signals beyond semantic objects and task state.

## New Retrieval Providers

### 1. Execution Monitor (`execution_monitor`)
Tracks code execution results, errors, timing, and memory usage.

**Data collected:**
- Command/code executed
- Tool name (execute_r_code, bash, etc.)
- Exit code (0 = success, 1 = error)
- Result or error message
- Timestamp

**Use cases:**
- Debugging failed executions
- Understanding error patterns
- Tracking command history

### 2. System Info (`system_info`)
Captures system and device information.

**Data collected:**
- OS type, version, platform, architecture
- R version and platform
- Memory (total, available, used percentage)
- CPU (cores, model)
- Working directory

**Use cases:**
- Environment-specific debugging
- Resource availability checks
- Platform compatibility issues

### 3. Runtime State (`runtime_state`)
Monitors R runtime environment state.

**Data collected:**
- Active connections (description, class, mode, status)
- Graphics devices (number, name)
- R options (width, digits, scipen, etc.)
- Search path
- Loaded packages (excluding base packages)
- Environment variables (PATH, HOME, R_LIBS, etc.)

**Use cases:**
- Package dependency issues
- Connection management
- Configuration debugging

## Implementation

### New Files

1. **R/context_collectors.R**
   - `collect_execution_log()`: Extracts execution history from generation results and Computer instances
   - `collect_system_info()`: Gathers OS, R version, memory, and CPU info
   - `collect_runtime_state()`: Captures connections, devices, options, packages, and env vars

2. **tests/testthat/test-context-collectors.R**
   - 11 test cases covering all three collectors
   - Integration tests with context state synthesis
   - Retrieval scoring and ranking tests

### Modified Files

1. **R/context_budget.R**
   - Updated `synthesize_context_state()` to call new collectors
   - Collectors populate `state$execution_log`, `state$system_info`, `state$runtime_state`
   - Added context events for tracking refreshes

2. **R/context_management.R**
   - Already had provider definitions and scoring weights (no changes needed)
   - Retrieval functions `build_execution_monitor_retrieval()`, `build_system_info_retrieval()`, `build_runtime_state_retrieval()` already existed

## Configuration

All three providers are enabled by default with sensible limits and scoring weights:

```r
create_context_management_config(
  retrieval_providers = c(
    "execution_monitor",  # enabled by default
    "system_info",        # enabled by default
    "runtime_state"       # enabled by default
  ),
  retrieval_provider_limits = list(
    execution_monitor = 3L,
    system_info = 2L,
    runtime_state = 3L
  ),
  retrieval_scoring_policy = list(
    provider_weights = list(
      execution_monitor = 1.15,  # prioritize execution errors
      system_info = 0.95,
      runtime_state = 1.0
    ),
    execution_error_weight = 0.4,
    system_metric_weight = 0.3,
    runtime_signal_weight = 0.35
  )
)
```

## Usage Example

```r
session <- create_chat_session(
  model = create_anthropic("claude-opus-4"),
  tools = create_computer_tools(working_dir = getwd())
)

session$set_context_management_config(
  create_context_management_config(
    mode = "adaptive",
    retrieval_providers = c("execution_monitor", "system_info", "runtime_state")
  )
)

# Execute code that fails
session$send("Read a file that doesn't exist")

# Context state now includes execution error
state <- session$get_context_state()
# state$execution_log contains the failed command and error

# Next query can retrieve relevant execution context
session$send("Why did that fail?")
# Retrieval system surfaces the execution error automatically
```

## Test Coverage

- **47 tests** in test-context-collectors.R
- **247 total tests** pass in context-* test suite
- Coverage includes:
  - Individual collector functions
  - Integration with context state synthesis
  - Retrieval scoring and ranking
  - Query matching and filtering
  - Error prioritization

## Design Decisions

1. **Collector functions are internal**: Marked with `@keywords internal` to keep API surface clean
2. **Automatic collection**: Collectors run during `synthesize_context_state()` without explicit calls
3. **Graceful degradation**: All collectors use `tryCatch()` to handle platform differences
4. **Selective data**: Only key options, packages, and env vars are collected to avoid noise
5. **Recency bias**: Execution log is limited to recent entries (default 10) to stay relevant

## Future Enhancements

Potential extensions:
- Memory profiling integration (Rprofmem)
- Performance timing (microbenchmark)
- Network activity monitoring
- File system access tracking
- GPU/accelerator detection
