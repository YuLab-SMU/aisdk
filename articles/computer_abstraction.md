# Computer Abstraction Layer

## Introduction

The **Computer Abstraction Layer** is a modern agent design pattern
(2026) that implements a hierarchical action space. Instead of loading
many tool definitions into the context window, agents use a small set of
atomic tools that provide access to the operating system layer—bash,
filesystem, and code execution.

This approach, inspired by production systems like Claude Code and
Manus, reduces context window usage by 30-50% while providing more
flexibility for complex tasks.

## Key Concepts

### Hierarchical Action Space

                        ┌─────────────────┐
                        │   LLM Context   │
                        │   (Agent)       │
                        └────────┬────────┘
                                 │
                        ┌────────▼────────┐
                        │  Atomic Tools   │
                        │  • bash         │  ← Only 4 tools in context
                        │  • read_file    │
                        │  • write_file   │
                        │  • execute_r    │
                        └────────┬────────┘
                                 │
                        ┌────────▼────────┐
                        │  Computer Layer │
                        │  • Scripts      │  ← Unlimited actions via
                        │  • CLIs         │     bash/filesystem
                        │  • Utilities    │
                        └─────────────────┘

### Benefits

- **Reduced Context Usage**: 4 atomic tools vs. dozens of skill tools
- **More Flexibility**: Any bash command, CLI, or script can be executed
- **Better Scalability**: Add new skills without increasing context size
- **Production-Proven**: Same pattern used by Claude Code and Manus

## Basic Usage

### Using Genesis with Computer Tools

The simplest way to use the computer abstraction is through the
[`genesis()`](https://YuLab-SMU.github.io/aisdk/reference/genesis.md)
function:

``` r
library(aisdk)

# Traditional mode (loads all skill tools into context)
result <- genesis("Analyze the iris dataset")

# Computer tools mode (30-50% less context usage)
result <- genesis(
  "Analyze the iris dataset and create a summary plot",
  use_computer_tools = TRUE
)
```

### Genesis V2 with Computer Tools

For complex tasks requiring quality assurance:

``` r
result <- genesis_v2(
  "Build a complete data analysis pipeline with visualizations",
  use_computer_tools = TRUE,
  max_iterations = 5,
  quality_threshold = 85,
  verbose = TRUE
)
```

## Advanced Usage

### Creating a Computer Instance

For fine-grained control, create a Computer instance directly:

``` r
# Create computer in working directory
comp <- Computer$new(
  working_dir = getwd(),
  sandbox_mode = "permissive"
)
```

### Sandbox Modes

The computer abstraction supports three sandbox modes:

| Mode           | Description                        |
|----------------|------------------------------------|
| `"none"`       | No restrictions (use with caution) |
| `"permissive"` | Basic safety checks (default)      |
| `"strict"`     | Blocks dangerous commands          |

``` r
# Strict mode for production
comp <- Computer$new(
  working_dir = getwd(),
  sandbox_mode = "strict"
)

# This will be blocked in strict mode
result <- comp$bash("rm -rf /")
```

### Computer Methods

#### Bash Execution

Execute shell commands, CLIs, or scripts:

``` r
# Simple command
result <- comp$bash("ls -la")

# Pipe commands
result <- comp$bash("cat data.csv | head -n 5")

# Run CLI tools
result <- comp$bash("Rscript analysis.R")
```

#### File Operations

Read and write files:

``` r
# Read a file
result <- comp$read_file("config.yaml")

# Write a file
result <- comp$write_file("output.txt", "Hello, World!")

# Create nested directories
result <- comp$write_file("results/analysis/plot.png", plot_data)
```

#### R Code Execution

Execute R code in an isolated process:

``` r
# Simple computation
result <- comp$execute_r_code("mean(1:10)")

# Data analysis
result <- comp$execute_r_code("
  data <- read.csv('data.csv')
  summary(data)
")

# Create plots
result <- comp$execute_r_code("
  library(ggplot2)
  ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme_minimal()
")
```

### Using Computer Tools in Agents

Create agents with computer tools:

``` r
# Create computer tools
computer_tools <- create_computer_tools(
  working_dir = getwd(),
  sandbox_mode = "permissive"
)

# Create agent with computer tools
agent <- create_agent(
  name = "DataAnalyst",
  description = "Analyzes data using bash and R",
  tools = computer_tools
)

# Run agent
result <- agent$run(
  task = "Load mtcars and create a scatter plot of mpg vs wt",
  model = "anthropic:claude-3-5-sonnet-20241022"
)
```

## Accessing Skills via Computer Tools

When using `use_computer_tools = TRUE`, skills are accessed through the
filesystem rather than direct tool calls:

``` r
# With computer tools, skills live in inst/skills/
# Agent reads SKILL.md to understand the skill
# Then executes scripts via bash

# Example workflow:
# 1. Read skill documentation
# result <- comp$read_file("inst/skills/greeting/SKILL.md")
#
# 2. Execute skill script
# result <- comp$bash("Rscript inst/skills/greeting/scripts/greet.R")
#
# 3. Read skill references if needed
# result <- comp$read_file("inst/skills/greeting/references/examples.md")
```

### Genesis Computer Prompt

When `use_computer_tools = TRUE`, Genesis uses a specialized prompt that
instructs the agent on how to access skills:

    ## Skills
    Skills are available in the inst/skills/ directory. Each skill has:
    - SKILL.md: Instructions and documentation
    - scripts/: Executable R scripts
    - references/: Reference materials

    To use a skill:
    1. Read inst/skills/<skill_name>/SKILL.md to understand it
    2. Execute scripts via: bash('Rscript inst/skills/<skill_name>/scripts/<script>.R')
    3. Read references if needed: read_file('inst/skills/<skill_name>/references/<file>')

## Execution Logging

The Computer class maintains an execution log for observability:

``` r
# Execute some commands
comp$bash("echo 'hello'")
comp$read_file("data.txt")
comp$execute_r_code("summary(mtcars)")

# Get execution log
log <- comp$get_log()
print(log)

# Clear log
comp$clear_log()
```

## Best Practices

### 1. Choose the Right Mode

Use traditional mode (`use_computer_tools = FALSE`) when: - You need
simple, straightforward tasks - Working with a small number of
well-defined tools - Backward compatibility is important

Use computer tools mode (`use_computer_tools = TRUE`) when: - Tasks
involve complex data analysis pipelines - You need to run custom scripts
or CLIs - Context window usage is a concern - Tasks require flexible,
dynamic actions

### 2. Set Appropriate Sandboxing

``` r
# Development: No sandboxing for flexibility
comp <- Computer$new(sandbox_mode = "none")

# Testing: Permissive mode
comp <- Computer$new(sandbox_mode = "permissive")

# Production: Strict mode for safety
comp <- Computer$new(sandbox_mode = "strict")
```

### 3. Structure Your Skills

For computer tools mode, organize skills with clear SKILL.md files:

``` r
# inst/skills/data_analysis/SKILL.md
---
name: data_analysis
description: Statistical analysis and visualization for datasets
---

# Data Analysis Skill

This skill provides tools for analyzing datasets.

## Quick Start

Execute: `bash('Rscript inst/skills/data_analysis/scripts/analyze.R')`

## Parameters

Pass parameters via environment variables or modify the script.
```

### 4. Handle Errors Gracefully

``` r
result <- comp$bash("Rscript analysis.R")

if (result$error) {
  # Handle error
  message("Command failed: ", result$stderr)
} else {
  # Process output
  message("Output: ", result$stdout)
}
```

## Comparison: Traditional vs. Computer Tools

| Feature          | Traditional Mode    | Computer Tools Mode |
|------------------|---------------------|---------------------|
| Tool Definitions | Loaded into context | 4 atomic tools only |
| Context Usage    | Higher              | 30-50% lower        |
| Skill Access     | Direct tool calls   | Via bash/filesystem |
| Flexibility      | Limited by tool set | Unlimited (bash)    |
| Best For         | Simple tasks        | Complex workflows   |
| Learning Curve   | Lower               | Higher              |

## Performance Tips

1.  **Cache Expensive Operations**: Use the `cache` parameter in
    [`genesis()`](https://YuLab-SMU.github.io/aisdk/reference/genesis.md)
2.  **Optimize Scripts**: Keep skill scripts focused and efficient
3.  **Use Appropriate Timeouts**: Set `timeout_ms` based on expected
    execution time
4.  **Monitor Context Usage**: Enable `verbose = TRUE` to track token
    usage

## Troubleshooting

### Command Not Found

``` r
# Use full paths or ensure CLI is installed
result <- comp$bash("/usr/local/bin/python3 script.py")
```

### Permission Errors

``` r
# Ensure working directory has correct permissions
comp <- Computer$new(working_dir = tempdir())
```

### Timeout Issues

``` r
# Increase timeout for long-running commands
result <- comp$bash("Rscript long_analysis.R", timeout_ms = 120000)
```

## References

- [Genesis User
  Guide](https://YuLab-SMU.github.io/aisdk/doc/GENESIS_USER_GUIDE.md)
- [Building
  Agents](https://YuLab-SMU.github.io/aisdk/articles/agents.md)
- [Tools Reference](https://YuLab-SMU.github.io/aisdk/articles/tools.md)

## Changelog

- **Version 1.0.0**: Initial release of computer abstraction layer
- Inspired by production systems: Claude Code, Manus, Amp Code
- Follows 2026 agent design patterns from industry research
