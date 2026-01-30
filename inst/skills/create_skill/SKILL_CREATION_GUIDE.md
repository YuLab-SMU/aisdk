# Skill Creation Agent Guide

The **Skill Creation Agent** is a specialized AI agent designed to help you extend the capabilities of the AISDK system. It acts as an "AI Engineer" that can write the code and configuration for new skills based on your high-level descriptions.

## Overview

A "Skill" in this system is a package of specialized knowledge and tools. It consists of:
1.  **SKILL.md**: Instructions for the AI on how to use the skill.
2.  **Scripts**: R functions that perform the actual work (e.g., data scraping, complex analysis).

The Skill Creation Agent automates the process of building these files.

## Getting Started

### 1. Create the Agent
Use the factory function `create_skill_creator_agent()` to instantiate the agent.

```r
library(aisdk)

# Create the agent
creator_agent <- create_skill_creator_agent()
```

### 2. Define Your Idea
Think about what you want the new skill to do. For example, "I want a skill that can fetch stock prices from Yahoo Finance."

### 3. Run the Agent
Pass your request to the agent. Be as descriptive as possible about the desired functionality.

```r
# Task the agent
result <- creator_agent$run(
  "Create a 'stock_analyzer' skill. It should have a script to fetch stock data for a given symbol using 'quantmod' and another script to plot the closing price series.",
  model = "openai:gpt-4o"
)

# Print the agent's response
cat(result$text)
```

## How It Works

When you run the agent, it performs the following steps:
1.  **Analyzes Request**: Understands the requirements (inputs, outputs, libraries needed).
2.  **Creates Structure**: Sets up the directory `inst/skills/[skill_name]`.
3.  **Writes Metadata**: Generates `SKILL.md` with a description and usage protocol.
4.  **Implements Logic**: Writes R scripts in `inst/skills/[skill_name]/scripts/`.

## Post-Creation

After the agent finishes, the new skill is immediately available in your `inst/skills` directory, but you may need to reload your registry to use it in the current session if you are strictly using `system.file`. If using a local path registry, it might be available instantly.

To use the new skill with another agent:

```r
# Load the new skill into a registry
registry <- create_skill_registry("inst/skills")

# Create a general agent that uses this registry
analyst <- create_agent(
    name = "StockAnalyst", 
    description = "Analyzes stocks", 
    tools = create_skill_tools(registry)
)

# Use the new skill
analyst$run("Analyze the performance of AAPL", model = "openai:gpt-4o")
```

## Troubleshooting

-   **"Skill not found"**: Ensure the `inst/skills` directory path is correct when creating your registry.
-   **Script errors**: The agent writes code, but it might not be perfect. You can manually inspect and fix the scripts in `inst/skills/[skill_name]/scripts/` if needed.

## Handling Interruptions

If the process is interrupted (e.g., network error or forced stop), don't worry! The Skill Creation tools are **idempotent**, meaning they can be run multiple times without causing issues.

### How to Resume
You have two options:

1.  **Simply Rerun**: Run the exact same `creator_agent$run(...)` command again. The agent will re-verify the directory structure and files, overwriting them if necessary, and continue to the next steps.
2.  **Contextual Resume**: You can ask the agent to finish the job by mentioning what's already done.
    ```r
    creator_agent$run(
      "Continue creating the 'stock_analyzer' skill. Check the existing files to see what is left to do.",
      model = ...
    )
    ```
