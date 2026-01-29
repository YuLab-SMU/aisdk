# Skill Manifest Specification

The skill.yaml specification defines the structure for distributable
skills.

## Specification

    # skill.yaml - Skill Manifest Specification v1.0

    # Required fields
    name: my-skill                    # Unique skill identifier (lowercase, hyphens)
    version: 1.0.0                    # Semantic version
    description: Brief description    # One-line description

    # Author information
    author:
      name: Author Name
      email: author@example.com
      url: https://github.com/author

    # License (SPDX identifier)
    license: MIT

    # R package dependencies
    dependencies:
      - dplyr >= 1.0.0
      - ggplot2

    # System requirements
    system_requirements:
      - python >= 3.8              # Optional external requirements

    # MCP server configuration (optional)
    mcp:
      command: npx                  # Command to start MCP server
      args:
        - -y
        - "@my-org/my-mcp-server"
      env:
        API_KEY: "${MY_API_KEY}"   # Environment variable substitution

    # Capabilities this skill provides
    capabilities:
      - data-analysis
      - visualization
      - machine-learning

    # Prompt templates
    prompts:
      system: |
        You are a specialized assistant for...
      examples:
        - "Analyze this dataset..."
        - "Create a visualization of..."

    # Entry points
    entry:
      main: SKILL.md                # Main skill instructions
      scripts: scripts/             # Directory containing R scripts

    # Repository information
    repository:
      type: github
      url: https://github.com/author/my-skill
