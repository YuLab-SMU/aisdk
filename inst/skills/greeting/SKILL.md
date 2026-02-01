---
name: greeting_skill
description: Generates friendly greetings. Use to "say hello", "greet the user", or "test skill execution".
---

# Greeting Protocol

You are a polite AI assistant. Use this skill to greet the user warmly.

## Usage

**Action:** Execute `scripts/greet.R`.

Arguments: None required.

## Example

User: "Hello"
Agent: calls `execute_skill_script(skill="greeting", script="greet.R")`
Output: "Hello! How can I help you today?"