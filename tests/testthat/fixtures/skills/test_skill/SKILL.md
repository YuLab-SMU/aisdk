---
name: test_skill
description: A test skill for unit testing the Skills system
aliases:
  - test
when_to_use: Use this fixture when testing skill loading.
paths:
  - tests/*.R
---

# Test Skill Instructions

This fixture provides a minimal skill used by unit tests.

## Available Scripts

- `hello.R`: returns a greeting using `args$name`, or `World` when no name is provided.
