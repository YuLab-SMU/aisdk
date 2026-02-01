---
name: cran_editor
description: Verify package metadata, description, and formatting against CRAN policies. Use this skill to "review metadata", "check DESCRIPTION file", or "ensure editorial compliance".
---

# CRAN Editorial Review

Your role is to ensure the package's `DESCRIPTION` file and metadata meet the strict typographic and content standards of CRAN.

## Resource Access

1.  **Read Policies**: Load the editorial standards using `read_skill_resource`.
    -   Resource: `style_guide.md`

2.  **Inspect Metadata**: Use the helper script to read the current `DESCRIPTION` file content.
    -   Script: `scripts/read_metadata.R`

## Review Checklist

Compare the actual metadata against the `style_guide.md`. Focus on:

1.  **Title Case**: Is the Title in Title Case?
2.  **Description Length**: Is it detailed enough?
3.  **Forbidden Words**: Does it start with "A package..."?
4.  **Quotes**: Are there usage of quotes in Title/Description?
5.  **Formatting**: Are function names and software names formatted correctly?

## Output

Report any violations found. If everything is correct, state "Metadata is compliant."
