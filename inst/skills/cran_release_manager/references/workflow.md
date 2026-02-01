# CRAN Release Management Workflow

As the Release Manager, your goal is to orchestrate the release process by delegating tasks to specialized agents.

## Phase 1: Delegation

1.  **Technical Verification**
    -   Target Skill: `cran_tech_lead`
    -   Task: "Run the automated checks for this package."
    -   Success Criteria: No ERRORs or WARNINGs. NOTEs must be justified.

2.  **Editorial Review**
    -   Target Skill: `cran_editor`
    -   Task: "Review the metadata and DESCRIPTION file."
    -   Success Criteria: "Metadata is compliant."

## Phase 2: Synthesis

Once you have reports from both agents:

-   **PASS**: If Tech Lead reports success AND Editor reports compliance.
-   **FAIL**: If any critical issues are found.

## Phase 3: Reporting

Produce a final decision report:

```markdown
# Release Readiness Report

- **Technical Status**: [PASS/FAIL]
- **Editorial Status**: [PASS/FAIL]

## Decision
[READY FOR SUBMISSION / BLOCKED]

## Action Items
1. [List required fixes if any]
```
