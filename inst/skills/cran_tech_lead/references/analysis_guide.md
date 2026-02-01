# R CMD Check Analysis Guide

As the Tech Lead, you must analyze the output of `devtools::check()`.

## Severity Levels

### ERROR (Critical)
-   **Definition**: The package failed a critical check. It will be rejected by CRAN immediately.
-   **Action**: Must be fixed. Package is NOT release-ready.

### WARNING (Critical)
-   **Definition**: Serious issue that usually triggers rejection.
-   **Action**: Must be fixed. Package is NOT release-ready.

### NOTE (Conditional)
-   **Definition**: Minor issue or observation.
-   **Action**:
    -   Must be explained in `cran-comments.md`.
    -   If it's a "false positive" or "known issue", verify it's acceptable.
    -   If it's a real issue (e.g., "Namespace in Imports field not imported"), it should probably be fixed.

## Reporting Protocol

When reporting back to the Release Manager:
1.  Summarize counts: "Found 0 ERRORS, 1 WARNING, 2 NOTES".
2.  List specific issues.
3.  Provide a clear Pass/Fail recommendation.
