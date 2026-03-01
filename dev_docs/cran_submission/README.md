# CRAN Submission Guide for aisdk

This directory contains documentation and guidelines for ensuring the `aisdk` package complies with CRAN submission standards.

## Overview

Submitting to CRAN requires strict adherence to their policies regarding documentation, examples, file operations, and network usage. This guide summarizes the key lessons learned from our recent CRAN checks and the fixes we applied.

## Key Compliance Areas

### 1. File System Operations (`getwd()` vs `tempdir()`)
- **Policy**: Packages must not write to the user's home filespace or the current working directory (`getwd()`) by default during checks or examples.
- **Fix**: All default parameters that specify a save/working directory MUST use `tempdir()` in non-interactive sessions.
- **Pattern**:
  ```R
  # BAD: 
  create_artifact_dir <- function(base_dir = getwd()) { ... }
  
  # GOOD:
  create_artifact_dir <- function(base_dir = if (interactive()) getwd() else tempdir()) { ... }
  ```

### 2. Network Operations & Graceful Failure
- **Policy**: Functions that access internet resources must fail gracefully with an informative message if the resource is unavailable, rather than halting execution (`stop()` or `abort()`).
- **Fix**: Network requests (like those in `utils_http.R`) must check for internet connectivity first.
- **Pattern**:
  ```R
  if (!curl::has_internet()) {
    message("No internet connection. Cannot reach the API.")
    return(NULL) # Return a safe fallback, not an error
  }
  ```

### 3. Executable Examples & CI Timeouts
- **Policy**: Examples must be runnable. If an example takes too long or requires external authentication (like LLM API keys) that isn't available on CRAN's servers, it must be protected.
- **Fix**: Wrapped LLM-dependent examples in `\donttest{}` AND `if (interactive()) { ... }` blocks. This ensures the code is syntax-checked but bypassed during CI and CRAN automated checks, preventing timeouts.
- **Pattern**:
  ```R
  #' @examples
  #' \donttest{
  #' if (interactive()) {
  #'   # Code that requires an API key or is long-running
  #'   chat <- console_chat("openai:gpt-4o")
  #' }
  #' }
  ```
  *(Note: Use `\dontrun{}` only for examples that genuinely cannot be executed under any standard circumstance, e.g., requiring a user-specific local file that doesn't exist in the package).*

### 4. R CMD Check Warnings & NOTEs
- **Cross-References**: Roxygen generates warnings for things like `choices[0]` where `[0]` might be misinterpreted as a cross-reference link (`\link{0}`). Escape brackets or use plain text in documentation.
- **Usage Mismatches**: Ensure documenting `@param` matches the exact arguments accepted by the exported function. Do not document internal R6 method parameters at the top-level factory function unless they are passed through.
- **Non-Standard Files**: Top-level files specific to development (e.g., `_pkgdown.yml`, `Rbuild.R`, `test_gemini.py`) must be added to `.Rbuildignore` to prevent "non-standard file/directory" NOTEs.

## Checklist for Future PRs

Before submitting a major feature or preparing for a release:
1. Run `devtools::document()` to update all `.Rd` files.
2. Run standard checks: `devtools::check(args = c("--no-examples"))` for speed.
3. Run CRAN-strict checks: `devtools::check(args = "--as-cran")` and resolve all ERRORs, WARNINGs, and NOTEs.
