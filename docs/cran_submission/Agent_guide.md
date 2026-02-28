# AI Agent Development Guide for CRAN Compliance

As an AI Agent working on the `aisdk` package, you must consistently follow these CRAN policies to ensure your code generation does not introduce compliance regressions. 

**Read and strictly adhere to the following rules when writing R code or generating documentation.**

---

## 1. Documentation & Examples (`@examples`)

When writing roxygen2 `@examples` for new functions, classes, or methods:

- **Never create examples that unconditionally hit external APIs.** CRAN servers (and CI environments) generally do not have API keys (like `OPENAI_API_KEY`).
- **Use the `\donttest{}` + `if(interactive())` pattern** for all code blocks that require network access, API keys, or take longer than 5 seconds to run.
  
  **Required Template:**
  ```R
  #' @examples
  #' \donttest{
  #' if (interactive()) {
  #'   model <- create_openai()$language_model("gpt-4o")
  #'   result <- generate_text(model, "Hello")
  #' }
  #' }
  ```
- **Only use `\dontrun{}` if the code is fundamentally broken** in a test environment (e.g., it explicitly requires a file like `"data.csv"` that is not shipped with the package). CRAN discourages `\dontrun{}` if `\donttest{}` can be used instead.
- **Beware of `[]` in Roxygen text:** Roxygen might interpret `word[0]` as a link `word\link{0}`. Use backticks for code literals (<code>`word[0]`</code>) or rewrite.

## 2. Default File Paths and `getwd()`

CRAN strictly forbids packages from writing to the user's home directory or `getwd()` silently during tests/examples.

- **Rule:** Do not use `getwd()` or `"."` as a default parameter for paths where the package will write files.
- **Fix:** Use the `if (interactive()) getwd() else tempdir()` pattern.
- **Example Implementation:**
  ```R
  # Incorrect:
  init_project <- function(dir = getwd()) { ... }
  
  # Correct:
  init_project <- function(dir = if (interactive()) getwd() else tempdir()) { ... }
  ```

## 3. Network Resilience & Graceful Failures

CRAN requires that internet-dependent packages fail gracefully. 

- **Rule:** Never use `stop()`, `rlang::abort()`, or let `httr2` throw unhandled exceptions if the internet is down.
- **Implementation:** Always check for internet connectivity using `curl::has_internet()` before making a request. If `FALSE`, emit a `message()` (not a warning or error) and return gracefully (e.g., `return(NULL)` or return an empty state).
- **Example Implementation:**
  ```R
  if (!requireNamespace("curl", quietly = TRUE) || !curl::has_internet()) {
    message("No internet connection available. Skipping API call.")
    return(NULL)
  }
  ```

## 4. Package Dependencies

- If you introduce a new dependency (e.g., a new R package needed for a skill or a provider), you **must** check if it's listed in the `DESCRIPTION` file.
- Add it to `Imports:` if it's required for core functionality. 
- Add it to `Suggests:` if it's only needed for specific optional features, and use `requireNamespace("pkg", quietly = TRUE)` inside the function.

## 5. Non-Standard Files

- If you create temporary script files, development notes, or Python testing files in the root directory, they **must** be added to `.Rbuildignore`.
- CRAN `R CMD check` will flag any unrecognized file at the top level with a NOTE.

---
**Summary Checklist for the AI Agent:**
- [ ] Did I use `tempdir()` fallback for any file write functions?
- [ ] Are my LLM examples wrapped in `\donttest{}` and `if(interactive())`?
- [ ] Have I handled network absence gracefully with `message()` instead of `stop()`?
- [ ] Is any new root-level dev file added to `.Rbuildignore`?
