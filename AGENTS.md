# Repository Guidelines

## Project Structure & Module Organization
Core package code lives in `R/`. Generated documentation lives in `man/`; do not edit `.Rd` files by hand because they are regenerated from roxygen comments in `R/`. Tests live in `tests/testthat/` with the entrypoints `tests/testthat.R` and `tests/test_providers_manual.R`. Package assets and shipped resources belong in `inst/` (for example `inst/skills/`, `inst/extdata/`, and `inst/www/`). Quarto and site content live in `vignettes/`, `docs/`, and `_quarto.yml`. Development notes in `dev_docs/`, `dev_logs/`, and `plan/` are not part of the package build.

## Build, Test, and Development Commands
- `Rscript -e "devtools::document()"`: regenerate `NAMESPACE` and `man/*.Rd` from roxygen comments.
- `R CMD build .`: build the source tarball.
- `R CMD check aisdk_*.tar.gz`: run package checks on the built tarball.
- `R CMD check --as-cran .`: run a CRAN-style check from the working tree.
- `Rscript -e "testthat::test_dir('tests/testthat')"`: run the main test suite.
- `Rscript -e "testthat::test_file('tests/testthat/test-provider-openai.R')"`: run a single test file while iterating.

## Coding Style & Naming Conventions
Use standard R style with 2-space indentation and clear, descriptive names. Prefer `snake_case` for functions and helpers, and `CamelCase` only for R6 classes such as `BailianProvider` or `SandboxManager`. Keep provider implementations grouped by file name, for example `R/provider_openai.R` and `tests/testthat/test-provider-openai.R`. This repo uses `roxygen2`; document exported functions inline. `.lintr` disables indentation, line-length, and commented-code linters, but new code should still be formatted consistently.

## Testing Guidelines
Tests use `testthat` edition 3. Name new files `test-<feature>.R` and keep fixtures/helpers in `tests/testthat/helper-*.R` or `tests/testthat/setup.R`. Add or update tests for any behavior change, especially around providers, streaming, sessions, and sandbox execution. Avoid committing generated `Rplots*.pdf` artifacts.

## Commit & Pull Request Guidelines
Recent history favors short, imperative subjects such as `Fix inspector overlay CLI rendering`, `fix: resolve R CMD check warning`, and `feat(knitr): refactor {ai} engine`. Follow that pattern: keep the first line concise and optionally use prefixes like `fix:`, `feat(...)`, `docs:`, or `chore:`. For pull requests, include a clear summary, the user-facing impact, linked issues, and the exact commands you ran (`devtools::document()`, `R CMD build`, `R CMD check`). Add screenshots or terminal output when changing console UI, review panels, or Quarto docs.

## Documentation & Packaging Notes
Before submitting changes, rebuild documentation and verify the tarball rather than only the live repo tree. Keep package-site URLs current and prefer ASCII-safe documentation content so PDF manual generation does not fail on CRAN.

## CRAN Submission Checklist
- Run `Rscript -e "devtools::document()"` before building so `man/` and `NAMESPACE` match the code.
- Submit-check the tarball, not just the repo tree: `R CMD build .` then `R CMD check --as-cran aisdk_*.tar.gz`.
- Keep documentation ASCII-safe where possible. Non-ASCII text in roxygen, `README`, or vignettes can break the PDF manual on CRAN.
- Update or remove redirected URLs. CRAN flags stale links in `DESCRIPTION`, `README.md`, and generated `.Rd` files.
- Keep the build clean via `.Rbuildignore`. Do not let `dev_logs/`, `docs/`, hidden files, local env files, or nested `.git/` directories leak into the tarball.
- Avoid non-portable file names and overlong paths in shipped files.
- Check for accidental artifacts before release, especially `Rplots*.pdf`, `.DS_Store`, `.Rhistory`, and `.RData`.
- If you change exported functions, examples, or package metadata, rerun the relevant tests and mention the exact check commands in the PR.
