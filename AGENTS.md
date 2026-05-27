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

### Hermetic tests + local-vs-CI parity
- **`.Renviron` will silently break "default value" tests.** Any test that asserts the built-in default of something resolved via `Sys.getenv("X", default)` must wrap the body in `withr::with_envvar(c(X = NA), { ... })`. `devtools::test()` reuses the dev session env and passes; `devtools::check()` + GitHub Actions start fresh R and re-read `.Renviron`, exposing the override. Precedent: `test-provider-deepseek.R:29` (`DEEPSEEK_MODEL`).
- **Empty env vars are different from missing env vars.** CI or `.Renviron` can set `FOO=""`, which bypasses `Sys.getenv("FOO", unset = default)` defaults. For env-driven defaults, explicitly test both missing and empty cases with `withr::with_envvar(c(FOO = NA))` and `withr::with_envvar(c(FOO = ""))`. Recent precedent: `OPENAI_BASE_URL=""` produced an empty base URL vector in CI while local targeted tests passed.
- **Local checks should mirror GitHub Actions, not only targeted tests.** Before pushing provider/config/default-value changes, run the closest local equivalent: `rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran", "--no-vignettes"), build_args = c("--no-manual", "--no-build-vignettes"), error_on = "warning")`. `devtools::load_all()` and `testthat::test_file()` are useful while iterating but do not validate tarball installation, `.Rbuildignore`, clean env behavior, or warning-as-failure policy.
- **Do not assert async OS state with one shot.** Process reaping (`pgrep`/`ps`), temp-file unlink, port release, etc. are fast on macOS and noticeably slower on Linux CI. Poll with a deadline (`for (i in 1:80) { Sys.sleep(0.1); if (<cond>) break }`) instead of `Sys.sleep(0.5)` + a single check. Pattern lives in `test-r-introspect-tools.R` ("kills the whole process tree").
- **`callr::r(env=...)` is additive, not replacement.** The child inherits the parent env + your additions. To scrub a sensitive var, explicitly set it to `""` AND start the child with `cmdargs = c(..., "--no-environ")` so `.Renviron` cannot re-add it. See `r_eval_build_env()` for the working pattern.
- **Don't assert upstream guarantees you don't own.** Tests that check `callr::kill_tree()` leaves no orphans, that Linux reparenting is prompt, or that the kernel reaps a process in N ms are asserting on a library/OS contract, not on your code. Either `skip_on_ci()`/`skip_on_os()` them, or replace with an assertion about your code's user-visible result (return value, status code, error class).

### After every push, watch the CI run
A green `devtools::check()` locally does not imply green GitHub Actions. After pushing a non-trivial change, run `gh run list --branch <branch> --limit 1` and, on failure, `gh run view <id> --log-failed | grep -E "Failure|Error"` to read the actual assertion. CI failures are work-in-progress, not noise; do not re-trigger without investigating.

## Commit & Pull Request Guidelines
Recent history favors short, imperative subjects such as `Fix inspector overlay CLI rendering`, `fix: resolve R CMD check warning`, and `feat(knitr): refactor {ai} engine`. Follow that pattern: keep the first line concise and optionally use prefixes like `fix:`, `feat(...)`, `docs:`, or `chore:`. For pull requests, include a clear summary, the user-facing impact, linked issues, and the exact commands you ran (`devtools::document()`, `R CMD build`, `R CMD check`). Add screenshots or terminal output when changing console UI, review panels, or Quarto docs.

## Documentation & Packaging Notes
Before submitting changes, rebuild documentation and verify the tarball rather than only the live repo tree. Keep package-site URLs current and prefer ASCII-safe documentation content so PDF manual generation does not fail on CRAN.

## CRAN Submission Checklist
- Run `Rscript -e "devtools::document()"` before building so `man/` and `NAMESPACE` match the code.
- Submit-check the tarball, not just the repo tree: `R CMD build .` then `R CMD check --as-cran aisdk_*.tar.gz`.
- Keep documentation ASCII-safe where possible. Non-ASCII text in roxygen, `README`, or vignettes can break the PDF manual on CRAN.
- Update or remove redirected URLs. CRAN flags stale links in `DESCRIPTION`, `README.md`, and generated `.Rd` files.
- Do not let writing APIs default to `getwd()`, `"."`, or the user's home directory. Use `tempdir()` by default and write to temp locations in examples, tests, and vignettes unless the user explicitly passes a path.
- Do not use `<<-` or write to `.GlobalEnv` in package code. Keep mutable state in local environments, R6 fields, or explicit package-private environments.
- Do not leave runnable examples on unexported functions. Either export the function or omit the example block.
- Keep the build clean via `.Rbuildignore`. Do not let `dev_logs/`, `docs/`, hidden files, local env files, or nested `.git/` directories leak into the tarball.
- Avoid non-portable file names and overlong paths in shipped files.
- Check for accidental artifacts before release, especially `Rplots*.pdf`, `.DS_Store`, `.Rhistory`, and `.RData`.
- If you change exported functions, examples, or package metadata, rerun the relevant tests and mention the exact check commands in the PR.
