---
name: r-debug
description: Triage R errors when the surfaced message is a symptom, not the
  root cause. Use whenever an error mentions "non-zero exit status", "had
  warnings", "installation of package X failed", an opaque subprocess result,
  garbled text, a session crash, or whenever the user invokes ask_ai() and the
  collected error fingerprint feels too thin for the real failure.
aliases:
  - r-error-triage
  - r-diagnosis
user-invocable: true
---

# r-debug — R Error Triage Skill

## When this skill applies

R's `geterrmessage()` returns only the **last** top-level error string.
`last.warning` holds only the most recent warning batch. Anything written
to the terminal by a child process — compilers, `install.packages` workers,
`system()`/`system2()`/`processx` calls — never enters the parent R session
and is not in the `ask_ai()` fingerprint.

This skill exists for the recurring case where the surfaced message is a
**symptom** of a deeper failure that lives somewhere else: an install log,
a child process's stderr, a system library, a locale setting, an SSL cert.
The job is to find the real root cause, not paraphrase the symptom.

Trigger this skill (read this file fully) when any of these are true:

- The error contains: `non-zero exit status`, `had warnings`, `installation
  failed`, `command not found`, `child process error`, `cannot allocate`,
  `corrupted MCH file`, `R Session Aborted`, `segfault`.
- The warning summary lists `installation of package … had non-zero exit status`.
- The error references a package the user just tried to install (cause is
  likely in the install log, not in the surfaced one-liner).
- The error references a path, file encoding, locale, proxy, or SSL.
- The user says the real error scrolled past, or shows you a screenshot
  with much more output than is in the fingerprint.

## The core workflow

1. **Read the fingerprint carefully.** Identify the surface symptom and
   which symptom class below it most likely belongs to.
2. **State a hypothesis.** "I think the real cause is X, located in Y."
3. **Gather evidence with the tools you have.** Use `r_session_state`,
   `r_eval`, `bash`, `read_file`. Never claim a root cause without
   evidence pulled from the live session or a log file.
4. **Revise the hypothesis** if evidence contradicts it. Repeat until
   you can name a specific cause and a specific fix.
5. **Tell the user the fix as a runnable step**, not a discussion.

## Symptom → root-cause map

For each symptom class below: the surface phrase, the most common real
causes, and the specific places to look. Always look before you guess.

### 1. `install.packages()` / `install_github()` / `BiocManager::install()` failure
**Symptoms**: `installation of package 'X' had non-zero exit status`;
`ERROR: dependency 'Y' is not available for package 'X'`; `package 'X' is
not available (for R version Z)`.

**Real causes (ordered by frequency)**:
- A transitive R-package dependency was not on the configured repositories.
- A system library was missing (compilation failed inside the package).
- The user's `.libPaths()[1]` is not writable; install went to a wrong lib.
- Repos misconfigured (e.g. BiocManager not added; HTTPS blocked).
- R version mismatch — the package's `DESCRIPTION` requires a newer R.

**Where the real message lives**: `00install.out` written under
`tempdir()` during build. It is deleted on success but **persists on
failure** until the R process ends.

**Detailed playbook**: `references/install-failures.md`.

### 2. Compilation failure for a source package (Rcpp, src/, sf, terra…)
**Symptoms**: `gcc: error: …`; `make: *** Error 1`; `fatal error: X.h: No
such file or directory`; `cannot find -lY`.

**Real causes**:
- Missing system header / `-dev` package (`libxml2-dev`, `libssl-dev`,
  `libcurl4-openssl-dev`, `libgdal-dev`, etc).
- On macOS: missing Xcode Command Line Tools, missing brew formula,
  missing `gfortran`, or `~/.R/Makevars` pointing to the wrong compiler.
- Outdated compiler that cannot build the package's C++ standard.

**Where to look**: same `00install.out`. On macOS, `~/.R/Makevars` and
`xcode-select -p` matter. On Linux, `pkg-config --list-all | grep <lib>`.

**Detailed playbook**: `references/compilation-failures.md`.

### 3. Locale / encoding issues
**Symptoms**: `invalid multibyte character`; `string is not in this
locale`; Chinese/Japanese characters printed as `<U+XXXX>`; CSV reads
return mojibake.

**Real causes**:
- `Sys.getlocale()` reports `C` or a mismatched codepage.
- File has a BOM or non-UTF-8 encoding the reader didn't expect.
- On Windows: legacy GBK/CP936 vs UTF-8 mismatch.

**Where to look**: `Sys.getlocale()`, `Sys.getenv("LANG")`,
`l10n_info()`, the file's first bytes (`readBin(file, "raw", 4)`).

### 4. Subprocess stderr was lost
**Symptoms**: `system()` / `system2()` returned non-zero with empty
`stderr`. Opaque "command failed". Output appeared on the terminal but
the R object is empty.

**Real cause**: `system()` and friends default to letting stderr stream
to the terminal rather than capturing it. The information is gone from R.

**Fix**: re-run with `r_eval` and either redirect inside the shell
(`cmd 2>&1`) or use `system2(..., stderr = TRUE)` / `processx::run()`.

### 5. Namespace conflict / masking
**Symptoms**: `could not find function "X"`; a function returns the
wrong type; results suddenly differ after `library(...)`.

**Where to look**: `search()`, `find("X")`, `conflicts(detail = TRUE)`,
`getAnywhere("X")`. The order of attached packages matters.

### 6. Network / SSL / proxy failure
**Symptoms**: `Couldn't resolve host`; `SSL connect error`;
`Operation timed out`; download stalls.

**Where to look**: `Sys.getenv("http_proxy" | "https_proxy")`,
`getOption("repos")`, `getOption("timeout")`,
`curl::has_internet()`, `curl::nslookup("cran.r-project.org")`,
`Sys.getenv("CURL_CA_BUNDLE")`, `Sys.getenv("SSL_CERT_FILE")`.

### 7. Bioconductor specifics
**Symptoms**: `package 'X' is not available` for a known Bioc package;
`Bioconductor version 'a.b' requires R version 'c.d'`; install succeeds
but `BiocManager::valid()` warns.

**Where to look**: `BiocManager::version()`,
`BiocManager::repositories()`, `BiocManager::valid()`.

### 8. R session crash / segfault
**Symptoms**: `R Session Aborted`; "R encountered a fatal error"; a
hard restart after a specific line.

**Real causes**: native-code bug in a package, OOM, malformed serialized
object, ggplot device crash. The R-level traceback is gone after the
restart.

**Where to look**: ask the user for the exact last line; reproduce in a
fresh `r_eval` subprocess with minimal data; check
`tools::package_dependencies()` for recent native updates.

### 9. Quarto / knitr / R Markdown render failure
**Symptoms**: "Quitting from lines …" with a tiny error; pandoc
non-zero exit; an `Rscript -e rmarkdown::render(...)` returned an opaque
status.

**Where to look**: re-run with `r_eval` calling `rmarkdown::render(...,
quiet = FALSE)`; the stderr from the spawned pandoc / Rscript will then
be in the `r_eval` capture rather than lost to the original terminal.

## Anti-patterns

- **Do not** restate the surfaced error message back to the user as the
  diagnosis. That is the symptom they already saw.
- **Do not** suggest `install.packages("X")` for a missing dependency
  without first checking that `X` is on the configured repos via
  `available.packages()`. If it isn't, the install will fail the same way.
- **Do not** assume the captured error is current. If warnings indicate a
  package install failure, the surfaced error may belong to a different,
  earlier command — verify by re-running with `r_eval`.
- **Do not** declare success until you have produced a specific fix the
  user can paste into their console.

## References

- `references/install-failures.md` — detailed playbook for symptom class 1
- `references/compilation-failures.md` — symptom class 2
- `references/encoding-locale.md` — symptom class 3
- `references/subprocess-stderr.md` — symptom class 4
- `references/namespace-masking.md` — symptom class 5
- `references/network-ssl.md` — symptom class 6
- `references/bioconductor.md` — symptom class 7
- `references/session-crash.md` — symptom class 8
