# Install Failures — Detailed Playbook

The single most common case where `ask_ai()`'s fingerprint is misleading.
The surfaced one-liner is a wrapper status; the real error is in the
install log written by R CMD INSTALL.

## Symptom signatures

- `installation of package 'X' had non-zero exit status` — wrapper.
- `ERROR: dependency 'Y' is not available for package 'X'` — concrete.
- `package 'X' is not available (for R version Z)` — concrete (repo / version).
- `package 'X' had non-zero exit status` inside a multi-package install —
  one of several; the per-package log is per-package.
- `there is no package called 'X'` after install reported success — the
  package went to a lib path not on `.libPaths()`.

## Step-by-step triage

### Step 1: confirm what the user actually ran

Read the recent commands block in the fingerprint. Look for
`install.packages(...)`, `install_github(...)`, `remotes::install_*(...)`,
`devtools::install_*(...)`, `BiocManager::install(...)`,
`pak::pak(...)`. The function used changes where the log lives.

### Step 2: find the install log

R CMD INSTALL writes `00install.out` to a per-build directory under
`tempdir()`. After a failure the file is **kept** until the R process
exits. Use `bash` to find recent ones (do not invent the path):

```bash
# macOS / Linux
find "$TMPDIR" -name '00install.out' -mmin -30 2>/dev/null
# fallback to common locations if $TMPDIR is empty:
find /tmp -name '00install.out' -mmin -30 2>/dev/null
```

Or from R (no shell dependence, also works on Windows):

```r
# tool call: r_eval with this code
list.files(
  path = tempdir(),
  pattern = "^00install\\.out$",
  recursive = TRUE,
  full.names = TRUE
)
```

`read_file` on the most recent path. The actual `ERROR:` line is usually
in the last 30 lines.

### Step 3: classify the real cause

After reading the install log, the underlying message will be one of:

| Real cause | Looks like in 00install.out |
|---|---|
| Missing R dep | `ERROR: dependency 'Y' is not available for package 'X'` |
| Missing system header | `fatal error: 'libxml/parser.h' file not found` |
| Missing system lib | `ld: library not found for -lcurl` |
| Compiler too old | `error: 'std::variant' is not a member of 'std'` |
| BiocManager not loaded | `package 'X' is a Bioconductor package` (not in CRAN listings) |
| Repository auth | `cannot open URL '...'` with 401/403 |
| Disk / lib path | `cannot write to ... permission denied` |

### Step 4: gather the matching live evidence

Use `r_session_state` to confirm the user's environment supports the fix
you are about to suggest. Specifically check:

- `.libPaths()` — is the **first** entry writable? If not, install goes to a
  user lib that may not be on the search path of later R sessions.
- `getOption("repos")` — does it include `BioCsoft` / `BioCann` for
  Bioconductor packages? Is the CRAN mirror reachable?
- `Sys.getenv("R_LIBS_USER")` — does it point where the user expects?
- For `install_github`: does the user have a `GITHUB_PAT` set? (the env
  var section of `r_session_state` will show this; the value is masked.)

For a missing R dependency, confirm whether the dep is even available:

```r
# r_eval
deps <- "Y"  # the missing dependency name from the install log
ap <- available.packages()
deps %in% rownames(ap)
# If FALSE, ap is from CRAN only; for Bioc:
ap_bioc <- available.packages(repos = BiocManager::repositories())
deps %in% rownames(ap_bioc)
```

### Step 5: produce a runnable fix

Tell the user exactly what to paste. Examples:

**Missing Bioc dependency**
```r
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("Y")
# then retry the original install
install.packages("X")
```

**Missing system header on macOS**
```bash
brew install libxml2 openssl
# then in R, after pkg-config picks them up:
install.packages("X", type = "source")
```

**Missing system header on Debian/Ubuntu**
```bash
sudo apt-get install -y libxml2-dev libssl-dev libcurl4-openssl-dev
```

**Lib path not writable**
```r
# Choose a user-writable lib once:
dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE)
.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))
install.packages("X")
```

## Worked example: the `gpemisc`/`confuns` case (issue #24)

Fingerprint showed only `installation of package 'confuns' had non-zero
exit status`. The real terminal output had:

```
ERROR: dependency 'gpemisc' is not available for package 'confuns'
```

Triage path that should have been followed:

1. Recognize the symptom class (1).
2. `r_eval` listing `00install.out` files in `tempdir()`.
3. `read_file` on the most recent one → finds the `gpemisc` line.
4. `r_eval`: `"gpemisc" %in% rownames(available.packages())` → FALSE.
5. `r_eval`: check Bioconductor / GitHub: search the GitHub remote of
   the calling package's `DESCRIPTION` for `Remotes:` to see if `gpemisc`
   was meant to come from a non-CRAN source.
6. Tell the user to install `gpemisc` from its actual source first.

That whole chain is invisible to a model that only sees the fingerprint.
The skill exists so the model knows to walk that chain on its own.
