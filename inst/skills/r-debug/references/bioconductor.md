# Bioconductor — Detailed Playbook

Bioconductor's version-coupling to R is the single most common cause of
"package X is not available" errors that look like a missing dependency
but are actually a version-policy refusal.

## The version coupling

- Each R version has exactly one matching Bioc release line.
- `BiocManager::install("X")` for an R that is out of sync with Bioc's
  current release silently fails with "not available" for many packages.
- Across major upgrades, install errors cascade because half the user's
  installed packages are from a release line that does not match R.

## Step 1 — check the live versions

```r
# r_eval
R.Version()$version.string
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::version()
BiocManager::repositories()
```

The repositories returned must include `BioCsoft` and `BioCann`. If they
do not, `getOption("repos")` is overriding them.

## Step 2 — validate the lib state

```r
# r_eval
BiocManager::valid()
# returns either TRUE, or a list `out_of_date` + `too_new` to summarize
```

`too_new` packages came from a different Bioc release line than what
this R is paired with — those are usually the real problem.

## Step 3 — fix patterns

**Just installed Bioc, missing dep**:
```r
BiocManager::install(c("X", "Y"), update = FALSE, ask = FALSE)
```

**R version + Bioc mismatch**: tell the user to either upgrade R to the
version that matches the desired Bioc, or pin Bioc to the older release
that matches their R:
```r
BiocManager::install(version = "3.18", update = FALSE, ask = FALSE)
```

**Many `too_new` packages after an R downgrade**:
```r
BiocManager::install(update = TRUE, ask = FALSE)
# this realigns the existing lib to the Bioc release for the current R
```

## Step 4 — when a Bioc package is in `Remotes:` of a CRAN/GitHub package

Some GitHub packages depend on a Bioc package but only declare it in
`Imports:` — `install_github` will then surface `package 'X' is not
available`. The fix:

```r
BiocManager::install("X")
remotes::install_github("user/repo")  # retry
```
