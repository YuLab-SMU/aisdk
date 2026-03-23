## Test environments

- macOS Tahoe 26.3, aarch64-apple-darwin20
- R 4.5.2

## R CMD check results

This is a new submission.

I ran:

```sh
Rscript -e "devtools::document()"
R CMD build .
env PATH="/Users/xiayh/Library/TinyTeX/bin/universal-darwin:$PATH" \
  _R_CHECK_FORCE_SUGGESTS_=false \
  R CMD check --as-cran aisdk_1.0.0.tar.gz
```

`R CMD check --as-cran` completed with 3 NOTEs and no ERRORs or WARNINGs:

1. `New submission`
2. `unable to verify current time`
3. HTML validation was skipped locally because the system `tidy` is not recent enough

During local checking, the following suggested packages were not available:
`httptest2`, `skimr`, `torch`, and `onnx`.
For this reason, the final local check was run with `_R_CHECK_FORCE_SUGGESTS_=false`.

## Resubmission notes

- Added `AGENTS.md` to `.Rbuildignore` so local agent instructions are excluded from the source tarball.
- Reworked the roxygen documentation for `Mission`, `MissionStep`, and
  `MissionOrchestrator` to remove angle-bracket type annotations such as
  `list<Mission>` and `list<MissionStep>`, which were being emitted into the
  HTML manual as invalid tags during CRAN incoming checks.
