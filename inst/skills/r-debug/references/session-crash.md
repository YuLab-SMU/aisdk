# Session Crash / Segfault — Detailed Playbook

After a crash, the R-level traceback is gone, the call stack is gone,
the environment was reset. You only have what the user remembers and the
files on disk.

## Step 1 — establish the trigger

Ask the user:

- What was the exact last line they ran?
- Was it the first time they ran it, or did it work before?
- Did they recently install or upgrade any package?
- Did they recently load a large object from `.RData`?

Without one of these, the next steps are guesswork.

## Step 2 — reproduce in `r_eval`

The biggest advantage `r_eval` gives here is **process isolation**: a
crash in the subprocess does not kill the agent's host R session.

```r
# r_eval
library(SuspectPackage)
# the exact reproducer
```

If `r_eval` returns `status: SUBPROCESS_ERROR` with no R-level error
body but the message mentions a non-zero exit signal, that is the
signature of a native crash (segfault, abort).

## Step 3 — narrow with a minimal reproducer

Halve the input. Drop arguments to defaults. Replace the data with a
synthetic version. If the crash disappears, the diff is informative.

For data-shape-dependent crashes:

```r
# r_eval
str(input)
dim(input); class(input); typeof(input)
anyNA(input)
```

## Step 4 — known-cause crash families

- **ggplot device crash**: `dev.off()` left a stale device; try
  `graphics.off()` then re-run.
- **OOM (out of memory)**: `gc(reset = TRUE)` followed by inspecting
  object sizes via `pryr::mem_used()` or `lobstr::obj_sizes()`.
- **Recent native package upgrade**: roll back via `remotes::install_version("X", version = "old.version")`.
- **Corrupt `.RData`**: do not auto-load on startup —
  `R --no-restore`. Check for `~/.RData` and `<project>/.RData`.
- **Reticulate / Python segfault**: check `reticulate::py_config()` for a
  mismatched Python and ensure the user is not loading torch+keras together.

## Step 5 — escalate honestly

If the crash is in package native code and unrelated to user code, the
right move is to file an issue at the package's bug tracker with the
minimal reproducer. Say so. Do not pretend to fix what is upstream's bug.
