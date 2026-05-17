# Subprocess stderr was lost — Detailed Playbook

`system()` and `system2()` default to letting stderr stream to the
terminal rather than capturing it. The R-level return value is just an
integer status. If that status is non-zero and the user did not redirect
stderr, the actual diagnostic is gone from R memory.

The fingerprint will show only the wrapper warning
("running command 'X' had status N") or nothing at all.

## How to re-capture

### Option A — re-run inside `r_eval`

`r_eval` already routes the subprocess's stderr to a file and reads it
back into the structured result. Re-execute the same command:

```r
# r_eval
system2("X", c("arg1", "arg2"), stdout = TRUE, stderr = TRUE)
```

The `[stderr_begin]/[stderr_end]` block in the `r_eval` result will
contain what the user originally saw scroll past.

### Option B — use `processx`

If `processx` is installed, it captures both streams cleanly:

```r
# r_eval
processx::run("X", c("arg1", "arg2"), error_on_status = FALSE)
# returns list(status, stdout, stderr, timeout)
```

### Option C — bash redirect

If the underlying command is best run as a shell pipeline, use the
`bash` tool with explicit redirection:

```bash
X arg1 arg2 2>&1
```

## Common offenders

- `git2r` / `gert` shelling out to git for auth → use `gert::git_clone(verbose = TRUE)` or set `GIT_TERMINAL_PROMPT=0`.
- `rstan` / `cmdstanr` compiling models → re-run with `r_eval` to see compiler output.
- `httr` calling `system()` for downloads → use `httr::config(verbose = TRUE)`.
- `Sys.which()` returning `""` for a tool the user expects on PATH →
  inspect `Sys.getenv("PATH")` (already in `r_session_state`).

## Don't lose the lesson

If the user's own code calls `system()` without capturing, suggest they
add `stdout = TRUE, stderr = TRUE` going forward — not just for this
debugging session. The lost-output problem is preventable at the call
site.
