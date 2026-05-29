## Submission summary

This is an update of 'aisdk' from the current CRAN version 1.1.0 to 1.4.10.

### Reason for the version jump

The package has undergone a substantial internal refactor that split
several optional sub-systems (multi-agent orchestration, MCP client/
server, provider plug-ins, skill ecosystem, channels/messaging) into
separate companion packages. The CRAN-resident `aisdk` is now the
self-contained core that exposes the unified provider interface,
agent/tool/skill base classes, and the request/retry machinery.

### Resubmission note

A previous attempt at 1.4.9 failed the CRAN incoming Debian pre-test
because a process-tree teardown test in
`tests/testthat/test-r-introspect-tools.R` polled `pgrep` for the
reaping of `sh` + `sleep` grandchildren, which is not deterministic
on Linux check farms (the kernel may reparent the grandchild to PID 1
before `processx::kill_tree()` can reach it).

The TIMEOUT user-visible behaviour is fully covered by a separate
test in the same file. The race-prone polling test now also calls
`testthat::skip_on_cran()` (it was already skipped on Windows and CI
runners). No code outside the test was changed for this fix.

## Test environments

* local macOS arm64, R 4.5.2 (R CMD check --as-cran)
* win-builder R-devel
* mac-builder R-release (R 4.6.0 Patched, arm64)

## R CMD check results

0 errors | 0 warnings | 0 notes
(local check produces a single "unable to verify current time" NOTE
that is environment-specific and does not appear on CRAN's servers.)

## Downstream dependencies

There are currently no reverse dependencies on CRAN.

Best regards,
Yonghe Xia
