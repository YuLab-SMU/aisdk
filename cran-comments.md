## Submission summary

This is version 1.4.12. It exports a small, stable "extension API" (seven
helper functions for HTTP requests and image handling) so that a companion
package, `aisdk.providers`, can build on the core machinery through the public
interface instead of reaching into the `aisdk` namespace. It also adds an
opt-in, interactive install prompt when a user requests a provider that is
supplied by a companion package.

### Reason for this submission

`aisdk.providers` (a separate package, submitted immediately after this one)
supplies a number of long-tail, OpenAI-/Anthropic-compatible provider adapters
that were split out of the core during the recent refactor. Those adapters need
a handful of the core's request/retry and image helpers. Rather than have the
companion access unexported internals, this release promotes exactly seven
helpers to the exported surface, documented with `\keyword{internal}` so they
stay out of the main help index but are available to package authors. Their
behaviour is unchanged; this is purely additive.

A second, additive change makes the provider registry offer to install the
companion package when a user asks for one of its providers and it is not yet
installed. The prompt only appears in interactive sessions (via
`rlang::check_installed()`); in non-interactive sessions, scripts, `R CMD
check` and CRAN runs it never installs anything and instead produces a clear
error naming the package to install. The companion package is referenced
dynamically and is therefore not declared in `Suggests` (it is not yet on
CRAN at the time of this submission); it will be added to `Suggests` in a
later release once it is published.

### Submission timing

This follows shortly after 1.4.11. The change is small and additive but is a
prerequisite for the concurrent `aisdk.providers` submission, which cannot be
accepted until these helpers are part of the exported interface.

## Test environments

* local macOS arm64, R release (R CMD check --as-cran)
* win-builder R-devel
* mac-builder R-release

## R CMD check results

0 errors | 0 warnings | 0 notes
(local checks may emit an "unable to verify current time" NOTE and a
LaTeX/`pdflatex`-not-available NOTE that are specific to this machine and do
not appear on CRAN's servers.)

## Downstream dependencies

There are currently no reverse dependencies on CRAN.

Best regards,
Yonghe Xia
