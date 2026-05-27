# Namespace Conflicts & Masking — Detailed Playbook

## Symptoms

- `could not find function "X"` — X was never attached, or was masked
  away, or X is in a namespace you have not imported.
- A familiar function returns the wrong type or different output after
  the user attached a new package.
- `dplyr::filter` vs `stats::filter` (the canonical example).
- An S3 method dispatch picks the wrong method silently.

## Triage steps

### Step 1 — confirm the function exists somewhere

```r
# r_eval
getAnywhere("X")
# lists all environments / namespaces where X is defined,
# annotates which one is currently active
```

If `getAnywhere` returns nothing, the function genuinely does not exist
in any attached/loadable namespace. The user mistyped, or the package
defining it is not installed.

### Step 2 — see what is masking what

```r
# r_eval
search()                 # ordered attach list
conflicts(detail = TRUE) # what is masked, and by which package
find("X")                # which attached envs define X
```

The rule: the **first** attached package on `search()` wins. `library()`
adds at position 2 (right after `.GlobalEnv`), so the most recently
attached package masks earlier ones.

### Step 3 — pick a disambiguation strategy

For a one-off call:

```r
dplyr::filter(x, y > 0)   # always wins, regardless of search order
```

For a script that depends on a specific resolution:

```r
library(conflicted)
conflict_prefer("filter", "dplyr")
```

For an interactive session where the user did not realize they masked
something:

```r
detach("package:OFFENDER", unload = TRUE)
```

### Step 4 — S4 / S3 dispatch surprises

```r
# r_eval
methods("X")              # all S3 methods for generic X
showMethods("X")          # all S4 methods
getMethod("X", "ClassY")  # the specific method that would dispatch
```

If the user is using Bioconductor objects, S4 dispatch on
`SingleCellExperiment` / `SummarizedExperiment` is often the source of
"this used to work" surprises after a package upgrade.
