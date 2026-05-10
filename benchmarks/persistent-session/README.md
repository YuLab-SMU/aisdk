# Persistent R Session Benchmark

This benchmark measures the workflow problem discussed in the article: large R
objects are expensive to reload when an AI coding assistant repeatedly writes
scripts and launches fresh R processes.

It compares two deterministic execution modes:

- `codex_rscript_restart`: each operation is written to a generated R script and
  executed with a fresh `Rscript` process. The benchmark object is loaded again
  for every operation.
- `aisdk_live_session`: operations are evaluated in one long-lived R environment
  per repeat. The benchmark object is loaded once, then later operations reuse
  live objects and intermediate results. This models `console_chat()` local mode
  and `execute_r_code_local`.

The benchmark records wall time, R CPU time, reload counts, R GC memory
estimates, process RSS where the platform exposes it through `ps`, status/error
details, generated outputs, and plot artifacts.

## Quick Run

From the repository root:

```sh
Rscript benchmarks/persistent-session/run_benchmark.R \
  --rows 6000 \
  --cols 2000 \
  --repeats 3
```

This creates a synthetic single-cell-like object and writes results under:

```text
benchmarks/persistent-session/results/<timestamp>/
```

For a fast smoke test:

```sh
Rscript benchmarks/persistent-session/run_benchmark.R \
  --rows 800 \
  --cols 300 \
  --repeats 1
```

For a heavier local test:

```sh
Rscript benchmarks/persistent-session/run_benchmark.R \
  --rows 20000 \
  --cols 5000 \
  --repeats 3
```

## Use a Real Object

Use any `.rds` object shaped like:

```r
list(
  counts = matrix(...),       # genes/features x cells/samples; Matrix sparse objects also work
  meta = data.frame(...)      # one row per column in counts
)
```

`meta` should contain `group` and `batch` columns. A `.qs` file also works if
the `qs` package is installed.

```sh
Rscript benchmarks/persistent-session/run_benchmark.R \
  --data-path /path/to/sce_like_object.rds \
  --repeats 3
```

If your real object is a Seurat or SingleCellExperiment object, convert it once
to the minimal shape above before running the benchmark. Keeping the measured
workflow simple makes the comparison about session persistence, not framework
method dispatch.

## Outputs

The runner writes:

- `operation_metrics.csv`: one row per mode, repeat, and operation.
- `summary_by_operation.csv`: median/mean/p95 timing and memory by operation.
- `summary_by_mode.csv`: total time, peak memory, and reload count by mode.
- `manifest.json`: run metadata and file paths.
- `operation_timing.png`: per-operation wall-time comparison.
- `cumulative_time.png`: cumulative workflow timing.
- `memory_footprint.png`: per-operation RSS comparison.
- `reload_count.png`: object reload count by mode.

Regenerate plots from a previous run:

```sh
Rscript benchmarks/persistent-session/plot_results.R \
  --out-dir benchmarks/persistent-session/results/<timestamp>
```

## Reading the Result

The expected signal is not that evaluating a single R expression is faster in
aisdk. The expected signal is that interactive analysis often repeats small
operations around the same large object. In the restart workflow, each operation
pays the object-load cost. In the live-session workflow, load happens once and
subsequent operations reuse `obj`, `obj_filt`, `obj_norm`, and `group_means`.

This is the exact gap that `aisdk::console_chat()` local mode is intended to
cover for R analysis.
