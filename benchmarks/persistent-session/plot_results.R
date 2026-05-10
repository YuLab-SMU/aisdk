#!/usr/bin/env Rscript

cmd_args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", cmd_args[grepl("^--file=", cmd_args)])
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(file_arg[[1]], winslash = "/", mustWork = FALSE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
if (!file.exists(file.path(script_dir, "benchmark_helpers.R"))) {
  script_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
source(file.path(script_dir, "benchmark_helpers.R"))

parsed <- bench_parse_args()

if (bench_has_flag(parsed, "help")) {
  cat(
    "Regenerate benchmark plots\n\n",
    "Usage:\n",
    "  Rscript benchmarks/persistent-session/plot_results.R --out-dir PATH\n",
    "  Rscript benchmarks/persistent-session/plot_results.R --metrics PATH --out-dir PATH\n\n",
    "Options:\n",
    "  --out-dir PATH       Benchmark output directory or plot destination.\n",
    "  --metrics PATH       operation_metrics.csv path. Defaults to OUT/operation_metrics.csv.\n",
    "  --help               Show this help.\n",
    sep = ""
  )
  quit(save = "no", status = 0)
}

out_dir <- bench_arg(parsed, "out-dir", NULL)
metrics_path <- bench_arg(parsed, "metrics", NULL)

if (is.null(out_dir) && is.null(metrics_path)) {
  stop("Provide --out-dir or --metrics. Use --help for examples.", call. = FALSE)
}

if (is.null(metrics_path)) {
  metrics_path <- file.path(out_dir, "operation_metrics.csv")
}
metrics_path <- normalizePath(metrics_path, winslash = "/", mustWork = TRUE)

if (is.null(out_dir)) {
  out_dir <- dirname(metrics_path)
}
out_dir <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

metrics <- utils::read.csv(metrics_path, stringsAsFactors = FALSE, check.names = FALSE)
summary <- bench_summarize_metrics(metrics)
totals <- bench_total_summary(metrics)

summary_path <- file.path(out_dir, "summary_by_operation.csv")
totals_path <- file.path(out_dir, "summary_by_mode.csv")
utils::write.csv(summary, summary_path, row.names = FALSE)
utils::write.csv(totals, totals_path, row.names = FALSE)

plot_files <- bench_plot_results(metrics, summary, out_dir)

cat("Plots regenerated in: ", out_dir, "\n", sep = "")
if (length(plot_files) > 0) {
  for (path in unname(plot_files)) {
    cat("  - ", path, "\n", sep = "")
  }
}

