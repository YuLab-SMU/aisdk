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
    "Persistent session benchmark\n\n",
    "Usage:\n",
    "  Rscript benchmarks/persistent-session/run_benchmark.R [options]\n\n",
    "Options:\n",
    "  --out-dir PATH       Output directory. Defaults to a timestamped results dir.\n",
    "  --data-path PATH     Existing .rds or .qs object. If absent, a synthetic object is generated.\n",
    "  --rows N             Synthetic gene/feature rows. Default: 6000.\n",
    "  --cols N             Synthetic cell/sample columns. Default: 2000.\n",
    "  --groups N           Synthetic group count. Default: 6.\n",
    "  --repeats N          Workflow repeats per mode. Default: 3.\n",
    "  --seed N             Synthetic data seed. Default: 1.\n",
    "  --force-data         Regenerate synthetic data even if the file exists.\n",
    "  --skip-codex         Skip Rscript restart baseline.\n",
    "  --skip-aisdk         Skip aisdk live-session mode.\n",
    "  --help               Show this help.\n",
    sep = ""
  )
  quit(save = "no", status = 0)
}

timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
default_out_dir <- file.path(script_dir, "results", timestamp)
out_dir <- bench_arg(parsed, "out-dir", default_out_dir)
out_dir <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

data_path <- bench_arg(parsed, "data-path", NULL)
rows <- bench_arg(parsed, "rows", 6000L, type = "integer")
cols <- bench_arg(parsed, "cols", 2000L, type = "integer")
groups <- bench_arg(parsed, "groups", 6L, type = "integer")
repeats <- bench_arg(parsed, "repeats", 3L, type = "integer")
seed <- bench_arg(parsed, "seed", 1L, type = "integer")
force_data <- bench_has_flag(parsed, "force-data")
skip_codex <- bench_has_flag(parsed, "skip-codex")
skip_aisdk <- bench_has_flag(parsed, "skip-aisdk")

if (is.null(data_path)) {
  data_path <- file.path(out_dir, "synthetic_sc_object.rds")
}
data_path <- normalizePath(data_path, winslash = "/", mustWork = FALSE)

if (!file.exists(data_path) || force_data) {
  message("Generating synthetic object: ", data_path)
  data_info <- bench_generate_object(
    path = data_path,
    rows = rows,
    cols = cols,
    groups = groups,
    seed = seed
  )
} else {
  message("Using existing benchmark object: ", data_path)
  data_info <- list(
    path = data_path,
    rows = NA_integer_,
    cols = NA_integer_,
    groups = NA_integer_,
    file_size_mb = bench_file_size_mb(data_path),
    object_size_mb = NA_real_
  )
}

helper_path <- normalizePath(file.path(script_dir, "benchmark_helpers.R"), winslash = "/", mustWork = TRUE)
scripts_dir <- file.path(out_dir, "generated_scripts")
metrics_dir <- file.path(out_dir, "child_metrics")
dir.create(scripts_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)

run_id <- paste0("persistent-session-", timestamp)
tasks <- bench_task_specs()
all_metrics <- list()

message("Benchmark object size on disk: ", round(bench_file_size_mb(data_path), 2), " MB")
message("Workflow tasks: ", length(tasks), "; repeats per mode: ", repeats)

if (!skip_codex) {
  message("Running Codex-style baseline: new Rscript process per operation")
  for (repeat_id in seq_len(repeats)) {
    for (task_index in seq_along(tasks)) {
      task <- tasks[[task_index]]
      message(sprintf("  [codex repeat %02d] %s", repeat_id, task$label))
      script_path <- file.path(
        scripts_dir,
        sprintf("codex_repeat%02d_%02d_%s.R", repeat_id, task_index, task$id)
      )
      metrics_path <- file.path(
        metrics_dir,
        sprintf("codex_repeat%02d_%02d_%s.rds", repeat_id, task_index, task$id)
      )

      bench_write_child_script(
        path = script_path,
        helper_path = helper_path,
        task = task,
        data_path = data_path,
        output_dir = out_dir,
        metrics_path = metrics_path,
        repeat_id = repeat_id,
        run_id = run_id
      )
      metric <- bench_run_rscript_task(script_path, metrics_path)
      all_metrics[[length(all_metrics) + 1L]] <- metric
    }
  }
}

if (!skip_aisdk) {
  message("Running aisdk-style live session: one R environment per repeat")
  for (repeat_id in seq_len(repeats)) {
    env <- new.env(parent = globalenv())
    assign("bench_data_path", data_path, envir = env)
    assign("bench_output_dir", out_dir, envir = env)
    assign("bench_mode", "aisdk_live_session", envir = env)
    assign("bench_repeat_id", repeat_id, envir = env)
    assign("bench_run_id", run_id, envir = env)

    for (task in tasks) {
      message(sprintf("  [aisdk repeat %02d] %s", repeat_id, task$label))
      code <- task$code
      load_count <- 0L
      if (identical(task$id, "load_object")) {
        code <- paste("obj <- bench_read_object(bench_data_path)", task$code, sep = "\n")
        load_count <- 1L
      }

      metric <- bench_measure_code(
        code = code,
        envir = env,
        mode = "aisdk_live_session",
        operation = task$id,
        operation_label = task$label,
        repeat_id = repeat_id,
        run_id = run_id,
        data_path = data_path,
        load_count = load_count
      )
      metric$exit_status <- 0L
      metric$stdout_preview <- NA_character_
      metric$stderr_preview <- NA_character_
      all_metrics[[length(all_metrics) + 1L]] <- metric
    }
  }
}

metrics <- bench_as_data_frame(all_metrics)
summary <- bench_summarize_metrics(metrics)
totals <- bench_total_summary(metrics)

metrics_path <- file.path(out_dir, "operation_metrics.csv")
summary_path <- file.path(out_dir, "summary_by_operation.csv")
totals_path <- file.path(out_dir, "summary_by_mode.csv")
utils::write.csv(metrics, metrics_path, row.names = FALSE)
utils::write.csv(summary, summary_path, row.names = FALSE)
utils::write.csv(totals, totals_path, row.names = FALSE)

plot_files <- bench_plot_results(metrics, summary, out_dir)

manifest <- list(
  run_id = run_id,
  created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  script_dir = normalizePath(script_dir, winslash = "/", mustWork = FALSE),
  out_dir = out_dir,
  data = data_info,
  options = list(
    repeats = repeats,
    rows = rows,
    cols = cols,
    groups = groups,
    seed = seed,
    skip_codex = skip_codex,
    skip_aisdk = skip_aisdk
  ),
  files = list(
    operation_metrics = metrics_path,
    summary_by_operation = summary_path,
    summary_by_mode = totals_path,
    plots = as.list(plot_files)
  ),
  interpretation = list(
    codex_rscript_restart = "Simulates an AI coding workflow that writes a fresh script and runs Rscript for each operation, causing the large object to reload each time.",
    aisdk_live_session = "Simulates aisdk local execution in a persistent R session: the object is loaded once per repeat and later operations reuse live objects."
  )
)

json_path <- file.path(out_dir, "manifest.json")
jsonlite::write_json(manifest, json_path, auto_unbox = TRUE, pretty = TRUE)

cat("\nBenchmark complete\n")
cat("Output directory: ", out_dir, "\n", sep = "")
cat("Metrics: ", metrics_path, "\n", sep = "")
cat("Summary: ", summary_path, "\n", sep = "")
cat("Mode totals: ", totals_path, "\n", sep = "")
cat("Manifest: ", json_path, "\n", sep = "")
if (length(plot_files) > 0) {
  cat("Plots:\n")
  for (path in unname(plot_files)) {
    cat("  - ", path, "\n", sep = "")
  }
}
