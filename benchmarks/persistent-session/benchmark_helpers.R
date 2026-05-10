`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

bench_script_dir <- function() {
  frame_files <- vapply(sys.frames(), function(frame) {
    frame$ofile %||% NA_character_
  }, character(1))
  frame_files <- frame_files[!is.na(frame_files)]
  if (length(frame_files) > 0) {
    return(dirname(normalizePath(frame_files[[length(frame_files)]], mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

bench_parse_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  values <- list()
  flags <- character()

  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (!startsWith(arg, "--")) {
      i <- i + 1L
      next
    }

    key_value <- sub("^--", "", arg)
    if (grepl("=", key_value, fixed = TRUE)) {
      key <- sub("=.*$", "", key_value)
      value <- sub("^[^=]*=", "", key_value)
      values[[key]] <- value
      i <- i + 1L
      next
    }

    key <- key_value
    next_arg <- if (i < length(args)) args[[i + 1L]] else NA_character_
    if (!is.na(next_arg) && !startsWith(next_arg, "--")) {
      values[[key]] <- next_arg
      i <- i + 2L
    } else {
      flags <- c(flags, key)
      i <- i + 1L
    }
  }

  list(values = values, flags = unique(flags))
}

bench_arg <- function(parsed, name, default = NULL, type = "character") {
  value <- parsed$values[[name]] %||% default
  if (is.null(value)) {
    return(NULL)
  }

  switch(
    type,
    integer = as.integer(value),
    numeric = as.numeric(value),
    logical = as.logical(value),
    character = as.character(value),
    value
  )
}

bench_has_flag <- function(parsed, name) {
  name %in% parsed$flags
}

bench_r_literal <- function(x) {
  paste(deparse(x, control = "all"), collapse = "\n")
}

bench_rss_mb <- function(pid = Sys.getpid()) {
  if (.Platform$OS.type == "windows") {
    return(NA_real_)
  }

  out <- suppressWarnings(
    tryCatch(
      system2("ps", c("-o", "rss=", "-p", as.character(pid)), stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    )
  )
  out <- trimws(out)
  out <- out[nzchar(out)]
  if (length(out) == 0) {
    return(NA_real_)
  }

  rss_kb <- suppressWarnings(as.numeric(out[[length(out)]]))
  if (is.na(rss_kb)) {
    return(NA_real_)
  }
  rss_kb / 1024
}

bench_gc_mb <- function(gc_matrix, column) {
  if (is.null(gc_matrix) || !(column %in% colnames(gc_matrix))) {
    return(NA_real_)
  }

  n_used <- gc_matrix["Ncells", column] %||% NA_real_
  v_used <- gc_matrix["Vcells", column] %||% NA_real_
  if (is.na(n_used) || is.na(v_used)) {
    return(NA_real_)
  }

  # R reports GC cells, not bytes. These approximations match R's own
  # conventional Ncells/Vcells byte estimates closely enough for comparison.
  (as.numeric(n_used) * 56 + as.numeric(v_used) * 8) / 1024^2
}

bench_file_size_mb <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(NA_real_)
  }
  as.numeric(file.info(path)$size) / 1024^2
}

bench_max_or_na <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  max(x)
}

bench_object_size_mb <- function(name, envir) {
  if (!exists(name, envir = envir, inherits = FALSE)) {
    return(NA_real_)
  }
  as.numeric(utils::object.size(get(name, envir = envir, inherits = FALSE))) / 1024^2
}

bench_read_object <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "qs")) {
    if (!requireNamespace("qs", quietly = TRUE)) {
      stop("Package 'qs' is required to read .qs benchmark objects.", call. = FALSE)
    }
    return(qs::qread(path))
  }

  readRDS(path)
}

bench_generate_object <- function(path,
                                  rows = 6000L,
                                  cols = 2000L,
                                  groups = 6L,
                                  seed = 1L) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  set.seed(seed)

  counts <- matrix(
    stats::rpois(rows * cols, lambda = 1.2),
    nrow = rows,
    ncol = cols
  )
  storage.mode(counts) <- "integer"
  rownames(counts) <- sprintf("Gene%05d", seq_len(rows))
  colnames(counts) <- sprintf("Cell%05d", seq_len(cols))

  meta <- data.frame(
    cell = colnames(counts),
    group = sample(sprintf("group_%02d", seq_len(groups)), cols, replace = TRUE),
    batch = sample(sprintf("batch_%02d", seq_len(4L)), cols, replace = TRUE),
    condition = sample(c("control", "treated"), cols, replace = TRUE),
    stringsAsFactors = FALSE
  )

  obj <- list(
    counts = counts,
    meta = meta,
    assay = "synthetic_counts",
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    note = "Synthetic single-cell-like object for persistent-session timing benchmarks."
  )

  saveRDS(obj, path, compress = FALSE)
  invisible(list(
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    rows = rows,
    cols = cols,
    groups = groups,
    file_size_mb = bench_file_size_mb(path),
    object_size_mb = as.numeric(utils::object.size(obj)) / 1024^2
  ))
}

bench_prepare_qc <- function(obj) {
  obj$meta$lib_size <- colSums(obj$counts)
  obj$meta$detected_genes <- colSums(obj$counts > 0)
  obj
}

bench_filter_cells <- function(obj, lower_quantile = 0.10) {
  if (!all(c("lib_size", "detected_genes") %in% names(obj$meta))) {
    obj <- bench_prepare_qc(obj)
  }

  lib_cutoff <- stats::quantile(obj$meta$lib_size, lower_quantile, names = FALSE)
  gene_cutoff <- stats::quantile(obj$meta$detected_genes, lower_quantile, names = FALSE)
  keep <- obj$meta$lib_size >= lib_cutoff & obj$meta$detected_genes >= gene_cutoff

  list(
    counts = obj$counts[, keep, drop = FALSE],
    meta = obj$meta[keep, , drop = FALSE],
    cutoffs = c(lib_size = lib_cutoff, detected_genes = gene_cutoff),
    kept = sum(keep),
    dropped = sum(!keep)
  )
}

bench_normalize_top_genes <- function(obj, n_top = 1000L) {
  gene_score <- rowSums(obj$counts)
  top <- order(gene_score, decreasing = TRUE)
  top <- top[seq_len(min(n_top, length(top)))]
  lib_size <- pmax(colSums(obj$counts), 1)
  norm <- log1p(t(t(obj$counts[top, , drop = FALSE]) / lib_size) * 10000)

  list(
    norm = norm,
    genes = rownames(obj$counts)[top],
    meta = obj$meta,
    n_top = length(top)
  )
}

bench_group_means <- function(norm_obj) {
  groups <- norm_obj$meta$group
  group_index <- split(seq_along(groups), groups)
  vapply(
    group_index,
    function(idx) rowMeans(norm_obj$norm[, idx, drop = FALSE]),
    numeric(nrow(norm_obj$norm))
  )
}

bench_task_specs <- function() {
  list(
    list(
      id = "load_object",
      label = "Load object",
      code = paste(
        "list(",
        "  dimensions = dim(obj$counts),",
        "  cells = nrow(obj$meta),",
        "  object_mb = round(as.numeric(utils::object.size(obj)) / 1024^2, 2)",
        ")",
        sep = "\n"
      )
    ),
    list(
      id = "inspect_metadata",
      label = "Inspect metadata",
      code = paste(
        "group_counts <- sort(table(obj$meta$group), decreasing = TRUE)",
        "batch_counts <- sort(table(obj$meta$batch), decreasing = TRUE)",
        "list(",
        "  dimensions = dim(obj$counts),",
        "  groups = head(group_counts, 5),",
        "  batches = head(batch_counts, 4)",
        ")",
        sep = "\n"
      )
    ),
    list(
      id = "qc_summary",
      label = "QC summary",
      code = paste(
        "obj <- bench_prepare_qc(obj)",
        "list(",
        "  lib_size = summary(obj$meta$lib_size),",
        "  detected_genes = summary(obj$meta$detected_genes)",
        ")",
        sep = "\n"
      )
    ),
    list(
      id = "filter_cells",
      label = "Filter cells",
      code = paste(
        "obj_filt <- bench_filter_cells(obj, lower_quantile = 0.10)",
        "list(",
        "  kept = obj_filt$kept,",
        "  dropped = obj_filt$dropped,",
        "  dimensions = dim(obj_filt$counts),",
        "  cutoffs = round(obj_filt$cutoffs, 2)",
        ")",
        sep = "\n"
      )
    ),
    list(
      id = "normalize_top_genes",
      label = "Normalize top genes",
      code = paste(
        "if (!exists('obj_filt', inherits = FALSE)) obj_filt <- bench_filter_cells(obj, lower_quantile = 0.10)",
        "obj_norm <- bench_normalize_top_genes(obj_filt, n_top = 1000L)",
        "list(",
        "  dimensions = dim(obj_norm$norm),",
        "  mean_value = round(mean(obj_norm$norm), 4),",
        "  object_mb = round(as.numeric(utils::object.size(obj_norm)) / 1024^2, 2)",
        ")",
        sep = "\n"
      )
    ),
    list(
      id = "group_means",
      label = "Group means",
      code = paste(
        "if (!exists('obj_filt', inherits = FALSE)) obj_filt <- bench_filter_cells(obj, lower_quantile = 0.10)",
        "if (!exists('obj_norm', inherits = FALSE)) obj_norm <- bench_normalize_top_genes(obj_filt, n_top = 1000L)",
        "group_means <- bench_group_means(obj_norm)",
        "list(",
        "  dimensions = dim(group_means),",
        "  first_value = round(group_means[1, 1], 4)",
        ")",
        sep = "\n"
      )
    ),
    list(
      id = "plot_qc",
      label = "Plot QC",
      code = paste(
        "obj <- bench_prepare_qc(obj)",
        "plot_path <- file.path(bench_output_dir, sprintf('%s_repeat%02d_qc.png', bench_mode, bench_repeat_id))",
        "grDevices::png(plot_path, width = 1200, height = 900, res = 144)",
        "graphics::plot(",
        "  obj$meta$lib_size, obj$meta$detected_genes,",
        "  pch = 16, col = grDevices::adjustcolor('#2f6f9f', alpha.f = 0.35),",
        "  xlab = 'Library size', ylab = 'Detected genes', main = 'QC scatter'",
        ")",
        "grDevices::dev.off()",
        "list(path = plot_path, bytes = file.info(plot_path)$size)",
        sep = "\n"
      )
    ),
    list(
      id = "adjust_threshold",
      label = "Adjust threshold",
      code = paste(
        "if (!all(c('lib_size', 'detected_genes') %in% names(obj$meta))) obj <- bench_prepare_qc(obj)",
        "threshold <- stats::quantile(obj$meta$lib_size, 0.25, names = FALSE)",
        "keep_strict <- obj$meta$lib_size >= threshold",
        "list(",
        "  threshold = round(threshold, 2),",
        "  kept = sum(keep_strict),",
        "  dropped = sum(!keep_strict)",
        ")",
        sep = "\n"
      )
    ),
    list(
      id = "export_summary",
      label = "Export summary",
      code = paste(
        "if (!exists('obj_filt', inherits = FALSE)) obj_filt <- bench_filter_cells(obj, lower_quantile = 0.10)",
        "if (!exists('obj_norm', inherits = FALSE)) obj_norm <- bench_normalize_top_genes(obj_filt, n_top = 1000L)",
        "if (!exists('group_means', inherits = FALSE)) group_means <- bench_group_means(obj_norm)",
        "summary_path <- file.path(bench_output_dir, sprintf('%s_repeat%02d_group_means.csv', bench_mode, bench_repeat_id))",
        "summary_df <- data.frame(gene = rownames(group_means), group_means, check.names = FALSE)",
        "utils::write.csv(summary_df, summary_path, row.names = FALSE)",
        "list(path = summary_path, rows = nrow(summary_df), cols = ncol(summary_df), bytes = file.info(summary_path)$size)",
        sep = "\n"
      )
    )
  )
}

bench_measure_code <- function(code,
                               envir,
                               mode,
                               operation,
                               operation_label,
                               repeat_id,
                               run_id,
                               data_path = NA_character_,
                               load_count = 0L) {
  invisible(gc())
  invisible(gc(reset = TRUE))

  rss_before <- bench_rss_mb()
  pt0 <- proc.time()
  t0 <- Sys.time()
  status <- "ok"
  error <- NA_character_
  result_preview <- NA_character_
  output <- character()

  output <- tryCatch(
    {
      capture.output({
        value <- withVisible(eval(parse(text = code), envir = envir))
        if (isTRUE(value$visible) && !is.null(value$value)) {
          print(value$value)
        }
      })
    },
    error = function(e) {
      status <<- "error"
      error <<- conditionMessage(e)
      character()
    }
  )

  t1 <- Sys.time()
  pt <- proc.time() - pt0
  gc_after <- gc()
  rss_after <- bench_rss_mb()

  if (length(output) > 0) {
    result_preview <- paste(utils::head(output, 20L), collapse = "\n")
    result_preview <- substr(result_preview, 1L, 2000L)
  }

  list(
    run_id = run_id,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    mode = mode,
    repeat_id = repeat_id,
    operation = operation,
    operation_label = operation_label,
    status = status,
    error = error,
    wall_sec = as.numeric(difftime(t1, t0, units = "secs")),
    process_wall_sec = as.numeric(difftime(t1, t0, units = "secs")),
    user_cpu_sec = unname(as.numeric(pt[["user.self"]])),
    sys_cpu_sec = unname(as.numeric(pt[["sys.self"]])),
    rss_before_mb = rss_before,
    rss_after_mb = rss_after,
    rss_delta_mb = rss_after - rss_before,
    process_max_rss_mb = bench_max_or_na(c(rss_before, rss_after)),
    gc_used_mb = bench_gc_mb(gc_after, "used"),
    gc_max_mb = bench_gc_mb(gc_after, "max used"),
    obj_size_mb = bench_object_size_mb("obj", envir),
    obj_filt_size_mb = bench_object_size_mb("obj_filt", envir),
    obj_norm_size_mb = bench_object_size_mb("obj_norm", envir),
    input_data_mb = bench_file_size_mb(data_path),
    load_count = load_count,
    output_lines = length(output),
    output_preview = result_preview
  )
}

bench_write_child_script <- function(path,
                                     helper_path,
                                     task,
                                     data_path,
                                     output_dir,
                                     metrics_path,
                                     repeat_id,
                                     run_id) {
  code <- paste(
    sprintf("source(%s)", bench_r_literal(helper_path)),
    sprintf("bench_data_path <- %s", bench_r_literal(data_path)),
    sprintf("bench_output_dir <- %s", bench_r_literal(output_dir)),
    "bench_mode <- 'codex_rscript_restart'",
    sprintf("bench_repeat_id <- %dL", repeat_id),
    sprintf("bench_run_id <- %s", bench_r_literal(run_id)),
    "dir.create(bench_output_dir, recursive = TRUE, showWarnings = FALSE)",
    sprintf("task_code <- %s", bench_r_literal(paste("obj <- bench_read_object(bench_data_path)", task$code, sep = "\n"))),
    "metrics <- bench_measure_code(",
    "  code = task_code,",
    "  envir = environment(),",
    "  mode = bench_mode,",
    sprintf("  operation = %s,", bench_r_literal(task$id)),
    sprintf("  operation_label = %s,", bench_r_literal(task$label)),
    "  repeat_id = bench_repeat_id,",
    "  run_id = bench_run_id,",
    "  data_path = bench_data_path,",
    "  load_count = 1L",
    ")",
    sprintf("saveRDS(metrics, %s)", bench_r_literal(metrics_path)),
    sep = "\n"
  )

  writeLines(code, path, useBytes = TRUE)
  invisible(path)
}

bench_run_rscript_task <- function(script_path,
                                   metrics_path,
                                   poll_interval = 0.05) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("Package 'processx' is required for subprocess resource monitoring.", call. = FALSE)
  }

  rscript <- file.path(R.home("bin"), "Rscript")
  start <- Sys.time()
  proc <- processx::process$new(
    rscript,
    c("--vanilla", normalizePath(script_path, winslash = "/", mustWork = TRUE)),
    stdout = "|",
    stderr = "|"
  )

  max_rss_mb <- NA_real_
  while (proc$is_alive()) {
    rss <- bench_rss_mb(proc$get_pid())
    if (!is.na(rss)) {
      max_rss_mb <- bench_max_or_na(c(max_rss_mb, rss))
    }
    Sys.sleep(poll_interval)
  }
  proc$wait()
  end <- Sys.time()

  stdout <- proc$read_all_output()
  stderr <- proc$read_all_error()
  exit_status <- proc$get_exit_status()

  if (file.exists(metrics_path)) {
    metrics <- readRDS(metrics_path)
  } else {
    metrics <- list(
      status = "error",
      error = "Child process did not write metrics.",
      output_preview = NA_character_
    )
  }

  metrics$process_wall_sec <- as.numeric(difftime(end, start, units = "secs"))
  metrics$process_max_rss_mb <- bench_max_or_na(c(metrics$process_max_rss_mb %||% NA_real_, max_rss_mb))
  metrics$exit_status <- exit_status
  metrics$stdout_preview <- substr(stdout, 1L, 2000L)
  metrics$stderr_preview <- substr(stderr, 1L, 2000L)
  if (!identical(exit_status, 0L) && identical(metrics$status, "ok")) {
    metrics$status <- "error"
    metrics$error <- paste("Child process exited with status", exit_status)
  }

  metrics
}

bench_as_data_frame <- function(metrics) {
  rows <- lapply(metrics, function(x) {
    fields <- c(
      "run_id", "timestamp", "mode", "repeat_id", "operation",
      "operation_label", "status", "error", "wall_sec", "process_wall_sec",
      "user_cpu_sec", "sys_cpu_sec", "rss_before_mb", "rss_after_mb",
      "rss_delta_mb", "process_max_rss_mb", "gc_used_mb", "gc_max_mb",
      "obj_size_mb", "obj_filt_size_mb", "obj_norm_size_mb", "input_data_mb",
      "load_count", "output_lines", "exit_status", "output_preview",
      "stdout_preview", "stderr_preview"
    )
    row <- lapply(fields, function(field) x[[field]] %||% NA)
    names(row) <- fields
    as.data.frame(row, stringsAsFactors = FALSE)
  })

  df <- do.call(rbind, rows)
  numeric_fields <- c(
    "repeat_id", "wall_sec", "process_wall_sec", "user_cpu_sec",
    "sys_cpu_sec", "rss_before_mb", "rss_after_mb", "rss_delta_mb",
    "process_max_rss_mb", "gc_used_mb", "gc_max_mb", "obj_size_mb",
    "obj_filt_size_mb", "obj_norm_size_mb", "input_data_mb", "load_count",
    "output_lines", "exit_status"
  )
  for (field in intersect(numeric_fields, names(df))) {
    df[[field]] <- suppressWarnings(as.numeric(df[[field]]))
  }
  df
}

bench_summarize_metrics <- function(metrics) {
  op_order <- unique(metrics$operation)
  mode_order <- unique(metrics$mode)
  groups <- split(metrics, list(metrics$mode, metrics$operation), drop = TRUE)

  summary_rows <- lapply(groups, function(df) {
    ok <- df[df$status == "ok", , drop = FALSE]
    values <- ok$process_wall_sec
    rss_values <- ok$process_max_rss_mb
    data.frame(
      mode = df$mode[[1]],
      operation = df$operation[[1]],
      operation_label = df$operation_label[[1]],
      n = nrow(df),
      n_ok = nrow(ok),
      median_process_wall_sec = if (length(values)) stats::median(values, na.rm = TRUE) else NA_real_,
      mean_process_wall_sec = if (length(values)) mean(values, na.rm = TRUE) else NA_real_,
      p95_process_wall_sec = if (length(values)) as.numeric(stats::quantile(values, 0.95, na.rm = TRUE, names = FALSE)) else NA_real_,
      median_user_cpu_sec = if (nrow(ok)) stats::median(ok$user_cpu_sec, na.rm = TRUE) else NA_real_,
      median_sys_cpu_sec = if (nrow(ok)) stats::median(ok$sys_cpu_sec, na.rm = TRUE) else NA_real_,
      median_max_rss_mb = if (length(rss_values)) stats::median(rss_values, na.rm = TRUE) else NA_real_,
      median_gc_max_mb = if (nrow(ok)) stats::median(ok$gc_max_mb, na.rm = TRUE) else NA_real_,
      median_gc_used_mb = if (nrow(ok)) stats::median(ok$gc_used_mb, na.rm = TRUE) else NA_real_,
      max_rss_mb = if (length(rss_values)) bench_max_or_na(rss_values) else NA_real_,
      total_load_count = sum(df$load_count, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, summary_rows)
  out$operation <- factor(out$operation, levels = op_order)
  out$mode <- factor(out$mode, levels = mode_order)
  out <- out[order(out$operation, out$mode), , drop = FALSE]
  out$operation <- as.character(out$operation)
  out$mode <- as.character(out$mode)
  rownames(out) <- NULL
  out
}

bench_total_summary <- function(metrics) {
  groups <- split(metrics, metrics$mode, drop = TRUE)
  rows <- lapply(groups, function(df) {
    ok <- df[df$status == "ok", , drop = FALSE]
    data.frame(
      mode = df$mode[[1]],
      operations = nrow(df),
      ok_operations = nrow(ok),
      total_process_wall_sec = sum(ok$process_wall_sec, na.rm = TRUE),
      total_user_cpu_sec = sum(ok$user_cpu_sec, na.rm = TRUE),
      total_sys_cpu_sec = sum(ok$sys_cpu_sec, na.rm = TRUE),
      max_process_rss_mb = bench_max_or_na(ok$process_max_rss_mb),
      max_gc_mb = bench_max_or_na(ok$gc_max_mb),
      total_load_count = sum(ok$load_count, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

bench_plot_results <- function(metrics, summary, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    pdf_path <- file.path(out_dir, "benchmark_plots.pdf")
    grDevices::pdf(pdf_path, width = 12, height = 8)
    on.exit(grDevices::dev.off(), add = TRUE)
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    graphics::par(mfrow = c(2, 2), mar = c(8, 4, 3, 1))

    op_labels <- unique(summary$operation_label)
    modes <- unique(summary$mode)
    wall_mat <- vapply(modes, function(mode) {
      stats::setNames(summary$median_process_wall_sec[summary$mode == mode], summary$operation_label[summary$mode == mode])[op_labels]
    }, numeric(length(op_labels)))
    graphics::barplot(
      t(wall_mat),
      beside = TRUE,
      las = 2,
      ylab = "Median wall time (sec)",
      main = "Wall time by operation"
    )

    totals <- bench_total_summary(metrics)
    graphics::barplot(
      stats::setNames(totals$total_process_wall_sec, totals$mode),
      ylab = "Total wall time (sec)",
      main = "Total wall time"
    )

    memory_column <- if (any(is.finite(summary$median_max_rss_mb))) {
      "median_max_rss_mb"
    } else {
      "median_gc_max_mb"
    }
    memory_label <- if (identical(memory_column, "median_max_rss_mb")) {
      "Median max RSS (MB)"
    } else {
      "Median R GC max estimate (MB)"
    }
    memory_mat <- vapply(modes, function(mode) {
      stats::setNames(summary[[memory_column]][summary$mode == mode], summary$operation_label[summary$mode == mode])[op_labels]
    }, numeric(length(op_labels)))
    graphics::barplot(
      t(memory_mat),
      beside = TRUE,
      las = 2,
      ylab = memory_label,
      main = "Memory footprint"
    )

    graphics::barplot(
      stats::setNames(totals$total_load_count, totals$mode),
      ylab = "Object reload count",
      main = "Reload count"
    )
    return(invisible(pdf_path))
  }

  ggplot2 <- asNamespace("ggplot2")
  summary$operation_label <- factor(summary$operation_label, levels = unique(summary$operation_label))
  metrics$operation_label <- factor(metrics$operation_label, levels = levels(summary$operation_label))
  metrics <- metrics[order(metrics$mode, metrics$repeat_id, metrics$operation_label), , drop = FALSE]
  metrics$cumulative_wall_sec <- ave(
    metrics$process_wall_sec,
    interaction(metrics$mode, metrics$repeat_id, drop = TRUE),
    FUN = cumsum
  )

  p_wall <- ggplot2$ggplot(summary, ggplot2$aes(x = operation_label, y = median_process_wall_sec, fill = mode)) +
    ggplot2$geom_col(position = ggplot2$position_dodge(width = 0.75), width = 0.68) +
    ggplot2$labs(x = NULL, y = "Median wall time (sec)", fill = "Mode", title = "Operation timing") +
    ggplot2$theme_minimal(base_size = 12) +
    ggplot2$theme(axis.text.x = ggplot2$element_text(angle = 35, hjust = 1))

  p_cum <- ggplot2$ggplot(metrics, ggplot2$aes(x = operation_label, y = cumulative_wall_sec, group = interaction(mode, repeat_id), color = mode)) +
    ggplot2$geom_line(alpha = 0.7) +
    ggplot2$geom_point(size = 1.8) +
    ggplot2$labs(x = NULL, y = "Cumulative wall time (sec)", color = "Mode", title = "Cumulative time across workflow") +
    ggplot2$theme_minimal(base_size = 12) +
    ggplot2$theme(axis.text.x = ggplot2$element_text(angle = 35, hjust = 1))

  memory_column <- if (any(is.finite(summary$median_max_rss_mb))) {
    "median_max_rss_mb"
  } else {
    "median_gc_max_mb"
  }
  memory_label <- if (identical(memory_column, "median_max_rss_mb")) {
    "Median max RSS (MB)"
  } else {
    "Median R GC max estimate (MB)"
  }
  summary$memory_mb <- summary[[memory_column]]

  p_rss <- ggplot2$ggplot(summary, ggplot2$aes(x = operation_label, y = memory_mb, fill = mode)) +
    ggplot2$geom_col(position = ggplot2$position_dodge(width = 0.75), width = 0.68) +
    ggplot2$labs(x = NULL, y = memory_label, fill = "Mode", title = "Memory footprint") +
    ggplot2$theme_minimal(base_size = 12) +
    ggplot2$theme(axis.text.x = ggplot2$element_text(angle = 35, hjust = 1))

  totals <- bench_total_summary(metrics)
  p_load <- ggplot2$ggplot(totals, ggplot2$aes(x = mode, y = total_load_count, fill = mode)) +
    ggplot2$geom_col(width = 0.55) +
    ggplot2$labs(x = NULL, y = "Object reload count", title = "How often the large object is loaded") +
    ggplot2$theme_minimal(base_size = 12) +
    ggplot2$theme(legend.position = "none")

  files <- c(
    operation_timing = file.path(out_dir, "operation_timing.png"),
    cumulative_time = file.path(out_dir, "cumulative_time.png"),
    memory_footprint = file.path(out_dir, "memory_footprint.png"),
    reload_count = file.path(out_dir, "reload_count.png")
  )

  ggplot2$ggsave(files[["operation_timing"]], p_wall, width = 12, height = 7, dpi = 150)
  ggplot2$ggsave(files[["cumulative_time"]], p_cum, width = 12, height = 7, dpi = 150)
  ggplot2$ggsave(files[["memory_footprint"]], p_rss, width = 12, height = 7, dpi = 150)
  ggplot2$ggsave(files[["reload_count"]], p_load, width = 8, height = 6, dpi = 150)

  invisible(files)
}
