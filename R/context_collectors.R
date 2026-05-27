#' @title Context State Collectors
#' @description
#' Internal helpers for collecting execution monitoring, system/device info,
#' and R runtime state signals to populate context state fields.
#' @name context_collectors
NULL

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

#' @keywords internal
collect_execution_log <- function(session = NULL, generation_result = NULL, max_entries = 10L) {
  log_entries <- list()

  if (!is.null(generation_result)) {
    tool_calls <- generation_result$all_tool_calls %||% list()
    tool_results <- generation_result$all_tool_results %||% list()

    if (length(tool_calls) > 0 && length(tool_results) > 0) {
      calls_by_id <- setNames(tool_calls, vapply(tool_calls, function(tc) tc$id %||% "", character(1)))

      for (tool_result in tool_results) {
        tool_call <- calls_by_id[[tool_result$id %||% ""]]
        if (is.null(tool_call)) {
          next
        }

        if (!(tool_result$name %in% c("execute_r_code", "execute_r_code_local", "bash"))) {
          next
        }

        command <- if (tool_result$name == "bash") {
          tool_call$arguments$command %||% ""
        } else {
          tool_call$arguments$code %||% ""
        }

        entry <- list(
          command = command,
          tool = tool_result$name,
          timestamp = as.character(Sys.time())
        )

        if (isTRUE(tool_result$is_error)) {
          entry$error <- tool_result$result %||% "Tool execution error"
          entry$exit_code <- 1L
        } else {
          entry$result <- tool_result$result %||% ""
          entry$exit_code <- 0L
        }

        log_entries[[length(log_entries) + 1L]] <- entry
      }
    }
  }

  if (!is.null(session) && inherits(session, "ChatSession")) {
    computer <- tryCatch(session$get_metadata("computer_instance", default = NULL), error = function(e) NULL)
    if (!is.null(computer) && inherits(computer, "Computer")) {
      computer_log <- tryCatch(computer$get_execution_log(), error = function(e) list())
      for (entry in computer_log) {
        log_entries[[length(log_entries) + 1L]] <- entry
      }
    }
  }

  if (length(log_entries) > max_entries) {
    log_entries <- tail(log_entries, max_entries)
  }

  log_entries
}

#' @keywords internal
collect_system_info <- function() {
  info <- list()

  info$os <- list(
    type = Sys.info()[["sysname"]],
    version = Sys.info()[["release"]],
    platform = R.version$platform,
    arch = R.version$arch
  )

  info$r_version <- paste(R.version$major, R.version$minor, sep = ".")
  info$r_platform <- R.version$platform

  mem_info <- tryCatch({
    if (.Platform$OS.type == "unix") {
      if (file.exists("/proc/meminfo")) {
        lines <- readLines("/proc/meminfo", n = 10L)
        total_line <- grep("^MemTotal:", lines, value = TRUE)
        avail_line <- grep("^MemAvailable:", lines, value = TRUE)

        total_kb <- if (length(total_line) > 0) {
          as.numeric(sub("^MemTotal:\\s+(\\d+).*", "\\1", total_line))
        } else {
          NA_real_
        }
        avail_kb <- if (length(avail_line) > 0) {
          as.numeric(sub("^MemAvailable:\\s+(\\d+).*", "\\1", avail_line))
        } else {
          NA_real_
        }

        list(
          total_mb = total_kb / 1024,
          available_mb = avail_kb / 1024,
          used_percent = if (!is.na(total_kb) && !is.na(avail_kb) && total_kb > 0) {
            ((total_kb - avail_kb) / total_kb) * 100
          } else {
            NA_real_
          }
        )
      } else {
        NULL
      }
    } else if (.Platform$OS.type == "windows") {
      NULL
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(mem_info)) {
    info$memory <- mem_info
  }

  cpu_info <- tryCatch({
    cores <- parallel::detectCores(logical = FALSE)
    model <- if (.Platform$OS.type == "unix" && file.exists("/proc/cpuinfo")) {
      lines <- readLines("/proc/cpuinfo", n = 50L)
      model_line <- grep("^model name", lines, value = TRUE)
      if (length(model_line) > 0) {
        sub("^model name\\s*:\\s*", "", model_line[[1]])
      } else {
        NULL
      }
    } else {
      NULL
    }

    list(
      cores = cores,
      model = model
    )
  }, error = function(e) list(cores = NA_integer_, model = NULL))

  info$cpu <- cpu_info
  info$working_dir <- getwd()

  info
}

#' @keywords internal
collect_runtime_state <- function(session = NULL) {
  state <- list()

  state$connections <- tryCatch({
    all_cons <- showConnections(all = TRUE)
    if (nrow(all_cons) == 0) {
      list()
    } else {
      lapply(seq_len(nrow(all_cons)), function(i) {
        list(
          description = all_cons[i, "description"],
          class = all_cons[i, "class"],
          mode = all_cons[i, "mode"],
          status = if (all_cons[i, "opened"] == "opened") "open" else "closed"
        )
      })
    }
  }, error = function(e) list())

  state$graphics_devices <- tryCatch({
    devs <- grDevices::dev.list()
    if (length(devs) == 0) {
      list()
    } else {
      lapply(devs, function(dev_num) {
        list(
          number = dev_num,
          name = names(devs)[devs == dev_num]
        )
      })
    }
  }, error = function(e) list())



  state$options <- tryCatch({
    key_options <- c("width", "digits", "scipen", "stringsAsFactors", "warn", "error", "max.print")
    opts <- options()
    opts[names(opts) %in% key_options]
  }, error = function(e) list())

  state$search_path <- tryCatch(search(), error = function(e) character(0))

  state$loaded_packages <- tryCatch({
    loaded <- loadedNamespaces()
    loaded[!grepl("^(base|utils|stats|graphics|grDevices|methods|datasets)$", loaded)]
  }, error = function(e) character(0))

  state$env_vars <- tryCatch({
    key_vars <- c("PATH", "HOME", "R_LIBS", "R_LIBS_USER", "TMPDIR", "LANG", "LC_ALL")
    all_vars <- Sys.getenv()
    all_vars[names(all_vars) %in% key_vars]
  }, error = function(e) list())

  state
}
