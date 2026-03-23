#' @title Mission Orchestrator: Concurrent Mission Scheduling
#' @description
#' MissionOrchestrator R6 class for managing multiple concurrent Missions.
#' Implements the Coordinator layer from Symphony's architecture:
#' poll loop, concurrency slots, stall detection, and result aggregation.
#'
#' Concurrency model:
#' \itemize{
#'   \item Synchronous batch: run_all() executes up to max_concurrent missions
#'     simultaneously using parallel::mclapply (fork-based on Unix, sequential on Windows).
#'   \item Async per-mission: run_async() launches a mission in a callr background process
#'     and returns a handle for polling.
#' }
#' @name mission_orchestrator
NULL

#' @title MissionOrchestrator Class
#' @description
#' R6 class that manages a queue of Missions and executes them within
#' a concurrency limit. Provides full observability via status summaries
#' and stall detection through reconcile().
#' @export
MissionOrchestrator <- R6::R6Class(
  "MissionOrchestrator",
  lock_objects = FALSE,
  public = list(
    #' @field pending_queue List of `Mission` objects waiting to run.
    pending_queue = NULL,
    #' @field running List of `Mission` objects currently executing.
    running = NULL,
    #' @field completed List of `Mission` objects that have finished.
    completed = NULL,
    #' @field max_concurrent Maximum simultaneous missions. Default 3.
    max_concurrent = 3,
    #' @field global_session Optional SharedSession shared across all missions.
    global_session = NULL,
    #' @field global_model Default model ID for missions that don't specify one.
    global_model = NULL,
    #' @field async_handles list of callr::r_bg handles (for run_async).
    async_handles = NULL,

    #' @description Initialize a new MissionOrchestrator.
    #' @param max_concurrent Maximum simultaneous missions. Default 3.
    #' @param model Optional default model for all missions.
    #' @param session Optional shared SharedSession.
    initialize = function(max_concurrent = 3,
                          model = NULL,
                          session = NULL) {
      self$max_concurrent  <- max_concurrent
      self$global_model    <- model
      self$global_session  <- session
      self$pending_queue   <- list()
      self$running         <- list()
      self$completed       <- list()
      self$async_handles   <- list()
    },

    #' @description Submit a Mission to the orchestrator queue.
    #' @param mission A Mission object.
    #' @return Invisible self for chaining.
    submit = function(mission) {
      if (!inherits(mission, "Mission")) {
        rlang::abort("submit() requires a Mission object.")
      }

      # Inject global model/session if mission doesn't have them
      if (is.null(mission$model) && !is.null(self$global_model)) {
        mission$model <- self$global_model
      }
      if (is.null(mission$session) || is.null(mission$session$get_model_id())) {
        if (!is.null(self$global_session)) {
          mission$session <- self$global_session
        }
      }

      self$pending_queue <- c(self$pending_queue, list(mission))
      invisible(self)
    },

    #' @description Run all submitted missions respecting the concurrency limit.
    #' @details
    #' Missions are executed in batches of max_concurrent. Within each batch,
    #' missions run in parallel (via parallel::mclapply on Unix, sequentially
    #' on Windows). Completed missions are moved to $completed.
    #' @param model Optional model override for all missions in this run.
    #' @return Invisibly returns the list of completed `Mission` objects.
    run_all = function(model = NULL) {
      effective_model <- model %||% self$global_model

      if (length(self$pending_queue) == 0) {
        message("[MissionOrchestrator] No missions in queue.")
        return(invisible(self$completed))
      }

      all_missions <- self$pending_queue
      self$pending_queue <- list()

      # Ensure each mission has a model
      for (m in all_missions) {
        if (is.null(m$model) && !is.null(effective_model)) {
          m$model <- effective_model
        }
        if (is.null(m$model)) {
          rlang::abort(paste0(
            "Mission '", m$id, "' has no model. ",
            "Provide model to Orchestrator or each Mission."
          ))
        }
      }

      # Split into batches of max_concurrent
      batches <- private$split_into_batches(all_missions, self$max_concurrent)

      for (batch in batches) {
        batch_results <- private$run_batch(batch, effective_model)
        self$completed <- c(self$completed, batch_results)
      }

      invisible(self$completed)
    },

    #' @description Run a single Mission asynchronously in a background process.
    #' @details
    #' Uses callr::r_bg to launch the mission in a separate R process.
    #' The mission state is serialized to a temp file, executed, and the result
    #' is written back. Call $poll_async() to check completion.
    #' @param mission A Mission object.
    #' @param model Optional model override.
    #' @return A list with $handle (callr process), $mission_id, $checkpoint_path.
    run_async = function(mission, model = NULL) {
      if (!requireNamespace("callr", quietly = TRUE)) {
        rlang::abort("callr package required for run_async(). Install with: install.packages('callr')")
      }

      effective_model <- model %||% mission$model %||% self$global_model
      if (is.null(effective_model)) {
        rlang::abort("No model specified for async mission.")
      }
      mission$model <- effective_model

      # Save mission state to temp file for IPC
      checkpoint_path <- tempfile(pattern = paste0("mission_", mission$id, "_"), fileext = ".rds")
      mission$save(checkpoint_path)

      result_path <- paste0(checkpoint_path, "_result.rds")

      # Launch callr background process
      handle <- callr::r_bg(
        func = function(checkpoint_path, result_path, pkg_lib) {
          .libPaths(pkg_lib)
          library(aisdk)
          m <- Mission$new(goal = "placeholder")
          m$resume(checkpoint_path)
          m$run()
          m$save(result_path)
          m$status
        },
        args = list(
          checkpoint_path = checkpoint_path,
          result_path     = result_path,
          pkg_lib         = .libPaths()
        )
      )

      async_entry <- list(
        handle          = handle,
        mission         = mission,
        checkpoint_path = checkpoint_path,
        result_path     = result_path,
        started_at      = Sys.time()
      )

      self$async_handles <- c(self$async_handles, list(async_entry))
      invisible(async_entry)
    },

    #' @description Poll all async handles and collect completed missions.
    #' @return Named list with `completed` (list of `Mission` objects) and
    #'   `still_running` (integer count).
    poll_async = function() {
      completed_now <- list()
      still_running <- list()

      for (entry in self$async_handles) {
        if (entry$handle$is_alive()) {
          still_running <- c(still_running, list(entry))
        } else {
          # Process completed
          if (file.exists(entry$result_path)) {
            m <- Mission$new(goal = "placeholder")
            tryCatch(
              {
                m$resume(entry$result_path)
                self$completed <- c(self$completed, list(m))
                completed_now <- c(completed_now, list(m))
              },
              error = function(e) {
                warning("Failed to load async mission result: ", conditionMessage(e))
              }
            )
            unlink(entry$result_path)
          }
          unlink(entry$checkpoint_path)
        }
      }

      self$async_handles <- still_running

      list(
        completed     = completed_now,
        still_running = length(still_running)
      )
    },

    #' @description Stall detection: check for missions that appear stuck.
    #' @param stall_threshold_secs Missions running longer than this are flagged.
    #'   Default 600 (10 minutes).
    #' @return list of stalled mission IDs.
    reconcile = function(stall_threshold_secs = 600) {
      stalled_ids <- character(0)

      for (entry in self$async_handles) {
        elapsed <- as.numeric(difftime(Sys.time(), entry$started_at, units = "secs"))
        if (elapsed > stall_threshold_secs && entry$handle$is_alive()) {
          stalled_ids <- c(stalled_ids, entry$mission$id)
          warning(sprintf(
            "[MissionOrchestrator] Mission '%s' has been running for %.0f seconds (threshold: %.0f).",
            entry$mission$id, elapsed, stall_threshold_secs
          ))
        }
      }

      stalled_ids
    },

    #' @description Get a status summary of all missions.
    #' @return A data.frame with id, goal, status, n_steps columns.
    status = function() {
      all_missions <- c(
        self$pending_queue,
        self$completed,
        lapply(self$async_handles, function(e) e$mission)
      )

      if (length(all_missions) == 0) {
        return(data.frame(
          id     = character(0),
          goal   = character(0),
          status = character(0),
          n_steps = integer(0),
          stringsAsFactors = FALSE
        ))
      }

      data.frame(
        id      = sapply(all_missions, function(m) m$id),
        goal    = sapply(all_missions, function(m) substr(m$goal, 1, 50)),
        status  = sapply(all_missions, function(m) m$status),
        n_steps = sapply(all_missions, function(m) length(m$steps %||% list())),
        stringsAsFactors = FALSE
      )
    },

    #' @description Print method.
    print = function() {
      cat("<MissionOrchestrator>\n")
      cat("  Max concurrent: ", self$max_concurrent, "\n")
      cat("  Pending:        ", length(self$pending_queue), "\n")
      cat("  Async running:  ", length(self$async_handles), "\n")
      cat("  Completed:      ", length(self$completed), "\n")
      if (length(self$completed) > 0) {
        cat("  Results:\n")
        for (m in self$completed) {
          cat(sprintf("    [%s] %s: %s\n", m$status, m$id, substr(m$goal, 1, 40)))
        }
      }
      invisible(self)
    }
  ),

  private = list(

    # Split mission list into batches
    split_into_batches = function(missions, batch_size) {
      n <- length(missions)
      if (n == 0) return(list())
      starts <- seq(1, n, by = batch_size)
      lapply(starts, function(s) {
        e <- min(s + batch_size - 1, n)
        missions[s:e]
      })
    },

    # Run a batch of missions with fork-based parallelism
    run_batch = function(missions, model) {
      if (length(missions) == 0) return(list())

      # Windows: sequential fallback
      if (.Platform$OS.type == "windows" || length(missions) == 1) {
        results <- lapply(missions, function(m) {
          tryCatch(
            {
              m$run(model = model)
              m
            },
            error = function(e) {
              m$status <- "failed"
              m
            }
          )
        })
        return(results)
      }

      # Unix: parallel fork via parallel::mclapply
      n_cores <- min(length(missions), parallel::detectCores(logical = FALSE) %||% 2)

      parallel::mclapply(missions, function(m) {
        tryCatch(
          {
            m$run(model = model)
            m
          },
          error = function(e) {
            m$status <- "failed"
            m
          }
        )
      }, mc.cores = n_cores)
    }
  )
)

#' @title Create a Mission Orchestrator
#' @description
#' Factory function to create a MissionOrchestrator.
#' @param max_concurrent Maximum simultaneous missions. Default 3.
#' @param model Optional default model for all missions.
#' @param session Optional shared SharedSession.
#' @return A MissionOrchestrator object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   orchestrator <- create_mission_orchestrator(max_concurrent = 5, model = "openai:gpt-4o")
#'
#'   orchestrator$submit(create_mission("Task A", executor = agent_a))
#'   orchestrator$submit(create_mission("Task B", executor = agent_b))
#'   orchestrator$submit(create_mission("Task C", executor = agent_c))
#'
#'   results <- orchestrator$run_all()
#'   print(orchestrator$status())
#' }
#' }
create_mission_orchestrator <- function(max_concurrent = 3,
                                        model = NULL,
                                        session = NULL) {
  MissionOrchestrator$new(
    max_concurrent = max_concurrent,
    model          = model,
    session        = session
  )
}

# Null-coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
