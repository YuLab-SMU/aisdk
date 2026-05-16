#' @title Mission System: Global Agent Leadership Layer
#' @description
#' Mission and MissionStep R6 classes providing a persistent goal-tracking layer
#' above Agent/AgentTeam/Flow. A Mission maintains a state machine across multiple
#' steps, handles failure recovery with exponential backoff and error-context injection,
#' supports LLM-driven dynamic planning, DAG-based step dependencies, and checkpoint
#' persistence for resume-after-interrupt.
#'
#' Architecture position:
#' Mission -> (MissionStep -> executor: Agent | AgentTeam | Flow | function)
#'         -> SharedSession (shared state across steps)
#'         -> MissionHookHandler (observability)
#'
#' @name mission
NULL

# ---------------------------------------------------------------------------
# Default stall policy
# ---------------------------------------------------------------------------

#' @keywords internal
default_stall_policy <- function() {
  list(
    on_tool_failure   = "inject_error_and_retry",
    on_step_timeout   = "skip_or_replan",
    on_max_retries    = "escalate",
    escalate_fn       = NULL  # user-supplied function(mission, step)
  )
}

#' @keywords internal
format_mission_semantic_payload <- function(name, payload) {
  identity <- payload$identity %||% list()
  schema <- payload$schema %||% list()
  semantics <- payload$semantics %||% list()
  hint <- payload$workflow_hint %||% NULL

  lines <- c(
    paste0("- ", name, ": adapter=", payload$adapter %||% "unknown"),
    if (!is.null(identity$primary_class)) paste0("  class: ", identity$primary_class)
      else if (!is.null(identity$class)) paste0("  class: ", paste(identity$class, collapse = ", ")),
    if (!is.null(semantics$summary)) paste0("  semantics: ", semantics$summary),
    if (!is.null(schema$assays) && length(schema$assays) > 0) paste0("  assays: ", paste(schema$assays, collapse = ", ")),
    if (!is.null(schema$reduced_dims) && length(schema$reduced_dims) > 0) paste0("  reduced_dims: ", paste(schema$reduced_dims, collapse = ", ")),
    if (!is.null(schema$col_data_columns) && length(schema$col_data_columns) > 0) paste0("  colData: ", paste(schema$col_data_columns, collapse = ", ")),
    if (!is.null(schema$metadata_columns) && length(schema$metadata_columns) > 0) paste0("  metadata columns: ", paste(schema$metadata_columns, collapse = ", ")),
    if (!is.null(schema$seqlevels) && length(schema$seqlevels) > 0) paste0("  seqlevels: ", paste(utils::head(schema$seqlevels, 10), collapse = ", "))
  )

  if (is.list(hint) && length(hint) > 0) {
    if (!is.null(hint$steps) && length(hint$steps) > 0) {
      lines <- c(lines, paste0("  workflow hint: ", paste(hint$steps, collapse = " -> ")))
    } else {
      hint_text <- paste(utils::capture.output(str(hint, max.level = 1)), collapse = " ")
      lines <- c(lines, paste0("  workflow hint: ", hint_text))
    }
  }

  paste(lines[!is.na(lines) & nzchar(lines)], collapse = "\n")
}

# ---------------------------------------------------------------------------
# MissionStep
# ---------------------------------------------------------------------------

#' @title MissionStep Class
#' @description
#' A single execution unit within a Mission. Each step wraps an executor
#' (Agent, AgentTeam, Flow, or plain R function) and handles its own
#' retry loop with error-history injection.
#' @export
MissionStep <- R6::R6Class(
  "MissionStep",
  lock_objects = FALSE,
  public = list(
    #' @field id Unique step identifier.
    id = NULL,
    #' @field description Natural language description of what this step does.
    description = NULL,
    #' @field executor Agent | AgentTeam | Flow | function to perform the step.
    executor = NULL,
    #' @field status Current status: "pending"|"running"|"done"|"failed"|"retrying".
    status = "pending",
    #' @field max_retries Maximum retry attempts before escalation. Default 2.
    max_retries = 2,
    #' @field retry_count Number of retries attempted so far.
    retry_count = 0,
    #' @field timeout_secs Optional per-step timeout in seconds. NULL = no timeout.
    timeout_secs = NULL,
    #' @field parallel If TRUE, this step may run concurrently with other parallel steps.
    parallel = FALSE,
    #' @field depends_on Character vector of step IDs that must complete before this step.
    depends_on = NULL,
    #' @field result The text result from the executor on success.
    result = NULL,
    #' @field error_history List of failure records, each containing
    #'   `attempt`, `error`, and `timestamp`.
    error_history = NULL,

    #' @description Initialize a MissionStep.
    #' @param id Unique step ID (e.g., "step_1").
    #' @param description Natural language task description.
    #' @param executor Agent, AgentTeam, Flow, or R function.
    #' @param max_retries Maximum retries before stall escalation. Default 2.
    #' @param timeout_secs Optional per-step timeout. Default NULL.
    #' @param parallel Can run in parallel with other parallel steps. Default FALSE.
    #' @param depends_on Character vector of prerequisite step IDs. Default NULL.
    initialize = function(id,
                          description,
                          executor = NULL,
                          max_retries = 2,
                          timeout_secs = NULL,
                          parallel = FALSE,
                          depends_on = NULL) {
      self$id           <- id
      self$description  <- description
      self$executor     <- executor
      self$max_retries  <- max_retries
      self$timeout_secs <- timeout_secs
      self$parallel     <- parallel
      self$depends_on   <- depends_on
      self$error_history <- list()
    },

    #' @description Execute this step once (no retry logic; handled by Mission).
    #' @param session A ChatSession for shared state.
    #' @param model Model ID string.
    #' @param context Optional error-injection context string.
    #' @return Character string result, or stops with an error.
    run = function(session, model, context = NULL) {
      executor <- self$executor

      if (is.null(executor)) {
        stop(sprintf("MissionStep '%s' has no executor.", self$id))
      }

      # Build the full task prompt: description + context (error injection)
      task <- self$description
      if (!is.null(context) && nzchar(context)) {
        task <- paste0(task, "\n\n[EXECUTION CONTEXT]\n", context)
      }

      result_text <- if (inherits(executor, "Agent")) {
        res <- executor$run(
          task      = task,
          session   = session,
          model     = model,
          max_steps = 20
        )
        res$text

      } else if (inherits(executor, "AgentTeam")) {
        res <- executor$run(task = task, session = session, model = model)
        res$text %||% ""

      } else if (inherits(executor, "Flow")) {
        # Flow needs a primary agent; use current or create a generic runner
        primary <- executor$current()
        if (is.null(primary)) {
          primary <- Agent$new(
            name        = "MissionRunner",
            description = "General purpose runner for mission steps"
          )
          primary$model <- model
        }
        res <- executor$run(primary, task)
        res$text %||% ""

      } else if (is.function(executor)) {
        as.character(executor(task, session, context))

      } else {
        stop(sprintf(
          "MissionStep '%s': unknown executor type '%s'.",
          self$id, paste(class(executor), collapse = "/")
        ))
      }

      result_text
    },

    #' @description Print method.
    print = function() {
      cat("<MissionStep>\n")
      cat("  ID:          ", self$id, "\n")
      cat("  Status:      ", self$status, "\n")
      cat("  Description: ", substr(self$description, 1, 60), "\n")
      cat("  Retries:     ", self$retry_count, "/", self$max_retries, "\n")
      cat("  Parallel:    ", self$parallel, "\n")
      invisible(self)
    }
  )
)

# ---------------------------------------------------------------------------
# Mission
# ---------------------------------------------------------------------------

#' @title Mission Class
#' @description
#' R6 class representing a persistent, goal-oriented execution mission.
#' A Mission is the global leadership layer above Agent/AgentTeam/Flow.
#'
#' Key capabilities:
#' \itemize{
#'   \item Full state machine: pending -> planning -> running -> succeeded/failed/stalled
#'   \item LLM-driven auto-planning: converts a goal string into ordered MissionSteps
#'   \item Step-level retry with exponential backoff + error-context injection
#'   \item DAG dependency resolution (depends_on)
#'   \item Parallel step execution (parallel = TRUE groups)
#'   \item Checkpoint persistence: save() / resume()
#'   \item Full audit log for post-mortem analysis
#'   \item MissionHookHandler integration for observability
#' }
#' @export
Mission <- R6::R6Class(
  "Mission",
  lock_objects = FALSE,
  public = list(
    #' @field id Unique mission UUID.
    id = NULL,
    #' @field goal Natural language goal description.
    goal = NULL,
    #' @field steps List of `MissionStep` objects.
    steps = NULL,
    #' @field status Mission status string.
    status = "pending",
    #' @field session SharedSession used across all steps.
    session = NULL,
    #' @field model Default model ID for this mission.
    model = NULL,
    #' @field stall_policy Named list defining failure recovery behavior.
    stall_policy = NULL,
    #' @field hooks MissionHookHandler for lifecycle events.
    hooks = NULL,
    #' @field audit_log List of event records in chronological order.
    audit_log = NULL,
    #' @field auto_plan If TRUE and steps is NULL, use LLM to plan before running.
    auto_plan = TRUE,
    #' @field default_executor Default executor for auto-planned steps.
    default_executor = NULL,

    #' @description Initialize a new Mission.
    #' @param goal Natural language goal description.
    #' @param steps Optional list of `MissionStep` objects. If NULL and
    #'   auto_plan=TRUE, the LLM plans them.
    #' @param model Default model ID (e.g., "anthropic:claude-opus-4-6").
    #' @param executor Default executor for all steps (Agent, AgentTeam, Flow, or function).
    #'   Used for auto-planned steps when no per-step executor is specified.
    #' @param stall_policy Named list with on_tool_failure, on_step_timeout, on_max_retries,
    #'   escalate_fn. Defaults to default_stall_policy().
    #' @param hooks MissionHookHandler for lifecycle events.
    #' @param session Optional SharedSession. Created automatically if NULL.
    #' @param auto_plan If TRUE, call LLM to decompose goal into steps when steps is NULL.
    initialize = function(goal,
                          steps = NULL,
                          model = NULL,
                          executor = NULL,
                          stall_policy = NULL,
                          hooks = NULL,
                          session = NULL,
                          auto_plan = TRUE) {
      if (missing(goal) || !is.character(goal) || !nzchar(goal)) {
        rlang::abort("Mission 'goal' is required and must be a non-empty string.")
      }

      self$id             <- private$generate_id()
      self$goal           <- goal
      self$steps          <- steps
      self$model          <- model
      self$default_executor <- executor
      self$stall_policy   <- stall_policy %||% default_stall_policy()
      self$hooks          <- hooks
      self$auto_plan      <- auto_plan
      self$audit_log      <- list()

      # Create session if not provided
      if (!is.null(session)) {
        self$session <- session
      } else {
        self$session <- create_shared_session(model = model)
      }
    },

    #' @description Run the Mission synchronously until completion or stall.
    #' @param model Optional model override. Falls back to self$model.
    #' @param ... Additional arguments (reserved for future use).
    #' @return Invisible self (inspect $status, $steps, $audit_log for results).
    run = function(model = NULL, ...) {
      effective_model <- model %||% self$model
      if (is.null(effective_model)) {
        rlang::abort("No model specified. Provide 'model' to Mission$run() or set at initialize().")
      }
      mission_started_at <- Sys.time()

      # Update session model if needed
      if (!is.null(self$session) && !is.null(effective_model)) {
        if (is.null(self$session$get_model_id())) {
          self$session$switch_model(effective_model)
        }
      }

      private$transition("running")

      if (!is.null(self$hooks)) self$hooks$trigger_mission_start(self)
      private$log_event("mission_start", list(goal = self$goal, model = effective_model))

      # Auto-plan: decompose goal into steps using LLM
      if (is.null(self$steps) && isTRUE(self$auto_plan)) {
        private$transition("planning")
        if (!is.null(self$hooks)) self$hooks$trigger_planner_start(self)
        private$log_event("planner_start")
        planning_started_at <- Sys.time()
        tryCatch(
          private$plan(effective_model),
          error = function(e) {
            private$transition("failed", paste("Planning failed:", conditionMessage(e)))
            error_message <- conditionMessage(e)
            private$log_event("planning_failed", list(
              error_type = private$classify_error_type(error_message),
              error_message = error_message,
              duration_ms = private$duration_ms(planning_started_at)
            ))
            rlang::abort(paste("Mission planning failed:", conditionMessage(e)))
          }
        )
        if (!is.null(self$hooks)) self$hooks$trigger_planner_done(self)
        if (!is.null(self$hooks)) self$hooks$trigger_mission_planned(self)
        private$log_event("planner_done", list(
          duration_ms = private$duration_ms(planning_started_at),
          n_steps = length(self$steps %||% list())
        ))
        private$transition("running")
      }

      if (is.null(self$steps) || length(self$steps) == 0) {
        rlang::abort("Mission has no steps. Provide steps or enable auto_plan with a default executor.")
      }

      # Execute steps respecting DAG order and parallel groups
      private$execute_all_steps(effective_model)

      # Final status
      failed_steps <- Filter(function(s) s$status == "failed", self$steps)
      if (length(failed_steps) == 0) {
        private$transition("succeeded")
      } else {
        private$transition("failed", paste(
          length(failed_steps), "step(s) failed:",
          paste(sapply(failed_steps, function(s) s$id), collapse = ", ")
        ))
      }

      if (!is.null(self$hooks)) self$hooks$trigger_mission_done(self)
      private$log_event("mission_end", list(
        status = self$status,
        duration_ms = private$duration_ms(mission_started_at)
      ))
      private$log_event("mission_done", list(
        status = self$status,
        duration_ms = private$duration_ms(mission_started_at)
      ))

      invisible(self)
    },

    #' @description Save mission state to a file for later resumption.
    #' @param path File path (.rds).
    save = function(path) {
      state <- list(
        id            = self$id,
        goal          = self$goal,
        status        = self$status,
        model         = self$model,
        auto_plan     = self$auto_plan,
        stall_policy  = self$stall_policy,
        audit_log     = self$audit_log,
        steps = lapply(self$steps %||% list(), function(s) {
          list(
            id           = s$id,
            description  = s$description,
            status       = s$status,
            max_retries  = s$max_retries,
            retry_count  = s$retry_count,
            timeout_secs = s$timeout_secs,
            parallel     = s$parallel,
            depends_on   = s$depends_on,
            result       = s$result,
            error_history = s$error_history
          )
        })
      )
      saveRDS(state, path)
      invisible(self)
    },

    #' @description Resume a Mission from a saved checkpoint.
    #' @param path File path to a previously saved mission state (.rds).
    #' @details Steps that are already "done" are skipped. Pending/failed/retrying
    #'   steps are re-executed. The executor must be re-attached via $set_executor()
    #'   or by providing a default_executor at Mission creation.
    resume = function(path) {
      state <- readRDS(path)

      self$id         <- state$id
      self$goal       <- state$goal
      self$status     <- state$status
      self$model      <- state$model %||% self$model
      self$auto_plan  <- state$auto_plan
      self$audit_log  <- state$audit_log %||% list()

      # Restore steps (without executors — user must re-attach)
      if (!is.null(state$steps) && length(state$steps) > 0) {
        self$steps <- lapply(state$steps, function(sd) {
          step <- MissionStep$new(
            id           = sd$id,
            description  = sd$description,
            executor     = self$default_executor,  # re-attach default
            max_retries  = sd$max_retries %||% 2,
            timeout_secs = sd$timeout_secs,
            parallel     = sd$parallel %||% FALSE,
            depends_on   = sd$depends_on
          )
          step$status       <- sd$status %||% "pending"
          step$retry_count  <- sd$retry_count %||% 0
          step$result       <- sd$result
          step$error_history <- sd$error_history %||% list()
          step
        })
      }

      private$log_event("mission_resumed", list(from_path = path))
      invisible(self)
    },

    #' @description Get a summary of step statuses.
    #' @return Named character vector: step_id -> status.
    step_summary = function() {
      if (is.null(self$steps)) return(character(0))
      statuses <- sapply(self$steps, function(s) s$status)
      names(statuses) <- sapply(self$steps, function(s) s$id)
      statuses
    },

    #' @description Print method.
    print = function() {
      cat("<Mission>\n")
      cat("  ID:     ", self$id, "\n")
      cat("  Goal:   ", substr(self$goal, 1, 70), "\n")
      cat("  Status: ", self$status, "\n")
      cat("  Steps:  ", length(self$steps %||% list()), "\n")
      if (!is.null(self$steps)) {
        summary <- self$step_summary()
        for (nm in names(summary)) {
          cat("    -", nm, ":", summary[[nm]], "\n")
        }
      }
      invisible(self)
    }
  ),

  private = list(

    # Generate a unique mission ID
    generate_id = function() {
      paste0("mission_", format(Sys.time(), "%Y%m%d%H%M%S"), "_",
             paste(sample(c(letters, 0:9), 6, replace = TRUE), collapse = ""))
    },

    # Transition mission status and log
    transition = function(new_status, reason = NULL) {
      old_status <- self$status
      self$status <- new_status
      private$log_event("status_transition", list(
        from   = old_status,
        to     = new_status,
        reason = reason
      ))
    },

    # Append to audit log
    log_event = function(event_type, data = list()) {
      entry <- utils::modifyList(
        list(
          event = event_type,
          timestamp = Sys.time(),
          step_id = NULL,
          attempt = NULL,
          error_type = NULL,
          error_message = NULL,
          duration_ms = NULL
        ),
        data,
        keep.null = TRUE
      )
      self$audit_log <- c(self$audit_log, list(entry))
    },

    duration_ms = function(started_at) {
      if (is.null(started_at)) {
        return(NULL)
      }
      as.numeric(difftime(Sys.time(), started_at, units = "secs")) * 1000
    },

    classify_error_type = function(error_message) {
      if (is.null(error_message) || !nzchar(error_message)) {
        return(NULL)
      }
      lower <- tolower(error_message)
      if (grepl("timeout|time limit", lower)) {
        return("timeout")
      }
      if (grepl("permission denied|denied", lower)) {
        return("permission")
      }
      if (grepl("not found|unknown|missing", lower)) {
        return("missing_resource")
      }
      "execution_error"
    },

    build_planning_context = function(max_objects = 5L) {
      session <- self$session
      if (is.null(session) || !inherits(session, "ChatSession")) {
        return(list(text = "", payload = list()))
      }

      env_vars <- session$list_envir()
      env_vars <- env_vars[!grepl("^\\.", env_vars)]
      if (length(env_vars) == 0) {
        return(list(text = "", payload = list()))
      }

      env_vars <- env_vars[seq_len(min(length(env_vars), max_objects))]
      payloads <- lapply(env_vars, function(var_name) {
        obj <- get(var_name, envir = session$get_envir(), inherits = FALSE)
        payload <- describe_semantic_object(
          obj,
          name = var_name,
          session = session
        )
        payload$workflow_hint <- get_semantic_workflow_hint(
          obj,
          goal = self$goal,
          session = session
        )
        payload
      })

      names(payloads) <- env_vars
      payload_lines <- Map(format_mission_semantic_payload, env_vars, payloads)
      payload_lines <- Filter(function(x) nzchar(x %||% ""), payload_lines)
      workflow_hint_payload <- payloads[
        names(Filter(function(x) {
          is.list(x$workflow_hint) && length(x$workflow_hint) > 0
        }, payloads))
      ]
      if (length(payload_lines) == 0) {
        return(list(text = "", payload = payloads, workflow_hint_payload = workflow_hint_payload))
      }

      workflow_hint_text <- ""
      if (length(workflow_hint_payload) > 0) {
        workflow_hint_json <- jsonlite::toJSON(
          lapply(names(workflow_hint_payload), function(var_name) {
            list(
              object = var_name,
              workflow_hint = workflow_hint_payload[[var_name]]$workflow_hint
            )
          }),
          auto_unbox = TRUE,
          pretty = TRUE
        )
        workflow_hint_text <- paste0(
          "\n[SESSION WORKFLOW HINTS JSON]\n",
          workflow_hint_json,
          "\n"
        )
      }

      list(
        text = paste0(
          "[SESSION OBJECT SEMANTICS]\n",
          "Use the following live-session object semantics and workflow hints when planning.\n",
          paste(payload_lines, collapse = "\n\n"),
          "\n",
          workflow_hint_text
        ),
        payload = payloads,
        workflow_hint_payload = workflow_hint_payload,
        objects_with_hints = names(Filter(function(x) {
          is.list(x$workflow_hint) && length(x$workflow_hint) > 0
        }, payloads))
      )
    },

    # LLM-driven planning: decompose goal into MissionSteps
    plan = function(model) {
      if (is.null(self$default_executor)) {
        rlang::abort(paste0(
          "auto_plan=TRUE requires a default 'executor' at Mission creation. ",
          "Provide executor = your_agent or define steps manually."
        ))
      }

      planning_context <- private$build_planning_context()
      if (!is.null(self$session) && inherits(self$session, "ChatSession")) {
        self$session$set_memory(
          paste0("mission_", self$id, "_planning_semantics"),
          planning_context$payload %||% list()
        )
        self$session$set_memory(
          paste0("mission_", self$id, "_planning_hint_objects"),
          planning_context$objects_with_hints %||% character(0)
        )
        self$session$set_memory(
          paste0("mission_", self$id, "_planning_workflow_hints"),
          planning_context$workflow_hint_payload %||% list()
        )
      }
      private$log_event("planning_context_built", list(
        n_objects = length(planning_context$payload %||% list()),
        objects_with_hints = planning_context$objects_with_hints %||% character(0),
        n_workflow_hints = length(planning_context$workflow_hint_payload %||% list())
      ))

      plan_prompt <- paste0(
        "You are a mission planner. Break down the following goal into ",
        "concrete, sequential execution steps.\n\n",
        if (nzchar(planning_context$text %||% "")) paste0(planning_context$text, "\n") else "",
        "Goal: ", self$goal, "\n\n",
        "Return ONLY a valid JSON array (no markdown, no explanation). ",
        "Each element must have:\n",
        "  - \"id\": string (e.g. \"step_1\")\n",
        "  - \"description\": string (clear task for an AI agent)\n",
        "  - \"can_parallel\": boolean (true if this step is independent)\n\n",
        "Example output:\n",
        "[{\"id\":\"step_1\",\"description\":\"Load and inspect the data\",\"can_parallel\":false}]"
      )

      plan_result <- generate_text(
        model      = model,
        prompt     = plan_prompt,
        system     = "Return ONLY valid JSON. No markdown fences.",
        max_tokens = 2000
      )

      raw_text <- plan_result$text %||% ""

      # Strip markdown fences if present
      raw_text <- gsub("^```[a-z]*\\n?", "", raw_text)
      raw_text <- gsub("\\n?```$", "", raw_text)
      raw_text <- trimws(raw_text)

      steps_data <- tryCatch(
        jsonlite::fromJSON(raw_text, simplifyVector = FALSE),
        error = function(e) {
          rlang::abort(paste("LLM returned invalid JSON for planning:", conditionMessage(e)))
        }
      )

      if (!is.list(steps_data) || length(steps_data) == 0) {
        rlang::abort("LLM planning returned empty step list.")
      }

      self$steps <- lapply(seq_along(steps_data), function(i) {
        s <- steps_data[[i]]
        MissionStep$new(
          id          = s$id %||% paste0("step_", i),
          description = s$description %||% paste("Step", i),
          executor    = self$default_executor,
          parallel    = isTRUE(s$can_parallel)
        )
      })

      private$log_event("planning_done", list(n_steps = length(self$steps)))
    },

    # Execute all steps respecting depends_on and parallel groups
    execute_all_steps = function(model) {
      if (is.null(self$steps) || length(self$steps) == 0) return(invisible(NULL))

      # Topological execution: iterate until all steps are done or failed
      max_rounds <- length(self$steps) * 3  # safety limit
      round <- 0

      while (round < max_rounds) {
        round <- round + 1

        # Find steps ready to run (deps satisfied, not yet done/running)
        ready <- Filter(function(s) {
          if (s$status %in% c("done", "running", "failed")) return(FALSE)
          if (!is.null(s$depends_on) && length(s$depends_on) > 0) {
            dep_statuses <- sapply(s$depends_on, function(dep_id) {
              dep <- private$find_step(dep_id)
              if (is.null(dep)) "missing" else dep$status
            })
            if (!all(dep_statuses == "done")) return(FALSE)
          }
          TRUE
        }, self$steps)

        if (length(ready) == 0) {
          # No more runnable steps — either all done or blocked by failures
          break
        }

        # Separate parallel and serial steps
        parallel_steps <- Filter(function(s) isTRUE(s$parallel), ready)
        serial_steps   <- Filter(function(s) !isTRUE(s$parallel), ready)

        # Execute parallel group concurrently (using parallel::mclapply)
        if (length(parallel_steps) > 0) {
          private$execute_parallel_group(parallel_steps, model)
        }

        # Execute first serial step (one at a time)
        if (length(serial_steps) > 0) {
          private$execute_step_with_retry(serial_steps[[1]], model)
        }

        # If no progress possible, break
        all_terminal <- all(sapply(self$steps, function(s) s$status %in% c("done", "failed")))
        if (all_terminal) break
      }
    },

    # Find a step by ID
    find_step = function(step_id) {
      for (s in self$steps) {
        if (identical(s$id, step_id)) return(s)
      }
      NULL
    },

    # Execute a group of parallel steps using parallel::mclapply
    # Falls back to sequential on Windows (mclapply doesn't fork on Windows)
    execute_parallel_group = function(steps, model) {
      if (.Platform$OS.type == "windows" || length(steps) <= 1) {
        # Sequential fallback
        for (s in steps) {
          private$execute_step_with_retry(s, model)
        }
        return(invisible(NULL))
      }

      # Parallel execution: run each step in a forked process
      # Results are written back to session memory by the forked process,
      # so we use a temp-file rendezvous pattern for result passing
      step_ids <- sapply(steps, function(s) s$id)
      session_ref <- self$session

      results <- parallel::mclapply(steps, function(step) {
        tryCatch(
          {
            result <- step$run(session = session_ref, model = model)
            list(id = step$id, result = result, error = NULL)
          },
          error = function(e) {
            list(id = step$id, result = NULL, error = conditionMessage(e))
          }
        )
      }, mc.cores = min(length(steps), parallel::detectCores(logical = FALSE) %||% 2))

      # Apply results back to steps
      for (res in results) {
        step <- private$find_step(res$id)
        if (is.null(step)) next
        if (!is.null(res$result)) {
          step$status <- "done"
          step$result <- res$result
          self$session$set_memory(paste0("step_", step$id, "_result"), res$result)
          if (!is.null(self$hooks)) self$hooks$trigger_step_done(step, res$result, attempt = 1L)
          private$log_event("step_success", list(
            step_id = step$id,
            attempt = 1L
          ))
          private$log_event("step_done", list(step_id = step$id, attempt = 1L))
        } else {
          step$error_history <- c(step$error_history, list(list(
            attempt   = 1,
            error     = res$error,
            timestamp = Sys.time()
          )))
          step$status <- "failed"
          private$handle_stall(step, model)
        }
      }
    },

    # Execute a single step with full retry loop
    execute_step_with_retry = function(step, model) {
      attempt <- 0

      repeat {
        attempt <- attempt + 1
        step$retry_count <- attempt - 1
        step$status      <- "running"
        started_at <- Sys.time()

        if (!is.null(self$hooks)) self$hooks$trigger_step_start(step, attempt)
        private$log_event("step_start", list(step_id = step$id, attempt = attempt))

        # Inject failure history as context for the executor
        context <- NULL
        if (length(step$error_history) > 0) {
          last_errors <- tail(step$error_history, 2)
          context <- paste0(
            "PREVIOUS ATTEMPT(S) FAILED. Please approach this differently.\n\n",
            paste(sapply(last_errors, function(e) {
              paste0("Attempt ", e$attempt, ": ", e$error)
            }), collapse = "\n")
          )
        }

        # Optional timeout wrapper
        if (!is.null(step$timeout_secs)) {
          result_text <- tryCatch(
            {
              withCallingHandlers(
                setTimeLimit(elapsed = step$timeout_secs, transient = TRUE, {
                  step$run(session = self$session, model = model, context = context)
                }),
                error = function(e) {
                  if (grepl("reached elapsed time limit", conditionMessage(e))) {
                    stop(paste0("Step timeout after ", step$timeout_secs, "s"))
                  }
                  stop(e)
                }
              )
            },
            error = function(e) {
              conditionMessage(e)  # treat as failure
              NULL
            }
          )
        } else {
          result_text <- tryCatch(
            step$run(session = self$session, model = model, context = context),
            error = function(e) {
              step$error_history <- c(step$error_history, list(list(
                attempt   = attempt,
                error     = conditionMessage(e),
                timestamp = Sys.time()
              )))
              if (!is.null(self$hooks)) {
                self$hooks$trigger_step_failed(step, conditionMessage(e), attempt)
              }
              private$log_event("step_failure", list(
                step_id = step$id,
                attempt = attempt,
                error_type = private$classify_error_type(conditionMessage(e)),
                error_message = conditionMessage(e),
                duration_ms = private$duration_ms(started_at)
              ))
              private$log_event("step_failed", list(
                step_id = step$id,
                attempt = attempt,
                error_type = private$classify_error_type(conditionMessage(e)),
                error_message = conditionMessage(e),
                error = conditionMessage(e),
                duration_ms = private$duration_ms(started_at)
              ))
              NULL
            }
          )
        }

        if (!is.null(result_text)) {
          # Success
          step$status <- "done"
          step$result <- result_text
          self$session$set_memory(paste0("step_", step$id, "_result"), result_text)
          if (!is.null(self$hooks)) self$hooks$trigger_step_done(step, result_text, attempt = attempt)
          if (!is.null(self$hooks) && attempt > 1) {
            self$hooks$trigger_retry_success(step, result_text, attempt)
          }
          private$log_event("step_success", list(
            step_id = step$id,
            attempt = attempt,
            duration_ms = private$duration_ms(started_at)
          ))
          if (attempt > 1) {
            private$log_event("retry_success", list(
              step_id = step$id,
              attempt = attempt,
              duration_ms = private$duration_ms(started_at)
            ))
          }
          private$log_event("step_done", list(
            step_id = step$id,
            attempt = attempt,
            duration_ms = private$duration_ms(started_at)
          ))
          return(invisible(result_text))
        }

        # Failed: check retry budget
        if (attempt > step$max_retries) {
          step$status <- "failed"
          private$handle_stall(step, model)
          return(invisible(NULL))
        }

        # Retry with exponential backoff
        step$status <- "retrying"
        delay_secs <- min(2^(attempt - 1), 30)
        if (!is.null(self$hooks)) {
          self$hooks$trigger_retry_start(step, attempt + 1L)
        }
        private$log_event("retry_start", list(
          step_id = step$id,
          attempt = attempt + 1L
        ))
        Sys.sleep(delay_secs)
      }
    },

    # Handle stall according to stall_policy
    handle_stall = function(step, model) {
      policy <- self$stall_policy

      if (!is.null(self$hooks)) self$hooks$trigger_mission_stall(self, step)
      private$log_event("mission_stall", list(
        step_id = step$id,
        attempt = step$retry_count + 1L,
        error_type = "max_retries",
        error_message = "Mission stalled after max retries."
      ))

      action <- policy$on_max_retries %||% "escalate"

      if (action == "escalate") {
        # Call user-supplied escalate_fn if provided
        if (is.function(policy$escalate_fn)) {
          tryCatch(
            policy$escalate_fn(self, step),
            error = function(e) {
              warning("escalate_fn threw an error: ", conditionMessage(e))
            }
          )
        } else {
          # Interactive escalation: ask user what to do
          if (interactive()) {
            private$interactive_escalation(step, model)
          } else {
            message(sprintf(
              "[Mission STALL] Mission '%s' stalled at step '%s' after %d attempts.\n",
              self$id, step$id, step$retry_count + 1
            ))
          }
        }
      } else if (action == "skip") {
        step$status <- "done"  # mark as done so downstream steps can proceed
        step$result <- "[SKIPPED: max retries exceeded]"
        private$log_event("step_skipped", list(step_id = step$id))
      }
      # "abort" — leave status as "failed", execution loop will stop naturally
    },

    # Interactive escalation: ask user for guidance
    interactive_escalation = function(step, model) {
      cli::cli_alert_danger(
        "Mission stalled at step {.field {step$id}} after {step$retry_count + 1} attempts"
      )
      cli::cli_text("Step description: {.emph {step$description}}")

      # Show recent errors
      if (length(step$error_history) > 0) {
        cli::cli_h3("Recent errors:")
        recent_errors <- tail(step$error_history, 2)
        for (err in recent_errors) {
          cli::cli_text("  Attempt {err$attempt}: {.val {err$error}}")
        }
      }

      # Ask user what to do
      response <- utils::menu(
        c(
          "Let the agent explain the problem and suggest solutions",
          "Retry this step one more time",
          "Skip this step and continue",
          "Abort the mission"
        ),
        title = "\nWhat would you like to do?"
      )

      if (response == 1) {
        # Ask agent to explain
        cli::cli_alert_info("Asking agent to analyze the problem...")

        error_summary <- paste(
          sapply(step$error_history, function(e) {
            paste0("Attempt ", e$attempt, ": ", e$error)
          }),
          collapse = "\n"
        )

        explanation_prompt <- paste0(
          "I encountered repeated failures while trying to: ", step$description, "\n\n",
          "Error history:\n", error_summary, "\n\n",
          "Please analyze what went wrong and suggest:\n",
          "1. The root cause of the failures\n",
          "2. Alternative approaches to accomplish this task\n",
          "3. Whether this task is feasible or if we should skip it"
        )

        explanation <- tryCatch(
          {
            result <- generate_text(
              model = model,
              prompt = explanation_prompt,
              system = "You are a helpful assistant analyzing task failures. Be concise and actionable.",
              max_tokens = 1000
            )
            result$text
          },
          error = function(e) {
            paste("Failed to get explanation:", conditionMessage(e))
          }
        )

        cli::cli_h3("Agent's analysis:")
        cli::cli_text(explanation)

        # Ask again what to do
        response2 <- utils::menu(
          c(
            "Retry with this new understanding",
            "Skip this step",
            "Abort the mission"
          ),
          title = "\nBased on this analysis, what should we do?"
        )

        if (response2 == 1) {
          # Retry: reset retry count to allow one more attempt
          step$retry_count <- max(0, step$max_retries - 1)
          step$status <- "pending"
          # Add explanation to error history as context
          step$error_history <- c(step$error_history, list(list(
            attempt = step$retry_count + 1,
            error = paste0("[AGENT ANALYSIS] ", explanation),
            timestamp = Sys.time()
          )))
          private$log_event("escalation_retry", list(step_id = step$id))
        } else if (response2 == 2) {
          step$status <- "done"
          step$result <- "[SKIPPED: user decision after escalation]"
          private$log_event("escalation_skip", list(step_id = step$id))
        } else {
          step$status <- "failed"
          private$log_event("escalation_abort", list(step_id = step$id))
        }

      } else if (response == 2) {
        # Retry: reset retry count to allow one more attempt
        step$retry_count <- max(0, step$max_retries - 1)
        step$status <- "pending"
        private$log_event("escalation_retry", list(step_id = step$id))

      } else if (response == 3) {
        # Skip
        step$status <- "done"
        step$result <- "[SKIPPED: user decision]"
        private$log_event("escalation_skip", list(step_id = step$id))

      } else {
        # Abort
        step$status <- "failed"
        private$log_event("escalation_abort", list(step_id = step$id))
      }
    }
  )
)

# ---------------------------------------------------------------------------
# Factory functions
# ---------------------------------------------------------------------------

#' @title Create a Mission
#' @description
#' Factory function to create a new Mission object.
#' @param goal Natural language goal description.
#' @param steps Optional list of MissionStep objects. If NULL and auto_plan=TRUE,
#'   the LLM will decompose the goal into steps automatically.
#' @param model Default model ID (e.g., "anthropic:claude-opus-4-6").
#' @param executor Default executor (Agent, AgentTeam, Flow, or R function).
#'   Required when auto_plan = TRUE.
#' @param stall_policy Named list for failure recovery. See default_stall_policy().
#' @param hooks MissionHookHandler for lifecycle events.
#' @param session Optional SharedSession. Created automatically if NULL.
#' @param auto_plan If TRUE (default), use LLM to create steps when none are provided.
#' @return A Mission object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # Auto-planned mission
#'   agent <- create_agent("Analyst", "Analyzes data", model = "openai:gpt-4o")
#'   mission <- create_mission(
#'     goal     = "Load the iris dataset and summarize each species",
#'     executor = agent,
#'     model    = "openai:gpt-4o"
#'   )
#'   mission$run()
#'
#'   # Manual steps
#'   mission2 <- create_mission(
#'     goal  = "Data pipeline",
#'     steps = list(
#'       create_step("step_1", "Load CSV data", executor = agent),
#'       create_step("step_2", "Summarize statistics", executor = agent,
#'                   depends_on = "step_1")
#'     ),
#'     model = "openai:gpt-4o"
#'   )
#'   mission2$run()
#' }
#' }
create_mission <- function(goal,
                           steps = NULL,
                           model = NULL,
                           executor = NULL,
                           stall_policy = NULL,
                           hooks = NULL,
                           session = NULL,
                           auto_plan = TRUE) {
  Mission$new(
    goal         = goal,
    steps        = steps,
    model        = model,
    executor     = executor,
    stall_policy = stall_policy,
    hooks        = hooks,
    session      = session,
    auto_plan    = auto_plan
  )
}

#' @title Create a MissionStep
#' @description
#' Factory function to create a MissionStep.
#' @param id Unique step ID (e.g., "step_1").
#' @param description Natural language task description.
#' @param executor Agent, AgentTeam, Flow, or R function to execute the step.
#' @param max_retries Maximum retry attempts before stall escalation. Default 2.
#' @param timeout_secs Optional per-step timeout in seconds. Default NULL.
#' @param parallel If TRUE, this step may run in parallel with other parallel steps.
#' @param depends_on Character vector of prerequisite step IDs.
#' @return A MissionStep object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   step <- create_step(
#'     id          = "load_data",
#'     description = "Load the CSV file and return a summary",
#'     executor    = my_agent,
#'     max_retries = 3
#'   )
#' }
#' }
create_step <- function(id,
                        description,
                        executor = NULL,
                        max_retries = 2,
                        timeout_secs = NULL,
                        parallel = FALSE,
                        depends_on = NULL) {
  MissionStep$new(
    id           = id,
    description  = description,
    executor     = executor,
    max_retries  = max_retries,
    timeout_secs = timeout_secs,
    parallel     = parallel,
    depends_on   = depends_on
  )
}

# Null-coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
