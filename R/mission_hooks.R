#' @title Mission Hook System
#' @description
#' Mission-level event hooks for observability and intervention.
#' Provides a higher-level hook system above the agent-level HookHandler,
#' enabling monitoring and control of the entire Mission lifecycle.
#' @name mission_hooks
NULL

#' @title MissionHookHandler Class
#' @description
#' R6 class to manage Mission-level lifecycle hooks.
#' Supported events span the full Mission state machine:
#' planning -> step execution -> completion / stall / escalation.
#' @export
MissionHookHandler <- R6::R6Class(
  "MissionHookHandler",
  public = list(
    #' @field hooks Named list of hook functions.
    hooks = NULL,

    #' @description Initialize a MissionHookHandler.
    #' @param hooks_list Named list of hook functions. Supported hooks:
    #' \itemize{
    #'   \item on_mission_start(mission) - Called when a Mission begins running.
    #'   \item on_planner_start(mission) - Called immediately before auto-planning starts.
    #'   \item on_planner_done(mission) - Called after auto-planning produces steps.
    #'   \item on_mission_planned(mission) - Called after LLM planning produces steps.
    #'   \item on_step_start(step, attempt) - Called before each step attempt.
    #'   \item on_retry_start(step, attempt) - Called before a retry attempt begins.
    #'   \item on_retry_success(step, result, attempt) - Called when a retried step succeeds.
    #'   \item on_step_success(step, result, attempt) - Alias hook for step success.
    #'   \item on_step_done(step, result) - Called when a step succeeds.
    #'   \item on_step_failure(step, error, attempt) - Alias hook for step failure.
    #'   \item on_step_failed(step, error, attempt) - Called on each step failure.
    #'   \item on_mission_stall(mission, step) - Called when a step exceeds max_retries.
    #'   \item on_mission_end(mission) - Alias hook for mission completion.
    #'   \item on_mission_done(mission) - Called when the Mission completes (succeeded or failed).
    #' }
    initialize = function(hooks_list = list()) {
      self$hooks <- hooks_list
    },

    #' @description Trigger on_mission_start.
    #' @param mission The Mission object.
    trigger_mission_start = function(mission) {
      private$trigger("on_mission_start", mission)
    },

    #' @description Trigger on_planner_start.
    #' @param mission The Mission object.
    trigger_planner_start = function(mission) {
      private$trigger("on_planner_start", mission)
    },

    #' @description Trigger on_planner_done.
    #' @param mission The Mission object (steps are now populated).
    trigger_planner_done = function(mission) {
      private$trigger("on_planner_done", mission)
    },

    #' @description Trigger on_mission_planned.
    #' @param mission The Mission object (steps are now populated).
    trigger_mission_planned = function(mission) {
      private$trigger("on_mission_planned", mission)
    },

    #' @description Trigger on_step_start.
    #' @param step The MissionStep object.
    #' @param attempt Integer attempt number (1 = first try).
    trigger_step_start = function(step, attempt) {
      private$trigger("on_step_start", step, attempt)
    },

    #' @description Trigger on_retry_start.
    #' @param step The MissionStep object.
    #' @param attempt Integer attempt number.
    trigger_retry_start = function(step, attempt) {
      private$trigger("on_retry_start", step, attempt)
    },

    #' @description Trigger on_retry_success.
    #' @param step The MissionStep object.
    #' @param result The text result from the executor.
    #' @param attempt Integer attempt number.
    trigger_retry_success = function(step, result, attempt) {
      private$trigger("on_retry_success", step, result, attempt)
    },

    #' @description Trigger on_step_done.
    #' @param step The MissionStep object.
    #' @param result The text result from the executor.
    #' @param attempt Integer attempt number.
    trigger_step_done = function(step, result, attempt = NULL) {
      private$trigger("on_step_success", step, result, attempt)
      private$trigger("on_step_done", step, result)
    },

    #' @description Trigger on_step_failed.
    #' @param step The MissionStep object.
    #' @param error The error message string.
    #' @param attempt Integer attempt number.
    trigger_step_failed = function(step, error, attempt) {
      private$trigger("on_step_failure", step, error, attempt)
      private$trigger("on_step_failed", step, error, attempt)
    },

    #' @description Trigger on_mission_stall.
    #' @param mission The Mission object.
    #' @param step The step that caused the stall.
    trigger_mission_stall = function(mission, step) {
      private$trigger("on_mission_stall", mission, step)
    },

    #' @description Trigger on_mission_done.
    #' @param mission The completed Mission object.
    trigger_mission_done = function(mission) {
      private$trigger("on_mission_end", mission)
      private$trigger("on_mission_done", mission)
    }
  ),
  private = list(
    trigger = function(hook_name, ...) {
      fn <- self$hooks[[hook_name]]
      if (!is.null(fn) && is.function(fn)) {
        tryCatch(
          fn(...),
          error = function(e) {
            warning(sprintf("Mission hook '%s' threw an error: %s", hook_name, conditionMessage(e)))
          }
        )
      }
    }
  )
)

#' @title Create Mission Hooks
#' @description
#' Factory function to create a MissionHookHandler from named hook functions.
#' @param on_mission_start Optional function(mission) called when a Mission begins.
#' @param on_planner_start Optional function(mission) called before auto-planning starts.
#' @param on_planner_done Optional function(mission) called after auto-planning produces steps.
#' @param on_mission_planned Optional function(mission) called after LLM planning.
#' @param on_step_start Optional function(step, attempt) called before each step attempt.
#' @param on_retry_start Optional function(step, attempt) called before a retry attempt begins.
#' @param on_retry_success Optional function(step, result, attempt) called when a retry succeeds.
#' @param on_step_success Optional function(step, result, attempt) called on step success.
#' @param on_step_done Optional function(step, result) called on step success.
#' @param on_step_failure Optional function(step, error, attempt) called on step failure.
#' @param on_step_failed Optional function(step, error, attempt) called on step failure.
#' @param on_mission_stall Optional function(mission, step) called on stall detection.
#' @param on_mission_end Optional function(mission) called when the Mission completes.
#' @param on_mission_done Optional function(mission) called on Mission completion.
#' @return A MissionHookHandler object.
#' @export
#' @examples
#' \donttest{
#' hooks <- create_mission_hooks(
#'   on_step_done = function(step, result) {
#'     message("Completed: ", step$description)
#'   },
#'   on_mission_stall = function(mission, step) {
#'     message("STALL detected at step: ", step$id)
#'   }
#' )
#' }
create_mission_hooks <- function(
    on_mission_start = NULL,
    on_planner_start = NULL,
    on_planner_done = NULL,
    on_mission_planned = NULL,
    on_step_start = NULL,
    on_retry_start = NULL,
    on_retry_success = NULL,
    on_step_success = NULL,
    on_step_done = NULL,
    on_step_failure = NULL,
    on_step_failed = NULL,
    on_mission_stall = NULL,
    on_mission_end = NULL,
    on_mission_done = NULL) {
  hooks_list <- list(
    on_mission_start = on_mission_start,
    on_planner_start = on_planner_start,
    on_planner_done = on_planner_done,
    on_mission_planned = on_mission_planned,
    on_step_start = on_step_start,
    on_retry_start = on_retry_start,
    on_retry_success = on_retry_success,
    on_step_success = on_step_success,
    on_step_done = on_step_done,
    on_step_failure = on_step_failure,
    on_step_failed = on_step_failed,
    on_mission_stall = on_mission_stall,
    on_mission_end = on_mission_end,
    on_mission_done = on_mission_done
  )
  # Remove NULL entries
  hooks_list <- Filter(Negate(is.null), hooks_list)
  MissionHookHandler$new(hooks_list)
}
