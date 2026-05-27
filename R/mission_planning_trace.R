#' @title Mission Planning Trace Helpers
#' @description Runtime helpers for reading and summarizing the semantic
#' planning trace stored on a mission session.
#' @name mission_planning_trace
NULL

#' Get Mission Planning Trace
#'
#' Retrieve the semantic planning snapshot and hint-bearing objects stored for a mission.
#'
#' @param session A `ChatSession` or `SharedSession`.
#' @param mission_id Mission identifier.
#' @return A named list containing `semantics` and `hint_objects`.
#' @export
get_mission_planning_trace <- function(session, mission_id) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    rlang::abort("session must be a ChatSession or SharedSession.")
  }
  if (!is.character(mission_id) || length(mission_id) != 1 || !nzchar(mission_id)) {
    rlang::abort("mission_id must be a non-empty string.")
  }

  list(
    semantics = session$get_memory(paste0("mission_", mission_id, "_planning_semantics"), default = list()),
    hint_objects = session$get_memory(paste0("mission_", mission_id, "_planning_hint_objects"), default = character(0))
  )
}

#' Summarize Mission Planning Trace
#'
#' Summarize the semantic planning snapshot used during mission planning.
#'
#' @param session A `ChatSession` or `SharedSession`.
#' @param mission_id Mission identifier.
#' @return A named list containing counts and adapter/class usage summaries.
#' @export
summarize_mission_planning_trace <- function(session, mission_id) {
  trace <- get_mission_planning_trace(session, mission_id)
  semantics <- trace$semantics %||% list()

  adapters <- vapply(semantics, function(x) x$adapter %||% NA_character_, character(1))
  classes <- vapply(semantics, function(x) {
    identity <- x$identity %||% list()
    identity$primary_class %||% paste(identity$class %||% character(0), collapse = ", ")
  }, character(1))

  list(
    n_objects = length(semantics),
    objects = names(semantics),
    adapters = unname(adapters),
    classes = unname(classes),
    n_hint_objects = length(trace$hint_objects %||% character(0)),
    hint_objects = trace$hint_objects %||% character(0)
  )
}

#' Score Mission Planning Trace
#'
#' Convert a mission planning trace into simple benchmark-oriented metrics.
#'
#' @param session A `ChatSession` or `SharedSession`.
#' @param mission_id Mission identifier.
#' @return A named list of scalar metrics.
#' @export
score_mission_planning_trace <- function(session, mission_id) {
  summary <- summarize_mission_planning_trace(session, mission_id)
  n_objects <- summary$n_objects %||% 0L
  n_hint_objects <- summary$n_hint_objects %||% 0L
  adapter_count <- length(unique(stats::na.omit(summary$adapters %||% character(0))))
  class_count <- length(unique(stats::na.omit(summary$classes %||% character(0))))

  list(
    n_objects = n_objects,
    n_hint_objects = n_hint_objects,
    hint_coverage = if (n_objects > 0) n_hint_objects / n_objects else 0,
    adapter_diversity = adapter_count,
    class_diversity = class_count,
    semantic_context_present = n_objects > 0,
    workflow_hint_present = n_hint_objects > 0
  )
}
