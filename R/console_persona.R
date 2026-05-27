#' @title Console Persona Helpers
#' @description
#' Internal helpers for session-scoped console personas, including defaults,
#' skill-backed personas, and manual overrides.
#' @name console_persona
#' @keywords internal
NULL

#' @keywords internal
console_default_persona_label <- function() {
  "default"
}

#' @keywords internal
console_default_persona_prompt <- function() {
  paste(
    "You have a default console persona.",
    "Be direct, calm, technically serious, and useful.",
    "Avoid empty reassurance and avoid exaggerated friendliness.",
    "When the user is distressed, stay steady, practical, and humane without becoming melodramatic.",
    "When a more specific active persona exists, it overrides this default persona."
  )
}

#' @keywords internal
console_new_persona_state <- function() {
  list(
    default = list(
      label = console_default_persona_label(),
      prompt = console_default_persona_prompt(),
      notes = character(0)
    ),
    active = list(
      label = console_default_persona_label(),
      prompt = console_default_persona_prompt(),
      source = "default",
      notes = character(0),
      locked = FALSE,
      skill_name = NULL
    )
  )
}

#' @keywords internal
console_get_persona_state <- function(session, initialize = TRUE) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    return(NULL)
  }

  envir <- session$get_envir()
  state <- envir$.console_persona_state %||% NULL
  if (is.null(state) && isTRUE(initialize)) {
    state <- console_new_persona_state()
    assign(".console_persona_state", state, envir = envir)
  }
  state
}

#' @keywords internal
console_save_persona_state <- function(session, state) {
  if (!is.null(session) && inherits(session, "ChatSession")) {
    assign(".console_persona_state", state, envir = session$get_envir())
  }
  invisible(state)
}

#' @keywords internal
console_current_persona <- function(session) {
  state <- console_get_persona_state(session)
  if (is.null(state)) {
    return(NULL)
  }
  state$active
}

#' @keywords internal
console_persona_status_label <- function(session) {
  persona <- console_current_persona(session)
  if (is.null(persona)) {
    return(console_default_persona_label())
  }

  label <- persona$label %||% console_default_persona_label()
  source <- persona$source %||% "default"
  if (identical(source, "default")) {
    return(label)
  }
  paste0(label, ":", source)
}

#' @keywords internal
console_compose_persona_prompt <- function(label, prompt, notes = character(0), source = "manual") {
  parts <- c(
    "[persona_begin]",
    paste0("Active persona: ", label %||% "persona"),
    paste0("Persona source: ", source %||% "manual"),
    "",
    prompt %||% ""
  )

  notes <- notes %||% character(0)
  notes <- trimws(notes)
  notes <- notes[nzchar(notes)]
  if (length(notes) > 0) {
    parts <- c(
      parts,
      "",
      "Persona evolution notes:",
      paste0("- ", notes)
    )
  }

  paste(c(parts, "[persona_end]"), collapse = "\n")
}

#' @keywords internal
console_set_manual_persona <- function(session, prompt, label = "custom", locked = TRUE) {
  state <- console_get_persona_state(session)
  state$active <- list(
    label = label %||% "custom",
    prompt = trimws(prompt %||% ""),
    source = "manual",
    notes = character(0),
    locked = isTRUE(locked),
    skill_name = NULL
  )
  console_save_persona_state(session, state)
}

#' @keywords internal
console_reset_persona <- function(session) {
  state <- console_get_persona_state(session)
  state$active <- list(
    label = state$default$label,
    prompt = state$default$prompt,
    source = "default",
    notes = state$default$notes %||% character(0),
    locked = FALSE,
    skill_name = NULL
  )
  console_save_persona_state(session, state)
}

#' @keywords internal
console_evolve_persona <- function(session, note) {
  state <- console_get_persona_state(session)
  note <- trimws(note %||% "")
  if (!nzchar(note)) {
    return(state$active)
  }

  if (identical(state$active$source %||% "default", "default")) {
    state$default$notes <- unique(c(state$default$notes %||% character(0), note))
    state$active$notes <- state$default$notes
  } else {
    state$active$notes <- unique(c(state$active$notes %||% character(0), note))
  }

  console_save_persona_state(session, state)
  state$active
}

#' @keywords internal
console_skill_persona_metadata <- function(skill) {
  if (is.null(skill) || !inherits(skill, "Skill")) {
    return(NULL)
  }

  persona_path <- file.path(skill$path, "persona.md")
  if (!file.exists(persona_path)) {
    return(NULL)
  }

  prompt <- paste(readLines(persona_path, warn = FALSE), collapse = "\n")
  if (!nzchar(trimws(prompt))) {
    return(NULL)
  }

  label <- skill$name
  meta_path <- file.path(skill$path, "meta.json")
  if (file.exists(meta_path)) {
    meta <- tryCatch(jsonlite::fromJSON(meta_path, simplifyVector = FALSE), error = function(e) NULL)
    meta_name <- meta$name %||% NULL
    if (!is.null(meta_name) && nzchar(trimws(meta_name))) {
      label <- trimws(meta_name)
    }
  }

  list(
    label = label,
    prompt = prompt,
    skill_name = skill$name
  )
}

#' @keywords internal
console_activate_skill_persona <- function(session, skill) {
  persona <- console_skill_persona_metadata(skill)
  if (is.null(persona)) {
    return(NULL)
  }

  state <- console_get_persona_state(session)
  state$active <- list(
    label = persona$label,
    prompt = persona$prompt,
    source = "skill",
    notes = character(0),
    locked = FALSE,
    skill_name = persona$skill_name
  )
  console_save_persona_state(session, state)
  state$active
}

#' @keywords internal
console_lock_skill_persona <- function(session, skill) {
  persona <- console_skill_persona_metadata(skill)
  if (is.null(persona)) {
    return(NULL)
  }

  state <- console_get_persona_state(session)
  state$active <- list(
    label = persona$label,
    prompt = persona$prompt,
    source = "skill",
    notes = character(0),
    locked = TRUE,
    skill_name = persona$skill_name
  )
  console_save_persona_state(session, state)
  state$active
}

#' @keywords internal
console_resolve_active_persona <- function(session, matched_skill_names = character(0)) {
  state <- console_get_persona_state(session)
  active <- state$active

  if (isTRUE(active$locked) && !identical(active$source %||% "default", "default")) {
    return(active)
  }

  registry <- console_get_skill_registry(session)
  if (!is.null(registry) && length(matched_skill_names) > 0) {
    for (skill_name in matched_skill_names) {
      skill <- registry$get_skill(skill_name)
      if (is.null(skill)) {
        next
      }
      persona <- console_activate_skill_persona(session, skill)
      if (!is.null(persona)) {
        return(persona)
      }
    }
  }

  if (identical(active$source %||% "default", "skill") && !isTRUE(active$locked)) {
    return(active)
  }

  if (!identical(active$source %||% "default", "default")) {
    return(active)
  }

  state$active$notes <- state$default$notes %||% character(0)
  console_save_persona_state(session, state)
  state$active
}

#' @keywords internal
console_build_persona_section <- function(session, matched_skill_names = character(0)) {
  persona <- console_resolve_active_persona(session, matched_skill_names = matched_skill_names)
  if (is.null(persona)) {
    return(NULL)
  }

  if (identical(persona$source %||% "default", "default") &&
      length(persona$notes %||% character(0)) == 0) {
    return(NULL)
  }

  console_compose_persona_prompt(
    label = persona$label %||% console_default_persona_label(),
    prompt = persona$prompt %||% console_default_persona_prompt(),
    notes = persona$notes %||% character(0),
    source = persona$source %||% "default"
  )
}
