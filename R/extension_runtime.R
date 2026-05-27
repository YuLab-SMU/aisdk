#' Console Extension Runtime
#'
#' Lightweight extension registry for console slash commands and optional tools.
#'
#' @name extension_runtime
NULL

#' @keywords internal
new_console_extension_runtime <- function(session = NULL, startup_dir = getwd(), extensions = "auto") {
  runtime <- new.env(parent = emptyenv())
  runtime$session <- session
  runtime$startup_dir <- normalizePath(startup_dir, winslash = "/", mustWork = FALSE)
  runtime$extensions <- list()
  runtime$commands <- list()
  runtime$tools <- list()
  runtime$enabled_tools <- character(0)
  runtime$extensions_setting <- extensions
  runtime
}

#' @keywords internal
console_extension_register <- function(runtime,
                                       id,
                                       commands = list(),
                                       tools = list(),
                                       state = list()) {
  if (is.null(runtime) || !is.environment(runtime)) {
    return(invisible(NULL))
  }
  id <- trimws(id %||% "")
  if (!nzchar(id)) {
    rlang::abort("Extension id must be non-empty.")
  }
  runtime$extensions[[id]] <- list(id = id, commands = names(commands), tools = names(tools), state = state %||% list())
  for (name in names(commands %||% list())) {
    runtime$commands[[name]] <- list(extension_id = id, handler = commands[[name]])
  }
  for (name in names(tools %||% list())) {
    runtime$tools[[name]] <- tools[[name]]
  }
  invisible(runtime$extensions[[id]])
}

#' @keywords internal
console_extension_builtin_specs <- function() {
  list(
    `r-console` = list(commands = c("r", "objects")),
    skills = list(commands = c("skills", "skill")),
    image = list(commands = c("image")),
    feishu = list(commands = c("feishu"))
  )
}

#' @keywords internal
console_register_builtin_extensions <- function(runtime) {
  specs <- console_extension_builtin_specs()
  for (id in names(specs)) {
    console_extension_register(
      runtime,
      id = id,
      commands = stats::setNames(vector("list", length(specs[[id]]$commands)), specs[[id]]$commands),
      tools = list(),
      state = list(builtin = TRUE)
    )
  }
  invisible(runtime)
}

#' @keywords internal
console_extension_load_file <- function(runtime, path) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  register_extension <- function(id, commands = list(), tools = list(), state = list()) {
    console_extension_register(runtime, id = id, commands = commands, tools = tools, state = state)
  }
  env <- new.env(parent = parent.frame())
  env$register_extension <- register_extension
  sys.source(path, envir = env, keep.source = FALSE)
  TRUE
}

#' @keywords internal
console_extension_paths <- function(startup_dir = getwd()) {
  root <- file.path(startup_dir, ".aisdk", "extensions")
  if (!dir.exists(root)) {
    return(character(0))
  }
  dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  file.path(dirs, "extension.R")
}

#' @keywords internal
console_extension_runtime_load <- function(session = NULL, startup_dir = getwd(), extensions = "auto") {
  runtime <- new_console_extension_runtime(session = session, startup_dir = startup_dir, extensions = extensions)
  console_register_builtin_extensions(runtime)
  for (path in console_extension_paths(startup_dir)) {
    tryCatch(console_extension_load_file(runtime, path), error = function(e) NULL)
  }
  if (!is.null(session) && inherits(session, "ChatSession")) {
    session$set_metadata("console_extensions", names(runtime$extensions))
    assign(".console_extension_runtime", runtime, envir = session$get_envir())
  }
  runtime
}

#' @keywords internal
console_extension_tools <- function(runtime, enabled = NULL) {
  if (is.null(runtime) || !is.environment(runtime)) {
    return(list())
  }
  enabled <- enabled %||% runtime$enabled_tools %||% character(0)
  if (!length(enabled)) {
    return(list())
  }
  runtime$tools[intersect(names(runtime$tools), enabled)]
}

#' @keywords internal
console_extension_summary_lines <- function(runtime) {
  if (is.null(runtime) || !is.environment(runtime) || !length(runtime$extensions)) {
    return("No extensions loaded.")
  }
  vapply(runtime$extensions, function(ext) {
    sprintf(
      "%s | commands: %s | tools: %s",
      ext$id,
      paste(ext$commands %||% character(0), collapse = ", "),
      paste(ext$tools %||% character(0), collapse = ", ")
    )
  }, character(1))
}
