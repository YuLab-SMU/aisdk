#' @title Interactive Prompt Primitives
#' @description
#' Small terminal prompt helpers (menu, confirm, free-text input) shared by
#' the model setup flow in this package, the aisdk.console front end, and
#' companion packages that accept injectable prompt hooks (e.g.
#' aisdk.channels). They render with cli when available and degrade to plain
#' `readline()` behaviour otherwise.
#' @name utils_prompts
NULL

#' @title Console Interactive Menu
#' @description
#' Present a numbered list of choices and return the user's selection.
#' Styled with cli to match the console chat interface. Similar to
#' \code{utils::menu()} but with cli formatting.
#'
#' @param title The question or prompt to display.
#' @param choices Character vector of options to present.
#' @return The index of the selected choice (integer), or \code{NULL} if
#'   cancelled (user enters 'q' or empty input).
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   selection <- console_menu("Which database?", c("PostgreSQL", "SQLite", "DuckDB"))
#' }
#' }
console_menu <- function(title, choices) {
  if (!interactive()) return(NULL)
  cli::cli_text("")
  cli::cli_alert_info(title)
  for (i in seq_along(choices)) {
    cli::cli_text("  {i}: {choices[[i]]}")
  }
  cli::cli_text("")
  repeat {
    # Flush before blocking: RStudio Server can hold buffered output while
    # readline() waits, which makes the prompt look frozen.
    utils::flush.console()
    response <- readline("Selection: ")
    response <- trimws(response)
    if (!nzchar(response) || tolower(response) == "q") return(NULL)
    num <- suppressWarnings(as.integer(response))
    if (!is.na(num) && num >= 1 && num <= length(choices)) {
      return(num)
    }
    cli::cli_alert_warning("Enter a number between 1 and {length(choices)}, or press Enter to cancel.")
  }
}

#' @title Console Confirmation Prompt
#' @description
#' Ask a yes/no question with numbered choices. Returns \code{TRUE} for yes,
#' \code{FALSE} for no, or \code{NULL} if cancelled.
#'
#' @param question The question to display.
#' @return \code{TRUE} if user selects Yes, \code{FALSE} for No, \code{NULL}
#'   if cancelled.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   if (isTRUE(console_confirm("Overwrite existing file?"))) {
#'     message("Overwriting...")
#'   }
#' }
#' }
console_confirm <- function(question) {
  if (!interactive()) return(NULL)
  selection <- console_menu(question, c("Yes", "No"))
  if (is.null(selection)) return(NULL)
  selection == 1L
}

#' @title Console Text Input
#' @description
#' Prompt the user for free-text input with optional default value.
#'
#' @param prompt The prompt message to display.
#' @param default Optional default value shown in brackets. Returned if user
#'   presses Enter without typing.
#' @return The user's input string, \code{default} if empty input and default
#'   is set, or \code{NULL} if empty input with no default.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   name <- console_input("Project name", default = "my-project")
#'   api_key <- console_input("API key")
#' }
#' }
console_input <- function(prompt, default = NULL) {
  if (!interactive()) return(default)
  hint <- if (!is.null(default)) paste0(" [", default, "]") else ""
  cli::cli_text("")
  utils::flush.console()
  response <- readline(paste0("  ", prompt, hint, ": "))
  response <- trimws(response)
  if (!nzchar(response) && !is.null(default)) return(default)
  if (!nzchar(response)) return(NULL)
  response
}
