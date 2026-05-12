#' @title AI Chat UI
#' @description
#' Creates a modern, streaming-ready chat interface for Shiny applications.
#'
#' @param id The namespace ID for the module.
#' @param height Height of the chat window (e.g. "400px").
#' @param backend UI backend to use. `"aisdk"` uses the built-in dependency-light
#'   interface. `"shinychat"` uses `shinychat::chat_ui()` when the optional
#'   `shinychat` package is installed.
#' @param models Optional character vector or list of selectable model IDs.
#' @param attachments Whether to show the built-in image attachment controls.
#' @param controls Whether to show runtime controls such as model selection and
#'   cancel/retry buttons when available.
#' @return A Shiny UI definition.
#' @export
aiChatUI <- function(id,
                     height = "500px",
                     backend = c("aisdk", "shinychat"),
                     models = NULL,
                     attachments = TRUE,
                     controls = TRUE) {
  backend <- match.arg(backend)

  # The built-in backend intentionally avoids bslib components so it works in
  # Shiny's default Bootstrap 3 pages.
  rlang::check_installed(c("shiny", "htmltools", "commonmark"))

  if (identical(backend, "shinychat")) {
    rlang::check_installed(
      "shinychat",
      reason = "to use `aiChatUI(backend = \"shinychat\")`. Install it or use `backend = \"aisdk\"`."
    )
    ns <- shiny::NS(id)
    return(shinychat::chat_ui(
      ns(shinychat_control_id()),
      height = height,
      messages = list(list(
        role = "assistant",
        content = "Hello! How can I help you today?"
      ))
    ))
  }

  ns <- shiny::NS(id)
  model_choices <- normalize_shiny_model_choices(models)

  htmltools::tagList(
    aisdk_chat_dependency(),
    shiny::div(
      id = ns("chat_root"),
      class = "aisdk-chat-root",
      `data-chat-id` = ns("chat_root"),
      `data-attachments` = if (isTRUE(attachments)) "true" else "false",
      style = sprintf("--aisdk-chat-height: %s;", html_escape(height)),
      if (isTRUE(controls)) {
        shiny::div(
          class = "aisdk-chat-toolbar",
          shiny::selectizeInput(
            ns("model"),
            label = NULL,
            choices = model_choices,
            selected = if (length(model_choices) > 0) model_choices[[1]] else NULL,
            width = "100%",
            options = list(
              create = TRUE,
              persist = FALSE,
              placeholder = "model id"
            )
          ),
          shiny::div(
            class = "aisdk-chat-actions",
            shiny::actionButton(ns("model_apply"), label = shiny::icon("refresh"), class = "aisdk-chat-icon", title = "Switch model"),
            shiny::actionButton(ns("retry"), label = shiny::icon("redo"), class = "aisdk-chat-icon", title = "Retry"),
            shiny::actionButton(ns("cancel"), label = shiny::icon("stop"), class = "aisdk-chat-icon", title = "Cancel")
          )
        )
      },
      shiny::div(
        id = ns("chat_messages"),
        class = "aisdk-chat-messages",
        `aria-live` = "polite",
        shiny::tags$article(
          class = "aisdk-message assistant",
          shiny::HTML(commonmark::markdown_html("Hello! How can I help you today?"))
        )
      ),
      shiny::actionButton(ns("jump_bottom"), label = shiny::icon("arrow-down"), class = "aisdk-jump-bottom", title = "Jump to bottom"),
      shiny::div(
        class = "aisdk-chat-input",
        if (isTRUE(attachments)) {
          shiny::div(
            class = "aisdk-attachment-row",
            shiny::fileInput(
              ns("attachments"),
              label = NULL,
              accept = c("image/png", "image/jpeg", "image/gif", "image/webp"),
              multiple = TRUE,
              buttonLabel = shiny::icon("image"),
              placeholder = "Images"
            ),
            shiny::div(class = "aisdk-attachment-previews")
          )
        },
        shiny::div(
          class = "aisdk-chat-input-row",
          shiny::div(
            class = "aisdk-chat-input-field",
            shiny::textAreaInput(
              ns("user_input"),
              label = NULL,
              placeholder = "Type your message...",
              rows = 1,
              resize = "none"
            )
          ),
          shiny::actionButton(
            ns("send"),
            label = shiny::icon("paper-plane"),
            class = "btn-primary aisdk-chat-send",
            title = "Send"
          )
        )
      )
    )
  )
}

#' @keywords internal
normalize_shiny_model_choices <- function(models = NULL) {
  if (is.null(models)) {
    return(character(0))
  }
  if (is.character(models)) {
    out <- models
    if (is.null(names(out))) {
      names(out) <- out
    } else {
      empty <- !nzchar(names(out))
      names(out)[empty] <- out[empty]
    }
    return(out)
  }
  if (is.list(models)) {
    ids <- vapply(models, function(model) {
      if (is.character(model)) {
        model[[1]]
      } else if (inherits(model, "LanguageModelV1")) {
        paste0(model$provider, ":", model$model_id)
      } else {
        model$id %||% model$value %||% model$model %||% ""
      }
    }, character(1))
    labels <- vapply(seq_along(models), function(i) {
      model <- models[[i]]
      if (!is.null(names(models)) && nzchar(names(models)[[i]])) {
        names(models)[[i]]
      } else if (is.list(model) && !inherits(model, "LanguageModelV1")) {
        model$label %||% ids[[i]]
      } else {
        ids[[i]]
      }
    }, character(1))
    keep <- nzchar(ids)
    ids <- ids[keep]
    labels <- labels[keep]
    stats::setNames(ids, labels)
  } else {
    rlang::abort("`models` must be NULL, a character vector, or a list.")
  }
}

#' @keywords internal
aisdk_chat_dependency <- function() {
  www_dir <- system.file("www", package = "aisdk")
  asset_paths <- file.path(www_dir, c("aisdk-chat.css", "aisdk-chat.js"))
  asset_mtime <- suppressWarnings(max(file.info(asset_paths)$mtime, na.rm = TRUE))
  version <- as.character(utils::packageVersion("aisdk"))
  if (is.finite(as.numeric(asset_mtime))) {
    version <- paste0(version, ".", as.integer(as.numeric(asset_mtime)))
  }

  htmltools::htmlDependency(
    name = "aisdk-chat",
    version = version,
    src = "www",
    package = "aisdk",
    stylesheet = "aisdk-chat.css",
    script = "aisdk-chat.js"
  )
}
