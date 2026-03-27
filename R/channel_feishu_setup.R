#' @title Feishu Channel Setup Wizard
#' @description
#' Interactive helper for configuring the Feishu channel runtime without
#' requiring users to manually edit environment variables or read demo scripts.
#' The wizard focuses on webhook mode by default and can optionally save the
#' collected configuration into a `.Renviron` file.
#' @param prompt_hooks Optional list of prompt hooks with any of:
#'   `menu`, `input`, `confirm`, `save`.
#' @param renviron_path Optional path to the `.Renviron` file used when saving.
#'   If `NULL`, the wizard will ask before writing.
#' @param current_model Optional current model id. When provided, this is reused
#'   as the default `FEISHU_MODEL` so the user does not need to choose a model again.
#' @param workdir Working directory that the Feishu bot should use.
#' @param session_root Session store root for the Feishu runtime.
#' @param host Local bind host for the webhook server.
#' @param port Local bind port for the webhook server.
#' @param path Local webhook path.
#' @return A list describing the chosen configuration and next-step commands.
#' @export
setup_feishu_channel <- function(prompt_hooks = list(),
                                 renviron_path = NULL,
                                 current_model = NULL,
                                 workdir = normalizePath(getwd(), winslash = "/", mustWork = FALSE),
                                 session_root = file.path(workdir, ".aisdk", "feishu"),
                                 host = "127.0.0.1",
                                 port = 8788L,
                                 path = "/feishu/webhook") {
  menu_fn <- prompt_hooks$menu %||% console_menu
  input_fn <- prompt_hooks$input %||% console_input
  confirm_fn <- prompt_hooks$confirm %||% console_confirm
  save_fn <- prompt_hooks$save %||% update_renviron

  mode <- "webhook"

  app_id <- input_fn("FEISHU_APP_ID")
  app_secret <- input_fn("FEISHU_APP_SECRET")

  if (!nzchar(app_id %||% "") || !nzchar(app_secret %||% "")) {
    return(list(
      cancelled = TRUE,
      summary = "Feishu setup cancelled because FEISHU_APP_ID or FEISHU_APP_SECRET was not provided."
    ))
  }

  model_id <- current_model %||% Sys.getenv("FEISHU_MODEL", "")
  if (!nzchar(model_id)) {
    model_id <- Sys.getenv("OPENAI_MODEL_ID", "")
  }
  if (!nzchar(model_id)) {
    model_id <- "openai:gpt-4o-mini"
  }

  verification_token <- ""
  encrypt_key <- ""
  chosen_workdir <- workdir
  chosen_session_root <- session_root
  chosen_host <- host
  chosen_port <- as.character(port)
  chosen_path <- path
  chosen_sandbox <- "strict"

  advanced_config <- isTRUE(confirm_fn("Do you want to review advanced Feishu settings?"))
  if (isTRUE(advanced_config)) {
    verification_token <- input_fn("FEISHU_VERIFICATION_TOKEN (optional)", default = "") %||% ""
    encrypt_key <- input_fn("FEISHU_ENCRYPT_KEY (optional)", default = "") %||% ""
    chosen_workdir <- input_fn("FEISHU_WORKDIR", default = workdir) %||% workdir
    chosen_session_root <- input_fn("FEISHU_SESSION_ROOT", default = session_root) %||% session_root
    chosen_host <- input_fn("FEISHU_HOST", default = host) %||% host
    chosen_port <- input_fn("FEISHU_PORT", default = as.character(port)) %||% as.character(port)
    chosen_path <- input_fn("FEISHU_PATH", default = path) %||% path
    chosen_sandbox <- input_fn("FEISHU_SANDBOX_MODE", default = "strict") %||% "strict"
  }

  updates <- c(
    list(
      FEISHU_APP_ID = app_id,
      FEISHU_APP_SECRET = app_secret,
      FEISHU_MODEL = model_id,
      FEISHU_WORKDIR = chosen_workdir,
      FEISHU_SESSION_ROOT = chosen_session_root,
      FEISHU_HOST = chosen_host,
      FEISHU_PORT = chosen_port,
      FEISHU_PATH = chosen_path,
      FEISHU_SANDBOX_MODE = chosen_sandbox
    ),
    if (nzchar(verification_token %||% "")) list(FEISHU_VERIFICATION_TOKEN = verification_token) else list(),
    if (nzchar(encrypt_key %||% "")) list(FEISHU_ENCRYPT_KEY = encrypt_key) else list()
  )

  save_config <- isTRUE(confirm_fn("Save this configuration to a .Renviron file?"))
  save_path <- renviron_path
  if (isTRUE(save_config)) {
    if (is.null(save_path) || !nzchar(save_path)) {
      save_path <- input_fn("Path to save environment file", default = ".Renviron") %||% ".Renviron"
    }
    save_fn(updates, path = save_path)
  }

  local_webhook_url <- sprintf(
    "http://%s:%s%s",
    chosen_host,
    chosen_port,
    if (startsWith(chosen_path, "/")) chosen_path else paste0("/", chosen_path)
  )

  commands <- list(
    start_r = "Rscript demo/demo_feishu_webhook.R"
  )
  if (identical(mode, "long_connection")) {
    commands$install_node <- "npm --prefix demo install"
    commands$start_bridge <- "node demo/feishu_longconn_bridge.mjs"
  }

  summary_lines <- c(
    "Feishu channel setup complete.",
    "Mode: Webhook server (pure R)",
    sprintf("Local webhook URL: %s", local_webhook_url),
    sprintf("Workdir: %s", chosen_workdir),
    sprintf("Session root: %s", chosen_session_root),
    sprintf("Model: %s", model_id),
    if (isTRUE(save_config)) sprintf("Saved to: %s", save_path) else "Configuration not saved automatically.",
    "",
    "Next steps:",
    "1. Start the R Feishu webhook runtime:",
    sprintf("   %s", commands$start_r)
  )

  summary_lines <- c(
    summary_lines,
    "2. Configure Feishu event subscription to call the webhook URL exposed through your tunnel or deployment.",
    "3. If you later want advanced settings such as encryption keys or a different webhook path, run the setup wizard again."
  )

  list(
    cancelled = FALSE,
    mode = mode,
    updates = updates,
    saved = isTRUE(save_config),
    renviron_path = save_path,
    local_webhook_url = local_webhook_url,
    commands = commands,
    summary = paste(summary_lines, collapse = "\n")
  )
}

#' @title Write Feishu Bridge Files
#' @description
#' Copy the packaged Feishu long-connection bridge resources from
#' `inst/extdata/feishu` into a user-selected directory. This is optional and is
#' only needed when the user explicitly chooses the advanced Node.js
#' long-connection workflow.
#' @param dest_dir Destination directory. Defaults to `tempdir()`.
#' @return A list with the destination directory, copied files, and suggested
#'   next-step commands.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   info <- write_feishu_bridge_files(tempdir())
#'   info$summary
#' }
#' }
write_feishu_bridge_files <- function(dest_dir = tempdir()) {
  source_dir <- system.file("extdata", "feishu", package = "aisdk")
  if (!nzchar(source_dir) || !dir.exists(source_dir)) {
    rlang::abort("Packaged Feishu bridge resources were not found.")
  }

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  files <- c("feishu_longconn_bridge.mjs", "package.json")
  copied <- vapply(files, function(name) {
    src <- file.path(source_dir, name)
    dst <- file.path(dest_dir, name)
    ok <- file.copy(src, dst, overwrite = TRUE)
    if (!isTRUE(ok)) {
      rlang::abort(sprintf("Failed to copy Feishu bridge file: %s", name))
    }
    normalizePath(dst, winslash = "/", mustWork = FALSE)
  }, character(1))

  summary <- paste(
    "Feishu bridge files written.",
    sprintf("Directory: %s", normalizePath(dest_dir, winslash = "/", mustWork = FALSE)),
    "Next steps:",
    "1. cd into that directory",
    "2. run: npm install",
    "3. run: node feishu_longconn_bridge.mjs",
    sep = "\n"
  )

  list(
    directory = normalizePath(dest_dir, winslash = "/", mustWork = FALSE),
    files = unname(copied),
    commands = c("npm install", "node feishu_longconn_bridge.mjs"),
    summary = summary
  )
}
