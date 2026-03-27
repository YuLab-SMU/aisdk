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
#' @param workdir Working directory that the Feishu bot should use.
#' @param session_root Session store root for the Feishu runtime.
#' @param host Local bind host for the webhook server.
#' @param port Local bind port for the webhook server.
#' @param path Local webhook path.
#' @return A list describing the chosen configuration and next-step commands.
#' @export
setup_feishu_channel <- function(prompt_hooks = list(),
                                 renviron_path = NULL,
                                 workdir = normalizePath(getwd(), winslash = "/", mustWork = FALSE),
                                 session_root = file.path(workdir, ".aisdk", "feishu"),
                                 host = "127.0.0.1",
                                 port = 8788L,
                                 path = "/feishu/webhook") {
  menu_fn <- prompt_hooks$menu %||% console_menu
  input_fn <- prompt_hooks$input %||% console_input
  confirm_fn <- prompt_hooks$confirm %||% console_confirm
  save_fn <- prompt_hooks$save %||% update_renviron

  mode_choice <- menu_fn(
    "How do you want to connect Feishu?",
    c(
      "Webhook server (Recommended, pure R)",
      "Long connection bridge (Advanced, requires Node.js)"
    )
  )
  if (is.null(mode_choice)) {
    return(list(cancelled = TRUE, summary = "Feishu setup cancelled."))
  }
  mode <- if (identical(mode_choice, 2L)) "long_connection" else "webhook"

  app_id <- input_fn("FEISHU_APP_ID")
  app_secret <- input_fn("FEISHU_APP_SECRET")

  if (!nzchar(app_id %||% "") || !nzchar(app_secret %||% "")) {
    return(list(
      cancelled = TRUE,
      summary = "Feishu setup cancelled because FEISHU_APP_ID or FEISHU_APP_SECRET was not provided."
    ))
  }

  verification_token <- if (identical(mode, "webhook")) {
    input_fn("FEISHU_VERIFICATION_TOKEN (optional)", default = "")
  } else {
    ""
  }
  encrypt_key <- if (identical(mode, "webhook")) {
    input_fn("FEISHU_ENCRYPT_KEY (optional)", default = "")
  } else {
    ""
  }

  model_choice <- menu_fn(
    "Which model setup do you want for the bot?",
    c(
      "OpenAI (Recommended)",
      "Anthropic",
      "Use existing environment only",
      "Mock mode"
    )
  )
  if (is.null(model_choice)) {
    return(list(cancelled = TRUE, summary = "Feishu setup cancelled during model selection."))
  }

  model_id <- ""
  provider_updates <- list()

  if (identical(model_choice, 1L)) {
    model_id <- input_fn("FEISHU_MODEL", default = "openai:gpt-4o-mini") %||% "openai:gpt-4o-mini"
    openai_key <- input_fn("OPENAI_API_KEY", default = "")
    if (nzchar(openai_key %||% "")) {
      provider_updates$OPENAI_API_KEY <- openai_key
    }
  } else if (identical(model_choice, 2L)) {
    model_id <- input_fn("FEISHU_MODEL", default = "anthropic:claude-sonnet-4-20250514") %||% "anthropic:claude-sonnet-4-20250514"
    anthropic_key <- input_fn("ANTHROPIC_API_KEY", default = "")
    if (nzchar(anthropic_key %||% "")) {
      provider_updates$ANTHROPIC_API_KEY <- anthropic_key
    }
  } else if (identical(model_choice, 3L)) {
    model_id <- input_fn("FEISHU_MODEL (leave empty to rely on current environment)", default = "") %||% ""
  } else {
    model_id <- ""
  }

  chosen_workdir <- input_fn("FEISHU_WORKDIR", default = workdir) %||% workdir
  chosen_session_root <- input_fn("FEISHU_SESSION_ROOT", default = session_root) %||% session_root
  chosen_host <- input_fn("FEISHU_HOST", default = host) %||% host
  chosen_port <- input_fn("FEISHU_PORT", default = as.character(port)) %||% as.character(port)
  chosen_path <- input_fn("FEISHU_PATH", default = path) %||% path
  chosen_sandbox <- input_fn("FEISHU_SANDBOX_MODE", default = "strict") %||% "strict"

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
    if (nzchar(encrypt_key %||% "")) list(FEISHU_ENCRYPT_KEY = encrypt_key) else list(),
    provider_updates
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
    sprintf("Mode: %s", if (identical(mode, "webhook")) "Webhook server" else "Long connection bridge"),
    sprintf("Local webhook URL: %s", local_webhook_url),
    sprintf("Workdir: %s", chosen_workdir),
    sprintf("Session root: %s", chosen_session_root),
    if (nzchar(model_id)) sprintf("Model: %s", model_id) else "Model: mock mode or existing environment",
    if (isTRUE(save_config)) sprintf("Saved to: %s", save_path) else "Configuration not saved automatically.",
    "",
    "Next steps:",
    "1. Start the R Feishu webhook runtime:",
    sprintf("   %s", commands$start_r)
  )

  if (identical(mode, "webhook")) {
    summary_lines <- c(
      summary_lines,
      "2. Configure Feishu event subscription to call the webhook URL exposed through your tunnel or deployment."
    )
  } else {
    summary_lines <- c(
      summary_lines,
      "2. Install the Node bridge dependency once:",
      sprintf("   %s", commands$install_node),
      "3. Start the long-connection bridge:",
      sprintf("   %s", commands$start_bridge)
    )
  }

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
