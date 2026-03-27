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
#' @param app_id Optional Feishu app id. When supplied, the wizard will not ask for it.
#' @param app_secret Optional Feishu app secret. When supplied, the wizard will not ask for it.
#' @param start_now Whether to offer immediate startup of the local webhook runtime.
#' @param workdir Working directory that the Feishu bot should use.
#' @param session_root Session store root for the Feishu runtime.
#' @param host Local bind host for the webhook server.
#' @param port Local bind port for the webhook server.
#' @param path Local webhook path.
#' @return A list describing the chosen configuration and next-step commands.
#' @export
feishu_node_bin <- function() {
  Sys.which("node")
}

feishu_npm_bin <- function() {
  Sys.which("npm")
}

feishu_default_connection_mode <- function() {
  if (nzchar(feishu_node_bin()) && nzchar(feishu_npm_bin())) {
    return("long_connection")
  }
  "webhook"
}

setup_feishu_channel <- function(prompt_hooks = list(),
                                 renviron_path = NULL,
                                 current_model = NULL,
                                 app_id = NULL,
                                 app_secret = NULL,
                                 start_now = TRUE,
                                 workdir = normalizePath(getwd(), winslash = "/", mustWork = FALSE),
                                 session_root = file.path(workdir, ".aisdk", "feishu"),
                                 host = "127.0.0.1",
                                 port = 8788L,
                                 path = "/feishu/webhook") {
  menu_fn <- prompt_hooks$menu %||% console_menu
  input_fn <- prompt_hooks$input %||% console_input
  confirm_fn <- prompt_hooks$confirm %||% console_confirm
  save_fn <- prompt_hooks$save %||% update_renviron

  mode <- feishu_default_connection_mode()

  app_id <- app_id %||% input_fn("FEISHU_APP_ID")
  app_secret <- app_secret %||% input_fn("FEISHU_APP_SECRET")

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

  mode_label <- if (identical(mode, "long_connection")) {
    "Long connection (recommended for local use)"
  } else {
    "Webhook server (pure R)"
  }

  summary_lines <- c(
    "Feishu connection setup complete.",
    sprintf("Mode: %s", mode_label),
    sprintf("Model: %s", model_id),
    if (isTRUE(save_config)) sprintf("Saved to: %s", save_path) else "Configuration not saved automatically."
  )

  launch <- NULL
  if (isTRUE(start_now) && isTRUE(confirm_fn("Start the local Feishu webhook runtime now?"))) {
    runtime_launch <- start_feishu_channel_service(
      app_id = app_id,
      app_secret = app_secret,
      model_id = model_id,
      verification_token = verification_token,
      encrypt_key = encrypt_key,
      workdir = chosen_workdir,
      session_root = chosen_session_root,
      host = chosen_host,
      port = as.integer(chosen_port),
      path = chosen_path,
      sandbox_mode = chosen_sandbox
    )
    launch <- list(runtime = runtime_launch)
    summary_lines <- c(
      summary_lines,
      "",
      sprintf("Local webhook runtime started in background (PID: %s).", runtime_launch$pid),
      sprintf("Listening URL: %s", runtime_launch$url)
    )

    if (identical(mode, "long_connection")) {
      bridge_dir <- file.path(chosen_workdir, ".aisdk", "feishu_bridge")
      bridge_launch <- start_feishu_long_connection_service(
        app_id = app_id,
        app_secret = app_secret,
        loopback_url = runtime_launch$url,
        bridge_dir = bridge_dir
      )
      launch$bridge <- bridge_launch
        summary_lines <- c(
        summary_lines,
        sprintf("Long-connection bridge started in background (PID: %s).", bridge_launch$pid),
        sprintf("Bridge directory: %s", bridge_launch$directory)
      )
    }
  }

  summary_lines <- c(
    summary_lines,
    "",
    "Next action:",
    "Open Feishu and send the bot a test message now.",
    "Example test message: 你好，帮我回复一句测试成功",
    if (identical(mode, "long_connection")) {
      "No public callback URL is required in this local mode."
    } else {
      sprintf("Make sure Feishu can reach your callback URL: %s", local_webhook_url)
    }
  )

  list(
    cancelled = FALSE,
    mode = mode,
    updates = updates,
    saved = isTRUE(save_config),
    renviron_path = save_path,
    local_webhook_url = local_webhook_url,
    commands = commands,
    launch = launch,
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

#' @title Start Feishu Channel Service
#' @description
#' Start the Feishu webhook runtime in a background R process for local use.
#' @param app_id Feishu app id.
#' @param app_secret Feishu app secret.
#' @param model_id Model id used by the Feishu runtime.
#' @param verification_token Optional callback verification token.
#' @param encrypt_key Optional event subscription encryption key.
#' @param workdir Working directory for tools and generated files.
#' @param session_root Session store root.
#' @param host Bind host.
#' @param port Bind port.
#' @param path Webhook path.
#' @param sandbox_mode Sandbox mode for the Feishu agent.
#' @return A list with the process id and local URL.
#' @export
start_feishu_channel_service <- function(app_id,
                                         app_secret,
                                         model_id = "openai:gpt-4o-mini",
                                         verification_token = "",
                                         encrypt_key = "",
                                         workdir = normalizePath(getwd(), winslash = "/", mustWork = FALSE),
                                         session_root = file.path(workdir, ".aisdk", "feishu"),
                                         host = "127.0.0.1",
                                         port = 8788L,
                                         path = "/feishu/webhook",
                                         sandbox_mode = "strict") {
  process <- callr::r_bg(
    func = function(app_id,
                    app_secret,
                    verification_token,
                    encrypt_key,
                    model_id,
                    workdir,
                    session_root,
                    host,
                    port,
                    path,
                    sandbox_mode) {
      library(aisdk)
      dir.create(session_root, recursive = TRUE, showWarnings = FALSE)
      agent_tools <- create_computer_tools(
        working_dir = workdir,
        sandbox_mode = sandbox_mode
      )
      feishu_agent <- create_agent(
        name = "FeishuAgent",
        description = "Feishu chat agent with local computer tools for code, files, shell, and R execution",
        system_prompt = paste0(
          "You are an execution-capable AI agent operating through Feishu chat.\n",
          "Use tools when they materially improve the answer.\n",
          "Do not ask the user to use terminal-only controls.\n",
          "If you generate a local file and include its absolute path in the final reply, the runtime will try to send it back as a Feishu attachment automatically.\n",
          "Working directory: ", workdir, "\n",
          "Sandbox mode: ", sandbox_mode, "\n"
        ),
        tools = agent_tools,
        model = model_id
      )
      runtime <- create_feishu_channel_runtime(
        session_store = create_file_channel_session_store(session_root),
        app_id = app_id,
        app_secret = app_secret,
        verification_token = if (nzchar(verification_token)) verification_token else NULL,
        encrypt_key = if (nzchar(encrypt_key)) encrypt_key else NULL,
        model = model_id,
        agent = feishu_agent
      )
      run_feishu_webhook_server(
        runtime = runtime,
        host = host,
        port = port,
        path = path
      )
    },
    args = list(
      app_id = app_id,
      app_secret = app_secret,
      verification_token = verification_token,
      encrypt_key = encrypt_key,
      model_id = model_id,
      workdir = workdir,
      session_root = session_root,
      host = host,
      port = as.integer(port),
      path = path,
      sandbox_mode = sandbox_mode
    ),
    supervise = TRUE
  )

  list(
    pid = process$get_pid(),
    url = sprintf(
      "http://%s:%s%s",
      host,
      as.integer(port),
      if (startsWith(path, "/")) path else paste0("/", path)
    )
  )
}

#' @title Start Feishu Long Connection Service
#' @description
#' Materialize the packaged Feishu bridge assets, install Node dependencies, and
#' start the bridge in the background. This mode avoids the need for a public
#' Feishu callback URL during local development.
#' @param app_id Feishu app id.
#' @param app_secret Feishu app secret.
#' @param loopback_url Local webhook URL that the bridge should forward events to.
#' @param bridge_dir Directory where the bridge assets should be written.
#' @return A list with the background process id and bridge directory.
#' @export
start_feishu_long_connection_service <- function(app_id,
                                                 app_secret,
                                                 loopback_url = "http://127.0.0.1:8788/feishu/webhook",
                                                 bridge_dir = file.path(tempdir(), "feishu_bridge")) {
  node_bin <- feishu_node_bin()
  npm_bin <- feishu_npm_bin()
  if (!nzchar(node_bin) || !nzchar(npm_bin)) {
    rlang::abort("Long-connection mode requires both 'node' and 'npm' to be installed and on PATH.")
  }

  info <- write_feishu_bridge_files(bridge_dir)

  install <- processx::run(
    npm_bin,
    c("install"),
    wd = info$directory,
    echo = FALSE,
    error_on_status = FALSE
  )
  if (!identical(install$status, 0L)) {
    rlang::abort(paste(
      "Failed to install Feishu bridge dependencies.",
      install$stderr %||% install$stdout %||% "",
      sep = "\n"
    ))
  }

  process <- processx::process$new(
    node_bin,
    c("feishu_longconn_bridge.mjs"),
    wd = info$directory,
    env = c(
      FEISHU_APP_ID = app_id,
      FEISHU_APP_SECRET = app_secret,
      FEISHU_LOOPBACK_URL = loopback_url
    ),
    cleanup = TRUE
  )

  list(
    pid = process$get_pid(),
    directory = info$directory,
    loopback_url = loopback_url
  )
}
