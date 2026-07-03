#' @title Model Configuration Files
#' @description
#' Helpers for reading project/global `aisdk.yaml` model configuration files.
#' @name config_model
NULL

#' @keywords internal
model_config_project_paths <- function() {
  c("aisdk.yaml", "aisdk.yml", file.path(".aisdk", "config.yaml"), file.path(".aisdk", "config.yml"))
}

#' @keywords internal
model_config_global_paths <- function() {
  config_home <- Sys.getenv("XDG_CONFIG_HOME", unset = "")
  if (!nzchar(config_home)) {
    config_home <- file.path(path.expand("~"), ".config")
  }
  explicit_path <- Sys.getenv("AISDK_CONFIG_FILE", unset = "")
  paths <- c(
    file.path(config_home, "aisdk", "config.yaml"),
    file.path(config_home, "aisdk", "config.yml"),
    explicit_path
  )
  unique(paths[nzchar(paths)])
}

#' @keywords internal
default_global_model_config_path <- function() {
  explicit_path <- Sys.getenv("AISDK_CONFIG_FILE", unset = "")
  if (nzchar(explicit_path)) {
    return(explicit_path)
  }
  model_config_global_paths()[[1]]
}

#' @keywords internal
model_config_existing_paths <- function(project_dir = getwd()) {
  project_paths <- file.path(project_dir, model_config_project_paths())
  global_paths <- path.expand(model_config_global_paths())
  unique(c(project_paths[file.exists(project_paths)], global_paths[file.exists(global_paths)]))
}

#' @keywords internal
read_model_config_file <- function(path) {
  if (!file.exists(path)) {
    return(list())
  }
  cfg <- yaml::read_yaml(path, eval.expr = FALSE)
  if (is.null(cfg)) {
    return(list())
  }
  if (!is.list(cfg)) {
    rlang::abort(sprintf("Model config file must contain a mapping: %s", path))
  }
  cfg
}

#' @keywords internal
merge_model_config <- function(base, override) {
  base <- base %||% list()
  override <- override %||% list()
  utils::modifyList(base, override, keep.null = TRUE)
}

#' @keywords internal
load_model_config <- function(project_dir = getwd(), paths = NULL) {
  if (is.null(paths)) {
    project_paths <- file.path(project_dir, model_config_project_paths())
    global_paths <- path.expand(model_config_global_paths())
    paths <- unique(c(global_paths[file.exists(global_paths)], project_paths[file.exists(project_paths)]))
  }
  cfg <- list()
  for (path in paths) {
    cfg <- merge_model_config(cfg, read_model_config_file(path))
  }
  cfg
}

#' @keywords internal
write_model_config_file <- function(config, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(config, path)
  invisible(TRUE)
}

#' @keywords internal
update_model_config_file <- function(path,
                                     provider_id,
                                     provider_config,
                                     model_id = NULL,
                                     model_config = NULL,
                                     default_model = NULL) {
  cfg <- read_model_config_file(path)
  cfg$model_providers <- cfg$model_providers %||% list()
  cfg$model_providers[[provider_id]] <- compact_null_list(provider_config)

  if (!is.null(model_id) && nzchar(model_id)) {
    cfg$models <- cfg$models %||% list()
    if (!is.null(model_config) && length(model_config) > 0) {
      cfg$models[[model_id]] <- compact_null_list(model_config)
    } else if (is.null(cfg$models[[model_id]])) {
      cfg$models[[model_id]] <- list()
    }
  }

  if (!is.null(default_model) && nzchar(default_model)) {
    cfg$default_model <- default_model
  }

  write_model_config_file(cfg, path)
}

#' @keywords internal
compact_null_list <- function(x) {
  x <- x %||% list()
  if (!is.list(x)) {
    return(x)
  }
  x <- x[!vapply(x, function(value) is.null(value) || (is.character(value) && length(value) == 1 && !nzchar(value)), logical(1))]
  lapply(x, compact_null_list)
}

#' @keywords internal
normalize_model_provider_id <- function(id) {
  id <- trimws(as.character(id %||% ""))
  if (!nzchar(id) || !grepl("^[A-Za-z][A-Za-z0-9_.-]*$", id)) {
    rlang::abort("Model provider IDs must match `^[A-Za-z][A-Za-z0-9_.-]*$`.")
  }
  id
}

#' @keywords internal
normalize_wire_api <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return("chat_completions")
  }
  raw_value <- value[[1]] %||% ""
  if (length(raw_value) == 0) {
    return("chat_completions")
  }
  raw_value <- raw_value[[1]] %||% ""
  if (is.na(raw_value) || !nzchar(trimws(as.character(raw_value)))) {
    return("chat_completions")
  }
  value <- tolower(trimws(as.character(raw_value)))
  aliases <- c(
    chat = "chat_completions",
    openai_chat = "chat_completions",
    chat_completions = "chat_completions",
    responses = "responses",
    openai_responses = "responses",
    anthropic = "anthropic_messages",
    anthropic_messages = "anthropic_messages"
  )
  resolved <- aliases[[value]]
  if (is.null(resolved)) {
    rlang::abort(sprintf(
      "`wire_api` must be one of %s.",
      paste(shQuote(names(aliases)), collapse = ", ")
    ))
  }
  resolved
}

#' @keywords internal
normalize_config_bool <- function(value, default = FALSE) {
  if (is.null(value)) {
    return(isTRUE(default))
  }
  if (is.logical(value)) {
    return(isTRUE(value))
  }
  tolower(trimws(as.character(value))) %in% c("true", "1", "yes", "on")
}

#' @keywords internal
model_config_provider_api_key <- function(provider_cfg) {
  api_key_env <- provider_cfg$api_key_env %||% NULL
  if (!is.null(api_key_env) && nzchar(api_key_env)) {
    return(Sys.getenv(api_key_env, unset = ""))
  }
  provider_cfg$api_key %||% ""
}

#' @keywords internal
# Resolve the Responses server-side state policy from a configured provider.
# Accepts an explicit `responses_state_mode`/`state_mode` string, or a boolean
# `responses_stateful` alias. Defaults to "stateless" so HTTP-stateless
# Responses proxies work out of the box.
model_config_responses_state_mode <- function(provider_cfg) {
  raw <- provider_cfg$responses_state_mode %||% provider_cfg$state_mode
  if (!is.null(raw) && nzchar(trimws(as.character(raw)))) {
    return(responses_normalize_state_mode(raw))
  }
  if (!is.null(provider_cfg$responses_stateful)) {
    return(if (normalize_config_bool(provider_cfg$responses_stateful, default = FALSE)) "server" else "stateless")
  }
  "stateless"
}

#' @keywords internal
model_config_provider_base_url <- function(provider_cfg) {
  urls <- normalize_base_urls(c(
    unlist(provider_cfg$base_url %||% NULL, use.names = FALSE),
    unlist(provider_cfg$base_urls %||% NULL, use.names = FALSE),
    unlist(provider_cfg$backup_base_urls %||% NULL, use.names = FALSE)
  ))
  paste(urls, collapse = ",")
}

#' @keywords internal
create_config_custom_provider <- function(provider_id, provider_cfg) {
  base_url <- model_config_provider_base_url(provider_cfg)
  if (!nzchar(base_url)) {
    rlang::abort(sprintf("Configured provider `%s` must define `base_url`.", provider_id))
  }
  wire_api <- normalize_wire_api(provider_cfg$wire_api %||% provider_cfg$api_format)
  create_custom_provider(
    provider_name = provider_id,
    base_url = base_url,
    api_key = model_config_provider_api_key(provider_cfg),
    api_format = wire_api,
    use_max_completion_tokens = normalize_config_bool(provider_cfg$use_max_completion_tokens, default = FALSE),
    disable_stream_options = normalize_config_bool(provider_cfg$disable_stream_options, default = TRUE),
    supports_native_tools = normalize_config_bool(provider_cfg$supports_native_tools, default = FALSE),
    responses_state_mode = model_config_responses_state_mode(provider_cfg)
  )
}

#' @keywords internal
register_configured_model_providers <- function(registry, config = load_model_config()) {
  providers <- config$model_providers %||% list()
  if (!is.list(providers) || length(providers) == 0) {
    return(invisible(registry))
  }

  for (provider_id in names(providers)) {
    id <- normalize_model_provider_id(provider_id)
    provider_cfg <- providers[[provider_id]]
    if (!is.list(provider_cfg)) {
      next
    }
    provider_type <- tolower(trimws(as.character(provider_cfg$type %||% "custom")))
    if (!provider_type %in% c("custom", "openai_compatible", "openai-compatible")) {
      next
    }
    registry$register(id, local({
      captured_id <- id
      captured_cfg <- provider_cfg
      function() create_config_custom_provider(captured_id, captured_cfg)
    }))
  }

  invisible(registry)
}

#' @keywords internal
model_config_default_model <- function(config = load_model_config()) {
  value <- config$default_model %||% NULL
  if ((is.null(value) || !nzchar(value %||% "")) && !is.null(config$model)) {
    if (!is.null(config$model_provider) && nzchar(config$model_provider %||% "")) {
      value <- paste0(config$model_provider, ":", config$model)
    } else {
      value <- config$model
    }
  }
  if (is.null(value) || !is.character(value) || !nzchar(value)) {
    return(NULL)
  }
  value
}

#' @keywords internal
model_config_runtime_options <- function(model_id = NULL, config = load_model_config()) {
  if (is.null(model_id) || !is.character(model_id) || !nzchar(model_id)) {
    return(list())
  }
  models <- config$models %||% list()
  model_cfg <- models[[model_id]] %||% list()
  default_model_id <- model_config_default_model(config)
  if ((is.null(model_cfg) || length(model_cfg) == 0) && identical(model_id, default_model_id)) {
    model_cfg <- config
  }
  if (!is.list(model_cfg)) {
    model_cfg <- list()
  }

  runtime <- Filter(Negate(is.null), list(
    context_window = model_cfg$context_window %||% model_cfg$model_context_window %||% NULL,
    max_output_tokens = model_cfg$max_output_tokens %||% model_cfg$model_max_output_tokens %||% NULL,
    max_tokens = model_cfg$max_tokens %||% model_cfg$model_max_tokens %||% NULL,
    thinking = model_cfg$thinking %||% model_cfg$model_thinking %||% NULL,
    thinking_budget = model_cfg$thinking_budget %||% model_cfg$model_thinking_budget %||% NULL,
    reasoning_effort = model_cfg$reasoning_effort %||% model_cfg$model_reasoning_effort %||% NULL
  ))

  if (length(runtime) == 0) {
    return(list())
  }
  compact_model_runtime_options(runtime)
}

#' @keywords internal
default_model_from_config <- function() {
  model_config_default_model(load_model_config())
}
