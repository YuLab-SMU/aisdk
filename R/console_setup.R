#' @title Console Setup Helpers
#' @description
#' Helpers for human-friendly chat-front-end startup (aisdk.console, aisdk.shiny), including
#' provider profile discovery, `.Renviron` persistence, and interactive model
#' selection.
#' @name console_setup
#' @keywords internal
NULL

#' @keywords internal
console_provider_specs <- function() {
  list(
    openai = list(
      id = "openai",
      label = "OpenAI",
      api_key_env = "OPENAI_API_KEY",
      base_url_env = "OPENAI_BASE_URL",
      backup_base_urls_env = "OPENAI_BASE_URLS",
      model_env = "OPENAI_MODEL",
      default_base_url = "https://api.openai.com/v1",
      default_model = "gpt-4o"
    ),
    anthropic = list(
      id = "anthropic",
      label = "Anthropic",
      api_key_env = "ANTHROPIC_API_KEY",
      base_url_env = "ANTHROPIC_BASE_URL",
      backup_base_urls_env = "ANTHROPIC_BASE_URLS",
      model_env = "ANTHROPIC_MODEL",
      default_base_url = "https://api.anthropic.com/v1",
      default_model = "claude-sonnet-4-20250514"
    ),
    gemini = list(
      id = "gemini",
      label = "Gemini",
      api_key_env = "GEMINI_API_KEY",
      base_url_env = "GEMINI_BASE_URL",
      backup_base_urls_env = "GEMINI_BASE_URLS",
      model_env = "GEMINI_MODEL",
      default_base_url = "https://generativelanguage.googleapis.com/v1beta/models",
      default_model = "gemini-2.5-flash"
    ),
    deepseek = list(
      id = "deepseek",
      label = "DeepSeek",
      api_key_env = "DEEPSEEK_API_KEY",
      base_url_env = "DEEPSEEK_BASE_URL",
      backup_base_urls_env = "DEEPSEEK_BASE_URLS",
      model_env = "DEEPSEEK_MODEL",
      default_base_url = "https://api.deepseek.com",
      default_model = "deepseek-v4"
    ),
    openrouter = list(
      id = "openrouter",
      label = "OpenRouter",
      api_key_env = "OPENROUTER_API_KEY",
      base_url_env = "OPENROUTER_BASE_URL",
      backup_base_urls_env = "OPENROUTER_BASE_URLS",
      model_env = "OPENROUTER_MODEL",
      default_base_url = "https://openrouter.ai/api/v1",
      default_model = "openai/gpt-4o-mini"
    ),
    xai = list(
      id = "xai",
      label = "xAI",
      api_key_env = "XAI_API_KEY",
      base_url_env = "XAI_BASE_URL",
      backup_base_urls_env = "XAI_BASE_URLS",
      model_env = "XAI_MODEL",
      default_base_url = "https://api.x.ai/v1",
      default_model = "grok-beta"
    ),
    nvidia = list(
      id = "nvidia",
      label = "NVIDIA",
      api_key_env = "NVIDIA_API_KEY",
      base_url_env = "NVIDIA_BASE_URL",
      backup_base_urls_env = "NVIDIA_BASE_URLS",
      model_env = "NVIDIA_MODEL",
      default_base_url = "https://integrate.api.nvidia.com/v1",
      default_model = ""
    ),
    volcengine = list(
      id = "volcengine",
      label = "Volcengine",
      api_key_env = "ARK_API_KEY",
      base_url_env = "ARK_BASE_URL",
      backup_base_urls_env = "ARK_BASE_URLS",
      model_env = "ARK_MODEL",
      default_base_url = "https://ark.cn-beijing.volces.com/api/v3",
      default_model = ""
    ),
    bailian = list(
      id = "bailian",
      label = "Bailian",
      api_key_env = "DASHSCOPE_API_KEY",
      base_url_env = "DASHSCOPE_BASE_URL",
      backup_base_urls_env = "DASHSCOPE_BASE_URLS",
      model_env = "DASHSCOPE_MODEL",
      default_base_url = "https://dashscope.aliyuncs.com/compatible-mode/v1",
      default_model = "qwen-plus"
    ),
    custom = list(
      id = "custom",
      label = "Custom API",
      api_key_env = "AISDK_CUSTOM_API_KEY",
      base_url_env = "AISDK_CUSTOM_BASE_URL",
      backup_base_urls_env = "AISDK_CUSTOM_BASE_URLS",
      model_env = "AISDK_CUSTOM_MODEL",
      api_format_env = "AISDK_CUSTOM_API_FORMAT",
      use_max_completion_tokens_env = "AISDK_CUSTOM_USE_MAX_COMPLETION_TOKENS",
      enable_stream_options_env = "AISDK_CUSTOM_ENABLE_STREAM_OPTIONS",
      supports_native_tools_env = "AISDK_CUSTOM_SUPPORTS_NATIVE_TOOLS",
      responses_state_mode_env = "AISDK_CUSTOM_RESPONSES_STATE_MODE",
      allow_empty_api_key = TRUE,
      default_base_url = "",
      default_model = "",
      default_api_format = "chat_completions",
      default_enable_stream_options = FALSE,
      default_supports_native_tools = FALSE,
      default_responses_state_mode = "stateless"
    ),
    aihubmix = list(
      id = "aihubmix",
      label = "AiHubMix",
      api_key_env = "AIHUBMIX_API_KEY",
      base_url_env = "AIHUBMIX_BASE_URL",
      backup_base_urls_env = "AIHUBMIX_BASE_URLS",
      model_env = "AIHUBMIX_MODEL",
      default_base_url = "https://aihubmix.com/v1",
      default_model = "claude-3-5-sonnet-20241022"
    ),
    moonshot = list(
      id = "moonshot",
      label = "Moonshot",
      api_key_env = "MOONSHOT_API_KEY",
      base_url_env = "MOONSHOT_BASE_URL",
      backup_base_urls_env = "MOONSHOT_BASE_URLS",
      model_env = "MOONSHOT_MODEL",
      default_base_url = "https://api.moonshot.cn/v1",
      default_model = "kimi-k2.6"
    ),
    kimi = list(
      id = "kimi",
      label = "Kimi Code",
      api_key_env = "KIMI_API_KEY",
      base_url_env = "KIMI_BASE_URL",
      backup_base_urls_env = "KIMI_BASE_URLS",
      model_env = "KIMI_MODEL_NAME",
      default_base_url = "https://api.kimi.com/coding/v1",
      default_model = "kimi-for-coding"
    )
  )
}

#' @keywords internal
strip_console_env_value <- function(value) {
  if (is.null(value) || !nzchar(value)) {
    return("")
  }

  value <- trimws(value)
  if ((startsWith(value, "\"") && endsWith(value, "\"")) ||
      (startsWith(value, "'") && endsWith(value, "'"))) {
    value <- substr(value, 2L, nchar(value) - 1L)
  }

  trimws(value)
}

#' @keywords internal
read_console_env_file <- function(path) {
  path <- path.expand(path)
  if (!file.exists(path)) {
    return(list(path = path, values = list()))
  }

  lines <- readLines(path, warn = FALSE)
  values <- list()

  for (line in lines) {
    trimmed <- trimws(line)
    if (!nzchar(trimmed) || startsWith(trimmed, "#")) {
      next
    }

    eq_pos <- regexpr("=", trimmed, fixed = TRUE)[1]
    if (eq_pos <= 1) {
      next
    }

    key <- trimws(substr(trimmed, 1L, eq_pos - 1L))
    value <- strip_console_env_value(substr(trimmed, eq_pos + 1L, nchar(trimmed)))
    if (nzchar(key)) {
      values[[key]] <- value
    }
  }

  list(path = path, values = values)
}

#' @keywords internal
build_console_model_profile <- function(source, scope, values, spec) {
  api_key <- values[[spec$api_key_env]] %||% ""
  base_url <- values[[spec$base_url_env]] %||% spec$default_base_url
  backup_base_urls <- values[[spec$backup_base_urls_env %||% ""]] %||% ""
  model <- values[[spec$model_env]] %||% ""
  api_format <- values[[spec$api_format_env %||% ""]] %||% spec$default_api_format %||% ""
  use_max_completion_tokens <- values[[spec$use_max_completion_tokens_env %||% ""]] %||% ""
  use_max_completion_tokens <- tolower(use_max_completion_tokens) %in% c("true", "1", "yes")
  enable_stream_options <- values[[spec$enable_stream_options_env %||% ""]] %||% ""
  enable_stream_options <- if (nzchar(enable_stream_options)) {
    tolower(enable_stream_options) %in% c("true", "1", "yes")
  } else {
    isTRUE(spec$default_enable_stream_options)
  }
  supports_native_tools <- values[[spec$supports_native_tools_env %||% ""]] %||% ""
  supports_native_tools <- if (nzchar(supports_native_tools)) {
    tolower(supports_native_tools) %in% c("true", "1", "yes")
  } else {
    isTRUE(spec$default_supports_native_tools)
  }
  responses_state_mode <- values[[spec$responses_state_mode_env %||% ""]] %||%
    spec$default_responses_state_mode %||% ""
  responses_state_mode <- if (nzchar(responses_state_mode)) {
    responses_normalize_state_mode(responses_state_mode)
  } else {
    ""
  }

  if (!nzchar(api_key) && !nzchar(model)) {
    return(NULL)
  }

  model_id <- if (nzchar(model)) paste0(spec$id, ":", model) else ""

  list(
    provider = spec$id,
    provider_label = spec$label,
    source = source,
    scope = scope,
    api_key = api_key,
    base_url = base_url,
    backup_base_urls = backup_base_urls,
    model = model,
    model_id = model_id,
    api_format = api_format,
    use_max_completion_tokens = use_max_completion_tokens,
    enable_stream_options = enable_stream_options,
    supports_native_tools = supports_native_tools,
    responses_state_mode = responses_state_mode,
    env = list(
      key = spec$api_key_env,
      base_url = spec$base_url_env,
      backup_base_urls = spec$backup_base_urls_env %||% NULL,
      model = spec$model_env,
      api_format = spec$api_format_env %||% NULL,
      use_max_completion_tokens = spec$use_max_completion_tokens_env %||% NULL,
      enable_stream_options = spec$enable_stream_options_env %||% NULL,
      supports_native_tools = spec$supports_native_tools_env %||% NULL,
      responses_state_mode = spec$responses_state_mode_env %||% NULL
    ),
    values = stats::setNames(
      c(
        list(api_key, base_url),
        if (!is.null(spec$backup_base_urls_env)) list(backup_base_urls) else list(),
        list(model),
        if (!is.null(spec$api_format_env)) list(api_format) else list(),
        if (!is.null(spec$use_max_completion_tokens_env)) list(if (isTRUE(use_max_completion_tokens)) "true" else "false") else list(),
        if (!is.null(spec$enable_stream_options_env)) list(if (isTRUE(enable_stream_options)) "true" else "false") else list(),
        if (!is.null(spec$supports_native_tools_env)) list(if (isTRUE(supports_native_tools)) "true" else "false") else list(),
        if (!is.null(spec$responses_state_mode_env)) list(responses_state_mode) else list()
      ),
      c(
        spec$api_key_env,
        spec$base_url_env,
        spec$backup_base_urls_env %||% character(0),
        spec$model_env,
        spec$api_format_env %||% character(0),
        spec$use_max_completion_tokens_env %||% character(0),
        spec$enable_stream_options_env %||% character(0),
        spec$supports_native_tools_env %||% character(0),
        spec$responses_state_mode_env %||% character(0)
      )
    )
  )
}

#' @keywords internal
discover_console_model_profiles <- function(project_path = ".Renviron",
                                            global_path = "~/.Renviron",
                                            project_config_path = NULL,
                                            global_config_path = NULL) {
  sources <- list(
    project = read_console_env_file(project_path),
    global = read_console_env_file(global_path)
  )
  specs <- console_provider_specs()
  profiles <- list()

  for (scope in names(sources)) {
    src <- sources[[scope]]
    for (spec in specs) {
      profile <- build_console_model_profile(
        source = src$path,
        scope = scope,
        values = src$values,
        spec = spec
      )
      if (!is.null(profile)) {
        profiles[[length(profiles) + 1L]] <- profile
      }
    }
  }

  c(
    profiles,
    discover_console_yaml_profiles(
      project_dir = dirname(normalizePath(project_path, winslash = "/", mustWork = FALSE)),
      project_config_path = project_config_path,
      global_config_path = global_config_path
    )
  )
}

#' @keywords internal
build_console_yaml_profile <- function(scope, path, provider_id, provider_cfg, model_id, model_cfg = list(), default_model = NULL) {
  urls <- normalize_base_urls(c(
    unlist(provider_cfg$base_url %||% NULL, use.names = FALSE),
    unlist(provider_cfg$base_urls %||% NULL, use.names = FALSE),
    unlist(provider_cfg$backup_base_urls %||% NULL, use.names = FALSE)
  ))
  base_url <- if (length(urls) > 0) urls[[1]] else ""
  backup_base_urls <- if (length(urls) > 1) paste(urls[-1], collapse = ",") else ""
  api_key_env <- provider_cfg$api_key_env %||% ""
  api_key <- if (nzchar(api_key_env)) Sys.getenv(api_key_env, unset = "") else provider_cfg$api_key %||% ""
  model_name <- sub("^[^:]+:", "", model_id)
  api_format <- normalize_wire_api(provider_cfg$wire_api %||% provider_cfg$api_format)

  list(
    provider = provider_id,
    provider_label = provider_cfg$name %||% provider_id,
    source = path,
    scope = scope,
    storage = "yaml",
    api_key = api_key,
    base_url = base_url,
    backup_base_urls = backup_base_urls,
    model = model_name,
    model_id = model_id,
    api_format = api_format,
    use_max_completion_tokens = normalize_config_bool(provider_cfg$use_max_completion_tokens, default = FALSE),
    enable_stream_options = !normalize_config_bool(provider_cfg$disable_stream_options, default = TRUE),
    supports_native_tools = normalize_config_bool(provider_cfg$supports_native_tools, default = FALSE),
    responses_state_mode = model_config_responses_state_mode(provider_cfg),
    is_default = identical(model_id, default_model),
    env = list(key = api_key_env, base_url = NULL, model = NULL),
    values = list()
  )
}

#' @keywords internal
discover_console_yaml_profiles <- function(project_dir = getwd(),
                                           project_config_path = NULL,
                                           global_config_path = NULL) {
  project_paths <- if (is.null(project_config_path)) {
    file.path(project_dir, model_config_project_paths())
  } else if (grepl("^(/|~|[A-Za-z]:[/\\\\])", project_config_path)) {
    path.expand(project_config_path)
  } else {
    file.path(project_dir, project_config_path)
  }
  global_paths <- if (is.null(global_config_path)) {
    path.expand(model_config_global_paths())
  } else {
    path.expand(global_config_path)
  }
  paths <- unique(c(project_paths[file.exists(project_paths)], global_paths[file.exists(global_paths)]))
  profiles <- list()
  if (length(paths) == 0) {
    return(profiles)
  }

  project_norm <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  for (path in paths) {
    cfg <- read_model_config_file(path)
    providers <- cfg$model_providers %||% list()
    if (!is.list(providers) || length(providers) == 0) {
      next
    }

    path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
    scope <- if (startsWith(path_norm, project_norm)) "project" else "global"
    default_model <- model_config_default_model(cfg)
    models <- cfg$models %||% list()

    for (provider_id in names(providers)) {
      provider_cfg <- providers[[provider_id]]
      if (!is.list(provider_cfg)) {
        next
      }
      provider_type <- tolower(trimws(as.character(provider_cfg$type %||% "custom")))
      if (!provider_type %in% c("custom", "openai_compatible", "openai-compatible")) {
        next
      }

      matching_models <- names(models)[startsWith(names(models), paste0(provider_id, ":"))]
      if (length(matching_models) == 0 && !is.null(default_model) && startsWith(default_model, paste0(provider_id, ":"))) {
        matching_models <- default_model
      }
      if (length(matching_models) == 0) {
        provider_model <- provider_cfg$model %||% NULL
        if (!is.null(provider_model) && nzchar(provider_model)) {
          matching_models <- paste0(provider_id, ":", provider_model)
        }
      }

      for (model_id in unique(matching_models)) {
        profiles[[length(profiles) + 1L]] <- build_console_yaml_profile(
          scope = scope,
          path = path,
          provider_id = provider_id,
          provider_cfg = provider_cfg,
          model_id = model_id,
          model_cfg = models[[model_id]] %||% list(),
          default_model = default_model
        )
      }
    }
  }

  profiles
}

#' @keywords internal
read_console_rprofile_default_model <- function(path) {
  path <- path.expand(path)
  if (!file.exists(path)) {
    return(list(path = path, model_id = ""))
  }

  lines <- readLines(path, warn = FALSE)
  pattern <- "aisdk\\.console_default_model\\s*=\\s*(['\"])([^'\"]+)\\1"

  for (line in rev(lines)) {
    trimmed <- trimws(line)
    if (!nzchar(trimmed) || startsWith(trimmed, "#")) {
      next
    }

    matches <- regexec(pattern, trimmed, perl = TRUE)
    captured <- regmatches(trimmed, matches)[[1]]
    if (length(captured) >= 3) {
      return(list(path = path, model_id = captured[[3]]))
    }
  }

  list(path = path, model_id = "")
}

#' @keywords internal
discover_console_default_model <- function(project_path = ".Rprofile",
                                           global_path = "~/.Rprofile") {
  sources <- list(
    project = read_console_rprofile_default_model(project_path),
    global = read_console_rprofile_default_model(global_path)
  )

  for (scope in names(sources)) {
    src <- sources[[scope]]
    model_id <- trimws(src$model_id %||% "")
    if (nzchar(model_id)) {
      return(list(
        scope = scope,
        path = src$path,
        model_id = model_id,
        source = "rprofile"
      ))
    }
  }

  option_model <- trimws(getOption("aisdk.console_default_model", "") %||% "")
  if (nzchar(option_model)) {
    return(list(
      scope = "session",
      path = NULL,
      model_id = option_model,
      source = "option"
    ))
  }

  config_default <- default_model_from_config()
  if (!is.null(config_default) && nzchar(config_default)) {
    return(list(
      scope = "config",
      path = NULL,
      model_id = config_default,
      source = "config"
    ))
  }

  list(scope = NULL, path = NULL, model_id = "", source = NULL)
}

#' @keywords internal
update_console_rprofile_default_model <- function(model_id, path = ".Rprofile") {
  path <- path.expand(path)
  lines <- if (file.exists(path)) readLines(path, warn = FALSE) else character(0)
  pattern <- "^\\s*options\\(\\s*aisdk\\.console_default_model\\s*="
  idx <- grep(pattern, lines, perl = TRUE)
  new_line <- sprintf("options(aisdk.console_default_model = %s)", deparse(model_id))

  if (length(idx) > 0) {
    lines[idx[[1]]] <- new_line
    if (length(idx) > 1) {
      lines <- lines[-idx[-1]]
    }
  } else {
    lines <- c(lines, new_line)
  }

  writeLines(lines, path)
  options(aisdk.console_default_model = model_id)
  invisible(TRUE)
}

#' @keywords internal
remember_console_default_model <- function(model_id, path = NULL) {
  options(aisdk.console_default_model = model_id)

  if (!is.null(path) && nzchar(path %||% "")) {
    update_console_rprofile_default_model(model_id, path = path)
  }

  invisible(model_id)
}

#' @keywords internal
console_rprofile_path_for_scope <- function(scope,
                                            project_path = ".Rprofile",
                                            global_path = "~/.Rprofile") {
  if (identical(scope, "global")) {
    return(global_path)
  }
  project_path
}

#' @keywords internal
console_config_path_for_scope <- function(scope,
                                          project_path = "aisdk.yaml",
                                          global_path = default_global_model_config_path()) {
  if (identical(scope, "global")) {
    return(global_path)
  }
  project_path
}

#' @keywords internal
console_api_key_env_for_provider <- function(provider_id) {
  safe_id <- toupper(gsub("[^A-Za-z0-9]+", "_", provider_id))
  paste0("AISDK_", safe_id, "_API_KEY")
}

#' @keywords internal
console_provider_id_from_label <- function(label) {
  id <- tolower(trimws(label %||% ""))
  id <- gsub("[^a-z0-9]+", "_", id)
  id <- gsub("^_+|_+$", "", id)
  if (!nzchar(id)) {
    return(NULL)
  }
  id
}

#' @keywords internal
find_console_profile_by_model_id <- function(model_id, profiles) {
  if (is.null(model_id) || !nzchar(model_id) || length(profiles) == 0) {
    return(NULL)
  }

  for (profile in profiles) {
    if (identical(profile$model_id %||% "", model_id)) {
      return(profile)
    }
  }

  NULL
}

#' @keywords internal
console_model_id_is_usable <- function(model_id, profile = NULL) {
  model_id <- trimws(model_id %||% "")
  if (!nzchar(model_id) || !grepl("^[^:]+:.+$", model_id)) {
    return(FALSE)
  }

  provider <- sub(":.*$", "", model_id)
  spec <- console_provider_specs()[[provider]]
  if (is.null(spec) && identical(profile$storage %||% "", "yaml")) {
    return(nzchar(profile$base_url %||% ""))
  }
  if (is.null(spec)) {
    return(FALSE)
  }

  if (!is.null(profile)) {
    if (!isTRUE(spec$allow_empty_api_key) && !nzchar(profile$api_key %||% "")) {
      return(FALSE)
    }
    if (identical(spec$id, "custom") && !nzchar(profile$base_url %||% "")) {
      return(FALSE)
    }
    return(TRUE)
  }

  has_key <- nzchar(Sys.getenv(spec$api_key_env, unset = ""))
  has_base <- nzchar(Sys.getenv(spec$base_url_env %||% "", unset = ""))
  if (identical(spec$id, "custom")) {
    return((has_key || isTRUE(spec$allow_empty_api_key)) && has_base)
  }
  has_key
}

#' Resolve the startup model from runtime defaults and .Rprofile/.Renviron
#'
#' Part of the companion-package extension API (used by \pkg{aisdk.shiny}).
#' @param project_path,global_path,project_rprofile_path,global_rprofile_path
#'   Paths consulted to discover a saved default model and its profile.
#' @param apply_profile_fn Function used to apply a discovered profile.
#' @return A list with `model_id`, `source`, and `profile`.
#' @keywords internal
#' @export
resolve_console_startup_model <- function(project_path = ".Renviron",
                                          global_path = "~/.Renviron",
                                          project_rprofile_path = ".Rprofile",
                                          global_rprofile_path = "~/.Rprofile",
                                          apply_profile_fn = apply_console_profile) {
  # Priority 1: Check runtime default from set_model() first
  runtime_default <- tryCatch(get_explicit_default_model(), error = function(e) NULL)
  model_id <- ""

  if (!is.null(runtime_default)) {
    if (is.character(runtime_default) && nzchar(runtime_default)) {
      model_id <- runtime_default
    } else if (inherits(runtime_default, "LanguageModelV1")) {
      provider <- runtime_default$provider %||% NULL
      model_name <- runtime_default$model_id %||% NULL
      if (!is.null(provider) && !is.null(model_name)) {
        model_id <- paste0(provider, ":", model_name)
      }
    }
  }

  # Priority 2: Fall back to saved default from .Rprofile
  if (!nzchar(model_id)) {
    saved_default <- discover_console_default_model(
      project_path = project_rprofile_path,
      global_path = global_rprofile_path
    )
    model_id <- trimws(saved_default$model_id %||% "")
  }

  # Priority 3: If still nothing, return NULL (will prompt user)
  if (!nzchar(model_id)) {
    return(list(model_id = NULL, source = "prompt", profile = NULL))
  }

  profiles <- discover_console_model_profiles(
    project_path = project_path,
    global_path = global_path
  )
  profile <- find_console_profile_by_model_id(model_id, profiles)

  if (!is.null(profile) && is.function(apply_profile_fn)) {
    apply_profile_fn(profile)
  }

  if (!console_model_id_is_usable(model_id, profile = profile)) {
    return(list(model_id = NULL, source = "invalid_default", profile = profile))
  }

  saved_source <- if (exists("saved_default", inherits = FALSE)) saved_default$source %||% "saved_default" else "saved_default"
  source_label <- if (!is.null(runtime_default)) "runtime_default" else saved_source
  list(model_id = model_id, source = source_label, profile = profile)
}

#' @keywords internal
format_console_profile_choice <- function(profile) {
  scope_label <- if (identical(profile$scope, "project")) "project" else "global"
  model_label <- if (nzchar(profile$model_id %||% "")) profile$model_id else paste0(profile$provider, ":<unset>")
  base_label <- profile$base_url %||% ""
  base_label <- sub("^https?://", "", base_label)
  base_label <- compact_text_preview(base_label, width = 32)
  sprintf("%s \u00b7 %s \u00b7 %s", scope_label, model_label, base_label)
}

#' Default Prompt Hooks for the Model Setup Flow
#'
#' Returns the default set of injectable hooks (menu, input, save,
#' apply-profile, remember-model, model-choices, detect-endpoint) used by the
#' interactive provider/model setup. Front ends can override individual hooks
#' to embed the setup flow in their own UI; `detect_endpoint` backs the
#' automatic wire-format/capability detection for custom endpoints and can be
#' replaced to avoid network probes. Part of the package-author extension API.
#'
#' @return A named list of hook functions.
#' @keywords internal
#' @export
default_console_prompt_hooks <- function() {
  list(
    menu = console_menu,
    input = console_input,
    save = update_renviron,
    apply_profile = apply_console_profile,
    remember_model = remember_console_default_model,
    model_choices = console_model_choices_for_provider,
    detect_endpoint = detect_console_endpoint_profile
  )
}

#' @keywords internal
resolve_console_provider_spec <- function(provider_id) {
  specs <- console_provider_specs()
  spec <- specs[[provider_id]]
  if (is.null(spec)) {
    rlang::abort(sprintf("Unknown console provider: %s", provider_id))
  }
  spec
}

#' @keywords internal
resolve_console_profile_provider_spec <- function(profile) {
  provider_id <- profile$provider %||% ""
  spec <- console_provider_specs()[[provider_id]]
  if (!is.null(spec)) {
    return(spec)
  }
  if (identical(profile$storage %||% "", "yaml")) {
    return(console_provider_specs()[["custom"]])
  }
  resolve_console_provider_spec(provider_id)
}

#' @keywords internal
choose_console_base_url <- function(spec, input_fn, existing_base_url = NULL) {
  existing_base_url <- existing_base_url %||% ""
  default_url <- if (nzchar(existing_base_url)) existing_base_url else spec$default_base_url %||% ""

  if (nzchar(default_url)) {
    return(input_fn("Base URL", default = default_url) %||% default_url)
  }

  base_url <- input_fn("Base URL")
  if (is.null(base_url) || !nzchar(base_url)) {
    return(NULL)
  }
  base_url
}

# --- Automatic endpoint detection --------------------------------------------
# The setup wizard used to ask users to pick the API wire format, tool-calling
# compatibility, stream_options support, and the Responses state mode. Those
# are facts about the endpoint, not preferences, so the custom-provider flow
# now sends a few tiny probe requests and decides automatically, falling back
# to the previous safe defaults when the endpoint cannot be reached.

#' @keywords internal
console_setup_note <- function(text) {
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_alert_info(text)
  } else {
    message(text)
  }
  invisible(NULL)
}

#' @keywords internal
console_setup_success <- function(text) {
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_alert_success(text)
  } else {
    message(text)
  }
  invisible(NULL)
}

#' @keywords internal
console_api_format_label <- function(api_format) {
  switch(api_format %||% "",
    chat_completions = "OpenAI Chat Completions",
    responses = "OpenAI Responses",
    anthropic_messages = "Anthropic Messages",
    api_format %||% ""
  )
}

#' @keywords internal
console_probe_request <- function(url, headers, body, timeout_seconds = 8) {
  req <- httr2::request(url)
  req <- httr2::req_headers(req, !!!headers)
  req <- prepare_json_post_request(req, body)
  req <- httr2::req_timeout(req, timeout_seconds)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  list(
    status = httr2::resp_status(resp),
    text = tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  )
}

#' @keywords internal
console_probe_headers <- function(api_key, api_format = "chat_completions") {
  h <- list(`Content-Type` = "application/json")
  if (identical(api_format, "anthropic_messages")) {
    h$`anthropic-version` <- "2023-06-01"
    if (nzchar(api_key %||% "")) {
      h$`x-api-key` <- api_key
    }
  } else if (nzchar(api_key %||% "")) {
    h$Authorization <- paste("Bearer", api_key)
  }
  h
}

#' @keywords internal
console_probe_chat_body <- function(model = "aisdk-setup-probe") {
  list(
    model = model,
    messages = list(list(role = "user", content = "ping")),
    max_tokens = 1
  )
}

#' @keywords internal
console_probe_tool <- function(flavor = "openai") {
  params <- list(type = "object", properties = structure(list(), names = character(0)))
  if (identical(flavor, "anthropic")) {
    list(name = "aisdk_probe", description = "Connectivity probe tool", input_schema = params)
  } else if (identical(flavor, "responses")) {
    list(type = "function", name = "aisdk_probe", description = "Connectivity probe tool", parameters = params)
  } else {
    list(
      type = "function",
      `function` = list(name = "aisdk_probe", description = "Connectivity probe tool", parameters = params)
    )
  }
}

#' @keywords internal
# TRUE when the route is served at all. A 404/405/501 usually means "this
# path does not exist on the proxy" -- except when the endpoint exists and
# rejects the probe model (OpenAI reports unknown models as 404
# model_not_found), which we recognize from the body text.
console_probe_route_exists <- function(result) {
  status <- result$status %||% 0L
  if (!status %in% c(404L, 405L, 501L)) {
    return(TRUE)
  }
  text <- result$text %||% ""
  grepl("model", text, ignore.case = TRUE) &&
    !grepl("path|route|endpoint", text, ignore.case = TRUE)
}

#' @keywords internal
console_probe_rejects <- function(result, what) {
  (result$status %||% 0L) >= 400L && grepl(what, result$text %||% "", ignore.case = TRUE)
}

#' @keywords internal
detect_console_chat_capabilities <- function(base_url, api_key, model, probe_fn = console_probe_request) {
  url <- paste0(sub("/+$", "", base_url), "/chat/completions")
  headers <- console_probe_headers(api_key)
  probe <- function(body) tryCatch(probe_fn(url, headers, body), error = function(e) NULL)

  first <- probe(c(console_probe_chat_body(model), list(
    stream = TRUE,
    stream_options = list(include_usage = TRUE),
    tools = list(console_probe_tool())
  )))
  if (is.null(first)) {
    return(list(enable_stream_options = FALSE, supports_native_tools = FALSE, detected = FALSE))
  }
  if ((first$status %||% 0L) < 400L) {
    return(list(enable_stream_options = TRUE, supports_native_tools = TRUE, detected = TRUE))
  }

  rejects_stream_options <- console_probe_rejects(first, "stream_options")
  rejects_tools <- console_probe_rejects(first, "tool")
  if (!rejects_stream_options && !rejects_tools) {
    # Rejected for an unrelated reason (auth, quota, model): inconclusive.
    return(list(enable_stream_options = FALSE, supports_native_tools = FALSE, detected = FALSE))
  }

  enable_stream_options <- !rejects_stream_options
  supports_native_tools <- !rejects_tools

  # Confirm the surviving flags work together before recording them.
  retry_body <- console_probe_chat_body(model)
  if (enable_stream_options) {
    retry_body$stream <- TRUE
    retry_body$stream_options <- list(include_usage = TRUE)
  }
  if (supports_native_tools) {
    retry_body$tools <- list(console_probe_tool())
  }
  second <- probe(retry_body)
  if (!is.null(second) && (second$status %||% 0L) >= 400L) {
    if (console_probe_rejects(second, "stream_options")) enable_stream_options <- FALSE
    if (console_probe_rejects(second, "tool")) supports_native_tools <- FALSE
  }

  list(
    enable_stream_options = enable_stream_options,
    supports_native_tools = supports_native_tools,
    detected = TRUE
  )
}

#' Detect the Wire Format and Capabilities of an OpenAI-Compatible Endpoint
#'
#' Sends a few tiny probe requests to decide which API surface the endpoint
#' serves (`chat_completions`, `responses`, or `anthropic_messages`) and,
#' once a model id is known, whether it accepts `stream_options` and native
#' tool definitions. Used by the custom-provider setup flow so users are not
#' asked wire-protocol questions; overridable via the `detect_endpoint`
#' prompt hook. Returns the previous safe defaults with `detected = FALSE`
#' when the endpoint cannot be reached.
#'
#' @param base_url Endpoint base URL.
#' @param api_key API key, if any.
#' @param model Optional model id; enables the capability probes.
#' @param api_format Optional known wire format; skips format detection.
#' @param probe_fn Probe transport, injectable for tests.
#' @return A list with `api_format`, `enable_stream_options`,
#'   `supports_native_tools`, `responses_state_mode`, and `detected`.
#' @noRd
detect_console_endpoint_profile <- function(base_url,
                                            api_key = NULL,
                                            model = NULL,
                                            api_format = NULL,
                                            probe_fn = console_probe_request) {
  fallback <- list(
    api_format = api_format %||% "chat_completions",
    enable_stream_options = FALSE,
    supports_native_tools = FALSE,
    responses_state_mode = "auto",
    detected = FALSE
  )
  base_url <- sub("/+$", "", base_url %||% "")
  if (!nzchar(base_url)) {
    return(fallback)
  }

  probe <- function(url, headers, body) {
    tryCatch(probe_fn(url, headers, body), error = function(e) NULL)
  }

  if (is.null(api_format)) {
    if (grepl("anthropic", base_url, ignore.case = TRUE)) {
      api_format <- "anthropic_messages"
    } else {
      chat <- probe(
        paste0(base_url, "/chat/completions"),
        console_probe_headers(api_key),
        console_probe_chat_body()
      )
      if (!is.null(chat) && console_probe_route_exists(chat)) {
        api_format <- "chat_completions"
      } else {
        responses <- probe(
          paste0(base_url, "/responses"),
          console_probe_headers(api_key),
          list(model = "aisdk-setup-probe", input = "ping", max_output_tokens = 16)
        )
        if (!is.null(responses) && console_probe_route_exists(responses)) {
          api_format <- "responses"
        } else {
          messages <- probe(
            paste0(base_url, "/messages"),
            console_probe_headers(api_key, "anthropic_messages"),
            console_probe_chat_body()
          )
          if (!is.null(messages) && console_probe_route_exists(messages)) {
            api_format <- "anthropic_messages"
          } else if (is.null(chat) && is.null(responses) && is.null(messages)) {
            # Endpoint unreachable: keep the safe defaults.
            return(fallback)
          } else {
            api_format <- "chat_completions"
          }
        }
      }
    }
  }

  out <- list(
    api_format = api_format,
    enable_stream_options = FALSE,
    supports_native_tools = FALSE,
    # Responses proxies vary in server-side state support; "auto" chains
    # turns when possible and self-heals to stateless on rejection.
    responses_state_mode = "auto",
    detected = TRUE
  )
  if (is.null(model) || !nzchar(model %||% "")) {
    return(out)
  }

  if (identical(api_format, "chat_completions")) {
    caps <- detect_console_chat_capabilities(base_url, api_key, model, probe_fn = probe_fn)
    out$enable_stream_options <- isTRUE(caps$enable_stream_options)
    out$supports_native_tools <- isTRUE(caps$supports_native_tools)
    out$detected <- isTRUE(caps$detected)
  } else if (identical(api_format, "anthropic_messages")) {
    result <- probe(
      paste0(base_url, "/messages"),
      console_probe_headers(api_key, "anthropic_messages"),
      c(console_probe_chat_body(model), list(tools = list(console_probe_tool("anthropic"))))
    )
    out$supports_native_tools <- !is.null(result) && !console_probe_rejects(result, "tool")
    out$detected <- !is.null(result)
  } else if (identical(api_format, "responses")) {
    result <- probe(
      paste0(base_url, "/responses"),
      console_probe_headers(api_key),
      list(
        model = model,
        input = "ping",
        max_output_tokens = 16,
        tools = list(console_probe_tool("responses"))
      )
    )
    out$supports_native_tools <- !is.null(result) && !console_probe_rejects(result, "tool")
    out$detected <- !is.null(result)
  }
  out
}

#' @keywords internal
# Suggest a provider id for a custom endpoint from its host name, e.g.
# "https://api.moonshot.cn/v1" -> "moonshot". Falls back to "custom" and
# avoids colliding with built-in provider ids.
console_suggest_custom_provider_id <- function(base_url) {
  host <- sub("^[a-z]+://", "", tolower(base_url %||% ""))
  host <- sub("[/:].*$", "", host)
  host <- sub("^(api|www|open|openapi)\\.", "", host)
  label <- strsplit(host, ".", fixed = TRUE)[[1]][1] %||% ""
  if (is.na(label)) {
    label <- ""
  }
  label <- gsub("[^a-z0-9-]", "-", label)
  label <- gsub("^-+|-+$", "", label)
  if (!nzchar(label) || grepl("^[0-9-]+$", label)) {
    return("custom")
  }
  if (label %in% names(console_provider_specs())) {
    return(paste0(label, "-api"))
  }
  label
}

#' @keywords internal
choose_console_api_key <- function(spec, menu_fn, input_fn, existing_api_key = NULL) {
  existing_api_key <- existing_api_key %||% ""
  key_prompt <- sprintf(
    "%s API key%s",
    spec$label,
    if (isTRUE(spec$allow_empty_api_key)) " (optional)" else ""
  )

  if (nzchar(existing_api_key)) {
    selection <- menu_fn(
      key_prompt,
      c("Keep saved key", "Enter new key")
    )
    if (is.null(selection)) {
      return(NULL)
    }
    if (identical(selection, 1L)) {
      return(existing_api_key)
    }
  }

  api_key <- input_fn(key_prompt)
  if (is.null(api_key) || !nzchar(api_key)) {
    if (isTRUE(spec$allow_empty_api_key)) {
      return("")
    }
    return(NULL)
  }
  api_key
}

#' @keywords internal
choose_console_model_id <- function(spec,
                                    menu_fn,
                                    input_fn,
                                    model_choices_fn,
                                    api_key,
                                    base_url,
                                    api_format = NULL,
                                    existing_model = NULL) {
  existing_model <- existing_model %||% ""
  provider_for_choices <- if (identical(spec$id, "custom")) {
    if (identical(api_format %||% spec$default_api_format %||% "chat_completions", "anthropic_messages")) "anthropic" else "openai"
  } else {
    spec$id
  }
  model_choices <- unique(c(existing_model[nzchar(existing_model)], model_choices_fn(provider_for_choices, api_key = api_key, base_url = base_url)))

  if (length(model_choices) > 0) {
    selection <- menu_fn(
      sprintf("Pick a %s model", spec$label),
      c(model_choices, "Type model ID")
    )
    if (is.null(selection)) {
      return(NULL)
    }
    if (selection <= length(model_choices)) {
      return(model_choices[[selection]])
    }
  }

  default_model <- if (nzchar(existing_model)) existing_model else spec$default_model
  model <- input_fn("Model ID", default = default_model) %||% default_model
  if (!nzchar(model)) {
    return(NULL)
  }
  model
}

#' @keywords internal
# Ask only WHERE to save (globally, this project, or session only). The
# storage format is decided automatically: named providers persist to
# .Renviron (their standard env vars), custom providers persist to aisdk.yaml
# (which carries wire format and capability flags) with the API key kept in
# .Renviron. Editing an existing profile offers to update it in place.
choose_console_save_target <- function(spec,
                                       menu_fn,
                                       existing_profile = NULL,
                                       project_path = ".Renviron",
                                       global_path = "~/.Renviron",
                                       project_rprofile_path = ".Rprofile",
                                       global_rprofile_path = "~/.Rprofile",
                                       project_config_path = "aisdk.yaml",
                                       global_config_path = default_global_model_config_path()) {
  use_yaml <- identical(spec$id, "custom")
  env_target <- function(scope) {
    list(
      mode = "save",
      path = if (identical(scope, "project")) project_path else global_path,
      scope = scope,
      rprofile_path = console_rprofile_path_for_scope(
        scope,
        project_path = project_rprofile_path,
        global_path = global_rprofile_path
      )
    )
  }
  target_for_scope <- function(scope) {
    if (!use_yaml) {
      return(env_target(scope))
    }
    list(
      mode = "yaml",
      path = console_config_path_for_scope(scope, project_config_path, global_config_path),
      env_path = if (identical(scope, "project")) project_path else global_path,
      scope = scope,
      rprofile_path = console_rprofile_path_for_scope(
        scope,
        project_path = project_rprofile_path,
        global_path = global_rprofile_path
      )
    )
  }
  session_target <- list(mode = "session", path = NULL, rprofile_path = NULL)

  if (!is.null(existing_profile)) {
    existing_scope <- existing_profile$scope %||% "project"
    update_target <- if (identical(existing_profile$storage %||% "", "yaml")) {
      list(
        mode = "yaml",
        path = existing_profile$source %||% console_config_path_for_scope(existing_scope, project_config_path, global_config_path),
        env_path = if (identical(existing_scope, "project")) project_path else global_path,
        scope = existing_scope,
        rprofile_path = console_rprofile_path_for_scope(
          existing_scope,
          project_path = project_rprofile_path,
          global_path = global_rprofile_path
        )
      )
    } else {
      env_target(existing_scope)
    }

    selection <- menu_fn(
      "Save setup?",
      c(
        sprintf("Update saved setup (%s)", existing_scope),
        "Save globally",
        "Save to this project",
        "Session only"
      )
    )
    if (is.null(selection)) {
      return(NULL)
    }
    return(switch(
      as.character(selection),
      "1" = update_target,
      "2" = target_for_scope("global"),
      "3" = target_for_scope("project"),
      session_target
    ))
  }

  selection <- menu_fn(
    "Save setup?",
    c("Save globally (recommended)", "Save to this project", "Session only")
  )
  if (is.null(selection)) {
    return(NULL)
  }
  switch(
    as.character(selection),
    "1" = target_for_scope("global"),
    "2" = target_for_scope("project"),
    session_target
  )
}

#' @keywords internal
finalize_console_profile <- function(spec,
                                     api_key,
                                     base_url,
                                     model,
                                     api_format = NULL,
                                     enable_stream_options = spec$default_enable_stream_options,
                                     supports_native_tools = spec$default_supports_native_tools,
                                     responses_state_mode = NULL,
                                     save_target,
                                     save_fn,
                                     remember_model_fn,
                                     provider_id = spec$id) {
  updates <- stats::setNames(
    c(
      list(api_key, base_url, model),
      if (!is.null(spec$api_format_env)) list(api_format %||% spec$default_api_format %||% "") else list(),
      if (!is.null(spec$enable_stream_options_env)) list(if (isTRUE(enable_stream_options)) "true" else "false") else list(),
      if (!is.null(spec$supports_native_tools_env)) list(if (isTRUE(supports_native_tools)) "true" else "false") else list(),
      if (!is.null(spec$responses_state_mode_env) && !is.null(responses_state_mode)) list(responses_state_mode) else list()
    ),
    c(
      spec$api_key_env,
      spec$base_url_env,
      spec$model_env,
      spec$api_format_env %||% character(0),
      spec$enable_stream_options_env %||% character(0),
      spec$supports_native_tools_env %||% character(0),
      if (!is.null(responses_state_mode)) spec$responses_state_mode_env %||% character(0) else character(0)
    )
  )

  provider_id <- provider_id %||% spec$id
  model_id <- paste0(provider_id, ":", model)
  if (!is.null(save_target) && identical(save_target$mode, "yaml")) {
    api_key_env <- console_api_key_env_for_provider(provider_id)
    provider_config <- list(
      type = "custom",
      name = if (!identical(provider_id, spec$id)) provider_id else spec$label,
      base_url = base_url,
      wire_api = api_format %||% spec$default_api_format %||% "chat_completions",
      api_key_env = api_key_env,
      requires_openai_auth = nzchar(api_key %||% ""),
      disable_stream_options = !isTRUE(enable_stream_options),
      supports_native_tools = isTRUE(supports_native_tools)
    )
    if (identical(provider_config$wire_api, "responses") && !is.null(responses_state_mode)) {
      provider_config$responses_state_mode <- responses_state_mode
    }
    update_model_config_file(
      path = save_target$path,
      provider_id = provider_id,
      provider_config = provider_config,
      model_id = model_id,
      model_config = list(),
      default_model = model_id
    )
    reset_default_registry()
    if (nzchar(api_key %||% "")) {
      save_fn(stats::setNames(list(api_key), api_key_env), path = save_target$env_path %||% ".Renviron")
    }
    remember_model_fn(model_id, path = save_target$rprofile_path %||% NULL)
  } else if (!is.null(save_target) && identical(save_target$mode, "save")) {
    save_fn(updates, path = save_target$path)
    remember_model_fn(model_id, path = save_target$rprofile_path %||% NULL)
  } else {
    do.call(Sys.setenv, updates)
    remember_model_fn(model_id, path = NULL)
  }

  model_id
}

#' @keywords internal
# Interactive setup for one provider. Users are asked only for the base URL
# (Enter keeps the default), the API key, the model, and where to save;
# wire format, tool compatibility, stream_options, and the Responses state
# mode are detected automatically for custom endpoints.
run_console_profile_setup <- function(spec,
                                      menu_fn,
                                      input_fn,
                                      save_fn,
                                      remember_model_fn,
                                      model_choices_fn,
                                      detect_endpoint_fn = detect_console_endpoint_profile,
                                      existing_profile = NULL,
                                      project_path = ".Renviron",
                                      global_path = "~/.Renviron",
                                      project_rprofile_path = ".Rprofile",
                                      global_rprofile_path = "~/.Rprofile",
                                      project_config_path = "aisdk.yaml",
                                      global_config_path = default_global_model_config_path()) {
  base_url <- choose_console_base_url(
    spec = spec,
    input_fn = input_fn,
    existing_base_url = existing_profile$base_url %||% NULL
  )
  if (is.null(base_url)) {
    return(NULL)
  }

  api_key <- choose_console_api_key(
    spec = spec,
    menu_fn = menu_fn,
    input_fn = input_fn,
    existing_api_key = existing_profile$api_key %||% NULL
  )
  if (is.null(api_key)) {
    return(NULL)
  }

  needs_detection <- identical(spec$id, "custom")
  api_format <- if (is.null(spec$api_format_env)) {
    spec$default_api_format %||% NULL
  } else {
    existing_profile$api_format %||% NULL
  }
  if (!is.null(api_format) && !nzchar(api_format)) {
    api_format <- NULL
  }
  if (needs_detection && is.null(api_format)) {
    console_setup_note("Detecting the endpoint API format...")
    detection <- detect_endpoint_fn(base_url = base_url, api_key = api_key)
    api_format <- detection$api_format %||% spec$default_api_format %||% "chat_completions"
    if (isTRUE(detection$detected)) {
      console_setup_success(paste0("Detected API format: ", console_api_format_label(api_format)))
    } else {
      console_setup_note(paste0(
        "Could not reach the endpoint; assuming ", console_api_format_label(api_format), "."
      ))
    }
  }

  model <- choose_console_model_id(
    spec = spec,
    menu_fn = menu_fn,
    input_fn = input_fn,
    model_choices_fn = model_choices_fn,
    api_key = api_key,
    base_url = base_url,
    api_format = api_format,
    existing_model = existing_profile$model %||% NULL
  )
  if (is.null(model)) {
    return(NULL)
  }

  capability_mode <- list(
    enable_stream_options = existing_profile$enable_stream_options %||% isTRUE(spec$default_enable_stream_options),
    supports_native_tools = existing_profile$supports_native_tools %||% isTRUE(spec$default_supports_native_tools)
  )
  responses_state_mode <- NULL
  if (needs_detection) {
    console_setup_note("Checking endpoint capabilities (tiny test requests)...")
    detection <- detect_endpoint_fn(
      base_url = base_url,
      api_key = api_key,
      model = model,
      api_format = api_format
    )
    if (isTRUE(detection$detected)) {
      capability_mode$enable_stream_options <- isTRUE(detection$enable_stream_options)
      capability_mode$supports_native_tools <- isTRUE(detection$supports_native_tools)
      console_setup_success(paste0(
        "Detected: ",
        if (capability_mode$supports_native_tools) "native tool calling" else "text tool fallback",
        if (identical(api_format, "chat_completions")) {
          paste0(", stream options ", if (capability_mode$enable_stream_options) "supported" else "unsupported")
        } else {
          ""
        }
      ))
    } else {
      console_setup_note("Could not verify capabilities; keeping safe defaults.")
    }
    if (identical(api_format, "responses")) {
      responses_state_mode <- responses_normalize_state_mode(
        existing_profile$responses_state_mode %||% detection$responses_state_mode %||% "auto"
      )
    }
  }

  save_target <- choose_console_save_target(
    spec = spec,
    menu_fn = menu_fn,
    existing_profile = existing_profile,
    project_path = project_path,
    global_path = global_path,
    project_rprofile_path = project_rprofile_path,
    global_rprofile_path = global_rprofile_path,
    project_config_path = project_config_path,
    global_config_path = global_config_path
  )
  if (is.null(save_target)) {
    return(NULL)
  }

  provider_id <- spec$id
  if (identical(spec$id, "custom") && identical(save_target$mode %||% "", "yaml")) {
    existing_provider <- if (!identical(existing_profile$provider %||% "", "custom")) existing_profile$provider else NULL
    suggested <- existing_provider %||% console_suggest_custom_provider_id(base_url)
    provider_answer <- input_fn("Setup name", default = suggested)
    provider_id <- console_provider_id_from_label(provider_answer %||% suggested)
    if (is.null(provider_id)) {
      return(NULL)
    }
  }

  finalize_console_profile(
    spec = spec,
    api_key = api_key,
    base_url = base_url,
    model = model,
    api_format = api_format,
    enable_stream_options = capability_mode$enable_stream_options,
    supports_native_tools = capability_mode$supports_native_tools,
    responses_state_mode = responses_state_mode,
    save_target = save_target,
    save_fn = save_fn,
    remember_model_fn = remember_model_fn,
    provider_id = provider_id
  )
}

#' @keywords internal
console_model_choices_for_provider <- function(provider, api_key = NULL, base_url = NULL) {
  static_models <- tryCatch(list_models(provider), error = function(e) data.frame())
  static_ids <- if (is.data.frame(static_models) && nrow(static_models) > 0) static_models$id else character(0)

  fetched <- tryCatch(fetch_api_models(provider, api_key = api_key, base_url = base_url), error = function(e) data.frame())
  fetched_ids <- if (is.data.frame(fetched) && nrow(fetched) > 0) fetched$id else character(0)

  unique(c(fetched_ids, static_ids))
}

#' @keywords internal
apply_console_profile <- function(profile) {
  values <- profile$values
  values <- values[vapply(values, function(x) nzchar(x %||% ""), logical(1))]
  if (length(values) > 0) {
    do.call(Sys.setenv, values)
  }
  invisible(profile)
}

#' @keywords internal
estimate_console_context_tokens <- function(session) {
  estimate_session_context_tokens(session)
}

#' @keywords internal
infer_console_context_window <- function(provider, model_id) {
  infer_session_context_window(provider, model_id)
}

#' Context Metrics for a Chat Session
#'
#' Returns estimated token usage and context-window figures for a session,
#' as used in the console status bar. Part of the package-author extension
#' API.
#'
#' @param session A `ChatSession` object.
#' @return A list of context metrics (estimated tokens, window size, usage).
#' @keywords internal
#' @export
get_console_context_metrics <- function(session) {
  get_session_context_metrics(session)
}

#' Compact Human-Readable Token Count
#'
#' Formats a token count as a compact string (`"1.2k"`, `"3.4M"`, `"n/a"`).
#' Part of the package-author extension API.
#'
#' @param tokens Numeric token count, or `NULL`/`NA`.
#' @return A single string.
#' @keywords internal
#' @export
format_console_token_compact <- function(tokens) {
  if (is.null(tokens) || is.na(tokens)) {
    return("n/a")
  }

  if (tokens >= 1000000) {
    sprintf("%.1fM", tokens / 1000000)
  } else if (tokens >= 1000) {
    sprintf("%.1fk", tokens / 1000)
  } else {
    as.character(as.integer(round(tokens)))
  }
}

#' Interactive Provider/Model Chooser
#'
#' Presents saved provider setups (from `.Renviron`, `aisdk.yaml`, and
#' `.Rprofile`) and the guided new-setup flow, returning the chosen model
#' id. The guided flow only asks for the base URL, API key, model, and where
#' to save; for custom endpoints the wire format and capability flags are
#' detected automatically with tiny probe requests (see the `detect_endpoint`
#' hook). This backs the console `/model` command and the first-run
#' experience; front ends can supply their own `prompt_hooks` to reuse the
#' flow. Part of the package-author extension API.
#'
#' @param project_path Project `.Renviron` path.
#' @param global_path Global `.Renviron` path.
#' @param project_rprofile_path Project `.Rprofile` path.
#' @param global_rprofile_path Global `.Rprofile` path.
#' @param project_config_path Project `aisdk.yaml` path.
#' @param global_config_path Global model-config path.
#' @param prompt_hooks Hook list as returned by
#'   [default_console_prompt_hooks()].
#' @return The selected model id (character scalar), or `NULL` if cancelled.
#' @keywords internal
#' @export
prompt_console_provider_profile <- function(project_path = ".Renviron",
                                            global_path = "~/.Renviron",
                                            project_rprofile_path = ".Rprofile",
                                            global_rprofile_path = "~/.Rprofile",
                                            project_config_path = "aisdk.yaml",
                                            global_config_path = default_global_model_config_path(),
                                            prompt_hooks = default_console_prompt_hooks()) {
  specs <- console_provider_specs()
  menu_fn <- prompt_hooks$menu %||% console_menu
  input_fn <- prompt_hooks$input %||% console_input
  save_fn <- prompt_hooks$save %||% update_renviron
  apply_profile_fn <- prompt_hooks$apply_profile %||% apply_console_profile
  remember_model_fn <- prompt_hooks$remember_model %||% remember_console_default_model
  model_choices_fn <- prompt_hooks$model_choices %||% console_model_choices_for_provider
  detect_endpoint_fn <- prompt_hooks$detect_endpoint %||% detect_console_endpoint_profile
  profiles <- discover_console_model_profiles(
    project_path = project_path,
    global_path = global_path,
    project_config_path = project_config_path,
    global_config_path = global_config_path
  )

  if (length(profiles) > 0) {
    choices <- c(
      vapply(profiles, format_console_profile_choice, character(1)),
      "Edit saved setup",
      "New setup"
    )
    selection <- menu_fn("Pick a saved setup", choices)
    if (!is.null(selection) && selection <= length(profiles)) {
      profile <- profiles[[selection]]
      apply_profile_fn(profile)
      remember_model_fn(
        profile$model_id,
        path = console_rprofile_path_for_scope(
          profile$scope %||% "project",
          project_path = project_rprofile_path,
          global_path = global_rprofile_path
        )
      )
      return(profile$model_id)
    }
    if (!is.null(selection) && identical(selection, length(profiles) + 1L)) {
      edit_selection <- menu_fn(
        "Pick a setup to edit",
        vapply(profiles, format_console_profile_choice, character(1))
      )
      if (is.null(edit_selection)) {
        return(NULL)
      }
      profile <- profiles[[edit_selection]]
      spec <- resolve_console_profile_provider_spec(profile)
      return(
        run_console_profile_setup(
          spec = spec,
          menu_fn = menu_fn,
          input_fn = input_fn,
          save_fn = save_fn,
          remember_model_fn = remember_model_fn,
          model_choices_fn = model_choices_fn,
          detect_endpoint_fn = detect_endpoint_fn,
          existing_profile = profile,
          project_path = project_path,
          global_path = global_path,
          project_rprofile_path = project_rprofile_path,
          global_rprofile_path = global_rprofile_path,
          project_config_path = project_config_path,
          global_config_path = global_config_path
        )
      )
    }
  }

  provider_choices <- vapply(specs, function(spec) spec$label, character(1))
  provider_selection <- menu_fn("Pick a provider", provider_choices)
  if (is.null(provider_selection)) {
    return(NULL)
  }

  spec <- specs[[provider_selection]]
  run_console_profile_setup(
    spec = spec,
    menu_fn = menu_fn,
    input_fn = input_fn,
    save_fn = save_fn,
    remember_model_fn = remember_model_fn,
    model_choices_fn = model_choices_fn,
    detect_endpoint_fn = detect_endpoint_fn,
    existing_profile = NULL,
    project_path = project_path,
    global_path = global_path,
    project_rprofile_path = project_rprofile_path,
    global_rprofile_path = global_rprofile_path,
    project_config_path = project_config_path,
    global_config_path = global_config_path
  )
}
