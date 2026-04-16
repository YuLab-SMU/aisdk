#' @title Console Setup Helpers
#' @description
#' Internal helpers for human-friendly `console_chat()` startup, including
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
      model_env = "OPENAI_MODEL",
      default_base_url = "https://api.openai.com/v1",
      default_model = "gpt-4o"
    ),
    anthropic = list(
      id = "anthropic",
      label = "Anthropic",
      api_key_env = "ANTHROPIC_API_KEY",
      base_url_env = "ANTHROPIC_BASE_URL",
      model_env = "ANTHROPIC_MODEL",
      default_base_url = "https://api.anthropic.com/v1",
      default_model = "claude-sonnet-4-20250514"
    ),
    gemini = list(
      id = "gemini",
      label = "Gemini",
      api_key_env = "GEMINI_API_KEY",
      base_url_env = "GEMINI_BASE_URL",
      model_env = "GEMINI_MODEL",
      default_base_url = "https://generativelanguage.googleapis.com/v1beta/models",
      default_model = "gemini-2.5-flash"
    ),
    deepseek = list(
      id = "deepseek",
      label = "DeepSeek",
      api_key_env = "DEEPSEEK_API_KEY",
      base_url_env = "DEEPSEEK_BASE_URL",
      model_env = "DEEPSEEK_MODEL",
      default_base_url = "https://api.deepseek.com",
      default_model = "deepseek-chat"
    ),
    openrouter = list(
      id = "openrouter",
      label = "OpenRouter",
      api_key_env = "OPENROUTER_API_KEY",
      base_url_env = "OPENROUTER_BASE_URL",
      model_env = "OPENROUTER_MODEL",
      default_base_url = "https://openrouter.ai/api/v1",
      default_model = "openai/gpt-4o-mini"
    ),
    xai = list(
      id = "xai",
      label = "xAI",
      api_key_env = "XAI_API_KEY",
      base_url_env = "XAI_BASE_URL",
      model_env = "XAI_MODEL",
      default_base_url = "https://api.x.ai/v1",
      default_model = "grok-beta"
    ),
    nvidia = list(
      id = "nvidia",
      label = "NVIDIA",
      api_key_env = "NVIDIA_API_KEY",
      base_url_env = "NVIDIA_BASE_URL",
      model_env = "NVIDIA_MODEL",
      default_base_url = "https://integrate.api.nvidia.com/v1",
      default_model = ""
    ),
    volcengine = list(
      id = "volcengine",
      label = "Volcengine",
      api_key_env = "ARK_API_KEY",
      base_url_env = "ARK_BASE_URL",
      model_env = "ARK_MODEL",
      default_base_url = "https://ark.cn-beijing.volces.com/api/v3",
      default_model = ""
    ),
    bailian = list(
      id = "bailian",
      label = "Bailian",
      api_key_env = "DASHSCOPE_API_KEY",
      base_url_env = "DASHSCOPE_BASE_URL",
      model_env = "DASHSCOPE_MODEL",
      default_base_url = "https://dashscope.aliyuncs.com/compatible-mode/v1",
      default_model = "qwen-plus"
    ),
    custom = list(
      id = "custom",
      label = "Custom API",
      api_key_env = "AISDK_CUSTOM_API_KEY",
      base_url_env = "AISDK_CUSTOM_BASE_URL",
      model_env = "AISDK_CUSTOM_MODEL",
      api_format_env = "AISDK_CUSTOM_API_FORMAT",
      use_max_completion_tokens_env = "AISDK_CUSTOM_USE_MAX_COMPLETION_TOKENS",
      allow_empty_api_key = TRUE,
      default_base_url = "",
      default_model = "",
      default_api_format = "chat_completions"
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
  model <- values[[spec$model_env]] %||% ""
  api_format <- values[[spec$api_format_env %||% ""]] %||% spec$default_api_format %||% ""
  use_max_completion_tokens <- values[[spec$use_max_completion_tokens_env %||% ""]] %||% ""
  use_max_completion_tokens <- tolower(use_max_completion_tokens) %in% c("true", "1", "yes")

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
    model = model,
    model_id = model_id,
    api_format = api_format,
    use_max_completion_tokens = use_max_completion_tokens,
    env = list(
      key = spec$api_key_env,
      base_url = spec$base_url_env,
      model = spec$model_env,
      api_format = spec$api_format_env %||% NULL,
      use_max_completion_tokens = spec$use_max_completion_tokens_env %||% NULL
    ),
    values = stats::setNames(
      c(
        list(api_key, base_url, model),
        if (!is.null(spec$api_format_env)) list(api_format) else list(),
        if (!is.null(spec$use_max_completion_tokens_env)) list(if (isTRUE(use_max_completion_tokens)) "true" else "false") else list()
      ),
      c(
        spec$api_key_env,
        spec$base_url_env,
        spec$model_env,
        spec$api_format_env %||% character(0),
        spec$use_max_completion_tokens_env %||% character(0)
      )
    )
  )
}

#' @keywords internal
discover_console_model_profiles <- function(project_path = ".Renviron",
                                            global_path = "~/.Renviron") {
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
  if (is.null(spec)) {
    return(FALSE)
  }

  if (!is.null(profile)) {
    if (!nzchar(profile$api_key %||% "")) {
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
    return(has_key && has_base)
  }
  has_key
}

#' @keywords internal
resolve_console_startup_model <- function(project_path = ".Renviron",
                                          global_path = "~/.Renviron",
                                          project_rprofile_path = ".Rprofile",
                                          global_rprofile_path = "~/.Rprofile",
                                          apply_profile_fn = apply_console_profile) {
  saved_default <- discover_console_default_model(
    project_path = project_rprofile_path,
    global_path = global_rprofile_path
  )

  model_id <- trimws(saved_default$model_id %||% "")
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

  list(model_id = model_id, source = saved_default$source %||% "default", profile = profile)
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

#' @keywords internal
default_console_prompt_hooks <- function() {
  list(
    menu = console_menu,
    input = console_input,
    save = update_renviron,
    apply_profile = apply_console_profile,
    remember_model = remember_console_default_model,
    model_choices = console_model_choices_for_provider
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
choose_console_base_url <- function(spec, menu_fn, input_fn, existing_base_url = NULL) {
  existing_base_url <- existing_base_url %||% ""
  has_default <- nzchar(spec$default_base_url %||% "")

  if (!has_default && !nzchar(existing_base_url)) {
    base_url <- input_fn("Base URL")
    if (is.null(base_url) || !nzchar(base_url)) {
      return(NULL)
    }
    return(base_url)
  }

  if (nzchar(existing_base_url)) {
    selection <- menu_fn(
      sprintf("%s base URL", spec$label),
      c(
        "Keep current",
        "Use default",
        "Custom URL"
      )
    )
    if (is.null(selection)) {
      return(NULL)
    }
    if (identical(selection, 1L)) {
      return(existing_base_url)
    }
    if (identical(selection, 2L)) {
      return(spec$default_base_url)
    }
    return(input_fn("Base URL", default = existing_base_url) %||% existing_base_url)
  }

  selection <- menu_fn(
    sprintf("%s base URL", spec$label),
    c(
      "Use default",
      "Custom URL"
    )
  )
  if (is.null(selection)) {
    return(NULL)
  }
  if (identical(selection, 1L)) {
    return(spec$default_base_url)
  }
  input_fn("Base URL", default = spec$default_base_url) %||% spec$default_base_url
}

#' @keywords internal
choose_console_api_format <- function(spec, menu_fn, existing_api_format = NULL) {
  if (is.null(spec$api_format_env)) {
    return(spec$default_api_format %||% NULL)
  }

  api_formats <- c(
    chat_completions = "OpenAI Chat Completions",
    responses = "OpenAI Responses",
    anthropic_messages = "Anthropic Messages"
  )
  current <- existing_api_format %||% spec$default_api_format %||% "chat_completions"
  current_label <- api_formats[[current]] %||% current

  if (!is.null(existing_api_format) && nzchar(existing_api_format)) {
    selection <- menu_fn(
      sprintf("%s API format", spec$label),
      c(
        paste("Keep current", sprintf("(%s)", current_label)),
        unname(api_formats)
      )
    )
    if (is.null(selection)) {
      return(NULL)
    }
    if (identical(selection, 1L)) {
      return(current)
    }
    return(names(api_formats)[[selection - 1L]])
  }

  selection <- menu_fn(sprintf("%s API format", spec$label), unname(api_formats))
  if (is.null(selection)) {
    return(NULL)
  }
  names(api_formats)[[selection]]
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
choose_console_save_target <- function(menu_fn,
                                       existing_profile = NULL,
                                       project_path = ".Renviron",
                                       global_path = "~/.Renviron",
                                       project_rprofile_path = ".Rprofile",
                                       global_rprofile_path = "~/.Rprofile") {
  if (!is.null(existing_profile)) {
    existing_scope <- existing_profile$scope %||% "project"
    existing_path <- if (identical(existing_scope, "project")) project_path else global_path
    existing_rprofile_path <- console_rprofile_path_for_scope(
      existing_scope,
      project_path = project_rprofile_path,
      global_path = global_rprofile_path
    )
    selection <- menu_fn(
      "Save setup?",
      c(
        sprintf("Overwrite saved (%s)", existing_scope),
        "Save to project",
        "Save globally",
        "Session only"
      )
    )
    if (is.null(selection)) {
      return(NULL)
    }
    return(switch(
      as.character(selection),
      "1" = list(mode = "save", path = existing_path, rprofile_path = existing_rprofile_path),
      "2" = list(mode = "save", path = project_path, rprofile_path = project_rprofile_path),
      "3" = list(mode = "save", path = global_path, rprofile_path = global_rprofile_path),
      list(mode = "session", path = NULL, rprofile_path = NULL)
    ))
  }

  selection <- menu_fn(
    "Save setup?",
    c("Save to project", "Save globally", "Session only")
  )
  if (is.null(selection)) {
    return(NULL)
  }
  switch(
    as.character(selection),
    "1" = list(mode = "save", path = project_path, rprofile_path = project_rprofile_path),
    "2" = list(mode = "save", path = global_path, rprofile_path = global_rprofile_path),
    list(mode = "session", path = NULL, rprofile_path = NULL)
  )
}

#' @keywords internal
finalize_console_profile <- function(spec,
                                     api_key,
                                     base_url,
                                     model,
                                     api_format = NULL,
                                     save_target,
                                     save_fn,
                                     remember_model_fn) {
  updates <- stats::setNames(
    c(
      list(api_key, base_url, model),
      if (!is.null(spec$api_format_env)) list(api_format %||% spec$default_api_format %||% "") else list()
    ),
    c(
      spec$api_key_env,
      spec$base_url_env,
      spec$model_env,
      spec$api_format_env %||% character(0)
    )
  )

  model_id <- paste0(spec$id, ":", model)
  if (!is.null(save_target) && identical(save_target$mode, "save")) {
    save_fn(updates, path = save_target$path)
    remember_model_fn(model_id, path = save_target$rprofile_path %||% NULL)
  } else {
    do.call(Sys.setenv, updates)
    remember_model_fn(model_id, path = NULL)
  }

  model_id
}

#' @keywords internal
run_console_profile_setup <- function(spec,
                                      menu_fn,
                                      input_fn,
                                      save_fn,
                                      remember_model_fn,
                                      model_choices_fn,
                                      existing_profile = NULL,
                                      project_path = ".Renviron",
                                      global_path = "~/.Renviron",
                                      project_rprofile_path = ".Rprofile",
                                      global_rprofile_path = "~/.Rprofile") {
  api_format <- choose_console_api_format(
    spec = spec,
    menu_fn = menu_fn,
    existing_api_format = existing_profile$api_format %||% NULL
  )
  if (is.null(api_format) && !is.null(spec$api_format_env)) {
    return(NULL)
  }

  base_url <- choose_console_base_url(
    spec = spec,
    menu_fn = menu_fn,
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

  save_target <- choose_console_save_target(
    menu_fn = menu_fn,
    existing_profile = existing_profile,
    project_path = project_path,
    global_path = global_path,
    project_rprofile_path = project_rprofile_path,
    global_rprofile_path = global_rprofile_path
  )
  if (is.null(save_target)) {
    return(NULL)
  }

  finalize_console_profile(
    spec = spec,
    api_key = api_key,
    base_url = base_url,
    model = model,
    api_format = api_format,
    save_target = save_target,
    save_fn = save_fn,
    remember_model_fn = remember_model_fn
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
  data <- tryCatch(session$as_list(), error = function(e) NULL)
  if (is.null(data)) {
    return(NA_real_)
  }

  system_prompt <- data$system_prompt %||% ""
  history <- data$history %||% list()

  message_chars <- 0
  if (nzchar(system_prompt)) {
    message_chars <- message_chars + nchar(system_prompt, type = "chars")
  }

  for (msg in history) {
    message_chars <- message_chars + nchar(msg$content %||% "", type = "chars")
    message_chars <- message_chars + nchar(msg$reasoning %||% "", type = "chars")
  }

  message_overhead <- length(history) * 8 + if (nzchar(system_prompt)) 12 else 0
  ceiling(message_chars / 4) + message_overhead
}

#' @keywords internal
infer_console_context_window <- function(provider, model_id) {
  model_id <- tolower(model_id %||% "")

  if (provider == "openai" && grepl("^(gpt-4o|gpt-4\\.1|gpt-5)", model_id)) {
    return(128000L)
  }
  if (provider == "anthropic" && grepl("^claude", model_id)) {
    return(200000L)
  }
  if (provider == "gemini" && grepl("^gemini", model_id)) {
    return(1000000L)
  }
  if (provider == "deepseek" && grepl("^deepseek", model_id)) {
    return(64000L)
  }

  NA_integer_
}

#' @keywords internal
get_console_context_metrics <- function(session) {
  model_ref <- tryCatch(session$get_model_id(), error = function(e) NULL)
  if (is.null(model_ref) || !nzchar(model_ref)) {
    return(NULL)
  }

  sep_pos <- regexpr(":", model_ref, fixed = TRUE)[1]
  if (sep_pos <= 1) {
    return(NULL)
  }

  provider <- substr(model_ref, 1L, sep_pos - 1L)
  model_id <- substr(model_ref, sep_pos + 1L, nchar(model_ref))
  info <- tryCatch(get_model_info(provider, model_id), error = function(e) NULL)

  context_window <- info$context$context_window %||% infer_console_context_window(provider, model_id)
  max_output <- info$context$max_output_tokens %||% NA_integer_
  used_tokens <- estimate_console_context_tokens(session)
  remaining_tokens <- if (!is.na(context_window) && !is.na(used_tokens)) max(context_window - used_tokens, 0) else NA_real_
  is_estimated <- is.null(info) || is.null(info$context$context_window)

  list(
    provider = provider,
    model_id = model_id,
    context_window = context_window,
    max_output = max_output,
    used_tokens = used_tokens,
    remaining_tokens = remaining_tokens,
    estimated = is_estimated
  )
}

#' @keywords internal
format_console_token_compact <- function(tokens) {
  if (is.null(tokens) || is.na(tokens)) {
    return("n/a")
  }

  if (tokens >= 1000) {
    sprintf("%.1fk", tokens / 1000)
  } else {
    as.character(as.integer(round(tokens)))
  }
}

#' @keywords internal
prompt_console_provider_profile <- function(project_path = ".Renviron",
                                            global_path = "~/.Renviron",
                                            project_rprofile_path = ".Rprofile",
                                            global_rprofile_path = "~/.Rprofile",
                                            prompt_hooks = default_console_prompt_hooks()) {
  specs <- console_provider_specs()
  profiles <- discover_console_model_profiles(project_path = project_path, global_path = global_path)
  menu_fn <- prompt_hooks$menu %||% console_menu
  input_fn <- prompt_hooks$input %||% console_input
  save_fn <- prompt_hooks$save %||% update_renviron
  apply_profile_fn <- prompt_hooks$apply_profile %||% apply_console_profile
  remember_model_fn <- prompt_hooks$remember_model %||% remember_console_default_model
  model_choices_fn <- prompt_hooks$model_choices %||% console_model_choices_for_provider

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
      spec <- resolve_console_provider_spec(profile$provider)
      return(
        run_console_profile_setup(
          spec = spec,
          menu_fn = menu_fn,
          input_fn = input_fn,
          save_fn = save_fn,
          remember_model_fn = remember_model_fn,
          model_choices_fn = model_choices_fn,
          existing_profile = profile,
          project_path = project_path,
          global_path = global_path,
          project_rprofile_path = project_rprofile_path,
          global_rprofile_path = global_rprofile_path
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
    existing_profile = NULL,
    project_path = project_path,
    global_path = global_path,
    project_rprofile_path = project_rprofile_path,
    global_rprofile_path = global_rprofile_path
  )
}
