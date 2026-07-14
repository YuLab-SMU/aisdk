library(aisdk)

test_that("read_console_env_file parses quoted values", {
  path <- tempfile(fileext = ".Renviron")
  writeLines(c(
    "OPENAI_API_KEY=\"sk-test\"",
    "OPENAI_BASE_URL=https://api.openai.com/v1",
    "OPENAI_MODEL='gpt-5-mini'",
    "# comment"
  ), path)

  parsed <- aisdk:::read_console_env_file(path)

  expect_equal(parsed$values$OPENAI_API_KEY, "sk-test")
  expect_equal(parsed$values$OPENAI_BASE_URL, "https://api.openai.com/v1")
  expect_equal(parsed$values$OPENAI_MODEL, "gpt-5-mini")
})

test_that("discover_console_model_profiles finds project and global profiles", {
  project_path <- tempfile(fileext = ".Renviron")
  global_path <- tempfile(fileext = ".Renviron")

  writeLines(c(
    "OPENAI_API_KEY=sk-project",
    "OPENAI_BASE_URL=https://api.openai.com/v1",
    "OPENAI_MODEL=gpt-5-mini"
  ), project_path)

  writeLines(c(
    "ANTHROPIC_API_KEY=sk-global",
    "ANTHROPIC_BASE_URL=https://api.anthropic.com/v1",
    "ANTHROPIC_MODEL=claude-sonnet-4-20250514"
  ), global_path)

  profiles <- aisdk:::discover_console_model_profiles(
    project_path = project_path,
    global_path = global_path
  )

  expect_length(profiles, 2)
  expect_equal(profiles[[1]]$scope, "project")
  expect_equal(profiles[[1]]$model_id, "openai:gpt-5-mini")
  expect_equal(profiles[[2]]$scope, "global")
  expect_equal(profiles[[2]]$model_id, "anthropic:claude-sonnet-4-20250514")
})

test_that("discover_console_model_profiles includes backup base URLs", {
  project_path <- tempfile(fileext = ".Renviron")
  global_path <- tempfile(fileext = ".Renviron")

  writeLines(c(
    "AISDK_CUSTOM_API_KEY=sk-project",
    "AISDK_CUSTOM_BASE_URL=https://primary.example/v1",
    "AISDK_CUSTOM_BASE_URLS=https://backup.example/v1",
    "AISDK_CUSTOM_MODEL=proxy-model",
    "AISDK_CUSTOM_API_FORMAT=responses",
    "AISDK_CUSTOM_RESPONSES_STATE_MODE=server"
  ), project_path)

  profiles <- aisdk:::discover_console_model_profiles(
    project_path = project_path,
    global_path = global_path
  )

  custom_idx <- which(vapply(profiles, function(p) identical(p$provider, "custom"), logical(1)))[[1]]
  custom <- profiles[[custom_idx]]
  expect_equal(custom$base_url, "https://primary.example/v1")
  expect_equal(custom$backup_base_urls, "https://backup.example/v1")
  expect_equal(custom$values$AISDK_CUSTOM_BASE_URLS, "https://backup.example/v1")
  expect_equal(custom$api_format, "responses")
  expect_equal(custom$responses_state_mode, "server")
  expect_equal(custom$values$AISDK_CUSTOM_RESPONSES_STATE_MODE, "server")
})

test_that("discover_console_model_profiles includes aisdk.yaml profiles", {
  tmp <- tempfile("aisdk-config-")
  dir.create(tmp)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp)

  writeLines(c(
    "default_model: yulab:gpt-5.5",
    "model_providers:",
    "  yulab:",
    "    type: custom",
    "    name: openai-yulab",
    "    base_url: http://186.244.210.51:8080/v1",
    "    wire_api: responses",
    "    responses_state_mode: auto",
    "    api_key_env: YULAB_OPENAI_API_KEY",
    "models:",
    "  yulab:gpt-5.5:",
    "    reasoning_effort: xhigh"
  ), "aisdk.yaml")

  profiles <- aisdk:::discover_console_model_profiles(
    project_path = file.path(tmp, ".Renviron"),
    global_path = tempfile(fileext = ".Renviron")
  )

  yulab <- profiles[[which(vapply(profiles, function(p) identical(p$model_id, "yulab:gpt-5.5"), logical(1)))[[1]]]]
  expect_equal(yulab$provider, "yulab")
  expect_equal(yulab$storage, "yaml")
  expect_equal(yulab$base_url, "http://186.244.210.51:8080/v1")
  expect_equal(yulab$api_format, "responses")
  expect_equal(yulab$responses_state_mode, "auto")
})

test_that("apply_console_profile exports provider values into current env", {
  profile <- list(
    values = list(
      OPENAI_API_KEY = "sk-temp",
      OPENAI_BASE_URL = "https://api.openai.com/v1",
      OPENAI_MODEL = "gpt-5-mini"
    )
  )

  old_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  old_base <- Sys.getenv("OPENAI_BASE_URL", unset = "")
  old_model <- Sys.getenv("OPENAI_MODEL", unset = "")
  on.exit({
    Sys.setenv(
      OPENAI_API_KEY = old_key,
      OPENAI_BASE_URL = old_base,
      OPENAI_MODEL = old_model
    )
  }, add = TRUE)

  aisdk:::apply_console_profile(profile)

  expect_equal(Sys.getenv("OPENAI_API_KEY"), "sk-temp")
  expect_equal(Sys.getenv("OPENAI_BASE_URL"), "https://api.openai.com/v1")
  expect_equal(Sys.getenv("OPENAI_MODEL"), "gpt-5-mini")
})

test_that("read_console_rprofile_default_model parses saved model option", {
  path <- tempfile(fileext = ".Rprofile")
  writeLines(c(
    "options(stringsAsFactors = FALSE)",
    "options(aisdk.console_default_model = \"openai:gpt-5-mini\")"
  ), path)

  parsed <- aisdk:::read_console_rprofile_default_model(path)

  expect_equal(parsed$model_id, "openai:gpt-5-mini")
})

test_that("update_console_rprofile_default_model writes and replaces saved model option", {
  old_default <- getOption("aisdk.console_default_model")
  on.exit(options(aisdk.console_default_model = old_default), add = TRUE)
  path <- tempfile(fileext = ".Rprofile")
  writeLines("options(aisdk.console_default_model = \"openai:gpt-4o\")", path)

  aisdk:::update_console_rprofile_default_model("anthropic:claude-sonnet-4-20250514", path = path)

  lines <- readLines(path, warn = FALSE)
  expect_length(grep("aisdk\\.console_default_model", lines), 1)
  expect_true(any(grepl("anthropic:claude-sonnet-4-20250514", lines, fixed = TRUE)))
})

test_that("resolve_console_startup_model uses saved default and matching profile", {
  old_default <- getOption("aisdk.console_default_model")
  on.exit(options(aisdk.console_default_model = old_default), add = TRUE)
  options(aisdk.console_default_model = NULL)
  project_path <- tempfile(fileext = ".Renviron")
  global_path <- tempfile(fileext = ".Renviron")
  project_rprofile_path <- tempfile(fileext = ".Rprofile")
  writeLines(c(
    "OPENAI_API_KEY=sk-project",
    "OPENAI_BASE_URL=https://api.openai.com/v1",
    "OPENAI_MODEL=gpt-5-mini"
  ), project_path)
  writeLines(
    "options(aisdk.console_default_model = \"openai:gpt-5-mini\")",
    project_rprofile_path
  )

  applied <- NULL
  resolved <- aisdk:::resolve_console_startup_model(
    project_path = project_path,
    global_path = global_path,
    project_rprofile_path = project_rprofile_path,
    global_rprofile_path = tempfile(fileext = ".Rprofile"),
    apply_profile_fn = function(profile) {
      applied <<- profile
      profile
    }
  )

  expect_equal(resolved$model_id, "openai:gpt-5-mini")
  expect_equal(resolved$source, "rprofile")
  expect_equal(applied$model_id, "openai:gpt-5-mini")
})

test_that("resolve_console_startup_model falls back when saved default is unusable", {
  old_default <- getOption("aisdk.console_default_model")
  on.exit(options(aisdk.console_default_model = old_default), add = TRUE)
  options(aisdk.console_default_model = NULL)
  old_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  on.exit({
    if (nzchar(old_key)) {
      Sys.setenv(OPENAI_API_KEY = old_key)
    } else {
      Sys.unsetenv("OPENAI_API_KEY")
    }
  }, add = TRUE)
  Sys.unsetenv("OPENAI_API_KEY")

  resolved <- aisdk:::resolve_console_startup_model(
    project_path = tempfile(fileext = ".Renviron"),
    global_path = tempfile(fileext = ".Renviron"),
    project_rprofile_path = tempfile(fileext = ".Rprofile"),
    global_rprofile_path = tempfile(fileext = ".Rprofile"),
    apply_profile_fn = function(profile) profile
  )

  expect_null(resolved$model_id)
  expect_equal(resolved$source, "prompt")

  path <- tempfile(fileext = ".Rprofile")
  writeLines("options(aisdk.console_default_model = \"openai:gpt-5-mini\")", path)
  resolved_invalid <- aisdk:::resolve_console_startup_model(
    project_path = tempfile(fileext = ".Renviron"),
    global_path = tempfile(fileext = ".Renviron"),
    project_rprofile_path = path,
    global_rprofile_path = tempfile(fileext = ".Rprofile"),
    apply_profile_fn = function(profile) profile
  )

  expect_null(resolved_invalid$model_id)
  expect_equal(resolved_invalid$source, "invalid_default")
})

test_that("console_model_id_is_usable allows custom profiles without API keys", {
  profile <- list(
    provider = "custom",
    api_key = "",
    base_url = "http://127.0.0.1:3000/v1"
  )

  expect_true(aisdk:::console_model_id_is_usable("custom:local-model", profile = profile))
})

# Status-line rendering of these metrics is covered in the aisdk.console
# package (tests/testthat/test-console-status.R).

test_that("prompt_console_provider_profile can choose an existing profile", {
  project_path <- tempfile(fileext = ".Renviron")
  global_path <- tempfile(fileext = ".Renviron")
  writeLines(c(
    "OPENAI_API_KEY=sk-project",
    "OPENAI_BASE_URL=https://api.openai.com/v1",
    "OPENAI_MODEL=gpt-5-mini"
  ), project_path)

  applied <- NULL
  remembered <- list()
  menu_answers <- c(1L)
  hooks <- list(
    menu = function(title, choices) {
      answer <- menu_answers[[1]]
      menu_answers <<- menu_answers[-1]
      answer
    },
    apply_profile = function(profile) {
      applied <<- profile
      profile
    },
    remember_model = function(model_id, path = NULL) {
      remembered <<- list(model_id = model_id, path = path)
      model_id
    }
  )

  model_id <- aisdk:::prompt_console_provider_profile(
    project_path = project_path,
    global_path = global_path,
    project_rprofile_path = "project-test.Rprofile",
    global_rprofile_path = "global-test.Rprofile",
    prompt_hooks = hooks
  )

  expect_equal(model_id, "openai:gpt-5-mini")
  expect_equal(applied$model_id, "openai:gpt-5-mini")
  expect_equal(remembered$model_id, "openai:gpt-5-mini")
  expect_equal(remembered$path, "project-test.Rprofile")
})

test_that("format_console_profile_choice uses compact labels", {
  profile <- list(
    scope = "project",
    model_id = "openai:gpt-5-mini",
    provider = "openai",
    base_url = "https://api.openai.com/v1"
  )

  label <- aisdk:::format_console_profile_choice(profile)

  expect_match(label, "^project · openai:gpt-5-mini · api\\.openai\\.com/v1$")
  expect_false(grepl("\\[project\\]", label, fixed = TRUE))
  expect_false(grepl("https://", label, fixed = TRUE))
})

test_that("prompt_console_provider_profile supports manual setup and persistence seam", {
  project_path <- tempfile(fileext = ".Renviron")
  global_path <- tempfile(fileext = ".Renviron")

  menu_answers <- c(
    1L, # provider: OpenAI
    1L, # select first fetched/static model
    2L  # save to this project
  )
  input_answers <- c(
    "https://api.openai.com/v1", # base URL (Enter keeps default)
    "sk-manual"
  )
  saved <- list()
  remembered <- list()

  hooks <- list(
    menu = function(title, choices) {
      answer <- menu_answers[[1]]
      menu_answers <<- menu_answers[-1]
      answer
    },
    input = function(prompt, default = NULL) {
      answer <- input_answers[[1]]
      input_answers <<- input_answers[-1]
      answer %||% default
    },
    model_choices = function(provider, api_key = NULL, base_url = NULL) {
      c("gpt-5-mini", "gpt-4o")
    },
    save = function(updates, path) {
      saved <<- list(updates = updates, path = path)
      TRUE
    },
    remember_model = function(model_id, path = NULL) {
      remembered <<- list(model_id = model_id, path = path)
      model_id
    }
  )

  model_id <- aisdk:::prompt_console_provider_profile(
    project_path = project_path,
    global_path = global_path,
    project_rprofile_path = "project-default.Rprofile",
    global_rprofile_path = "global-default.Rprofile",
    prompt_hooks = hooks
  )

  expect_equal(model_id, "openai:gpt-5-mini")
  expect_equal(saved$path, project_path)
  expect_equal(saved$updates$OPENAI_API_KEY, "sk-manual")
  expect_equal(saved$updates$OPENAI_BASE_URL, "https://api.openai.com/v1")
  expect_equal(saved$updates$OPENAI_MODEL, "gpt-5-mini")
  expect_equal(remembered$model_id, "openai:gpt-5-mini")
  expect_equal(remembered$path, "project-default.Rprofile")
})

test_that("prompt_console_provider_profile can edit and overwrite an existing profile", {
  project_path <- tempfile(fileext = ".Renviron")
  global_path <- tempfile(fileext = ".Renviron")
  writeLines(c(
    "OPENAI_API_KEY=sk-old",
    "OPENAI_BASE_URL=https://api.openai.com/v1",
    "OPENAI_MODEL=gpt-4o"
  ), project_path)

  menu_answers <- c(
    2L, # choose "Edit saved setup"
    1L, # edit the first saved profile
    2L, # enter new API key
    2L, # choose second model option
    1L  # update saved setup
  )
  input_answers <- c(
    "https://proxy.example.com/v1", # base URL replaces the saved one
    "sk-new"
  )
  saved <- list()
  remembered <- list()

  hooks <- list(
    menu = function(title, choices) {
      answer <- menu_answers[[1]]
      menu_answers <<- menu_answers[-1]
      answer
    },
    input = function(prompt, default = NULL) {
      answer <- input_answers[[1]]
      input_answers <<- input_answers[-1]
      answer %||% default
    },
    model_choices = function(provider, api_key = NULL, base_url = NULL) {
      c("gpt-4o", "gpt-5-mini")
    },
    save = function(updates, path) {
      saved <<- list(updates = updates, path = path)
      TRUE
    },
    remember_model = function(model_id, path = NULL) {
      remembered <<- list(model_id = model_id, path = path)
      model_id
    }
  )

  model_id <- aisdk:::prompt_console_provider_profile(
    project_path = project_path,
    global_path = global_path,
    project_rprofile_path = "project-edit.Rprofile",
    global_rprofile_path = "global-edit.Rprofile",
    prompt_hooks = hooks
  )

  expect_equal(model_id, "openai:gpt-5-mini")
  expect_equal(saved$path, project_path)
  expect_equal(saved$updates$OPENAI_API_KEY, "sk-new")
  expect_equal(saved$updates$OPENAI_BASE_URL, "https://proxy.example.com/v1")
  expect_equal(saved$updates$OPENAI_MODEL, "gpt-5-mini")
  expect_equal(remembered$model_id, "openai:gpt-5-mini")
  expect_equal(remembered$path, "project-edit.Rprofile")
})

test_that("prompt_console_provider_profile auto-detects custom endpoints and saves to yaml", {
  project_path <- tempfile(fileext = ".Renviron")
  global_path <- tempfile(fileext = ".Renviron")
  project_config_path <- tempfile(fileext = ".yaml")
  registry_env <- get(".registry_env", envir = asNamespace("aisdk"))
  old_registry <- registry_env$default %||% NULL
  registry_env$default <- ProviderRegistry$new()
  on.exit({
    registry_env$default <- old_registry
  }, add = TRUE)

  menu_answers <- c(
    10L, # provider: Custom API
    1L,  # select first fetched/static model
    2L   # save to this project
  )
  input_answers <- c(
    "https://proxy.example.com/v1",
    "sk-custom",
    "proxycorp" # setup name (suggested from the host)
  )
  detect_calls <- list()
  saved_env <- list()
  remembered <- list()

  hooks <- list(
    menu = function(title, choices) {
      answer <- menu_answers[[1]]
      menu_answers <<- menu_answers[-1]
      answer
    },
    input = function(prompt, default = NULL) {
      answer <- input_answers[[1]]
      input_answers <<- input_answers[-1]
      answer %||% default
    },
    model_choices = function(provider, api_key = NULL, base_url = NULL) {
      expect_equal(provider, "openai")
      c("proxy-model", "backup-model")
    },
    detect_endpoint = function(base_url, api_key = NULL, model = NULL, api_format = NULL) {
      detect_calls[[length(detect_calls) + 1L]] <<- list(model = model, api_format = api_format)
      list(
        api_format = api_format %||% "chat_completions",
        enable_stream_options = FALSE,
        supports_native_tools = FALSE,
        responses_state_mode = "auto",
        detected = TRUE
      )
    },
    save = function(updates, path) {
      saved_env <<- list(updates = updates, path = path)
      TRUE
    },
    remember_model = function(model_id, path = NULL) {
      remembered <<- list(model_id = model_id, path = path)
      model_id
    }
  )

  model_id <- aisdk:::prompt_console_provider_profile(
    project_path = project_path,
    global_path = global_path,
    project_config_path = project_config_path,
    project_rprofile_path = "project-custom.Rprofile",
    global_rprofile_path = "global-custom.Rprofile",
    prompt_hooks = hooks
  )

  cfg <- yaml::read_yaml(project_config_path)
  expect_equal(model_id, "proxycorp:proxy-model")
  # Two detection phases: format first (no model), then capabilities.
  expect_length(detect_calls, 2)
  expect_null(detect_calls[[1]]$model)
  expect_equal(detect_calls[[2]]$model, "proxy-model")
  expect_equal(detect_calls[[2]]$api_format, "chat_completions")
  expect_equal(cfg$default_model, "proxycorp:proxy-model")
  expect_equal(cfg$model_providers$proxycorp$wire_api, "chat_completions")
  expect_true(cfg$model_providers$proxycorp$disable_stream_options)
  expect_false(cfg$model_providers$proxycorp$supports_native_tools)
  expect_equal(cfg$model_providers$proxycorp$api_key_env, "AISDK_PROXYCORP_API_KEY")
  expect_equal(saved_env$updates$AISDK_PROXYCORP_API_KEY, "sk-custom")
  expect_equal(saved_env$path, project_path)
  expect_equal(remembered$model_id, "proxycorp:proxy-model")
})

test_that("prompt_console_provider_profile records detected Responses format with auto state", {
  project_path <- tempfile(fileext = ".Renviron")
  global_path <- tempfile(fileext = ".Renviron")
  project_config_path <- tempfile(fileext = ".yaml")
  registry_env <- get(".registry_env", envir = asNamespace("aisdk"))
  old_registry <- registry_env$default %||% NULL
  registry_env$default <- ProviderRegistry$new()
  on.exit({
    registry_env$default <- old_registry
  }, add = TRUE)

  menu_answers <- c(
    10L, # provider: Custom API
    1L,  # select first fetched/static model
    2L   # save to this project
  )
  input_answers <- c(
    "https://proxy.example.com/v1",
    "sk-custom",
    "yulab"
  )
  saved_env <- list()

  hooks <- list(
    menu = function(title, choices) {
      answer <- menu_answers[[1]]
      menu_answers <<- menu_answers[-1]
      answer
    },
    input = function(prompt, default = NULL) {
      answer <- input_answers[[1]]
      input_answers <<- input_answers[-1]
      answer %||% default
    },
    model_choices = function(provider, api_key = NULL, base_url = NULL) {
      c("gpt-5.5")
    },
    detect_endpoint = function(base_url, api_key = NULL, model = NULL, api_format = NULL) {
      list(
        api_format = "responses",
        enable_stream_options = FALSE,
        supports_native_tools = TRUE,
        responses_state_mode = "auto",
        detected = TRUE
      )
    },
    save = function(updates, path) {
      saved_env <<- list(updates = updates, path = path)
      TRUE
    },
    remember_model = function(model_id, path = NULL) model_id
  )

  model_id <- aisdk:::prompt_console_provider_profile(
    project_path = project_path,
    global_path = global_path,
    project_config_path = project_config_path,
    project_rprofile_path = "project-custom-responses.Rprofile",
    global_rprofile_path = "global-custom-responses.Rprofile",
    prompt_hooks = hooks
  )

  cfg <- yaml::read_yaml(project_config_path)
  expect_equal(model_id, "yulab:gpt-5.5")
  expect_equal(cfg$model_providers$yulab$wire_api, "responses")
  expect_equal(cfg$model_providers$yulab$responses_state_mode, "auto")
  expect_true(cfg$model_providers$yulab$supports_native_tools)
  expect_equal(saved_env$updates$AISDK_YULAB_API_KEY, "sk-custom")
})

test_that("prompt_console_provider_profile can edit dynamic YAML custom providers", {
  tmp <- tempfile("aisdk-config-")
  dir.create(tmp)
  project_path <- file.path(tmp, ".Renviron")
  global_path <- tempfile(fileext = ".Renviron")
  project_config_path <- file.path(tmp, "aisdk.yaml")

  writeLines(c(
    "default_model: yulab:gpt-5.5",
    "model_providers:",
    "  yulab:",
    "    type: custom",
    "    name: openai-yulab",
    "    base_url: https://proxy.example.com/v1",
    "    wire_api: responses",
    "    responses_state_mode: server",
    "    api_key_env: AISDK_YULAB_API_KEY",
    "models:",
    "  yulab:gpt-5.5:",
    "    reasoning_effort: xhigh"
  ), project_config_path)

  Sys.setenv(AISDK_YULAB_API_KEY = "sk-existing")
  on.exit(Sys.unsetenv("AISDK_YULAB_API_KEY"), add = TRUE)

  menu_answers <- c(
    2L, # edit saved setup
    1L, # edit first profile
    1L, # keep API key
    1L, # keep model
    1L  # update saved setup
  )
  input_answers <- list(
    NULL, # base URL: Enter keeps the saved one
    NULL  # setup name: Enter keeps "yulab"
  )
  saved_env <- list()
  remembered <- list()

  hooks <- list(
    menu = function(title, choices) {
      answer <- menu_answers[[1]]
      menu_answers <<- menu_answers[-1]
      answer
    },
    input = function(prompt, default = NULL) {
      answer <- input_answers[[1]]
      input_answers <<- input_answers[-1]
      answer %||% default
    },
    model_choices = function(provider, api_key = NULL, base_url = NULL) {
      c("gpt-5.5")
    },
    detect_endpoint = function(base_url, api_key = NULL, model = NULL, api_format = NULL) {
      # Saved wire format is passed through, so only capabilities run.
      expect_equal(api_format, "responses")
      list(
        api_format = "responses",
        enable_stream_options = FALSE,
        supports_native_tools = FALSE,
        responses_state_mode = "auto",
        detected = TRUE
      )
    },
    save = function(updates, path) {
      saved_env <<- list(updates = updates, path = path)
      TRUE
    },
    remember_model = function(model_id, path = NULL) {
      remembered <<- list(model_id = model_id, path = path)
      model_id
    }
  )

  model_id <- aisdk:::prompt_console_provider_profile(
    project_path = project_path,
    global_path = global_path,
    project_config_path = project_config_path,
    global_config_path = tempfile(fileext = ".yaml"),
    project_rprofile_path = "project-yaml.Rprofile",
    global_rprofile_path = "global-yaml.Rprofile",
    prompt_hooks = hooks
  )

  cfg <- yaml::read_yaml(project_config_path)
  expect_equal(model_id, "yulab:gpt-5.5")
  expect_equal(cfg$default_model, "yulab:gpt-5.5")
  expect_equal(cfg$model_providers$yulab$base_url, "https://proxy.example.com/v1")
  # The saved state mode wins over the detected default.
  expect_equal(cfg$model_providers$yulab$responses_state_mode, "server")
  expect_equal(saved_env$updates$AISDK_YULAB_API_KEY, "sk-existing")
  expect_equal(saved_env$path, project_path)
  expect_equal(remembered$model_id, "yulab:gpt-5.5")
})

test_that("prompt_console_provider_profile supports custom API setup without API key", {
  project_path <- tempfile(fileext = ".Renviron")
  global_path <- tempfile(fileext = ".Renviron")
  project_config_path <- tempfile(fileext = ".yaml")
  registry_env <- get(".registry_env", envir = asNamespace("aisdk"))
  old_registry <- registry_env$default %||% NULL
  registry_env$default <- ProviderRegistry$new()
  on.exit({
    registry_env$default <- old_registry
  }, add = TRUE)

  menu_answers <- c(
    10L, # provider: Custom API
    1L,  # select first fetched/static model
    2L   # save to this project
  )
  input_answers <- list(
    "http://172.16.153.230:8010/v1",
    NULL,   # no API key
    "lanlab" # setup name (IP hosts fall back to "custom" suggestion)
  )
  saved_env <- list()
  remembered <- list()

  hooks <- list(
    menu = function(title, choices) {
      answer <- menu_answers[[1]]
      menu_answers <<- menu_answers[-1]
      answer
    },
    input = function(prompt, default = NULL) {
      answer <- input_answers[[1]]
      input_answers <<- input_answers[-1]
      answer %||% default
    },
    model_choices = function(provider, api_key = NULL, base_url = NULL) {
      expect_equal(provider, "openai")
      expect_equal(api_key, "")
      expect_equal(base_url, "http://172.16.153.230:8010/v1")
      c("qwen3-8b-dflash")
    },
    detect_endpoint = function(base_url, api_key = NULL, model = NULL, api_format = NULL) {
      list(
        api_format = "chat_completions",
        enable_stream_options = TRUE,
        supports_native_tools = TRUE,
        responses_state_mode = "auto",
        detected = TRUE
      )
    },
    save = function(updates, path) {
      saved_env <<- list(updates = updates, path = path)
      TRUE
    },
    remember_model = function(model_id, path = NULL) {
      remembered <<- list(model_id = model_id, path = path)
      model_id
    }
  )

  model_id <- aisdk:::prompt_console_provider_profile(
    project_path = project_path,
    global_path = global_path,
    project_config_path = project_config_path,
    project_rprofile_path = "project-custom-no-key.Rprofile",
    global_rprofile_path = "global-custom-no-key.Rprofile",
    prompt_hooks = hooks
  )

  cfg <- yaml::read_yaml(project_config_path)
  expect_equal(model_id, "lanlab:qwen3-8b-dflash")
  expect_equal(cfg$model_providers$lanlab$base_url, "http://172.16.153.230:8010/v1")
  expect_equal(cfg$model_providers$lanlab$wire_api, "chat_completions")
  expect_false(cfg$model_providers$lanlab$disable_stream_options)
  expect_true(cfg$model_providers$lanlab$supports_native_tools)
  expect_false(cfg$model_providers$lanlab$requires_openai_auth)
  # No API key: nothing to persist into .Renviron.
  expect_length(saved_env, 0)
  expect_equal(remembered$model_id, "lanlab:qwen3-8b-dflash")
})

# --- Automatic endpoint detection --------------------------------------------

test_that("detect_console_endpoint_profile detects the wire format by probing routes", {
  calls <- list()
  fake_probe <- function(url, headers, body) {
    calls[[length(calls) + 1L]] <<- url
    if (grepl("/chat/completions$", url)) {
      return(list(status = 404L, text = "invalid_api_path: no such route"))
    }
    if (grepl("/responses$", url)) {
      return(list(status = 400L, text = "model not found"))
    }
    list(status = 404L, text = "no such path")
  }

  detected <- aisdk:::detect_console_endpoint_profile(
    "https://proxy.example.com/v1",
    api_key = "sk-x",
    probe_fn = fake_probe
  )

  expect_equal(detected$api_format, "responses")
  expect_true(detected$detected)
  expect_equal(detected$responses_state_mode, "auto")
  expect_length(calls, 2)
})

test_that("detect_console_endpoint_profile assumes Anthropic format from the URL", {
  detected <- aisdk:::detect_console_endpoint_profile(
    "https://api.anthropic.com/v1",
    api_key = "sk-x",
    probe_fn = function(url, headers, body) stop("no probe expected for format")
  )
  expect_equal(detected$api_format, "anthropic_messages")
  expect_true(detected$detected)
})

test_that("detect_console_endpoint_profile falls back safely when unreachable", {
  detected <- aisdk:::detect_console_endpoint_profile(
    "https://unreachable.example/v1",
    api_key = "sk-x",
    probe_fn = function(url, headers, body) stop("connection refused")
  )
  expect_equal(detected$api_format, "chat_completions")
  expect_false(detected$detected)
  expect_false(detected$enable_stream_options)
  expect_false(detected$supports_native_tools)
})

test_that("detect_console_chat_capabilities narrows flags from error messages", {
  bodies <- list()
  fake_probe <- function(url, headers, body) {
    bodies[[length(bodies) + 1L]] <<- body
    if (!is.null(body$stream_options)) {
      return(list(status = 400L, text = "stream_options is not supported"))
    }
    list(status = 200L, text = "{}")
  }

  caps <- aisdk:::detect_console_chat_capabilities(
    "https://proxy.example.com/v1", "sk-x", "proxy-model",
    probe_fn = fake_probe
  )

  expect_false(caps$enable_stream_options)
  expect_true(caps$supports_native_tools)
  expect_true(caps$detected)
  expect_length(bodies, 2)
  expect_null(bodies[[2]]$stream_options)
  expect_false(is.null(bodies[[2]]$tools))
})

test_that("detect_console_chat_capabilities is inconclusive on unrelated errors", {
  caps <- aisdk:::detect_console_chat_capabilities(
    "https://proxy.example.com/v1", "bad-key", "proxy-model",
    probe_fn = function(url, headers, body) list(status = 401L, text = "invalid api key")
  )
  expect_false(caps$enable_stream_options)
  expect_false(caps$supports_native_tools)
  expect_false(caps$detected)
})

test_that("console_probe_route_exists distinguishes missing routes from unknown models", {
  expect_true(aisdk:::console_probe_route_exists(list(status = 401L, text = "unauthorized")))
  expect_true(aisdk:::console_probe_route_exists(list(status = 400L, text = "bad request")))
  expect_true(aisdk:::console_probe_route_exists(
    list(status = 404L, text = "The model `aisdk-setup-probe` does not exist (model_not_found)")
  ))
  expect_false(aisdk:::console_probe_route_exists(
    list(status = 404L, text = "invalid_api_path: unknown route /v1/chat/completions")
  ))
  expect_false(aisdk:::console_probe_route_exists(list(status = 405L, text = "method not allowed")))
})

test_that("console_suggest_custom_provider_id derives ids from hosts", {
  # "moonshot" is a built-in provider id, so the suggestion gets suffixed.
  expect_equal(aisdk:::console_suggest_custom_provider_id("https://api.moonshot.cn/v1"), "moonshot-api")
  expect_equal(aisdk:::console_suggest_custom_provider_id("https://proxy.example.com/v1"), "proxy")
  expect_equal(aisdk:::console_suggest_custom_provider_id("http://172.16.153.230:8010/v1"), "custom")
  # Collides with a built-in provider id -> suffixed.
  expect_equal(aisdk:::console_suggest_custom_provider_id("https://api.openai.internal/v1"), "openai-api")
  expect_equal(aisdk:::console_suggest_custom_provider_id(""), "custom")
})
