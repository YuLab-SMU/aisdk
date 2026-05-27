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
    "AISDK_CUSTOM_MODEL=proxy-model"
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

test_that("console status line includes context metrics when available", {
  session <- aisdk::create_chat_session(model = "openai:gpt-5-mini")
  session$append_message("user", "hello world")
  app_state <- aisdk:::create_console_app_state(session, view_mode = "clean")

  line <- aisdk:::build_console_status_line(app_state)

  expect_match(line, "Model: openai:gpt-5-mini")
  expect_match(line, "Ctx: 400.0k")
  expect_match(line, "Used:")
  expect_match(line, "Left:")
  expect_match(line, "Out: 128.0k")
})

test_that("console status line shows DeepSeek V4 million-token context", {
  session <- aisdk::create_chat_session(model = "deepseek:deepseek-v4-flash")
  session$append_message("user", "hello world")
  app_state <- aisdk:::create_console_app_state(session, view_mode = "clean")

  line <- aisdk:::build_console_status_line(app_state)

  expect_match(line, "Model: deepseek:deepseek-v4-flash")
  expect_match(line, "Ctx: 1.0M")
  expect_match(line, "Out: 384.0k")
  expect_no_match(line, "Ctx\\(est\\): 64.0k")
})

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
    1L, # use default base url
    1L, # select first fetched/static model
    1L  # save to project
  )
  input_answers <- c("sk-manual")
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
    2L, # choose "Edit saved profile"
    1L, # edit the first saved profile
    3L, # enter custom base url
    2L, # enter new API key
    2L, # choose second model option
    1L  # overwrite existing profile
  )
  input_answers <- c(
    "https://proxy.example.com/v1",
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

test_that("prompt_console_provider_profile supports custom API setup", {
  project_path <- tempfile(fileext = ".Renviron")
  global_path <- tempfile(fileext = ".Renviron")

  menu_answers <- c(
    10L, # provider: Custom API
    1L,  # api format: OpenAI Chat Completions
    1L,  # compatibility: basic text fallback
    1L,  # select first fetched/static model
    1L   # save to project
  )
  input_answers <- c(
    "https://proxy.example.com/v1",
    "sk-custom"
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
      c("proxy-model", "backup-model")
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
    project_rprofile_path = "project-custom.Rprofile",
    global_rprofile_path = "global-custom.Rprofile",
    prompt_hooks = hooks
  )

  expect_equal(model_id, "custom:proxy-model")
  expect_equal(saved$path, project_path)
  expect_equal(saved$updates$AISDK_CUSTOM_API_KEY, "sk-custom")
  expect_equal(saved$updates$AISDK_CUSTOM_BASE_URL, "https://proxy.example.com/v1")
  expect_equal(saved$updates$AISDK_CUSTOM_MODEL, "proxy-model")
  expect_equal(saved$updates$AISDK_CUSTOM_API_FORMAT, "chat_completions")
  expect_equal(saved$updates$AISDK_CUSTOM_ENABLE_STREAM_OPTIONS, "false")
  expect_equal(saved$updates$AISDK_CUSTOM_SUPPORTS_NATIVE_TOOLS, "false")
  expect_equal(remembered$model_id, "custom:proxy-model")
  expect_equal(remembered$path, "project-custom.Rprofile")
})

test_that("prompt_console_provider_profile can save custom API setup to aisdk.yaml", {
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
    1L,  # api format: OpenAI Chat Completions
    2L,  # compatibility: native tools, no stream_options
    1L,  # select first fetched/static model
    3L   # save project aisdk.yaml
  )
  input_answers <- c(
    "https://proxy.example.com/v1",
    "sk-custom",
    "yulab"
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
    project_rprofile_path = "project-yaml.Rprofile",
    global_rprofile_path = "global-yaml.Rprofile",
    prompt_hooks = hooks
  )

  cfg <- yaml::read_yaml(project_config_path)
  expect_equal(model_id, "yulab:gpt-5.5")
  expect_equal(cfg$default_model, "yulab:gpt-5.5")
  expect_equal(cfg$model_providers$yulab$base_url, "https://proxy.example.com/v1")
  expect_equal(cfg$model_providers$yulab$api_key_env, "AISDK_YULAB_API_KEY")
  expect_true(cfg$model_providers$yulab$supports_native_tools)
  expect_true(cfg$model_providers$yulab$disable_stream_options)
  expect_equal(saved_env$updates$AISDK_YULAB_API_KEY, "sk-custom")
  expect_equal(saved_env$path, project_path)
  expect_equal(remembered$model_id, "yulab:gpt-5.5")
  expect_null(registry_env$default)
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
    1L, # keep API format
    1L, # keep compatibility
    1L, # keep base URL
    1L, # keep API key
    1L, # keep model
    1L  # overwrite saved YAML
  )
  input_answers <- list(NULL)
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
  expect_equal(saved_env$updates$AISDK_YULAB_API_KEY, "sk-existing")
  expect_equal(saved_env$path, project_path)
  expect_equal(remembered$model_id, "yulab:gpt-5.5")
})

test_that("prompt_console_provider_profile supports custom API setup without API key", {
  project_path <- tempfile(fileext = ".Renviron")
  global_path <- tempfile(fileext = ".Renviron")

  menu_answers <- c(
    10L, # provider: Custom API
    1L,  # api format: OpenAI Chat Completions
    1L,  # compatibility: basic text fallback
    1L,  # select first fetched/static model
    1L   # save to project
  )
  input_answers <- list(
    "http://172.16.153.230:8010/v1",
    NULL
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
      expect_equal(provider, "openai")
      expect_equal(api_key, "")
      expect_equal(base_url, "http://172.16.153.230:8010/v1")
      c("qwen3-8b-dflash")
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
    project_rprofile_path = "project-custom-no-key.Rprofile",
    global_rprofile_path = "global-custom-no-key.Rprofile",
    prompt_hooks = hooks
  )

  expect_equal(model_id, "custom:qwen3-8b-dflash")
  expect_equal(saved$path, project_path)
  expect_equal(saved$updates$AISDK_CUSTOM_API_KEY, "")
  expect_equal(saved$updates$AISDK_CUSTOM_BASE_URL, "http://172.16.153.230:8010/v1")
  expect_equal(saved$updates$AISDK_CUSTOM_MODEL, "qwen3-8b-dflash")
  expect_equal(saved$updates$AISDK_CUSTOM_API_FORMAT, "chat_completions")
  expect_equal(saved$updates$AISDK_CUSTOM_ENABLE_STREAM_OPTIONS, "false")
  expect_equal(saved$updates$AISDK_CUSTOM_SUPPORTS_NATIVE_TOOLS, "false")
  expect_equal(remembered$model_id, "custom:qwen3-8b-dflash")
  expect_equal(remembered$path, "project-custom-no-key.Rprofile")
})
