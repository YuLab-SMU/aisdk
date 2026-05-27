test_that("collect_execution_log captures tool execution from generation results", {
  generation_result <- list(
    all_tool_calls = list(
      list(id = "1", name = "execute_r_code", arguments = list(code = "x <- 1 + 1")),
      list(id = "2", name = "bash", arguments = list(command = "ls -la"))
    ),
    all_tool_results = list(
      list(id = "1", name = "execute_r_code", result = "2", is_error = FALSE),
      list(id = "2", name = "bash", result = "Error: command not found", is_error = TRUE)
    )
  )

  log <- aisdk:::collect_execution_log(session = NULL, generation_result = generation_result)

  expect_equal(length(log), 2)
  expect_equal(log[[1]]$command, "x <- 1 + 1")
  expect_equal(log[[1]]$tool, "execute_r_code")
  expect_equal(log[[1]]$exit_code, 0L)
  expect_equal(log[[2]]$command, "ls -la")
  expect_equal(log[[2]]$tool, "bash")
  expect_equal(log[[2]]$exit_code, 1L)
  expect_equal(log[[2]]$error, "Error: command not found")
})

test_that("collect_execution_log respects max_entries limit", {
  generation_result <- list(
    all_tool_calls = lapply(1:15, function(i) {
      list(id = as.character(i), name = "execute_r_code", arguments = list(code = sprintf("x <- %d", i)))
    }),
    all_tool_results = lapply(1:15, function(i) {
      list(id = as.character(i), name = "execute_r_code", result = as.character(i), is_error = FALSE)
    })
  )

  log <- aisdk:::collect_execution_log(session = NULL, generation_result = generation_result, max_entries = 10L)

  expect_equal(length(log), 10)
  expect_equal(log[[1]]$command, "x <- 6")
  expect_equal(log[[10]]$command, "x <- 15")
})

test_that("collect_system_info returns OS and R version info", {
  info <- aisdk:::collect_system_info()

  expect_true(is.list(info))
  expect_true(!is.null(info$os))
  expect_true(!is.null(info$os$type))
  expect_true(!is.null(info$r_version))
  expect_true(!is.null(info$r_platform))
  expect_true(!is.null(info$working_dir))
  expect_true(!is.null(info$cpu))
  expect_true(!is.null(info$cpu$cores))
})

test_that("collect_runtime_state returns connections, devices, options, and packages", {
  state <- aisdk:::collect_runtime_state(session = NULL)

  expect_true(is.list(state))
  expect_true(!is.null(state$connections))
  expect_true(!is.null(state$graphics_devices))
  expect_true(!is.null(state$options))
  expect_true(!is.null(state$search_path))
  expect_true(!is.null(state$loaded_packages))
  expect_true(!is.null(state$env_vars))
  expect_true(is.list(state$options))
  expect_true(is.character(state$search_path))
  expect_true(is.character(state$loaded_packages))
})

test_that("synthesize_context_state populates execution_log, system_info, and runtime_state", {
  mock_model <- MockModel$new()
  mock_model$add_response(
    tool_calls = list(list(
      id = "call_1",
      name = "execute_r_code",
      arguments = list(code = "x <- 1 + 1")
    ))
  )
  mock_model$add_response(text = "done")

  session <- aisdk::create_chat_session(
    model = mock_model,
    tools = create_computer_tools(working_dir = tempdir(), sandbox_mode = "permissive"),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(mode = "basic"))

  session$send("execute code")
  state <- session$get_context_state()

  expect_true(length(state$execution_log) >= 1)
  expect_equal(state$execution_log[[1]]$tool, "execute_r_code")
  expect_true(!is.null(state$system_info))
  expect_true(!is.null(state$system_info$os))
  expect_true(!is.null(state$runtime_state))
  expect_true(!is.null(state$runtime_state$search_path))
})

test_that("assemble_session_messages can retrieve execution_monitor hits", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(
    mode = "basic",
    retrieval_providers = c("execution_monitor"),
    retrieval_provider_order = c("execution_monitor")
  ))
  session$set_context_state(list(
    execution_log = list(
      list(
        command = "x <- read.csv('missing.csv')",
        tool = "execute_r_code",
        error = "Error: file 'missing.csv' not found",
        exit_code = 1L,
        timestamp = as.character(Sys.time())
      )
    )
  ))
  session$append_message("user", "Why did the file read fail?")

  assembled <- session$assemble_messages()

  expect_match(assembled$system, "Ranked retrieval hits:", fixed = TRUE)
  expect_match(assembled$system, "execution_monitor", fixed = TRUE)
  expect_match(assembled$system, "Error: file 'missing.csv' not found", fixed = TRUE)
})

test_that("assemble_session_messages can retrieve system_info hits", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(
    mode = "basic",
    retrieval_providers = c("system_info"),
    retrieval_provider_order = c("system_info")
  ))
  session$set_context_state(list(
    system_info = list(
      os = list(type = "Linux", version = "5.10.0", platform = "x86_64-pc-linux-gnu"),
      r_version = "4.3.0",
      memory = list(total_mb = 16384, available_mb = 8192, used_percent = 50),
      cpu = list(cores = 8, model = "Intel Core i7")
    )
  ))
  session$append_message("user", "What is the system memory status?")

  assembled <- session$assemble_messages()

  expect_match(assembled$system, "Ranked retrieval hits:", fixed = TRUE)
  expect_match(assembled$system, "system_info", fixed = TRUE)
  expect_match(assembled$system, "Memory", fixed = TRUE)
})

test_that("assemble_session_messages can retrieve runtime_state hits", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(
    mode = "basic",
    retrieval_providers = c("runtime_state"),
    retrieval_provider_order = c("runtime_state")
  ))
  session$set_context_state(list(
    runtime_state = list(
      loaded_packages = c("ggplot2", "dplyr", "tidyr"),
      search_path = c(".GlobalEnv", "package:ggplot2", "package:dplyr"),
      options = list(width = 80, digits = 7)
    )
  ))
  session$append_message("user", "Which packages are loaded?")

  assembled <- session$assemble_messages()

  expect_match(assembled$system, "Ranked retrieval hits:", fixed = TRUE)
  expect_match(assembled$system, "runtime_state", fixed = TRUE)
  expect_match(assembled$system, "Loaded Packages", fixed = TRUE)
})

test_that("execution_monitor retrieval prioritizes errors over success", {
  session <- aisdk::create_chat_session(
    model = MockModel$new(),
    system_prompt = "Base system"
  )
  session$set_context_management_config(create_context_management_config(
    mode = "basic",
    retrieval_providers = c("execution_monitor"),
    retrieval_provider_order = c("execution_monitor"),
    retrieval_min_hits = list(execution_monitor = 1L)
  ))
  session$set_context_state(list(
    execution_log = list(
      list(
        command = "x <- 1 + 1",
        tool = "execute_r_code",
        result = "2",
        exit_code = 0L,
        timestamp = as.character(Sys.time())
      ),
      list(
        command = "y <- read.csv('missing.csv')",
        tool = "execute_r_code",
        error = "Error: file not found",
        exit_code = 1L,
        timestamp = as.character(Sys.time())
      )
    )
  ))
  session$append_message("user", "What errors occurred in execution?")

  assembled <- session$assemble_messages()

  expect_match(assembled$system, "Ranked retrieval hits:", fixed = TRUE)
  expect_match(assembled$system, "execution_monitor", fixed = TRUE)
  expect_match(assembled$system, "Error: file not found", fixed = TRUE)
})
