test_that("create_console_agent creates valid agent", {
    agent <- create_console_agent()
    expect_s3_class(agent, "Agent")
    expect_equal(agent$name, "ConsoleAgent")
    expect_true(length(agent$tools) >= 8) # At least 8 tools (4 computer + 4 console)
})

test_that("create_console_tools includes all expected tools", {
    tools <- create_console_tools()
    tool_names <- sapply(tools, function(t) t$name)

    # Check computer tools

    expect_true("bash" %in% tool_names)
    expect_true("read_file" %in% tool_names)
    expect_true("write_file" %in% tool_names)
    expect_true("execute_r_code" %in% tool_names)

    # Check console-specific tools
    expect_true("list_directory" %in% tool_names)
    expect_true("find_files" %in% tool_names)
    expect_true("get_system_info" %in% tool_names)
    expect_true("get_environment" %in% tool_names)
})

test_that("list_directory tool works", {
    tools <- create_console_tools()
    list_dir_tool <- tools[[which(sapply(tools, function(t) t$name) == "list_directory")]]

    result <- list_dir_tool$run(list(path = "."))
    expect_true(grepl("Directory:", result))
    expect_true(grepl("items", result))
})

test_that("get_system_info tool works", {
    tools <- create_console_tools()
    sys_info_tool <- tools[[which(sapply(tools, function(t) t$name) == "get_system_info")]]

    result <- sys_info_tool$run(list())
    expect_true(grepl("System Information", result))
    expect_true(grepl("R Version:", result))
    expect_true(grepl("Working Directory:", result))
})

test_that("get_environment tool works", {
    tools <- create_console_tools()
    env_tool <- tools[[which(sapply(tools, function(t) t$name) == "get_environment")]]

    result <- env_tool$run(list(names = "HOME, R_HOME"))
    expect_true(grepl("HOME=", result))
    expect_true(grepl("R_HOME=", result))
})

test_that("get_environment masks sensitive values", {
    Sys.setenv(TEST_API_KEY = "sk-1234567890abcdef")
    on.exit(Sys.unsetenv("TEST_API_KEY"))

    tools <- create_console_tools()
    env_tool <- tools[[which(sapply(tools, function(t) t$name) == "get_environment")]]

    result <- env_tool$run(list(names = "TEST_API_KEY"))
    # Should be masked
    expect_false(grepl("sk-1234567890abcdef", result))
    expect_true(grepl("sk-1", result) || grepl("\\*\\*\\*\\*", result))
})

test_that("find_files tool works", {
    tools <- create_console_tools()
    find_tool <- tools[[which(sapply(tools, function(t) t$name) == "find_files")]]

    # Search for R files in current directory
    result <- find_tool$run(list(pattern = "*.R", path = ".", recursive = FALSE))
    # Should either find files or report "No files matching" - not an error
    expect_true(grepl("Found", result) || grepl("No files matching", result) || grepl("Directory not found", result))
})

test_that("console agent system prompt includes key elements", {
    agent <- create_console_agent()
    prompt <- agent$system_prompt

    expect_true(grepl("Terminal Assistant", prompt))
    expect_true(grepl("bash", prompt))
    expect_true(grepl("Working Directory", prompt))
    expect_true(grepl("Safety", prompt))
})
