# Tests for SharedSession, FlowStack, and Standard Agents
# Run with: testthat::test_file("tests/testthat/test-agent-system.R")

openai_model_id <- get_openai_model_id()

test_that("SharedSession initializes correctly", {
  session <- SharedSession$new(
    model = openai_model_id,
    sandbox_mode = "strict",
    trace_enabled = TRUE
  )


  expect_s3_class(session, "SharedSession")
  expect_s3_class(session, "ChatSession")
  expect_equal(session$get_sandbox_mode(), "strict")
})

test_that("SharedSession variable scoping works", {
  session <- SharedSession$new()

  # Set variables in different scopes

  session$set_var("x", 10, scope = "global")
  session$set_var("y", 20, scope = "agent1")
  session$set_var("z", 30, scope = "agent2")

  # Retrieve variables
  expect_equal(session$get_var("x", scope = "global"), 10)
  expect_equal(session$get_var("y", scope = "agent1"), 20)
  expect_equal(session$get_var("z", scope = "agent2"), 30)

  # Default scope is global
  expect_equal(session$get_var("x"), 10)

  # Non-existent variable returns default
  expect_null(session$get_var("nonexistent"))
  expect_equal(session$get_var("nonexistent", default = 99), 99)
})

test_that("SharedSession code execution works", {
  session <- SharedSession$new(sandbox_mode = "permissive")

  # Execute simple code
  result <- session$execute_code("x <- 1 + 1; x")
  expect_true(result$success)
  expect_equal(result$result, 2)

  # Variable persists in scope
  expect_equal(session$get_var("x"), 2)

  # Execute with output capture
  result <- session$execute_code("print('hello')")
  expect_true(result$success)
  expect_true(any(grepl("hello", result$output)))
})

test_that("SharedSession sandbox blocks dangerous code", {
  session <- SharedSession$new(sandbox_mode = "strict")

  # System calls should be blocked
  result <- session$execute_code("system('ls')")
  expect_false(result$success)
  expect_true(grepl("Sandbox violation", result$error))

  # File operations should be blocked in strict mode
  result <- session$execute_code("writeLines('test', 'file.txt')")
  expect_false(result$success)
})

test_that("SharedSession execution context tracking works", {
  session <- SharedSession$new(trace_enabled = TRUE)

  # Push context
  session$push_context("Agent1", "Task 1", NULL)
  ctx <- session$get_context()
  expect_equal(ctx$current_agent, "Agent1")
  expect_equal(ctx$depth, 1)

  # Push nested context
  session$push_context("Agent2", "Task 2", "Agent1")
  ctx <- session$get_context()
  expect_equal(ctx$current_agent, "Agent2")
  expect_equal(ctx$depth, 2)

  # Pop context
  popped <- session$pop_context("result")
  expect_equal(popped$agent, "Agent2")
  ctx <- session$get_context()
  expect_equal(ctx$current_agent, "Agent1")
  expect_equal(ctx$depth, 1)
})

test_that("SharedSession tracing works", {
  session <- SharedSession$new(trace_enabled = TRUE)

  # Execute some operations
  session$push_context("TestAgent", "Test task")
  session$execute_code("x <- 1")
  session$set_var("y", 2)
  session$pop_context()

  # Check trace
  trace <- session$get_trace()
  expect_true(length(trace) > 0)

  # Filter by event type
  context_events <- session$get_trace(event_types = "context_push")
  expect_true(all(sapply(context_events, function(e) e$type == "context_push")))

  # Get summary
  summary <- session$trace_summary()
  expect_true(summary$total_events > 0)
})

test_that("FlowStack initializes correctly", {
  session <- SharedSession$new()
  registry <- AgentRegistry$new()

  flow <- FlowStack$new(
    session = session,
    model = openai_model_id,
    registry = registry,
    max_depth = 5,
    enable_guardrails = TRUE
  )

  expect_s3_class(flow, "FlowStack")
  expect_s3_class(flow, "Flow")
  expect_equal(flow$depth(), 0)
})

test_that("FlowStack generates unified delegate tool", {
  session <- SharedSession$new()

  # Create test agents
  agent1 <- Agent$new(
    name = "TestAgent1",
    description = "Test agent 1"
  )
  agent2 <- Agent$new(
    name = "TestAgent2",
    description = "Test agent 2"
  )

  registry <- AgentRegistry$new(list(agent1, agent2))

  flow <- FlowStack$new(
    session = session,
    model = openai_model_id,
    registry = registry
  )

  # Generate delegate tool
  delegate_tool <- flow$generate_delegate_tool()

  expect_s3_class(delegate_tool, "Tool")
  expect_equal(delegate_tool$name, "delegate_task")
  expect_true(grepl("TestAgent1", delegate_tool$description))
  expect_true(grepl("TestAgent2", delegate_tool$description))
})

test_that("FlowStack delegation history tracking works", {
  session <- SharedSession$new()
  registry <- AgentRegistry$new()

  flow <- FlowStack$new(
    session = session,
    model = openai_model_id,
    registry = registry
  )

  # Initially empty
  expect_equal(length(flow$get_delegation_history()), 0)

  # Stats should show zero
  stats <- flow$delegation_stats()
  expect_equal(stats$total_delegations, 0)
})

test_that("DataAgent creates correctly", {
  agent <- create_data_agent()

  expect_s3_class(agent, "Agent")
  expect_equal(agent$name, "DataAgent")
  expect_true(length(agent$tools) >= 3)

  # Check tool names
  tool_names <- sapply(agent$tools, function(t) t$name)
  expect_true("transform_data" %in% tool_names)
  expect_true("summarize_data" %in% tool_names)
  expect_true("list_data" %in% tool_names)
})

test_that("FileAgent creates with safety constraints", {
  agent <- create_file_agent(
    allowed_dirs = c("./data", "./output"),
    allowed_extensions = c("csv", "json")
  )

  expect_s3_class(agent, "Agent")
  expect_equal(agent$name, "FileAgent")

  # Check tool names
  tool_names <- sapply(agent$tools, function(t) t$name)
  expect_true("read_file" %in% tool_names)
  expect_true("write_file" %in% tool_names)
  expect_true("list_files" %in% tool_names)
})

test_that("EnvAgent creates with install control", {
  # Without install permission
  agent_no_install <- create_env_agent(allow_install = FALSE)
  expect_s3_class(agent_no_install, "Agent")

  # With install permission
  agent_with_install <- create_env_agent(allow_install = TRUE)
  expect_s3_class(agent_with_install, "Agent")
})

test_that("Enhanced CoderAgent has debug tools", {
  agent <- create_coder_agent(safe_mode = TRUE)

  expect_s3_class(agent, "Agent")

  # Check for enhanced tools
  tool_names <- sapply(agent$tools, function(t) t$name)
  expect_true("execute_r_code" %in% tool_names)
  expect_true("list_session_variables" %in% tool_names)
  expect_true("inspect_variable" %in% tool_names)
  expect_true("debug_error" %in% tool_names)
})

test_that("Enhanced VisualizerAgent has recommendation tools", {
  agent <- create_visualizer_agent()

  expect_s3_class(agent, "Agent")

  # Check for enhanced tools
  tool_names <- sapply(agent$tools, function(t) t$name)
  expect_true("create_ggplot" %in% tool_names)
  expect_true("inspect_data" %in% tool_names)
  expect_true("recommend_plot" %in% tool_names)
  expect_true("list_plots" %in% tool_names)
})

test_that("Feature flags work correctly", {
  # Reset to defaults
  sdk_reset_features()

  # Check defaults
  expect_true(sdk_feature("use_shared_session"))
  expect_true(sdk_feature("use_flow_stack"))
  expect_false(sdk_feature("legacy_tool_format"))

  # Set a flag
  old_value <- sdk_set_feature("use_shared_session", FALSE)
  expect_true(old_value)
  expect_false(sdk_feature("use_shared_session"))

  # Reset
  sdk_reset_features()
  expect_true(sdk_feature("use_shared_session"))
})

test_that("create_session respects feature flags", {
  sdk_reset_features()

  # With shared session enabled (default)
  session1 <- create_session(model = openai_model_id)
  expect_s3_class(session1, "SharedSession")

  # With shared session disabled
  sdk_set_feature("use_shared_session", FALSE)
  session2 <- create_session(model = openai_model_id)
  expect_s3_class(session2, "ChatSession")
  expect_false(inherits(session2, "SharedSession"))

  sdk_reset_features()
})

test_that("create_standard_registry creates all agents", {
  registry <- create_standard_registry()

  agent_names <- registry$list_agents()
  expect_true("DataAgent" %in% agent_names)
  expect_true("FileAgent" %in% agent_names)
  expect_true("EnvAgent" %in% agent_names)
  expect_true("CoderAgent" %in% agent_names)
  expect_true("VisualizerAgent" %in% agent_names)
  expect_true("PlannerAgent" %in% agent_names)
})

test_that("create_standard_registry respects include flags", {
  registry <- create_standard_registry(
    include_data = FALSE,
    include_file = FALSE,
    include_env = FALSE
  )

  agent_names <- registry$list_agents()
  expect_false("DataAgent" %in% agent_names)
  expect_false("FileAgent" %in% agent_names)
  expect_false("EnvAgent" %in% agent_names)
  expect_true("CoderAgent" %in% agent_names)
})

test_that("Migration utilities work", {
  # Check compatibility
  result <- check_sdk_compatibility("0.8.0")
  expect_false(result$compatible)
  expect_true(length(result$suggestions) > 0)

  # Get migration pattern
  guidance <- migrate_pattern("ChatSession")
  expect_true(nzchar(guidance$old_pattern))
  expect_true(nzchar(guidance$new_pattern))
  expect_true(nzchar(guidance$example))

  # Unknown pattern
  unknown <- migrate_pattern("UnknownPattern")
  expect_true(grepl("not found", unknown$example))
})

test_that("Tool execution with session environment works", {
  session <- SharedSession$new(sandbox_mode = "permissive")

  # Create a simple tool that uses session environment
  test_tool <- Tool$new(
    name = "test_tool",
    description = "Test tool",
    parameters = z_object(
      value = z_number("A number")
    ),
    execute = function(args) {
      env <- args$.envir
      if (!is.null(env)) {
        assign("tool_result", args$value * 2, envir = env)
        return(paste0("Stored ", args$value * 2))
      }
      "No environment"
    }
  )

  # Execute tool with session environment
  env <- session$get_var(".envir", scope = "global")
  if (is.null(env)) {
    # Create scope environment
    session$create_scope("test")
    env <- new.env()
  }

  result <- test_tool$run(list(value = 21), envir = env)
  expect_true(grepl("42", result))
})

test_that("Agent as_tool conversion works", {
  agent <- Agent$new(
    name = "TestAgent",
    description = "A test agent for unit testing"
  )

  tool <- agent$as_tool()

  expect_s3_class(tool, "Tool")
  expect_equal(tool$name, "delegate_to_TestAgent")
  expect_true(grepl("TestAgent", tool$description))
})

test_that("AgentRegistry generates delegate tools", {
  agent1 <- Agent$new(name = "Agent1", description = "First agent")
  agent2 <- Agent$new(name = "Agent2", description = "Second agent")

  registry <- AgentRegistry$new(list(agent1, agent2))

  # Generate tools
  tools <- registry$generate_delegate_tools()

  expect_equal(length(tools), 2)
  tool_names <- sapply(tools, function(t) t$name)
  expect_true("delegate_to_Agent1" %in% tool_names)
  expect_true("delegate_to_Agent2" %in% tool_names)
})

test_that("AgentRegistry generates prompt section", {
  agent1 <- Agent$new(name = "Agent1", description = "First agent")
  agent2 <- Agent$new(name = "Agent2", description = "Second agent")

  registry <- AgentRegistry$new(list(agent1, agent2))

  prompt <- registry$generate_prompt_section()

  expect_true(grepl("AVAILABLE AGENTS", prompt))
  expect_true(grepl("Agent1", prompt))
  expect_true(grepl("Agent2", prompt))
})
