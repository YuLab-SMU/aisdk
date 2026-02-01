# Test Agent R6 class

test_that("Agent initializes correctly", {
  agent <- Agent$new(
    name = "TestAgent",
    description = "A test agent for unit testing",
    system_prompt = "You are a helpful test assistant."
  )

  expect_s3_class(agent, "Agent")
  expect_equal(agent$name, "TestAgent")
  expect_equal(agent$description, "A test agent for unit testing")
  expect_equal(agent$system_prompt, "You are a helpful test assistant.")
  expect_length(agent$tools, 0)
})

test_that("Agent requires name and description", {
  expect_error(
    Agent$new(description = "No name"),
    "name"
  )

  expect_error(
    Agent$new(name = "NoDesc"),
    "description"
  )

  expect_error(
    Agent$new(name = "", description = "Empty name"),
    "name"
  )
})

test_that("create_agent factory works", {
  agent <- create_agent(
    name = "FactoryAgent",
    description = "Created by factory function",
    system_prompt = "Test prompt"
  )

  expect_s3_class(agent, "Agent")
  expect_equal(agent$name, "FactoryAgent")
})

test_that("Agent initializes with tools", {
  # Create a mock tool
  mock_tool <- Tool$new(
    name = "mock_tool",
    description = "A mock tool",
    parameters = z_object(x = z_number("Input number")),
    execute = function(x) x * 2
  )

  agent <- Agent$new(
    name = "ToolAgent",
    description = "Agent with tools",
    tools = list(mock_tool)
  )

  expect_length(agent$tools, 1)
  expect_equal(agent$tools[[1]]$name, "mock_tool")
})

test_that("Agent can be converted to a tool", {
  agent <- Agent$new(
    name = "Greeter",
    description = "Says hello to people"
  )

  delegate_tool <- agent$as_tool()

  expect_s3_class(delegate_tool, "Tool")
  expect_equal(delegate_tool$name, "delegate_to_Greeter")
  expect_true(grepl("Greeter", delegate_tool$description))
  expect_true(grepl("Says hello", delegate_tool$description))
})

test_that("Agent print method works", {
  agent <- Agent$new(
    name = "PrintAgent",
    description = "Agent for testing print method"
  )

  expect_output(print(agent), "Agent")
  expect_output(print(agent), "PrintAgent")
})

test_that("Agent builds system prompt with context", {
  agent <- Agent$new(
    name = "ContextAgent",
    description = "Tests context injection",
    system_prompt = "You are a base assistant."
  )

  # Access private method for testing
  env <- agent$.__enclos_env__
  build_prompt <- env$private$build_system_prompt

  # Without context
  prompt1 <- build_prompt()
  expect_equal(prompt1, "You are a base assistant.")

  # With context
  prompt2 <- build_prompt(context = "Working for Manager. Task: Clean data.")
  expect_true(grepl("base assistant", prompt2))
  expect_true(grepl("CURRENT CONTEXT", prompt2))
  expect_true(grepl("Clean data", prompt2))
})
