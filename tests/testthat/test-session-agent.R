
test_that("ChatSession initializes from Agent", {
  # Create a mock agent
  agent <- Agent$new(
    name = "TestAgent",
    description = "A test agent",
    system_prompt = "You are a test agent.",
    tools = list(
      Tool$new(
        name = "test_tool", 
        description = "A test tool", 
        parameters = z_empty_object(description = "No parameters"),
        execute = function() "result"
      )
    )
  )

  # Initialize session from agent
  session <- ChatSession$new(agent = agent)
  
  # Check system prompt
  # Note: ChatSession$initialize merges agent prompt with session prompt (if any).
  # Since we didn't provide a session prompt, it should be just the agent's prompt.
  # Accessing private fields is tricky in R6 from outside, but we can check via behavior or if there's a getter.
  # ChatSession doesn't have a getter for system_prompt, but it has as_list()
  
  session_state <- session$as_list()
  expect_equal(session_state$system_prompt, "You are a test agent.")
  expect_equal(session_state$tool_names, c("test_tool"))
})

test_that("Agent$create_session returns valid session", {
  agent <- Agent$new(
    name = "TestAgent",
    description = "A test agent",
    system_prompt = "Agent Prompt"
  )
  
  session <- agent$create_session()
  expect_s3_class(session, "ChatSession")
  expect_equal(session$as_list()$system_prompt, "Agent Prompt")
})

test_that("Session from Agent maintains history", {
  # Setup mock model
  mock_model <- MockModel$new()
  mock_model$add_response(text = "Response 1")
  mock_model$add_response(text = "Response 2")
  
  agent <- Agent$new(
    name = "TestAgent",
    description = "A test agent",
    system_prompt = "Agent System Prompt"
  )
  
  session <- agent$create_session(model = mock_model)
  
  # First turn
  session$send("Hello")
  
  # Verify first call params
  expect_equal(length(mock_model$last_params$messages), 2) # System + User
  expect_equal(mock_model$last_params$messages[[1]]$role, "system")
  expect_equal(mock_model$last_params$messages[[1]]$content, "Agent System Prompt")
  expect_equal(mock_model$last_params$messages[[2]]$role, "user")
  expect_equal(mock_model$last_params$messages[[2]]$content, "Hello")
  
  # Second turn (what user "continue" scenario effectively is)
  session$send("Continue")
  
  # Verify second call params - should have history
  # System + User1 + Assistant1 + User2
  expect_equal(length(mock_model$last_params$messages), 4)
  expect_equal(mock_model$last_params$messages[[3]]$role, "assistant")
  expect_equal(mock_model$last_params$messages[[3]]$content, "Response 1")
  expect_equal(mock_model$last_params$messages[[4]]$role, "user")
  expect_equal(mock_model$last_params$messages[[4]]$content, "Continue")
})
