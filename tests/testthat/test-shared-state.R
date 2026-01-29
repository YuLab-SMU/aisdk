# Test Shared State: Session Environment Coupling for Multi-Agent Data Sharing




# --- Mock Model Helper ---

MockModel <- R6::R6Class("MockModel",
  inherit = LanguageModelV1,
  public = list(
    provider = "mock",
    model_id = "mock-model",
    responses = list(),
    last_params = NULL, # Added to capture parameters

    initialize = function(responses = list()) {
      self$responses <- responses
    },

    do_generate = function(params) {
      self$last_params <- params # Capture params
      
      if (length(self$responses) == 0) {
        return(list(text = "Mock response", tool_calls = NULL))
      }
      
      # Pop the first response
      resp <- self$responses[[1]]
      self$responses <- self$responses[-1]
      
      # Allow response to be a function of params
      if (is.function(resp)) {
        return(resp(params))
      }
      
      return(resp)
    },
    
    add_response = function(text = NULL, tool_calls = NULL) {
      self$responses <- c(self$responses, list(list(
        text = text,
        tool_calls = tool_calls,
        finish_reason = "stop",
        usage = list(total_tokens = 10)
      )))
    },
    
    format_tool_result = function(tool_call_id, tool_name, result) {
      list(
        role = "tool",
        tool_call_id = tool_call_id,
        name = tool_name,
        content = result
      )
    }
  )
)

# --- Tests: Tool Environment Parameter ---

test_that("Tool$run passes .envir in args", {
  # A tool that reads from environment via .envir
  reader_tool <- Tool$new(
    name = "read_x",
    description = "Read variable x from environment",
    parameters = z_object(.dummy = z_string("Unused")),
    execute = function(args) {
      if (!is.null(args$.envir) && exists("x", envir = args$.envir)) {
        get("x", envir = args$.envir)
      } else {
        "x not found"
      }
    }
  )
  
  # Create test environment
  test_env <- new.env()
  test_env$x <- 42
  
  # Without envir - should not find x
  result_without <- reader_tool$run(list())
  expect_equal(result_without, "x not found")
  
  # With envir - should find x via .envir
  result_with <- reader_tool$run(list(), envir = test_env)
  expect_equal(result_with, 42)
})

test_that("Tool can modify environment via .envir", {
  # A tool that assigns to environment
  writer_tool <- Tool$new(
    name = "write_y",
    description = "Write variable y to environment",
    parameters = z_object(
      value = z_number("Value to assign")
    ),
    execute = function(args) {
      if (!is.null(args$.envir)) {
        assign("y", args$value, envir = args$.envir)
        paste("Assigned y =", args$value)
      } else {
        "No environment provided"
      }
    }
  )
  
  test_env <- new.env()
  
  # Execute with environment
  result <- writer_tool$run(list(value = 100), envir = test_env)
  
  expect_equal(result, "Assigned y = 100")
  expect_true(exists("y", envir = test_env))
  expect_equal(test_env$y, 100)
})

test_that("execute_tool_calls passes environment to tools", {
  # Create a tool that both reads and writes
  calc_tool <- Tool$new(
    name = "double_x",
    description = "Double the value of x and store in result",
    parameters = z_object(.dummy = z_string("Unused")),
    execute = function(args) {
      env <- args$.envir
      x_val <- get("x", envir = env)
      assign("result", x_val * 2, envir = env)
      paste("Result:", x_val * 2)
    }
  )
  
  tools <- list(calc_tool)
  
  tool_calls <- list(
    list(id = "call_1", name = "double_x", arguments = list())
  )
  
  test_env <- new.env()
  test_env$x <- 10
  
  # Execute with environment
  results <- execute_tool_calls(tool_calls, tools, envir = test_env)
  
  expect_length(results, 1)
  expect_false(results[[1]]$is_error)
  expect_true(grepl("Result: 20", results[[1]]$result))
  expect_true(exists("result", envir = test_env))
  expect_equal(test_env$result, 20)
})

# --- Tests: Session Integration ---

test_that("Session environment flows through generate_text to tools", {
  # Create a tool that modifies session environment
  loader_tool <- Tool$new(
    name = "load_data",
    description = "Load data into session environment",
    parameters = z_object(
      name = z_string("Variable name"),
      value = z_number("Value to assign")
    ),
    execute = function(args) {
      if (!is.null(args$.envir)) {
        assign(args$name, args$value, envir = args$.envir)
        paste("Loaded", args$name, "=", args$value)
      } else {
        "No session environment available"
      }
    }
  )
  
  mock_model <- MockModel$new()
  session <- ChatSession$new(model = mock_model)
  
  # Program mock to call the tool, then return final response
  mock_model$add_response(
    tool_calls = list(list(
      id = "call_1",
      name = "load_data",
      arguments = list(name = "my_data", value = 42)
    ))
  )
  mock_model$add_response(text = "Data loaded successfully")
  
  tools <- list(loader_tool)
  
  result <- generate_text(
    model = mock_model,
    prompt = "Load my_data = 42",
    tools = tools,
    max_steps = 5,
    session = session
  )
  
  # Verify the variable was created in session environment
  expect_true(exists("my_data", envir = session$get_envir()))
  expect_equal(session$get_envir()$my_data, 42)
})

test_that("Agent$run passes session to tools", {
  # Create a tool that modifies session environment
  loader_tool <- Tool$new(
    name = "set_value",
    description = "Set a value in session",
    parameters = z_object(value = z_number("Value")),
    execute = function(args) {
      if (!is.null(args$.envir)) {
        assign("loaded_value", args$value, envir = args$.envir)
        "Value set"
      } else {
        "No session"
      }
    }
  )
  
  mock_model <- MockModel$new()
  session <- ChatSession$new(model = mock_model)
  
  # Create agent with the tool
  agent <- Agent$new(
    name = "LoaderAgent",
    description = "Loads data",
    tools = list(loader_tool)
  )
  
  # Program mock to call tool then respond
  mock_model$add_response(
    tool_calls = list(list(
      id = "c1",
      name = "set_value",
      arguments = list(value = 99)
    ))
  )
  mock_model$add_response(text = "Done")
  
  result <- agent$run(
    task = "Set value to 99",
    session = session,
    model = mock_model,
    max_steps = 3
  )
  
  # Verify session env was modified
  expect_true(exists("loaded_value", envir = session$get_envir()))
  expect_equal(session$get_envir()$loaded_value, 99)
})

# --- Tests: Cross-Agent Scenario ---

test_that("LoaderAgent creates x, CalcAgent computes x * 2", {
  mock_model <- MockModel$new()
  session <- ChatSession$new(model = mock_model)
  
  # LoaderAgent tool: creates x in session
  load_tool <- Tool$new(
    name = "create_x",
    description = "Create variable x",
    parameters = z_object(value = z_number("Value for x")),
    execute = function(args) {
      assign("x", args$value, envir = args$.envir)
      paste("Created x =", args$value)
    }
  )
  
  # CalcAgent tool: reads x and computes result
  calc_tool <- Tool$new(
    name = "compute_double",
    description = "Compute x * 2",
    parameters = z_object(.dummy = z_string("Unused")),
    execute = function(args) {
      x_val <- get("x", envir = args$.envir)
      result <- x_val * 2
      assign("double_x", result, envir = args$.envir)
      paste("x * 2 =", result)
    }
  )
  
  loader_agent <- Agent$new(
    name = "LoaderAgent",
    description = "Loads data",
    tools = list(load_tool)
  )
  
  calc_agent <- Agent$new(
    name = "CalcAgent",
    description = "Does calculations",
    tools = list(calc_tool)
  )
  
  # --- Simulate LoaderAgent run ---
  mock_model$add_response(
    tool_calls = list(list(id = "l1", name = "create_x", arguments = list(value = 10)))
  )
  mock_model$add_response(text = "Loaded x = 10")
  
  loader_agent$run(
    task = "Create x = 10",
    session = session,
    model = mock_model,
    max_steps = 3
  )
  
  # Verify x exists
  expect_true(exists("x", envir = session$get_envir()))
  expect_equal(session$get_envir()$x, 10)
  
  # --- Simulate CalcAgent run (sharing same session) ---
  mock_model$add_response(
    tool_calls = list(list(id = "c1", name = "compute_double", arguments = list()))
  )
  mock_model$add_response(text = "Computed x * 2 = 20")
  
  calc_agent$run(
    task = "Compute x * 2",
    session = session,
    model = mock_model,
    max_steps = 3
  )
  
  # Verify double_x exists and has correct value
  expect_true(exists("double_x", envir = session$get_envir()))
  expect_equal(session$get_envir()$double_x, 20)
})

test_that("Session environment is isolated from global", {
  mock_model <- MockModel$new()
  session <- ChatSession$new(model = mock_model)
  
  # Tool that creates a variable
  create_tool <- Tool$new(
    name = "create_secret",
    description = "Create secret variable",
    parameters = z_object(.dummy = z_string("Unused")),
    execute = function(args) {
      assign("secret_var", "session_only", envir = args$.envir)
      "Secret created"
    }
  )
  
  agent <- Agent$new(
    name = "SecretAgent",
    description = "Creates secrets",
    tools = list(create_tool)
  )
  
  mock_model$add_response(
    tool_calls = list(list(id = "s1", name = "create_secret", arguments = list()))
  )
  mock_model$add_response(text = "Done")
  
  agent$run(
    task = "Create secret",
    session = session,
    model = mock_model,
    max_steps = 3
  )
  
  # Variable should exist in session
  expect_true(exists("secret_var", envir = session$get_envir()))
  
  # Variable should NOT exist in global
  expect_false(exists("secret_var", envir = globalenv()))
})

test_that("Agent sees session environment objects in system prompt", {
  mock_model <- MockModel$new()
  session <- ChatSession$new(model = mock_model)
  
  # Inject data into session environment
  env <- session$get_envir()
  env$important_data <- data.frame(a = 1:5)
  session$set_memory("project_phase", "planning")
  
  agent <- Agent$new(
    name = "ObserverAgent",
    description = "Observes environment",
    system_prompt = "Tell me what you see."
  )
  
  # Using the updated MockModel that captures params
  agent$run("Look around", session = session, model = mock_model)
  
  # Inspect system prompt in the last message or system parameter
  passed_messages <- mock_model$last_params$messages
  
  # System prompt might be in 'system' param or part of messages depending on how generate_text handles it
  # generate_text merges system prompt into messages if it's a list.
  
  # Find system message
  system_msg <- NULL
  if (!is.null(mock_model$last_params$system)) {
      system_msg <- mock_model$last_params$system
  } else {
    for (msg in passed_messages) {
        if (msg$role == "system") {
        system_msg <- msg$content
        break
        }
    }
  }
  
  
  expect_true(!is.null(system_msg), info = "System message should not be null")
  expect_true(grepl("\\[SHARED SESSION CONTEXT\\]", system_msg), info = "Should contain shared session header")
  expect_true(grepl("important_data", system_msg), info = "Should contain important_data")
  expect_true(grepl("data.frame", system_msg), info = "Should contain data.frame class")
  expect_true(grepl("project_phase", system_msg), info = "Should contain memory project_phase")
})


