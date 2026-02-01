# Test Orchestration: AgentRegistry and Flow


# --- Mock Model Helper ---

# A simple mock model that behaves predictably for testing
MockModel <- R6::R6Class("MockModel",
  inherit = LanguageModelV1,
  public = list(
    provider = "mock",
    model_id = "mock-model",
    responses = list(),

    initialize = function(responses = list()) {
      self$responses <- responses
    },

    do_generate = function(params) {
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
    
    # Helper to add a response to the queue
    add_response = function(text = NULL, tool_calls = NULL) {
      self$responses <- c(self$responses, list(list(
        text = text,
        tool_calls = tool_calls,
        finish_reason = "stop",
        usage = list(total_tokens = 10)
      )))
    },
    
    # Required for ReAct loop
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

# --- Mock Agent Helper ---

MockAgent <- R6::R6Class("MockAgent",
  inherit = Agent,
  public = list(
    mock_run_func = NULL,
    
    initialize = function(name, description, run_func = NULL) {
      super$initialize(name, description)
      self$mock_run_func <- run_func
    },
    
    run = function(task, session = NULL, context = NULL, model = NULL, max_steps = 10, ...) {
      if (!is.null(self$mock_run_func)) {
        return(self$mock_run_func(task, session, context, ...))
      }
      list(text = paste0(self$name, " Done"))
    }
  )
)

# --- Tests ---

test_that("AgentRegistry manages agents correctly", {
  registry <- AgentRegistry$new()
  
  agent1 <- Agent$new("A1", "Desc1")
  agent2 <- Agent$new("A2", "Desc2")
  
  registry$register(agent1)
  registry$register(agent2)
  
  expect_true(registry$has("A1"))
  expect_true(registry$has("A2"))
  expect_false(registry$has("A3"))
  
  expect_equal(registry$get("A1")$name, "A1")
  expect_length(registry$list_agents(), 2)
  
  # Generation of prompt
  prompt <- registry$generate_prompt_section()
  expect_true(grepl("A1", prompt))
  expect_true(grepl("Desc2", prompt))
})

test_that("AgentRegistry creates delegation tools", {
  registry <- AgentRegistry$new()
  agent1 <- Agent$new("Worker", "Does work")
  registry$register(agent1)
  
  tools <- registry$generate_delegate_tools()
  expect_length(tools, 1)
  expect_equal(tools[[1]]$name, "delegate_to_Worker")
  expect_s3_class(tools[[1]], "Tool")
})

# --- Flow Tests: Using Registry properly ---

test_that("Flow manages stack depth with registry", {
  mock_model <- MockModel$new()
  session <- ChatSession$new(model = mock_model)
  
  # Create agents
  manager <- MockAgent$new("Manager", "The primary agent that delegates work")
  worker <- MockAgent$new("Worker", "Does the actual work")
  
  # Create registry and register the worker (not the manager)
  registry <- AgentRegistry$new()
  registry$register(worker)
  
  # Create flow WITH REGISTRY
  flow <- Flow$new(session = session, model = mock_model, registry = registry, max_depth = 3)
  
  expect_equal(flow$depth(), 0)
  
  # Program mock model to:
  # 1. Return a tool call for "delegate_to_Worker"
  # 2. Then return a final text response
  mock_model$add_response(
    text = NULL,
    tool_calls = list(
      list(
        id = "call_1",
        name = "delegate_to_Worker",
        arguments = list(task = "Do the work", context = "")
      )
    )
  )
  mock_model$add_response(text = "Manager received Worker result. Final answer.")
  
  # Hook Worker to observe depth
  observed_depth <- -1
  worker$mock_run_func <- function(task, session, context, ...) {
    observed_depth <<- flow$depth()
    list(text = "Worker completed the task")
  }
  
  # Run flow
  result <- flow$run(manager, "Root task for manager")
  
  # Assertions
  expect_equal(result$text, "Manager received Worker result. Final answer.")
  expect_equal(observed_depth, 1)  # Stack should have manager pushed when worker runs
  expect_equal(flow$depth(), 0)    # Stack should be empty after run
})

test_that("Flow enforces max depth with registry", {
  mock_model <- MockModel$new()
  session <- ChatSession$new(model = mock_model)
  
  # Create agents
  manager <- MockAgent$new("Manager", "Primary agent")
  level1 <- MockAgent$new("Level1", "First level agent")
  level2 <- MockAgent$new("Level2", "Second level agent")
  
  # Create registry
  registry <- AgentRegistry$new()
  registry$register(level1)
  registry$register(level2)
  
  # Create flow with max_depth = 1 (only one level of delegation allowed)
  flow <- Flow$new(session = session, model = mock_model, registry = registry, max_depth = 1)
  
  # level1 tries to delegate to level2 (should fail because depth limit)
  level1$mock_run_func <- function(task, session, context, ...) {
    # This should fail because we are already at depth 1
    result <- flow$delegate(level2, "Deep task")
    list(text = result)
  }
  
  # level2 would just complete (but should never be called)
  level2$mock_run_func <- function(...) {
    list(text = "Level2 should not run")
  }
  
  # Program mock model to delegate to level1
  mock_model$add_response(
    tool_calls = list(list(id = "c1", name = "delegate_to_Level1", arguments = list(task = "Task")))
  )
  mock_model$add_response(text = "Final")
  
  # Run
  result <- flow$run(manager, "Root")
  
  # The tool result from level1 should contain the error message
  # But this propagates through the ReAct loop. Let's check the session memory
  # or verify level2 was NOT called.
  
  # Actually, level1's run function returns the delegation error as text.
  # That error text is then fed back to mock_model as tool result.
  # mock_model then responds with "Final".
  
  expect_equal(result$text, "Final")
  
  # More direct test: call delegate directly
  flow2 <- Flow$new(session = session, model = mock_model, registry = registry, max_depth = 0)
  direct_result <- flow2$delegate(level1, "Any task")
  expect_true(grepl("Maximum delegation depth", direct_result))
})

test_that("Flow constructs recursive context", {
  mock_model <- MockModel$new()
  session <- ChatSession$new(model = mock_model)
  
  manager <- MockAgent$new("Manager", "Primary agent")
  worker <- MockAgent$new("Worker", "Does work")
  
  registry <- AgentRegistry$new()
  registry$register(worker)
  
  flow <- Flow$new(session = session, model = mock_model, registry = registry)
  
  # Program mock model to delegate
  mock_model$add_response(
    tool_calls = list(list(id = "1", name = "delegate_to_Worker", arguments = list(task = "Work on it")))
  )
  mock_model$add_response(text = "Final")
  
  # Capture context passed to Worker
  captured_context <- NULL
  worker$mock_run_func <- function(task, session, context, ...) {
    captured_context <<- context
    list(text = "Worker Done")
  }
  
  flow$run(manager, "Do everything")
  
  # Verify context was passed
  expect_true(!is.null(captured_context))
  expect_true(grepl("Manager", captured_context))
  expect_true(grepl("Do everything", captured_context))
  expect_true(grepl("Work on it", captured_context))
})

test_that("Flow delegate method works standalone", {
  mock_model <- MockModel$new()
  session <- ChatSession$new(model = mock_model)
  
  worker <- MockAgent$new("Worker", "Does work")
  
  flow <- Flow$new(session = session, model = mock_model, max_depth = 5)
  
  # Direct delegate call (not through run)
  worker$mock_run_func <- function(task, session, context, ...) {
    list(text = paste0("Completed: ", task))
  }
  
  result <- flow$delegate(worker, "Clean the data")
  
  expect_equal(result, "Completed: Clean the data")
  expect_equal(flow$depth(), 0)
})
