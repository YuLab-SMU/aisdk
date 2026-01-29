# Test ChatSession multi-agent features (memory and envir)

openai_model_id <- get_openai_model_id()

test_that("ChatSession has memory and envir fields", {
  session <- ChatSession$new(model = openai_model_id)

  # Memory should be initialized as empty list (names() returns NULL)
  expect_null(session$list_memory())

  # Envir should be initialized as a new environment
  expect_true(is.environment(session$get_envir()))
})

test_that("Session memory get/set works", {
  session <- ChatSession$new(model = openai_model_id)

  # Set values
  session$set_memory("key1", "value1")
  session$set_memory("key2", list(a = 1, b = 2))

  # Get values
  expect_equal(session$get_memory("key1"), "value1")
  expect_equal(session$get_memory("key2")$a, 1)

  # Default for missing key
  expect_null(session$get_memory("missing"))
  expect_equal(session$get_memory("missing", default = "default"), "default")
})

test_that("Session memory list works", {
  session <- ChatSession$new(model = openai_model_id)

  session$set_memory("key1", "val1")
  session$set_memory("key2", "val2")

  keys <- session$list_memory()
  expect_length(keys, 2)
  expect_true("key1" %in% keys)
  expect_true("key2" %in% keys)
})

test_that("Session memory clear works", {
  session <- ChatSession$new(model = openai_model_id)

  session$set_memory("key1", "val1")
  session$set_memory("key2", "val2")
  session$set_memory("key3", "val3")

  # Clear specific keys
 session$clear_memory(keys = c("key1", "key2"))
  expect_null(session$get_memory("key1"))
  expect_null(session$get_memory("key2"))
  expect_equal(session$get_memory("key3"), "val3")

  # Clear all
  session$clear_memory()
  expect_null(session$list_memory())  # names(list()) returns NULL
})

test_that("Session environment works", {
  session <- ChatSession$new(model = openai_model_id)

  env <- session$get_envir()

  # Assign to environment
  env$df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  env$model <- lm(x ~ 1, data = env$df)

  # List objects
  objects <- session$list_envir()
  expect_true("df" %in% objects)
  expect_true("model" %in% objects)

  # Retrieve from environment
  expect_equal(nrow(env$df), 3)
})

test_that("Session eval_in_session works", {
  session <- ChatSession$new(model = openai_model_id)

  env <- session$get_envir()
  env$x <- 10

  # Evaluate expression in session environment
  result <- session$eval_in_session(quote(x * 2))
  expect_equal(result, 20)

  # Create new variable via eval
  session$eval_in_session(quote(y <- x + 5))
  expect_equal(env$y, 15)
})

test_that("Session environment is isolated from global", {
  session <- ChatSession$new(model = openai_model_id)

  # Create local variable in session
  env <- session$get_envir()
  env$session_only <- "secret"

  # Should not pollute global environment
  expect_false(exists("session_only", envir = globalenv()))

  # But session can access global if needed
  global_val <- 42
  assign("global_test", global_val, globalenv())
  result <- session$eval_in_session(quote(global_test))
  expect_equal(result, 42)

  # Cleanup
  rm("global_test", envir = globalenv())
})

test_that("Session print shows memory and envir info", {
  session <- ChatSession$new(model = openai_model_id)

  session$set_memory("test_key", "test_value")
  env <- session$get_envir()
  env$test_var <- 123

  output <- capture.output(print(session))
  output_text <- paste(output, collapse = "\n")

  expect_true(grepl("Memory.*1.*key", output_text))
  expect_true(grepl("Envir.*1.*object", output_text))
})

test_that("Session can be initialized with custom memory and envir", {
  custom_memory <- list(preset_key = "preset_value")
  custom_envir <- new.env()
  custom_envir$preset_var <- 999

  session <- ChatSession$new(
    model = openai_model_id,
    memory = custom_memory,
    envir = custom_envir
  )

  expect_equal(session$get_memory("preset_key"), "preset_value")
  expect_equal(session$get_envir()$preset_var, 999)
})

test_that("Multiple agents can share session state", {
  # Simulate two agents sharing a session
  session <- ChatSession$new(model = openai_model_id)

  # Agent 1 creates data
  session$set_memory("agent1_status", "completed")
  env <- session$get_envir()
  env$shared_df <- data.frame(value = 1:5)

  # Agent 2 reads and modifies
  expect_equal(session$get_memory("agent1_status"), "completed")
  expect_equal(nrow(session$get_envir()$shared_df), 5)

  # Agent 2 adds its own data
  session$set_memory("agent2_status", "processed")
  env$result <- sum(env$shared_df$value)

  # Both are visible
  expect_equal(session$get_memory("agent2_status"), "processed")
  expect_equal(env$result, 15)
})
