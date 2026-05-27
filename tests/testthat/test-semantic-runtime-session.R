library(testthat)
library(aisdk)

helper_path <- file.path(test_path("helper-mock.R"))
source(helper_path)

# Semantic runtime regression tests for canonical session env invariants

test_that("ChatSession canonical env stores the semantic adapter registry", {
  session <- ChatSession$new(model = MockModel$new())
  env <- session$get_envir()

  expect_true(exists(".semantic_adapter_registry", envir = env, inherits = FALSE))
  expect_s3_class(get(".semantic_adapter_registry", envir = env, inherits = FALSE), "SemanticAdapterRegistry")
  expect_identical(
    get_semantic_adapter_registry(session = session),
    get(".semantic_adapter_registry", envir = env, inherits = FALSE)
  )
})

test_that("send and send_stream reuse the same session env and semantic registry", {
  model <- MockModel$new()
  model$add_response(text = "sync response")
  model$add_response(text = "stream response")

  session <- ChatSession$new(model = model)
  env <- session$get_envir()
  registry <- get(".semantic_adapter_registry", envir = env, inherits = FALSE)
  streamed <- character()

  session$send("hello")
  session$send_stream("stream hello", function(text, done) {
    streamed <<- c(streamed, text)
  })

  expect_identical(session$get_envir(), env)
  expect_identical(get(".semantic_adapter_registry", envir = env, inherits = FALSE), registry)
  expect_equal(paste(streamed, collapse = ""), "stream response")
  expect_equal(session$get_last_response(), "stream response")
})

test_that("SharedSession global scope is identical to the canonical session env", {
  session <- SharedSession$new(model = MockModel$new(), sandbox_mode = "permissive")
  env <- session$get_envir()

  session$set_var("from_scope", 10, scope = "global")
  expect_true(exists("from_scope", envir = env, inherits = FALSE))
  expect_equal(env$from_scope, 10)

  assign("from_env", 20, envir = env)
  expect_equal(session$get_var("from_env", scope = "global"), 20)
})

test_that("Computer isolated execution is explicit sandbox_exec and does not mutate live session env", {
  session <- ChatSession$new(model = MockModel$new())
  comp <- Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")
  var_name <- ".aisdk_semantic_runtime_live_value"

  assign(var_name, 7, envir = session$get_envir())

  result <- comp$execute_r_code(sprintf("%s <- 99; %s", var_name, var_name))

  expect_false(result$error)
  expect_equal(result$result, 99)
  expect_equal(result$execution_mode, "sandbox_exec")
  expect_equal(get(var_name, envir = session$get_envir(), inherits = FALSE), 7)
})
