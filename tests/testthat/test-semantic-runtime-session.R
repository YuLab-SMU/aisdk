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
  session <- ChatSession$new(model = MockModel$new())
  env <- session$get_envir()
  registry <- get(".semantic_adapter_registry", envir = env, inherits = FALSE)

  observed <- new.env(parent = emptyenv())

  local_mocked_bindings(
    generate_text = function(model,
                             prompt,
                             system = NULL,
                             tools = NULL,
                             max_steps = 1,
                             session = NULL,
                             hooks = NULL,
                             registry = NULL,
                             ...) {
      observed$send_session <- session
      observed$send_env <- session$get_envir()
      observed$send_registry <- get(".semantic_adapter_registry", envir = observed$send_env, inherits = FALSE)

      list(
        text = "sync response",
        tool_calls = NULL,
        finish_reason = "stop",
        usage = list(total_tokens = 1)
      )
    },
    stream_text = function(model,
                           prompt,
                           callback,
                           system = NULL,
                           registry = NULL,
                           tools = NULL,
                           max_steps = 1,
                           session = NULL,
                           hooks = NULL,
                           ...) {
      observed$stream_session <- session
      observed$stream_env <- session$get_envir()
      observed$stream_registry <- get(".semantic_adapter_registry", envir = observed$stream_env, inherits = FALSE)

      callback("stream response", TRUE)

      list(
        text = "stream response",
        messages_added = list(),
        usage = list(total_tokens = 1)
      )
    }
  )

  session$send("hello")
  session$send_stream("stream hello", function(text, done) NULL)

  expect_identical(observed$send_session, session)
  expect_identical(observed$send_env, env)
  expect_identical(observed$send_registry, registry)

  expect_identical(observed$stream_session, session)
  expect_identical(observed$stream_env, env)
  expect_identical(observed$stream_registry, registry)
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
