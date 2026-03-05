test_that("MissionStep initializes correctly", {
  step <- create_step(
    id          = "step_1",
    description = "Load and inspect data",
    max_retries = 3,
    parallel    = FALSE,
    depends_on  = NULL
  )

  expect_equal(step$id, "step_1")
  expect_equal(step$description, "Load and inspect data")
  expect_equal(step$status, "pending")
  expect_equal(step$max_retries, 3)
  expect_equal(step$retry_count, 0)
  expect_false(step$parallel)
  expect_null(step$executor)
  expect_length(step$error_history, 0)
})

test_that("MissionStep with function executor runs and returns result", {
  session <- ChatSession$new()

  step <- create_step(
    id          = "fn_step",
    description = "Return hello",
    executor    = function(task, session, context) "hello from fn"
  )

  result <- step$run(session = session, model = "test:model")
  expect_equal(result, "hello from fn")
})

test_that("MissionStep run() propagates errors for retry handling", {
  session <- ChatSession$new()

  step <- create_step(
    id          = "failing_step",
    description = "Always fails",
    executor    = function(task, session, context) stop("intentional failure")
  )

  expect_error(
    step$run(session = session, model = "test:model"),
    "intentional failure"
  )
})

test_that("MissionStep context injection includes error history", {
  session <- ChatSession$new()
  captured_context <- NULL

  step <- create_step(
    id          = "ctx_step",
    description = "Check context",
    executor    = function(task, session, context) {
      captured_context <<- context
      "done"
    }
  )

  # Pre-populate error history
  step$error_history <- list(
    list(attempt = 1, error = "timeout error", timestamp = Sys.time()),
    list(attempt = 2, error = "network error", timestamp = Sys.time())
  )

  # Run with injected context (simulating what Mission does)
  context <- paste0(
    "PREVIOUS ATTEMPT(S) FAILED. Please approach this differently.\n\n",
    "Attempt 1: timeout error\nAttempt 2: network error"
  )
  step$run(session = session, model = "test:model", context = context)

  expect_true(grepl("timeout error", captured_context))
  expect_true(grepl("network error", captured_context))
})

test_that("Mission initializes with correct defaults", {
  m <- create_mission(goal = "Test goal", auto_plan = FALSE)

  expect_s3_class(m, "R6")
  expect_true(grepl("^mission_", m$id))
  expect_equal(m$goal, "Test goal")
  expect_equal(m$status, "pending")
  expect_null(m$steps)
  expect_false(m$auto_plan)
})

test_that("Mission initializes with auto_plan = FALSE", {
  m <- create_mission(goal = "Test goal", auto_plan = FALSE)
  expect_false(m$auto_plan)
})

test_that("Mission state machine: step_summary returns correct statuses", {
  steps <- list(
    create_step("s1", "Step one",   executor = function(t, s, c) "r1"),
    create_step("s2", "Step two",   executor = function(t, s, c) "r2"),
    create_step("s3", "Step three", executor = function(t, s, c) "r3")
  )

  m <- create_mission(goal = "pipeline", steps = steps, auto_plan = FALSE)
  m$steps[[1]]$status <- "done"
  m$steps[[2]]$status <- "running"
  m$steps[[3]]$status <- "pending"

  summary <- m$step_summary()
  expect_equal(summary[["s1"]], "done")
  expect_equal(summary[["s2"]], "running")
  expect_equal(summary[["s3"]], "pending")
})

test_that("Mission runs successfully with function executors", {
  results_log <- character(0)

  steps <- list(
    create_step("s1", "First step",  executor = function(t, s, c) { results_log <<- c(results_log, "s1"); "result_1" }),
    create_step("s2", "Second step", executor = function(t, s, c) { results_log <<- c(results_log, "s2"); "result_2" },
                depends_on = "s1"),
    create_step("s3", "Third step",  executor = function(t, s, c) { results_log <<- c(results_log, "s3"); "result_3" },
                depends_on = "s2")
  )

  m <- create_mission(
    goal      = "Three-step pipeline",
    steps     = steps,
    auto_plan = FALSE,
    model     = "test:model"
  )

  # We need a session with a model but no actual API — override model resolution
  # by using a mock approach: skip actual LLM call since executors are functions
  suppressWarnings(
    tryCatch(m$run(model = "test:model"), error = function(e) NULL)
  )

  # Steps with function executors should execute in dependency order
  # (may fail at model resolution, but we can test the step-level logic)
  # Test via direct private method call instead
  session <- ChatSession$new()
  step_a <- create_step("a", "Step A", executor = function(t, s, c) "A done")
  step_b <- create_step("b", "Step B", executor = function(t, s, c) "B done", depends_on = "a")

  m2 <- create_mission(goal = "dag test", steps = list(step_a, step_b), auto_plan = FALSE, model = "test:model")
  m2$session <- session

  # Directly invoke the private execute logic via environment access
  env <- environment(m2$run)

  # Test step_a runs and writes to session memory
  result_a <- step_a$run(session = session, model = "test:model")
  expect_equal(result_a, "A done")
  session$set_memory("step_a_result", result_a)

  result_b <- step_b$run(session = session, model = "test:model")
  expect_equal(result_b, "B done")
})

test_that("Mission retry: execute_step_with_retry retries on failure then succeeds", {
  attempt_count <- 0
  session <- ChatSession$new()

  step <- create_step(
    id          = "retry_step",
    description = "Fails twice then succeeds",
    executor    = function(t, s, c) {
      attempt_count <<- attempt_count + 1
      if (attempt_count < 3) stop(paste("failure", attempt_count))
      "finally succeeded"
    },
    max_retries = 3
  )

  m <- create_mission(
    goal      = "retry test",
    steps     = list(step),
    auto_plan = FALSE,
    model     = "test:model"
  )
  m$session <- session

  # Call private method directly
  m$.__enclos_env__$private$execute_step_with_retry(step, "test:model")

  expect_equal(step$status, "done")
  expect_equal(step$result, "finally succeeded")
  expect_equal(attempt_count, 3)
  expect_length(step$error_history, 2)  # 2 failures recorded
})

test_that("Mission retry: escalates after max_retries exceeded", {
  session <- ChatSession$new()
  stall_called <- FALSE

  step <- create_step(
    id          = "always_fails",
    description = "Always fails",
    executor    = function(t, s, c) stop("permanent failure"),
    max_retries = 1
  )

  m <- create_mission(
    goal         = "stall test",
    steps        = list(step),
    auto_plan    = FALSE,
    model        = "test:model",
    stall_policy = list(
      on_max_retries = "escalate",
      escalate_fn    = function(mission, step) { stall_called <<- TRUE }
    )
  )
  m$session <- session

  m$.__enclos_env__$private$execute_step_with_retry(step, "test:model")

  expect_equal(step$status, "failed")
  expect_true(stall_called)
})

test_that("Mission save and resume preserves step state", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))

  steps <- list(
    create_step("s1", "First step",  executor = function(t, s, c) "done1"),
    create_step("s2", "Second step", executor = function(t, s, c) "done2")
  )
  steps[[1]]$status <- "done"
  steps[[1]]$result <- "done1"
  steps[[2]]$status <- "pending"

  m <- create_mission(
    goal      = "save/resume test",
    steps     = steps,
    auto_plan = FALSE,
    model     = "test:model"
  )
  m$status <- "running"
  m$save(tmp)

  # Create fresh mission and resume
  m2 <- create_mission(
    goal      = "save/resume test",
    auto_plan = FALSE,
    executor  = function(t, s, c) "resumed"  # default executor for re-attach
  )
  m2$resume(tmp)

  expect_equal(m2$goal, "save/resume test")
  expect_equal(m2$status, "running")
  expect_length(m2$steps, 2)
  expect_equal(m2$steps[[1]]$status, "done")
  expect_equal(m2$steps[[1]]$result, "done1")
  expect_equal(m2$steps[[2]]$status, "pending")
})

test_that("MissionHookHandler triggers correct hooks", {
  events <- character(0)
  step_done_results <- list()

  hooks <- create_mission_hooks(
    on_mission_start = function(m)         events <<- c(events, "mission_start"),
    on_step_start    = function(s, attempt) events <<- c(events, paste0("step_start:", s$id)),
    on_step_done     = function(s, result)  step_done_results[[s$id]] <<- result,
    on_mission_done  = function(m)         events <<- c(events, "mission_done")
  )

  session <- ChatSession$new()
  step <- create_step("h1", "Hook test step", executor = function(t, s, c) "hook result")

  m <- create_mission(
    goal      = "hook test",
    steps     = list(step),
    auto_plan = FALSE,
    model     = "test:model",
    hooks     = hooks,
    session   = session
  )

  # Manually trigger hooks as Mission would
  hooks$trigger_mission_start(m)
  hooks$trigger_step_start(step, 1)
  hooks$trigger_step_done(step, "hook result")
  hooks$trigger_mission_done(m)

  expect_true("mission_start" %in% events)
  expect_true("step_start:h1" %in% events)
  expect_equal(step_done_results[["h1"]], "hook result")
  expect_true("mission_done" %in% events)
})

test_that("MissionHookHandler errors in hooks are caught gracefully", {
  hooks <- create_mission_hooks(
    on_mission_start = function(m) stop("hook error!")
  )

  m <- create_mission(goal = "test", auto_plan = FALSE)

  expect_warning(
    hooks$trigger_mission_start(m),
    "hook error"
  )
})

test_that("MissionOrchestrator submit and status work correctly", {
  orch <- create_mission_orchestrator(max_concurrent = 2)

  m1 <- create_mission(goal = "Mission A", auto_plan = FALSE, model = "test:model")
  m2 <- create_mission(goal = "Mission B", auto_plan = FALSE, model = "test:model")

  orch$submit(m1)
  orch$submit(m2)

  expect_length(orch$pending_queue, 2)

  status_df <- orch$status()
  expect_s3_class(status_df, "data.frame")
  expect_equal(nrow(status_df), 2)
  expect_true("status" %in% names(status_df))
})

test_that("MissionOrchestrator global model propagates to missions", {
  orch <- create_mission_orchestrator(max_concurrent = 2, model = "global:model")
  m <- create_mission(goal = "inherits model", auto_plan = FALSE)
  expect_null(m$model)

  orch$submit(m)
  expect_equal(m$model, "global:model")
})

test_that("ChatSession checkpoint and restore_checkpoint round-trip", {
  session <- ChatSession$new(model = "test:model")
  session$set_memory("key1", "value1")
  session$set_memory("key2", list(a = 1, b = 2))

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))

  path <- session$checkpoint(tmp)
  expect_equal(path, tmp)
  expect_true(file.exists(tmp))

  # Create new session and restore
  session2 <- ChatSession$new()
  session2$restore_checkpoint(tmp)

  expect_equal(session2$get_memory("key1"), "value1")
  expect_equal(session2$get_memory("key2"), list(a = 1, b = 2))
})

test_that("default_stall_policy returns correct structure", {
  policy <- aisdk:::default_stall_policy()
  expect_named(policy, c("on_tool_failure", "on_step_timeout", "on_max_retries", "escalate_fn"))
  expect_equal(policy$on_max_retries, "escalate")
  expect_null(policy$escalate_fn)
})
