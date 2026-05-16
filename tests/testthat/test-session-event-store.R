test_that("console session event store writes custom and visible events separately", {
  startup <- tempfile("aisdk-session-store-")
  dir.create(startup, recursive = TRUE)
  on.exit(unlink(startup, recursive = TRUE), add = TRUE)

  session <- create_chat_session(model = "mock:test")
  event1 <- aisdk:::console_append_session_event(
    session,
    type = "custom",
    payload = list(extension_state = list(count = 1)),
    startup_dir = startup
  )
  event2 <- aisdk:::console_append_session_event(
    session,
    type = "custom_message",
    payload = list(message = list(role = "system", content = "visible summary")),
    startup_dir = startup,
    visible = TRUE
  )
  events <- aisdk:::console_read_session_events(session, startup_dir = startup)
  visible <- aisdk:::console_event_visible_messages(events)

  expect_match(event1$event_id, "^evt_")
  expect_match(event2$event_id, "^evt_")
  expect_length(events, 2)
  expect_length(visible, 1)
  expect_equal(visible[[1]]$content, "visible summary")
})

test_that("console branch tree can fork, checkout, and summarize", {
  session <- create_chat_session(model = "mock:test")

  tree <- aisdk:::console_branch_tree(session)
  branch <- aisdk:::console_fork_branch(session, "experiment")
  ok <- aisdk:::console_checkout_branch(session, "main")
  aisdk:::console_checkout_branch(session, branch)
  aisdk:::console_set_branch_summary(session, "branch summary")
  updated <- aisdk:::console_branch_tree(session)

  expect_equal(tree$active, "main")
  expect_true(ok)
  expect_equal(updated$active, branch)
  expect_equal(updated$branches[[branch]]$parent, "main")
  expect_equal(updated$branches[[branch]]$summary, "branch summary")
})

test_that("extension runtime loads builtins and custom extension commands", {
  startup <- tempfile("aisdk-ext-")
  ext_dir <- file.path(startup, ".aisdk", "extensions", "custom")
  dir.create(ext_dir, recursive = TRUE)
  on.exit(unlink(startup, recursive = TRUE), add = TRUE)
  writeLines(c(
    "register_extension(",
    "  id = 'custom',",
    "  commands = list(hello = function(...) 'hi'),",
    "  state = list(value = 1)",
    ")"
  ), file.path(ext_dir, "extension.R"))

  session <- create_chat_session(model = "mock:test")
  runtime <- aisdk:::console_extension_runtime_load(session, startup_dir = startup)

  expect_true("r-console" %in% names(runtime$extensions))
  expect_true("custom" %in% names(runtime$extensions))
  expect_true("hello" %in% names(runtime$commands))
  expect_equal(session$get_metadata("console_extensions")[[length(session$get_metadata("console_extensions"))]], "custom")
})
