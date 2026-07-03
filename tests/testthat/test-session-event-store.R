test_that("session event store writes custom and visible events separately", {
  startup <- tempfile("aisdk-session-store-")
  dir.create(startup, recursive = TRUE)
  on.exit(unlink(startup, recursive = TRUE), add = TRUE)

  session <- create_chat_session(model = "mock:test")
  event1 <- aisdk:::session_append_event(
    session,
    type = "custom",
    payload = list(extension_state = list(count = 1)),
    startup_dir = startup
  )
  event2 <- aisdk:::session_append_event(
    session,
    type = "custom_message",
    payload = list(message = list(role = "system", content = "visible summary")),
    startup_dir = startup,
    visible = TRUE
  )
  events <- aisdk:::session_read_events(session, startup_dir = startup)
  visible <- aisdk:::session_event_visible_messages(events)

  expect_match(event1$event_id, "^evt_")
  expect_match(event2$event_id, "^evt_")
  expect_length(events, 2)
  expect_length(visible, 1)
  expect_equal(visible[[1]]$content, "visible summary")
})

test_that("session branch tree can fork, checkout, and summarize", {
  session <- create_chat_session(model = "mock:test")

  tree <- aisdk:::session_branch_tree(session)
  branch <- aisdk:::session_fork_branch(session, "experiment")
  ok <- aisdk:::session_checkout_branch(session, "main")
  aisdk:::session_checkout_branch(session, branch)
  aisdk:::session_set_branch_summary(session, "branch summary")
  updated <- aisdk:::session_branch_tree(session)

  expect_equal(tree$active, "main")
  expect_true(ok)
  expect_equal(updated$active, branch)
  expect_equal(updated$branches[[branch]]$parent, "main")
  expect_equal(updated$branches[[branch]]$summary, "branch summary")
})

# The console extension runtime moved to the aisdk.console package; its
# loader test lives there (tests/testthat/test-extension-runtime.R).
