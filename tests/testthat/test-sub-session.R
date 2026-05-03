test_that("sub_session_query records compact result without parent env writes", {
  model <- MockModel$new()
  model$add_response(text = "child summary")
  session <- aisdk::create_chat_session(model = model)
  env <- session$get_envir()
  env$parent_value <- 42
  session$set_context_management_config(create_context_management_config(mode = "adaptive"))
  session$list_context_handles()

  result <- sub_session_query(
    session,
    task = "Summarize parent value.",
    context_handles = "object:parent_value",
    max_turns = 2
  )

  expect_true(result$success)
  expect_match(result$summary, "child summary", fixed = TRUE)
  expect_lte(result$max_turns, 2L)
  expect_false(exists("child_only", envir = session$get_envir(), inherits = FALSE))

  state <- session$get_context_state()
  expect_length(state$sub_sessions, 1)
  expect_match(state$sub_sessions[[1]]$summary, "child summary", fixed = TRUE)
  expect_true(any(vapply(state$tool_digest, function(item) identical(item$tool, "sub_session_query"), logical(1))))
  expect_false(any(grepl("\\[SELECTED CONTEXT HANDLES\\]", vapply(session$get_history(), function(msg) msg$content %||% "", character(1)))))
})
