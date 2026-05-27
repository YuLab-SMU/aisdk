test_that("context handles summarize live objects and memory without full payloads", {
  session <- aisdk::create_chat_session(model = MockModel$new())
  env <- session$get_envir()
  env$big_df <- data.frame(
    id = seq_len(200),
    text = rep(paste(rep("payload", 20), collapse = " "), 200),
    stringsAsFactors = FALSE
  )
  session$set_memory("analysis_note", list(topic = "context handles", status = "compact"))
  session$set_context_management_config(create_context_management_config(mode = "adaptive"))

  first <- session$list_context_handles()
  second <- session$list_context_handles()

  ids <- vapply(first, function(handle) handle$id, character(1))
  expect_true("object:big_df" %in% ids)
  expect_true("memory:analysis_note" %in% ids)
  expect_equal(
    sort(ids),
    sort(vapply(second, function(handle) handle$id, character(1)))
  )

  object_handle <- first[[which(ids == "object:big_df")]]
  expect_lt(nchar(object_handle$summary), 260)
  expect_match(object_handle$accessor, "object_peek", fixed = TRUE)
  expect_false(grepl(paste(rep("payload", 20), collapse = " "), object_handle$summary, fixed = TRUE))
})

test_that("context_get retrieves summaries and bounded full content", {
  session <- aisdk::create_chat_session(model = MockModel$new())
  env <- session$get_envir()
  env$demo_df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  session$set_memory("note", "important compact memory")
  session$set_context_management_config(create_context_management_config(mode = "adaptive"))

  handles <- session$list_context_handles()
  expect_true("object:demo_df" %in% vapply(handles, function(handle) handle$id, character(1)))

  summary <- context_get(session, "memory:note", detail = "summary")
  expect_equal(summary$kind, "memory")
  expect_match(summary$summary, "important compact memory", fixed = TRUE)

  full <- context_get(session, "object:demo_df", detail = "full")
  expect_equal(full$handle$id, "object:demo_df")
  expect_match(full$content, "demo_df", fixed = TRUE)
})
