test_that("context_search returns matching handles", {
  session <- aisdk::create_chat_session(model = MockModel$new())
  env <- session$get_envir()
  env$sales_df <- data.frame(region = c("east", "west"), sales = c(10, 20))
  session$set_memory("sales_plan", "Use the regional sales dataframe.")
  session$set_context_management_config(create_context_management_config(mode = "adaptive"))

  hits <- context_search(session, "sales dataframe", limit = 5)
  ids <- vapply(hits, function(handle) handle$id, character(1))
  expect_true(any(ids %in% c("object:sales_df", "memory:sales_plan")))

  object_hits <- context_search(session, "sales", kinds = "object", limit = 5)
  expect_true(all(vapply(object_hits, function(handle) identical(handle$kind, "object"), logical(1))))
})

test_that("object_peek delegates to semantic R inspection", {
  session <- aisdk::create_chat_session(model = MockModel$new())
  env <- session$get_envir()
  env$demo <- data.frame(x = 1:3)

  summary <- object_peek(session, "demo", detail = "summary")
  structure <- object_peek(session, "demo", detail = "structure")

  expect_match(summary, "Data Frame", fixed = TRUE)
  expect_match(structure, "demo", fixed = TRUE)
})

test_that("create_context_query_tools exposes session-bound tools", {
  session <- aisdk::create_chat_session(model = MockModel$new())
  env <- session$get_envir()
  env$demo <- data.frame(x = 1:3)
  session$set_context_management_config(create_context_management_config(mode = "adaptive"))

  tools <- create_context_query_tools(session)
  tool_names <- vapply(tools, function(tool_obj) tool_obj$name, character(1))

  expect_equal(tool_names, c("context_search", "context_get", "object_peek", "sub_session_query"))
  result <- tools[[which(tool_names == "context_search")]]$run(list(query = "demo"), envir = session$get_envir())
  expect_true(length(result) >= 1)
  expect_true(any(vapply(result, function(handle) identical(handle$id, "object:demo"), logical(1))))
})
