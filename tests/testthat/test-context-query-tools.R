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

  expect_equal(tool_names, c("context_search", "context_get", "memory_write", "object_peek", "sub_session_query"))
  result <- tools[[which(tool_names == "context_search")]]$run(list(query = "demo"), envir = session$get_envir())
  expect_true(length(result) >= 1)
  expect_true(any(vapply(result, function(handle) identical(handle$id, "object:demo"), logical(1))))
})

# --- Y2: agent-callable memory_write (structured note-taking) -----------------

test_that("memory_write persists a note that round-trips through the read side", {
  source(test_path("helper-mock.R"))
  session <- create_chat_session(model = MockModel$new())
  tools <- create_context_query_tools(session = session)
  memory_write <- Filter(function(t) t$name == "memory_write", tools)[[1]]
  expect_false(is.null(memory_write))

  out <- memory_write$run(list(key = "db_schema", value = "users(id, name, email)"))
  expect_match(out, "Saved note")
  # Persisted in session memory...
  expect_equal(session$get_memory("db_schema"), "users(id, name, email)")
  expect_true("db_schema" %in% session$list_memory())
  # ...and surfaced through the existing memory context-handle read path.
  handles <- aisdk:::build_memory_context_handles(session)
  expect_true(any(vapply(handles, function(h) identical(h$kind, "memory"), logical(1))))
})

test_that("memory_write guards empty keys and a missing session", {
  source(test_path("helper-mock.R"))
  session <- create_chat_session(model = MockModel$new())
  mw <- Filter(function(t) t$name == "memory_write", create_context_query_tools(session = session))[[1]]
  expect_match(mw$run(list(key = "", value = "x")), "non-empty")

  mw_nosess <- Filter(function(t) t$name == "memory_write",
                      create_context_query_tools(session = NULL))[[1]]
  expect_match(mw_nosess$run(list(key = "k", value = "v")), "requires a session")
})
