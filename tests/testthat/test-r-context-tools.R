test_that("resolve_r_binding prefers live session environment over search path", {
  env <- new.env(parent = globalenv())
  env$mean <- 42

  binding <- resolve_r_binding("mean", envir = env)

  expect_equal(binding$location, "session_env")
  expect_equal(binding$kind, "object")
  expect_equal(binding$object, 42)
})

test_that("resolve_r_binding can resolve a package function directly", {
  binding <- resolve_r_binding("lm", package = "stats")

  expect_equal(binding$location, "namespace")
  expect_equal(binding$package, "stats")
  expect_equal(binding$kind, "function")
  expect_true(is.function(binding$object))
})

test_that("list_r_objects returns compact metadata for live objects", {
  env <- new.env(parent = emptyenv())
  env$df <- data.frame(x = 1:3)
  env$model <- lm(mpg ~ wt, data = mtcars)

  objects <- list_r_objects(envir = env)

  expect_true(all(c("name", "class", "type", "size") %in% names(objects)))
  expect_true("df" %in% objects$name)
  expect_true("model" %in% objects$name)
})

test_that("inspect_r_object uses the active semantic adapter registry", {
  session <- create_chat_session(model = MockModel$new())
  env <- session$get_envir()
  env$custom_obj <- structure(list(value = 1), class = "custom_object_card")

  adapter <- create_semantic_adapter(
    name = "custom-object-adapter",
    priority = 100,
    supports = function(obj) inherits(obj, "custom_object_card"),
    capabilities = c("identity", "schema", "semantics"),
    render_summary = function(obj, name = NULL) paste("custom summary for", name),
    describe_semantics = function(obj) list(summary = "custom semantics")
  )
  register_semantic_adapter(adapter, session = session)

  summary_text <- inspect_r_object("custom_obj", session = session, detail = "summary")
  structured <- inspect_r_object("custom_obj", session = session, detail = "structured")

  expect_match(summary_text, "custom summary for custom_obj", fixed = TRUE)
  expect_equal(structured$adapter, "custom-object-adapter")
})

test_that("inspect_r_function reports signature and function kind", {
  env <- new.env(parent = globalenv())
  env$my_generic <- function(x, y = 1) UseMethod("my_generic")

  result <- inspect_r_function("my_generic", envir = env, detail = "summary")

  expect_match(result, "Signature: my_generic\\(x, y = 1\\)")
  expect_match(result, "Kind: closure, S3 generic", fixed = TRUE)
})

test_that("get_r_documentation returns installed help text and degrades for user functions", {
  doc_text <- get_r_documentation("lm", package = "stats", section = "summary")
  expect_match(doc_text, "Topic: stats::lm", fixed = TRUE)
  expect_match(doc_text, "Usage:", fixed = TRUE)

  env <- new.env(parent = globalenv())
  env$my_fn <- function(x) x
  fallback <- get_r_documentation("my_fn", envir = env)
  expect_match(fallback, "No installed Rd documentation found", fixed = TRUE)
  expect_match(fallback, "Fallback function summary:", fixed = TRUE)
})

test_that("get_r_source returns closure source and handles primitives clearly", {
  env <- new.env(parent = globalenv())
  env$my_fn <- function(x, y = 2) x + y

  source_text <- get_r_source("my_fn", envir = env, max_lines = 20L)
  primitive_text <- get_r_source("sum", package = "base")

  expect_match(source_text, "Source: my_fn", fixed = TRUE)
  expect_match(source_text, "function \\(x, y = 2\\)")
  expect_match(primitive_text, "not available in R because it is a primitive", fixed = TRUE)
})

test_that("create_r_context_tools exposes expected built-in tools", {
  tools <- create_r_context_tools()
  tool_names <- vapply(tools, function(t) t$name, character(1))

  expect_true(all(c(
    "list_r_objects",
    "inspect_r_object",
    "inspect_r_function",
    "get_r_documentation",
    "get_r_source"
  ) %in% tool_names))

  env <- new.env(parent = globalenv())
  env$demo_df <- data.frame(x = 1:2)

  inspect_tool <- tools[[which(tool_names == "inspect_r_object")]]
  result <- inspect_tool$run(list(name = "demo_df", detail = "summary"), envir = env)

  expect_match(result, "Data Frame", fixed = TRUE)
})
