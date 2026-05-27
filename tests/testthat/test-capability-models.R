library(testthat)
library(aisdk)

local_mock_registry <- function() {
  registry <- ProviderRegistry$new()
  registry$register("mock", function(model_id) {
    model <- MockModel$new()
    model$model_id <- model_id
    model$capabilities <- switch(model_id,
      "vision" = list(vision_input = TRUE),
      "session-vision" = list(vision_input = TRUE),
      "agent-vision" = list(vision_input = TRUE),
      "text" = list(vision_input = FALSE),
      list()
    )
    model
  })
  registry
}

local_capability_routes <- function() {
  old <- aisdk:::get_capability_model_routes()
  withr::defer(aisdk:::store_capability_model_routes(old), envir = parent.frame())
  clear_capability_model()
}

test_that("capability model routes can be set, listed, and cleared", {
  local_capability_routes()

  expect_null(get_capability_model("vision.inspect"))

  previous <- set_capability_model(
    "vision.inspect",
    "openai:gpt-4o",
    type = "language",
    required_model_capabilities = "vision_input"
  )

  expect_null(previous)
  expect_equal(get_capability_model("vision.inspect"), "openai:gpt-4o")

  routes <- list_capability_models()
  expect_equal(nrow(routes), 1L)
  expect_equal(routes$capability, "vision.inspect")
  expect_equal(routes$model, "openai:gpt-4o")
  expect_equal(routes$type, "language")
  expect_equal(routes$required_model_capabilities, "vision_input")

  previous <- set_capability_model("vision.inspect", "anthropic:claude-sonnet-4-20250514")
  expect_equal(previous, "openai:gpt-4o")
  expect_equal(get_capability_model("vision.inspect"), "anthropic:claude-sonnet-4-20250514")

  clear_capability_model("vision.inspect")
  expect_null(get_capability_model("vision.inspect"))
  expect_equal(nrow(list_capability_models()), 0L)
})

test_that("resolve_model_for_capability uses global routes and validates explicit false capabilities", {
  local_capability_routes()
  registry <- local_mock_registry()

  set_capability_model(
    "vision.inspect",
    "mock:vision",
    type = "language",
    required_model_capabilities = "vision_input"
  )

  model <- resolve_model_for_capability(
    "vision.inspect",
    type = "language",
    required_model_capabilities = "vision_input",
    registry = registry
  )

  expect_s3_class(model, "MockModel")
  expect_equal(model$model_id, "vision")

  set_capability_model(
    "vision.inspect",
    "mock:text",
    type = "language",
    required_model_capabilities = "vision_input"
  )

  expect_error(
    resolve_model_for_capability(
      "vision.inspect",
      type = "language",
      required_model_capabilities = "vision_input",
      registry = registry
    ),
    "does not advertise required model capability"
  )
})

test_that("session capability routes override global routes", {
  local_capability_routes()
  registry <- local_mock_registry()

  set_capability_model("vision.inspect", "mock:vision", type = "language")
  session <- ChatSession$new(model = "mock:text", registry = registry)
  session$set_capability_model(
    "vision.inspect",
    "mock:session-vision",
    type = "language",
    required_model_capabilities = "vision_input"
  )

  model <- resolve_model_for_capability(
    "vision.inspect",
    type = "language",
    required_model_capabilities = "vision_input",
    session = session,
    registry = registry
  )

  expect_equal(model$model_id, "session-vision")
  expect_equal(session$get_capability_model("vision.inspect"), "mock:session-vision")
  expect_equal(session$list_capability_models()$capability, "vision.inspect")
})

test_that("agent capability routes are inherited by sessions", {
  local_capability_routes()

  agent <- create_agent(
    name = "VisionAgent",
    description = "Uses a specialized vision model",
    capability_models = list(
      "vision.inspect" = list(
        model = "mock:agent-vision",
        type = "language",
        required_model_capabilities = "vision_input"
      )
    )
  )
  session <- create_chat_session(model = "mock:text", agent = agent)

  expect_equal(session$get_capability_model("vision.inspect"), "mock:agent-vision")
  expect_equal(session$get_envir()$.capability_models[["vision.inspect"]]$model, "mock:agent-vision")
})
