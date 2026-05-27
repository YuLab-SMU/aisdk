# Test ChatSession R6 class

openai_model_id <- get_openai_model_id()

test_that("ChatSession initializes correctly", {
  session <- ChatSession$new(
    model = openai_model_id,
    system_prompt = "You are a helpful assistant.",
    max_steps = 5
  )

  expect_s3_class(session, "ChatSession")
  expect_equal(session$get_model_id(), openai_model_id)
  expect_equal(length(session$get_history()), 0)
  expect_true(exists(".semantic_adapter_registry", envir = session$get_envir(), inherits = FALSE))
  expect_s3_class(get(".semantic_adapter_registry", envir = session$get_envir()), "SemanticAdapterRegistry")
})

test_that("register_semantic_adapter adds adapters to the session registry", {
  session <- ChatSession$new(model = openai_model_id)

  adapter <- create_semantic_adapter(
    name = "dummy-adapter",
    supports = function(obj) inherits(obj, "dummy_semantic_class"),
    capabilities = "identity",
    render_summary = function(obj, name = NULL) "dummy"
  )

  register_semantic_adapter(adapter, session = session)
  registry <- get_semantic_adapter_registry(session = session)

  expect_true("dummy-adapter" %in% registry$list_adapters())
})

test_that("create_chat_session factory works", {
  session <- create_chat_session(
    model = "anthropic:claude-3-5-sonnet-latest",
    system_prompt = "Test prompt"
  )

  expect_s3_class(session, "ChatSession")
  expect_equal(session$get_model_id(), "anthropic:claude-3-5-sonnet-latest")
})

test_that("append_message adds messages to history", {
  session <- ChatSession$new(model = openai_model_id)

  session$append_message("user", "Hello")
  session$append_message("assistant", "Hi there!")

  history <- session$get_history()
  expect_equal(length(history), 2)
  expect_equal(history[[1]]$role, "user")
  expect_equal(history[[1]]$content, "Hello")
  expect_equal(history[[2]]$role, "assistant")
  expect_equal(history[[2]]$content, "Hi there!")
})

test_that("get_last_response returns last assistant message", {
  session <- ChatSession$new(model = openai_model_id)

  expect_null(session$get_last_response())

  session$append_message("user", "Hello")
  expect_null(session$get_last_response())

  session$append_message("assistant", "Hi there!")
  expect_equal(session$get_last_response(), "Hi there!")

  session$append_message("user", "How are you?")
  expect_equal(session$get_last_response(), "Hi there!")

  session$append_message("assistant", "I'm doing well!")
  expect_equal(session$get_last_response(), "I'm doing well!")
})

test_that("clear_history removes all messages", {
  session <- ChatSession$new(model = openai_model_id)

  session$append_message("user", "Hello")
  session$append_message("assistant", "Hi!")

  expect_equal(length(session$get_history()), 2)

  session$clear_history()

  expect_equal(length(session$get_history()), 0)
})

test_that("switch_model changes the model", {
  session <- ChatSession$new(model = openai_model_id)

  expect_equal(session$get_model_id(), openai_model_id)

  session$switch_model("anthropic:claude-3-5-sonnet-latest")
  expect_equal(session$get_model_id(), "anthropic:claude-3-5-sonnet-latest")
})

test_that("stats tracks usage correctly", {
  session <- ChatSession$new(model = openai_model_id)

  stats <- session$stats()
  expect_equal(stats$messages_sent, 0)
  expect_equal(stats$total_tokens, 0)
  expect_equal(stats$tool_calls_made, 0)
})

test_that("as_list exports session state", {
  session <- ChatSession$new(
    model = openai_model_id,
    system_prompt = "Test system prompt",
    max_steps = 5,
    metadata = list(channel = list(channel_id = "feishu")),
    envir = local({
      e <- new.env(parent = emptyenv())
      e$.console_image_artifacts <- list(list(
        artifact_id = "img-0001",
        kind = "generated",
        artifacts = list(list(path = "/tmp/generated.png"))
      ))
      e$.console_image_artifact_next_id <- 2L
      e
    })
  )

  session$append_message("user", "Hello")
  session$append_message("assistant", "Hi!")

  data <- session$as_list()

  expect_equal(data$version, "1.0.0")
  expect_equal(data$model_id, openai_model_id)
  expect_equal(data$system_prompt, "Test system prompt")
  expect_equal(length(data$history), 2)
  expect_equal(data$max_steps, 5)
  expect_equal(data$metadata$channel$channel_id, "feishu")
  expect_equal(data$envir_state$console_image_artifacts[[1]]$artifact_id, "img-0001")
  expect_equal(data$envir_state$console_image_artifact_next_id, 2L)
})

test_that("restore_from_list restores session state", {
  # Create original session
  original <- ChatSession$new(
    model = openai_model_id,
    system_prompt = "Original prompt"
  )
  original$append_message("user", "Hello")
  original$append_message("assistant", "Hi!")

  # Export and restore
  data <- original$as_list()

  restored <- ChatSession$new()
  restored$restore_from_list(data)

  expect_equal(restored$get_model_id(), openai_model_id)
  expect_equal(length(restored$get_history()), 2)
  expect_equal(restored$get_last_response(), "Hi!")
})

test_that("restore_from_list restores console image artifact state into session environment", {
  data <- list(
    model_id = openai_model_id,
    history = list(),
    envir_state = list(
      console_image_artifacts = list(list(
        artifact_id = "img-0004",
        kind = "edited",
        artifacts = list(list(path = "/tmp/edited.png"))
      )),
      console_image_artifact_next_id = 5L
    )
  )

  restored <- ChatSession$new()
  restored$restore_from_list(data)
  envir <- restored$get_envir()

  expect_equal(envir$.console_image_artifacts[[1]]$artifact_id, "img-0004")
  expect_equal(envir$.console_image_artifact_next_id, 5L)
})

test_that("save and load_chat_session work with RDS", {
  skip_on_cran()

  session <- ChatSession$new(
    model = openai_model_id,
    system_prompt = "Test prompt"
  )
  session$append_message("user", "Hello")
  session$append_message("assistant", "Hi there!")

  # Save to temp file
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file), add = TRUE)

  session$save(temp_file)
  expect_true(file.exists(temp_file))

  # Load and verify
  loaded <- load_chat_session(temp_file)

  expect_s3_class(loaded, "ChatSession")
  expect_equal(loaded$get_model_id(), openai_model_id)
  expect_equal(length(loaded$get_history()), 2)
  expect_equal(loaded$get_last_response(), "Hi there!")
})

test_that("save and load_chat_session work with JSON", {
  skip_on_cran()

  session <- ChatSession$new(
    model = "anthropic:claude-3-5-sonnet-latest",
    system_prompt = "JSON test"
  )
  session$append_message("user", "Test message")
  session$append_message("assistant", "Test response")

  # Save to temp JSON file
  temp_file <- tempfile(fileext = ".json")
  on.exit(unlink(temp_file), add = TRUE)

  session$save(temp_file)
  expect_true(file.exists(temp_file))

  # Verify it's valid JSON
  json_content <- readLines(temp_file, warn = FALSE)
  parsed <- jsonlite::fromJSON(paste(json_content, collapse = "\n"))
  expect_equal(parsed$model_id, "anthropic:claude-3-5-sonnet-latest")

  # Load and verify
  loaded <- load_chat_session(temp_file)

  expect_s3_class(loaded, "ChatSession")
  expect_equal(loaded$get_model_id(), "anthropic:claude-3-5-sonnet-latest")
  expect_equal(length(loaded$get_history()), 2)
})

test_that("load_chat_session warns about missing tools", {
  skip_on_cran()

  # Create session with "tools" (we'll fake tool names in the export)
  session <- ChatSession$new(model = openai_model_id)
  session$append_message("user", "Hello")

  # Save
  temp_file <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_file), add = TRUE)

  # Manually add tool names to saved data
  data <- session$as_list()
  data$tool_names <- c("get_weather", "search_web")
  saveRDS(data, temp_file)

  # Load without providing tools - should warn
  expect_warning(
    loaded <- load_chat_session(temp_file),
    "Original session used tools"
  )
})

test_that("ChatSession print method works", {
  session <- ChatSession$new(
    model = openai_model_id
  )
  session$append_message("user", "Hello")

  expect_output(print(session), "ChatSession")
  expect_output(print(session), openai_model_id)
  expect_output(print(session), "1 messages")
})

test_that("ChatSession metadata helpers round-trip", {
  session <- ChatSession$new(model = openai_model_id)

  expect_null(session$get_metadata("channel"))

  session$set_metadata("channel", list(channel_id = "feishu"))
  session$merge_metadata(list(parent_session_key = "root"))

  expect_equal(session$get_metadata("channel")$channel_id, "feishu")
  expect_equal(session$get_metadata("parent_session_key"), "root")
  expect_true(all(c("channel", "parent_session_key") %in% session$list_metadata()))
})

test_that("ChatSession stores a single multimodal content block as a block list", {
  session <- ChatSession$new(model = MockModel$new())

  session$append_message("user", input_image("https://example.com/dog.png"))
  history <- session$get_history()

  expect_length(history[[1]]$content, 1)
  expect_equal(history[[1]]$content[[1]]$type, "input_image")
})
