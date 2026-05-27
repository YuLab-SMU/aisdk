test_that("Shiny chat session normalization accepts existing sessions", {
  chat <- create_chat_session(model = MockModel$new(), tools = list())

  expect_identical(
    aisdk:::normalize_shiny_chat_session(session = chat, model = "ignored"),
    chat
  )
})

test_that("Shiny chat session normalization reuses console startup model", {
  testthat::local_mocked_bindings(
    resolve_console_startup_model = function(...) {
      list(model_id = "mock:from-console", source = "test", profile = NULL)
    },
    .package = "aisdk"
  )

  chat <- aisdk:::normalize_shiny_chat_session()

  expect_identical(chat$get_model_id(), "mock:from-console")
})

test_that("Shiny explicit startup model beats console discovery", {
  testthat::local_mocked_bindings(
    resolve_console_startup_model = function(...) {
      list(model_id = "mock:from-console", source = "test", profile = NULL)
    },
    .package = "aisdk"
  )

  explicit <- aisdk:::normalize_shiny_chat_session(model = "mock:explicit")
  selected <- aisdk:::normalize_shiny_chat_session(selected_model = "mock:selected")

  expect_identical(explicit$get_model_id(), "mock:explicit")
  expect_identical(selected$get_model_id(), "mock:selected")
})

test_that("aiChatUI validates optional shinychat backend clearly", {
  ui <- aiChatUI("chat", backend = "aisdk")
  expect_s3_class(ui, "shiny.tag.list")
  deps <- htmltools::findDependencies(ui)
  expect_true(any(vapply(deps, function(dep) identical(dep$name, "aisdk-chat"), logical(1))))
  expect_match(as.character(ui), 'data-chat-id="chat-chat_root"', fixed = TRUE)
  expect_match(as.character(ui), 'id="chat-model"', fixed = TRUE)
  expect_match(as.character(ui), 'id="chat-model_apply"', fixed = TRUE)

  if (!requireNamespace("shinychat", quietly = TRUE)) {
    expect_error(
      aiChatUI("chat", backend = "shinychat"),
      "shinychat"
    )
  } else {
    ui <- aiChatUI("chat", backend = "shinychat")
    expect_s3_class(ui, "shiny.tag")
  }
})

test_that("aiChatServer validates optional shinychat backend clearly", {
  if (!requireNamespace("shinychat", quietly = TRUE)) {
    expect_error(
      aiChatServer("chat", model = MockModel$new(), backend = "shinychat"),
      "shinychat"
    )
  }
})

test_that("Shiny turn prompt combines explicit system and reactive context", {
  context <- function() list(color = "red", points = 3)

  prompt <- aisdk:::shiny_turn_system_prompt(
    base = "Base instruction",
    context = context
  )

  expect_match(prompt, "Base instruction", fixed = TRUE)
  expect_match(prompt, "Current application state", fixed = TRUE)
  expect_match(prompt, "- color: red", fixed = TRUE)
  expect_match(prompt, "- points: 3", fixed = TRUE)
})

test_that("Shiny runtime exposes current model when UI model list is empty", {
  chat <- create_chat_session(model = MockModel$new())
  runtime <- aisdk:::ShinyChatRuntime$new(chat_session = chat, models = NULL)

  snapshot <- runtime$current_model_snapshot()

  expect_identical(snapshot$id, "mock:mock-model")
  expect_identical(snapshot$label, "mock:mock-model")
})

test_that("Shiny thinking formatter can show or hide thinking blocks", {
  text <- "<think>\nwork it out\n</think>\n\nFinal answer"

  visible <- aisdk:::format_thinking_html(text, show_thinking = TRUE)
  hidden <- aisdk:::format_thinking_html(text, show_thinking = FALSE)

  expect_match(visible, "Thinking")
  expect_match(visible, "work it out")
  expect_match(visible, "Final answer")
  expect_no_match(hidden, "work it out")
  expect_match(hidden, "Final answer")
})

test_that("Shiny assistant streaming starts a structured turn event", {
  sent <- list()
  fake_session <- list(
    userData = new.env(parent = emptyenv()),
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    }
  )

  aisdk:::render_message(fake_session, "chat", "assistant", "", streaming = TRUE, turn_id = "turn_1")

  expect_length(sent, 1)
  expect_identical(sent[[1]]$type, "aisdk_chat_event_chat")
  expect_identical(sent[[1]]$message$type, "turn_start")
  expect_identical(sent[[1]]$message$turn_id, "turn_1")
  expect_identical(sent[[1]]$message$status, "thinking")
  expect_identical(fake_session$userData$streaming_content_text, "")
  expect_identical(fake_session$userData$streaming_thinking_text, "")
})

test_that("Shiny tool rendering uses structured tool events", {
  sent <- list()
  fake_session <- list(
    userData = new.env(parent = emptyenv()),
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    }
  )
  fake_session$userData$aisdk_chat_id <- "chat"
  fake_session$userData$aisdk_chat_renderer <- aisdk:::create_shiny_chat_renderer("aisdk", fake_session, "chat")
  fake_session$userData$streaming_turn_id <- "turn_1"
  fake_session$userData$streaming_tool_index <- 0L

  tool_id <- aisdk:::render_tool_start(
    fake_session,
    "chat",
    tool_name = "lookup",
    arguments = list(query = "x"),
    debug = FALSE
  )
  aisdk:::render_tool_result(
    fake_session,
    "chat",
    tool_name = "lookup",
    result = "ok",
    success = TRUE,
    debug = FALSE,
    tool_id = tool_id
  )

  event_messages <- Filter(function(x) identical(x$type, "aisdk_chat_event_chat"), sent)
  patches <- Filter(function(x) identical(x$message$type, "turn_patch"), event_messages)
  tool_start <- patches[[1]]$message
  tool_result <- patches[[2]]$message
  expect_identical(tool_start$blocks[[1]]$name, "lookup")
  expect_identical(tool_start$turn_id, "turn_1")
  expect_identical(tool_result$turn_id, "turn_1")
  expect_identical(tool_result$blocks[[1]]$block_id, tool_start$blocks[[1]]$block_id)
})

test_that("Shiny streaming content renders markdown incrementally", {
  sent <- list()
  fake_session <- list(
    userData = new.env(parent = emptyenv()),
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    }
  )
  fake_session$userData$aisdk_chat_renderer <- aisdk:::create_shiny_chat_renderer("aisdk", fake_session, "chat")
  fake_session$userData$streaming_turn_id <- "turn_1"
  fake_session$userData$streaming_thinking <- FALSE
  fake_session$userData$streaming_content_text <- ""
  fake_session$userData$streaming_thinking_text <- ""

  aisdk:::update_streaming_message(
    fake_session,
    "chat",
    "This is **bold**",
    show_thinking = TRUE
  )

  content_updates <- Filter(function(x) {
    identical(x$type, "aisdk_chat_event_chat") && identical(x$message$type, "turn_patch")
  }, sent)
  expect_true(length(content_updates) >= 1)
  expect_identical(content_updates[[length(content_updates)]]$message$turn_id, "turn_1")
  expect_match(content_updates[[length(content_updates)]]$message$blocks[[1]]$html, "<strong>bold</strong>", fixed = TRUE)
})

test_that("Shiny v2 turn patches carry stable ids, seq, and complete blocks", {
  sent <- list()
  fake_session <- list(
    userData = new.env(parent = emptyenv()),
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    }
  )
  renderer <- aisdk:::create_shiny_chat_renderer("aisdk", fake_session, "chat")

  renderer$assistant_start("turn_1", model = list(id = "mock:a", label = "Mock A"))
  renderer$content_replace("turn_1", "Hello **there**")
  renderer$content_replace("turn_1", "Hello **there**\n\n| a | b |\n|---|---|\n| 1 | 2 |")

  patches <- Filter(function(x) identical(x$message$type, "turn_patch"), sent)
  expect_true(length(patches) >= 2)
  seqs <- vapply(patches, function(x) x$message$seq, integer(1))
  expect_true(all(diff(seqs) > 0))
  expect_identical(patches[[length(patches)]]$message$turn_id, "turn_1")
  expect_length(patches[[length(patches)]]$message$blocks, 1)
  expect_match(patches[[length(patches)]]$message$blocks[[1]]$html, "<table>", fixed = TRUE)
})

test_that("Shiny markdown renderer enables table, tasklist, autolink, and tagfilter", {
  html <- aisdk:::render_chat_markdown(paste(
    "- [x] done",
    "",
    "| a | b |",
    "|---|---|",
    "| 1 | 2 |",
    "",
    "https://example.com",
    "",
    "<script>alert(1)</script>",
    sep = "\n"
  ))

  expect_match(html, "<table>", fixed = TRUE)
  expect_match(html, "checkbox", fixed = TRUE)
  expect_match(html, '<a href="https://example.com">', fixed = TRUE)
  expect_no_match(html, "<script>", fixed = TRUE)
})

test_that("Shiny renderer hides thinking when configured", {
  sent <- list()
  fake_session <- list(
    userData = new.env(parent = emptyenv()),
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    }
  )
  renderer <- aisdk:::create_shiny_chat_renderer("aisdk", fake_session, "chat", show_thinking = FALSE)
  fake_session$userData$aisdk_chat_renderer <- renderer
  fake_session$userData$streaming_turn_id <- "turn_1"
  fake_session$userData$streaming_thinking <- FALSE
  fake_session$userData$streaming_content_text <- ""
  fake_session$userData$streaming_thinking_text <- ""

  aisdk:::update_streaming_message(
    fake_session,
    "chat",
    "<think>hidden</think>Final",
    show_thinking = FALSE,
    renderer = renderer
  )

  expect_false(any(vapply(sent, function(x) {
    identical(x$message$type, "thinking_replace")
  }, logical(1))))
  expect_true(any(vapply(sent, function(x) {
    identical(x$message$type, "turn_patch") &&
      grepl("Final", x$message$blocks[[1]]$html, fixed = TRUE)
  }, logical(1))))
})

test_that("Shiny v2 final answer does not duplicate visible thinking block", {
  sent <- list()
  fake_session <- list(
    userData = new.env(parent = emptyenv()),
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    }
  )
  renderer <- aisdk:::create_shiny_chat_renderer("aisdk", fake_session, "chat", show_thinking = TRUE)
  fake_session$userData$aisdk_chat_renderer <- renderer
  fake_session$userData$streaming_turn_id <- "turn_1"
  fake_session$userData$streaming_thinking <- FALSE
  fake_session$userData$streaming_content_text <- ""
  fake_session$userData$streaming_thinking_text <- ""

  renderer$assistant_start("turn_1")
  aisdk:::update_streaming_message(fake_session, "chat", "<think>hidden work</think>\n\nFinal answer", renderer = renderer)
  aisdk:::finalize_streaming_message(fake_session, "chat", "<think>hidden work</think>\n\nFinal answer", renderer = renderer)

  done <- Filter(function(x) identical(x$message$type, "turn_done"), sent)[[1]]$message
  expect_length(done$blocks, 2)
  expect_identical(done$blocks[[1]]$type, "thinking")
  expect_identical(done$blocks[[2]]$type, "markdown")
  expect_no_match(done$blocks[[2]]$html, "hidden work", fixed = TRUE)
  expect_match(done$blocks[[2]]$html, "Final answer", fixed = TRUE)
})

test_that("Shiny named attachment blocks serialize as OpenAI content arrays", {
  content <- aisdk:::shiny_user_content_blocks(
    "评价一下这个卡片",
    list(`1` = input_image("https://example.com/card.png", media_type = "image/png"))
  )
  translated <- aisdk:::translate_message_content(content, target = "openai_chat")
  json <- jsonlite::toJSON(
    list(messages = list(list(role = "user", content = translated))),
    auto_unbox = TRUE,
    null = "null"
  )

  expect_null(names(content))
  expect_null(names(translated))
  expect_match(json, '"content":\\[\\{"type":"text"', perl = TRUE)
  expect_no_match(json, '"content":\\{', perl = TRUE)
})

test_that("Shiny renderer error clears busy state", {
  sent <- list()
  fake_session <- list(
    userData = new.env(parent = emptyenv()),
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    }
  )
  renderer <- aisdk:::create_shiny_chat_renderer("aisdk", fake_session, "chat")

  renderer$error(turn_id = "turn_1", message = "failed")

  expect_true(any(vapply(sent, function(x) {
    identical(x$message$type, "turn_error") &&
      identical(x$message$turn_id, "turn_1") &&
      identical(x$message$blocks[[1]]$content, "failed")
  }, logical(1))))
  expect_true(any(vapply(sent, function(x) {
    identical(x$message$type, "busy_patch") &&
      identical(x$message$busy, FALSE)
  }, logical(1))))
})

test_that("Shiny renderer uses the namespaced root id as the event channel", {
  sent <- list()
  fake_session <- list(
    ns = function(x) paste0("outer-chat-", x),
    userData = new.env(parent = emptyenv()),
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
    }
  )
  renderer <- aisdk:::create_shiny_chat_renderer("aisdk", fake_session, "chat")

  renderer$assistant_end(turn_id = "turn_1", content = "done")

  expect_identical(sent[[1]]$type, "aisdk_chat_event_outer-chat-chat_root")
  expect_identical(sent[[1]]$message$chat_id, "outer-chat-chat_root")
  expect_identical(sent[[1]]$message$type, "turn_done")
})

test_that("Shinychat backend uses message chunk protocol", {
  calls <- list()
  fake_session <- list(userData = new.env(parent = emptyenv()))
  testthat::local_mocked_bindings(
    shinychat_append_message_impl = function(id, msg, chunk = FALSE, operation = "append", session) {
      calls[[length(calls) + 1L]] <<- list(
        id = id,
        msg = msg,
        chunk = chunk,
        operation = operation
      )
    }
  )

  aisdk:::render_message(
    fake_session,
    "chat",
    "assistant",
    "",
    streaming = TRUE,
    backend = "shinychat"
  )
  aisdk:::update_streaming_message(
    fake_session,
    "chat",
    "This is **bold**",
    backend = "shinychat"
  )
  aisdk:::finalize_streaming_message(
    fake_session,
    "chat",
    content = "This is **bold**",
    backend = "shinychat"
  )

  expect_equal(calls[[1]]$chunk, "start")
  expect_equal(calls[[2]]$chunk, TRUE)
  expect_equal(calls[[3]]$chunk, "end")
  expect_equal(calls[[3]]$operation, "replace")
  expect_equal(calls[[3]]$msg$content, "This is **bold**")
})

test_that("Shiny chat uses ChatSession streaming tool execution path", {
  calc_tool <- tool(
    name = "calculate",
    description = "Perform a mathematical calculation on two numbers",
    parameters = z_object(
      a = z_number("The first number"),
      b = z_number("The second number"),
      operation = z_enum(c("add", "subtract", "multiply", "divide"), "The operator")
    ),
    execute = function(args) {
      switch(
        args$operation,
        add = args$a + args$b,
        subtract = args$a - args$b,
        multiply = args$a * args$b,
        divide = if (args$b != 0) args$a / args$b else "Error: Division by zero"
      )
    }
  )

  mock_model <- MockModel$new()
  mock_model$add_response(
    tool_calls = list(list(
      id = "call_1",
      name = "calculate",
      arguments = list(a = 2, b = 3, operation = "multiply")
    ))
  )
  mock_model$add_response(text = "The answer is 6.")

  chat <- aisdk:::normalize_shiny_chat_session(
    model = mock_model,
    tools = list(calc_tool)
  )

  chunks <- character()
  chat$send_stream("What is 2 * 3?", callback = function(text, done) {
    if (!isTRUE(done) && nzchar(text %||% "")) {
      chunks <<- c(chunks, text)
    }
  })

  expect_equal(chat$get_last_response(), "The answer is 6.")
  tool_messages <- Filter(function(msg) identical(msg$role, "tool"), chat$get_history())
  expect_length(tool_messages, 1)
  expect_match(tool_messages[[1]]$content, "6")
})

test_that("Shiny manager reports exited workers that leave running status", {
  fake_proc <- list(
    is_alive = function() FALSE,
    get_result = function() {
      stop("worker crashed")
    }
  )
  manager <- aisdk:::ShinyChatManager$new()
  tmp <- tempfile("aisdk-shiny-manager-")
  dir.create(tmp)
  withr::defer(unlink(tmp, recursive = TRUE, force = TRUE))
  manager$proc <- fake_proc
  manager$chunk_file <- file.path(tmp, "chunks.txt")
  manager$status_file <- file.path(tmp, "status.rds")
  manager$chunk_offset <- 0L
  file.create(manager$chunk_file)
  aisdk:::write_shiny_status(manager$status_file, list(status = "running"))

  result <- manager$poll()

  expect_match(result$error, "worker crashed")
})

test_that("Shiny manager reports exited workers that leave waiting tool status", {
  fake_proc <- list(
    is_alive = function() FALSE,
    get_result = function() {
      "finished without final status"
    }
  )
  manager <- aisdk:::ShinyChatManager$new()
  tmp <- tempfile("aisdk-shiny-manager-")
  dir.create(tmp)
  withr::defer(unlink(tmp, recursive = TRUE, force = TRUE))
  manager$proc <- fake_proc
  manager$chunk_file <- file.path(tmp, "chunks.txt")
  manager$status_file <- file.path(tmp, "status.rds")
  manager$chunk_offset <- 0L
  file.create(manager$chunk_file)
  aisdk:::write_shiny_status(manager$status_file, list(
    status = "waiting_tool",
    tool_index = 1L,
    tool_request = list(id = "call_1", name = "lookup", arguments = list())
  ))

  result <- manager$poll()

  expect_match(result$error, "exited before completing")
})

test_that("Shiny manager completes a real callr worker response", {
  manager <- aisdk:::ShinyChatManager$new()
  withr::defer(manager$cleanup())
  manager$start_generation(
    model = MockModel$new(list(list(
      text = "worker response",
      finish_reason = "stop"
    ))),
    messages = list(list(role = "user", content = "hi")),
    tools = list(),
    call_options = list(),
    max_steps = 10
  )

  result <- NULL
  for (i in seq_len(50)) {
    Sys.sleep(0.1)
    result <- manager$poll()
    if (!is.null(result) && (isTRUE(result$done) || !is.null(result$error))) {
      break
    }
  }

  expect_null(result$error)
  expect_true(isTRUE(result$done))
  expect_identical(result$text, "worker response")
  expect_identical(result$result$messages_added[[1]]$content, "worker response")
})

test_that("Shiny manager reports real callr worker errors", {
  manager <- aisdk:::ShinyChatManager$new()
  withr::defer(manager$cleanup())
  manager$start_generation(
    model = MockModel$new(list(function(params) {
      stop("model failed")
    })),
    messages = list(list(role = "user", content = "hi")),
    tools = list(),
    call_options = list(),
    max_steps = 10
  )

  result <- NULL
  for (i in seq_len(50)) {
    Sys.sleep(0.1)
    result <- manager$poll()
    if (!is.null(result) && (isTRUE(result$done) || !is.null(result$error))) {
      break
    }
  }

  expect_false(isTRUE(result$done))
  expect_match(result$error, "model failed")
  expect_no_match(result$error, "write_shiny_status", fixed = TRUE)
})

test_that("Shiny manager completes a real callr worker tool roundtrip", {
  calc_tool <- tool(
    name = "calculate",
    description = "Calculate a value",
    parameters = z_object(
      value = z_number("Value")
    ),
    execute = function(args) args$value
  )
  mock_model <- MockModel$new()
  mock_model$add_response(tool_calls = list(list(
    id = "call_1",
    name = "calculate",
    arguments = list(value = 42)
  )))
  mock_model$add_response(text = "The value is 42.")

  manager <- aisdk:::ShinyChatManager$new()
  withr::defer(manager$cleanup())
  manager$start_generation(
    model = mock_model,
    messages = list(list(role = "user", content = "calculate")),
    tools = list(calc_tool),
    call_options = list(),
    max_steps = 10
  )

  tool_request <- NULL
  final_result <- NULL
  for (i in seq_len(100)) {
    Sys.sleep(0.1)
    poll_result <- manager$poll()
    if (is.null(poll_result)) {
      next
    }
    if (!is.null(poll_result$tool_request)) {
      tool_request <- poll_result$tool_request
      manager$resolve_tool(list(
        id = tool_request$id,
        name = tool_request$name,
        result = "42",
        is_error = FALSE
      ))
    }
    if (isTRUE(poll_result$done) || !is.null(poll_result$error)) {
      final_result <- poll_result
      break
    }
  }

  expect_identical(tool_request$name, "calculate")
  expect_null(final_result$error)
  expect_true(isTRUE(final_result$done))
  expect_identical(final_result$result$text, "The value is 42.")
  expect_true(any(vapply(
    final_result$result$messages_added,
    function(msg) identical(msg$role, "tool"),
    logical(1)
  )))
})

test_that("Shiny chunk reader only consumes complete UTF-8 frames", {
  tmp <- tempfile("aisdk-shiny-chunks-")
  withr::defer(unlink(tmp, force = TRUE))

  first_text <- "\u4f60\u597d"
  second_text <- "\u4e16\u754c"
  first <- base64enc::base64encode(charToRaw(enc2utf8(first_text)))
  second <- base64enc::base64encode(charToRaw(enc2utf8(second_text)))
  split_at <- 3L
  writeBin(
    charToRaw(paste0(first, "\n", substr(second, 1L, split_at))),
    tmp
  )

  first_read <- aisdk:::read_shiny_chunks(tmp, 0L)
  expect_identical(first_read$value, first_text)

  cat(substr(second, split_at + 1L, nchar(second)), "\n", file = tmp, append = TRUE, sep = "")
  second_read <- aisdk:::read_shiny_chunks(tmp, first_read$offset)
  expect_identical(second_read$value, second_text)
})

test_that("Shiny tool request execution uses Tool run path and session environment", {
  counter_tool <- tool(
    name = "increment_counter",
    description = "Increment a counter in the session environment",
    parameters = z_object(
      amount = z_number("Increment amount")
    ),
    execute = function(args) {
      current <- get0("counter", envir = args$.envir, ifnotfound = 0)
      assign("counter", current + args$amount, envir = args$.envir)
      current + args$amount
    }
  )

  env <- new.env(parent = emptyenv())
  result <- aisdk:::execute_shiny_tool_request(
    tool_request = list(
      id = "call_counter",
      name = "increment_counter",
      arguments = list(amount = 4)
    ),
    tools = list(counter_tool),
    envir = env
  )

  expect_false(isTRUE(result$is_error))
  expect_equal(as.character(result$result), "4")
  expect_equal(env$counter, 4)
})

test_that("Shiny generation result preserves assistant tool calls and tool messages", {
  chat <- create_chat_session(model = MockModel$new())
  chat$append_message("user", "Use a tool")

  assistant_tool_message <- list(
    role = "assistant",
    content = "",
    tool_calls = list(list(
      id = "call_1",
      type = "function",
      `function` = list(name = "lookup", arguments = "{\"x\":1}")
    ))
  )
  tool_message <- list(
    role = "tool",
    tool_call_id = "call_1",
    name = "lookup",
    content = "42"
  )
  final_message <- list(role = "assistant", content = "The result is 42.")

  aisdk:::apply_shiny_generation_result(chat, list(
    messages_added = list(assistant_tool_message, tool_message, final_message)
  ))

  history <- chat$get_history()
  expect_equal(history[[2]]$tool_calls[[1]]$`function`$name, "lookup")
  expect_identical(history[[3]], tool_message)
  expect_equal(chat$get_last_response(), "The result is 42.")
})

test_that("Shiny result stream text exposes final reasoning as thinking block", {
  streamed <- aisdk:::shiny_result_stream_text(list(
    text = "Final answer",
    reasoning = "Internal reasoning summary"
  ))

  expect_match(streamed, "<think>", fixed = TRUE)
  expect_match(streamed, "Internal reasoning summary", fixed = TRUE)
  expect_match(streamed, "Final answer", fixed = TRUE)

  already_tagged <- aisdk:::shiny_result_stream_text(list(
    text = "<think>\nstreamed\n</think>\n\nFinal",
    reasoning = "duplicate"
  ))
  expect_no_match(already_tagged, "duplicate", fixed = TRUE)
})

test_that("Shinychat final text adapter can hide thinking", {
  text <- aisdk:::build_final_text(
    content_text = "Final answer",
    thinking_text = "Internal reasoning summary",
    full_content = "",
    show_thinking = TRUE
  )
  hidden <- aisdk:::build_final_text(
    content_text = "Final answer",
    thinking_text = "Internal reasoning summary",
    full_content = "",
    show_thinking = FALSE
  )

  expect_match(text, "Thinking")
  expect_match(text, "Internal reasoning summary")
  expect_identical(hidden, "Final answer")
})

test_that("Shiny final HTML preserves full answer when reasoning is appended at done", {
  html <- aisdk:::build_final_html(
    content_text = "",
    thinking_text = "Internal reasoning summary",
    full_content = "Final **answer**",
    show_thinking = TRUE
  )

  expect_match(html, "<strong>answer</strong>", fixed = TRUE)
  expect_match(html, "Internal reasoning summary", fixed = TRUE)
})

test_that("Shinychat backend uses namespaced inner input id", {
  expect_identical(aisdk:::shinychat_control_id(), "shinychat")
})

test_that("wrap_reactive_tools injects reactive values and Shiny session", {
  rv <- new.env(parent = emptyenv())
  rv$resolution <- 100
  shiny_session <- list(ns = identity)

  update_resolution_tool <- reactive_tool(
    name = "update_resolution",
    description = "Update the plot resolution",
    parameters = z_object(
      resolution = z_number("New resolution value")
    ),
    execute = function(rv, session, resolution) {
      rv$resolution <- resolution
      paste0("Resolution updated to ", resolution)
    }
  )

  wrapped <- wrap_reactive_tools(
    list(update_resolution_tool),
    rv = rv,
    session = shiny_session
  )

  result <- wrapped[[1]]$run(list(resolution = 250))

  expect_equal(result, "Resolution updated to 250")
  expect_equal(rv$resolution, 250)
})
