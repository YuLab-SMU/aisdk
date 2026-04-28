library(aisdk)

test_that("capture_r_execution captures output, messages, and warnings", {
  captured <- aisdk:::capture_r_execution({
    cat("hello\n")
    message("heads up")
    warning("careful")
  })

  expect_true(captured$ok)
  expect_equal(captured$output, "hello")
  expect_equal(captured$messages, "heads up")
  expect_equal(captured$warnings, "careful")

  formatted <- aisdk:::format_captured_execution(captured)
  expect_match(formatted, "hello")
  expect_match(formatted, "Message: heads up")
  expect_match(formatted, "Warning: careful")
})

test_that("Computer.execute_r_code captures warnings in returned output", {
  comp <- aisdk::Computer$new(working_dir = tempdir(), sandbox_mode = "permissive")
  result <- comp$execute_r_code("warning('careful'); 2 + 2")

  expect_false(result$error)
  expect_equal(result$warnings, "careful")
  rendered <- paste(result$output, collapse = "\n")
  expect_match(rendered, "4")
  expect_match(rendered, "Warning: careful")
})

test_that("compact tool labels hide raw json details", {
  label <- aisdk:::compact_tool_start_label(
    "execute_r_code",
    list(code = "packageVersion('ggtree')\nprint(installed.packages()[1, ])")
  )

  expect_match(label, "^Running R code:")
  expect_false(grepl("\\{|\\}|\\$installed", label))
})

test_that("handle_command toggles debug mode", {
  session <- aisdk::create_chat_session()

  on_result <- aisdk:::handle_command("/debug on", session, stream = TRUE, verbose = FALSE, show_thinking = FALSE)
  expect_true(on_result$verbose)
  expect_true(on_result$show_thinking)

  off_result <- aisdk:::handle_command("/debug off", session, stream = TRUE, verbose = TRUE, show_thinking = TRUE)
  expect_false(off_result$verbose)
  expect_false(off_result$show_thinking)
})

test_that("handle_command toggles inspect mode through app state", {
  session <- aisdk::create_chat_session()
  app_state <- aisdk:::create_console_app_state(session, view_mode = "clean")

  on_result <- aisdk:::handle_command(
    "/inspect on",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state
  )
  expect_false(on_result$verbose)
  expect_false(on_result$show_thinking)
  expect_true(on_result$refresh_status)
  expect_equal(app_state$view_mode, "inspect")

  off_result <- aisdk:::handle_command(
    "/inspect off",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state
  )
  expect_equal(app_state$view_mode, "clean")
  expect_true(off_result$refresh_status)
})

test_that("model command opens chooser when called without args", {
  session <- aisdk::create_chat_session(model = "openai:gpt-4o")
  app_state <- aisdk:::create_console_app_state(session, view_mode = "clean")

  result <- aisdk:::handle_command(
    "/model",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state,
    model_prompt_fn = function(...) NULL
  )

  expect_false(result$refresh_status)
  expect_equal(session$get_model_id(), "openai:gpt-4o")
})

test_that("model command can switch via chooser result", {
  session <- aisdk::create_chat_session(model = "openai:gpt-4o")
  app_state <- aisdk:::create_console_app_state(session, view_mode = "clean")

  result <- aisdk:::handle_command(
    "/model",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state,
    model_prompt_fn = function(...) "anthropic:claude-sonnet-4-20250514"
  )

  expect_true(result$refresh_status)
  expect_equal(session$get_model_id(), "anthropic:claude-sonnet-4-20250514")
  expect_equal(app_state$model_id, "anthropic:claude-sonnet-4-20250514")
})

test_that("model current reports the active model without switching", {
  session <- aisdk::create_chat_session(model = "openai:gpt-4o")
  app_state <- aisdk:::create_console_app_state(session, view_mode = "clean")

  result <- aisdk:::handle_command(
    "/model current",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state
  )

  expect_false(result$refresh_status)
  expect_equal(session$get_model_id(), "openai:gpt-4o")
})

test_that("model command updates context and thinking settings", {
  session <- aisdk::create_chat_session(model = "deepseek:deepseek-v4-flash")
  app_state <- aisdk:::create_console_app_state(session, view_mode = "clean")

  context_result <- aisdk:::handle_command(
    "/model context 512k",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state
  )
  expect_true(context_result$refresh_status)
  expect_equal(session$get_model_options()$context_window, 512000)

  aisdk:::handle_command("/model output 64k", session, stream = TRUE, verbose = FALSE, show_thinking = FALSE, app_state = app_state)
  aisdk:::handle_command("/model max-tokens 700", session, stream = TRUE, verbose = FALSE, show_thinking = FALSE, app_state = app_state)
  aisdk:::handle_command("/model thinking on", session, stream = TRUE, verbose = FALSE, show_thinking = FALSE, app_state = app_state)
  aisdk:::handle_command("/model effort high", session, stream = TRUE, verbose = FALSE, show_thinking = FALSE, app_state = app_state)
  aisdk:::handle_command("/model budget 2k", session, stream = TRUE, verbose = FALSE, show_thinking = FALSE, app_state = app_state)

  options <- session$get_model_options()
  expect_equal(options$max_output_tokens, 64000)
  expect_equal(options$call_options$max_tokens, 700)
  expect_true(options$call_options$thinking)
  expect_equal(options$call_options$reasoning_effort, "high")
  expect_equal(options$call_options$thinking_budget, 2000)

  line <- aisdk:::build_console_status_line(app_state)
  expect_match(line, "Ctx\\(est\\): 512.0k")
  expect_match(line, "Out: 64.0k")
  expect_match(line, "Think: on")
  expect_match(line, "Effort: high")
  expect_match(line, "Budget: 2.0k")
  expect_match(line, "Max: 700")

  clear_result <- aisdk:::handle_command(
    "/model thinking auto",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state
  )
  expect_true(clear_result$refresh_status)
  expect_null(aisdk:::list_get_exact(session$get_model_options()$call_options, "thinking"))
})

test_that("persona commands update session persona state", {
  session <- aisdk::create_chat_session()
  app_state <- aisdk:::create_console_app_state(session, view_mode = "clean")

  set_result <- aisdk:::handle_command(
    "/persona set You are a brutal but precise reviewer.",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state
  )
  expect_true(set_result$refresh_status)
  expect_equal(aisdk:::console_current_persona(session)$source, "manual")
  expect_equal(aisdk:::console_current_persona(session)$label, "custom")

  evolve_result <- aisdk:::handle_command(
    "/persona evolve Stay extra concise.",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state
  )
  expect_true(evolve_result$refresh_status)
  expect_true(any(grepl("Stay extra concise.", aisdk:::console_current_persona(session)$notes, fixed = TRUE)))

  reset_result <- aisdk:::handle_command(
    "/persona default",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state
  )
  expect_true(reset_result$refresh_status)
  expect_equal(aisdk:::console_current_persona(session)$source, "default")
})

test_that("console status line includes persona label", {
  session <- aisdk::create_chat_session(model = "openai:gpt-5-mini")
  aisdk:::console_set_manual_persona(session, "You are a skeptical reviewer.", label = "skeptic", locked = TRUE)
  app_state <- aisdk:::create_console_app_state(session, view_mode = "clean")

  line <- aisdk:::build_console_status_line(app_state)

  expect_match(line, "Persona: skeptic:manual")
})

test_that("inspect commands open and close overlay state", {
  session <- aisdk::create_chat_session()
  app_state <- aisdk:::create_console_app_state(session, view_mode = "inspect")
  aisdk:::console_app_start_turn(app_state, "Inspect latest turn")
  aisdk:::console_app_append_assistant_text(app_state, "Overlay me")
  aisdk:::console_app_finish_turn(app_state)

  turn_result <- aisdk:::handle_command(
    "/inspect turn",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state
  )
  overlay <- aisdk:::console_app_get_active_overlay(app_state)
  expect_true(turn_result$refresh_status)
  expect_equal(overlay$type, "inspector")
  expect_match(overlay$title, "Inspector Overlay")
  expect_equal(app_state$focus_target, "overlay:inspector")

  close_result <- aisdk:::handle_command(
    "/inspect close",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state
  )
  expect_true(close_result$refresh_status)
  expect_null(aisdk:::console_app_get_active_overlay(app_state))
  expect_equal(app_state$focus_target, "composer")
})

test_that("inspect next and prev navigate overlay tools", {
  session <- aisdk::create_chat_session()
  app_state <- aisdk:::create_console_app_state(session, view_mode = "inspect")
  aisdk:::console_app_start_turn(app_state, "Inspect tools")
  aisdk:::console_app_append_assistant_text(app_state, "Two tools ran")
  aisdk:::console_app_record_tool_start(app_state, "execute_r_code", list(code = "1 + 1"))
  aisdk:::console_app_record_tool_result(app_state, "execute_r_code", "2")
  aisdk:::console_app_record_tool_start(app_state, "bash", list(command = "pwd"))
  aisdk:::console_app_record_tool_result(app_state, "bash", "/tmp")
  aisdk:::console_app_finish_turn(app_state)

  turn_result <- aisdk:::handle_command(
    "/inspect turn",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state
  )
  expect_true(turn_result$refresh_status)
  expect_null(aisdk:::console_app_get_active_overlay(app_state)$payload$tool_index)

  next_result <- aisdk:::handle_command(
    "/inspect next",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state
  )
  overlay <- aisdk:::console_app_get_active_overlay(app_state)
  expect_true(next_result$refresh_status)
  expect_equal(overlay$payload$tool_index, 1)
  expect_match(overlay$title, "Tool 1")

  next_result_2 <- aisdk:::handle_command(
    "/inspect next",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state
  )
  overlay <- aisdk:::console_app_get_active_overlay(app_state)
  expect_true(next_result_2$refresh_status)
  expect_equal(overlay$payload$tool_index, 2)
  expect_match(overlay$title, "Tool 2")

  prev_result <- aisdk:::handle_command(
    "/inspect prev",
    session,
    stream = TRUE,
    verbose = FALSE,
    show_thinking = FALSE,
    app_state = app_state
  )
  overlay <- aisdk:::console_app_get_active_overlay(app_state)
  expect_true(prev_result$refresh_status)
  expect_equal(overlay$payload$tool_index, 1)
})

test_that("console status line reflects app state snapshot", {
  session <- aisdk::create_chat_session()
  app_state <- aisdk:::create_console_app_state(
    session,
    sandbox_mode = "strict",
    stream_enabled = FALSE,
    local_execution_enabled = TRUE,
    view_mode = "inspect"
  )
  app_state$model_id <- "openai:gpt-5"
  app_state$tool_state <- "running"

  line <- aisdk:::build_console_status_line(app_state)

  expect_match(line, "Model: openai:gpt-5")
  expect_match(line, "Sandbox: strict")
  expect_match(line, "View: inspect")
  expect_match(line, "Stream: off")
  expect_match(line, "Local: on")
  expect_match(line, "Tools: running")
})

test_that("console status lines fold for narrow widths", {
  session <- aisdk::create_chat_session(model = "openai:gpt-5-mini")
  app_state <- aisdk:::create_console_app_state(
    session,
    sandbox_mode = "strict",
    stream_enabled = FALSE,
    local_execution_enabled = TRUE,
    view_mode = "inspect"
  )
  app_state$tool_state <- "running"

  wide_lines <- aisdk:::build_console_status_lines(app_state, width = 140)
  narrow_lines <- aisdk:::build_console_status_lines(app_state, width = 58)

  expect_length(wide_lines, 1)
  expect_true(length(narrow_lines) >= 2)
  expect_true(all(nchar(narrow_lines, type = "width") <= 58))
  expect_true(any(grepl("^Model:", narrow_lines)))
  expect_true(any(grepl("Tools: running", narrow_lines, fixed = TRUE)))
})

test_that("console frame groups status timeline and overlay sections", {
  session <- aisdk::create_chat_session()
  app_state <- aisdk:::create_console_app_state(session, view_mode = "inspect")
  aisdk:::console_app_start_turn(app_state, "Inspect tools")
  aisdk:::console_app_append_assistant_text(app_state, "Two tools ran")
  aisdk:::console_app_record_tool_start(app_state, "execute_r_code", list(code = "1 + 1"))
  aisdk:::console_app_record_tool_result(app_state, "execute_r_code", "2")
  aisdk:::console_app_finish_turn(app_state)
  aisdk:::console_app_open_turn_overlay(app_state)

  frame <- aisdk:::build_console_frame(app_state)

  expect_equal(frame$status$type, "status")
  expect_equal(frame$status$tone, "muted")
  expect_equal(frame$timeline$type, "timeline")
  expect_equal(frame$timeline$tone, "subtle")
  expect_equal(frame$overlay$type, "overlay")
  expect_equal(frame$overlay$tone, "primary")
  expect_true(length(frame$status$lines) >= 2)
  expect_true(any(grepl("Model:", frame$status$lines, fixed = TRUE)))
  expect_true(any(grepl("execute_r_code", frame$timeline$lines, fixed = TRUE)))
  expect_true(any(grepl("Inspector Overlay", frame$overlay$lines, fixed = TRUE)))
  expect_true(isTRUE(frame$meta$has_overlay))
  expect_equal(frame$meta$focus_target, "overlay:inspector")
})

test_that("tool events are captured into the current turn timeline", {
  session <- aisdk::create_chat_session()
  app_state <- aisdk:::create_console_app_state(session, view_mode = "inspect")
  aisdk:::console_app_start_turn(app_state, "Check package version")

  old_opts <- options(
    aisdk.console_app_state = app_state,
    aisdk.tool_log_mode = "compact"
  )
  on.exit(options(old_opts), add = TRUE)

  aisdk:::cli_tool_start("execute_r_code", list(code = "packageVersion('ggtree')"))
  aisdk:::cli_tool_result("execute_r_code", "4.0.1", success = TRUE)
  aisdk:::console_app_finish_turn(app_state)

  turn <- aisdk:::console_app_get_current_turn(app_state)
  lines <- aisdk:::format_console_tool_timeline(turn)

  expect_length(turn$tool_calls, 1)
  expect_equal(turn$tool_calls[[1]]$status, "done")
  expect_match(turn$tool_calls[[1]]$args_summary, "Running R code")
  expect_match(turn$tool_calls[[1]]$result_summary, "R code completed")
  expect_length(lines, 1)
  expect_match(lines[[1]], "execute_r_code")
  expect_match(lines[[1]], "\\[done\\]")
})

test_that("tool diagnostics are stored separately from tool summaries", {
  session <- aisdk::create_chat_session()
  app_state <- aisdk:::create_console_app_state(session, view_mode = "inspect")
  aisdk:::console_app_start_turn(app_state, "Run diagnostic code")

  old_opts <- options(
    aisdk.console_app_state = app_state,
    aisdk.tool_log_mode = "compact"
  )
  on.exit(options(old_opts), add = TRUE)

  raw_result <- structure(
    "Result: 4\nMessage: heads up\nWarning: careful",
    aisdk_messages = "heads up",
    aisdk_warnings = "careful"
  )

  aisdk:::cli_tool_start("execute_r_code", list(code = "message('heads up'); warning('careful'); 2 + 2"))
  aisdk:::cli_tool_result("execute_r_code", raw_result, success = TRUE, raw_result = raw_result)
  aisdk:::console_app_finish_turn(app_state)

  turn <- aisdk:::console_app_get_current_turn(app_state)
  tool <- turn$tool_calls[[1]]
  lines <- aisdk:::format_console_tool_timeline(turn)

  expect_equal(tool$messages, "heads up")
  expect_equal(tool$warnings, "careful")
  expect_equal(turn$messages, "heads up")
  expect_equal(turn$warnings, "careful")
  expect_match(lines[[1]], "messages: 1")
  expect_match(lines[[1]], "warnings: 1")
})

test_that("streaming chunks accumulate into app state assistant text", {
  StreamingMockModel <- R6::R6Class(
    "StreamingMockModelForConsoleTests",
    inherit = aisdk:::LanguageModelV1,
    public = list(
      provider = "mock",
      model_id = "stream-mock",
      chunks = NULL,
      initialize = function(chunks) {
        self$chunks <- chunks
      },
      do_generate = function(params) {
        list(text = paste(self$chunks, collapse = ""), tool_calls = NULL, finish_reason = "stop")
      },
      do_stream = function(params, callback) {
        for (chunk in self$chunks) {
          callback(chunk, FALSE)
        }
        callback(NULL, TRUE)
        list(
          text = paste(self$chunks, collapse = ""),
          tool_calls = NULL,
          finish_reason = "stop",
          usage = list(total_tokens = length(self$chunks))
        )
      },
      format_tool_result = function(tool_call_id, tool_name, result) {
        list(role = "tool", tool_call_id = tool_call_id, name = tool_name, content = result)
      }
    )
  )

  model <- StreamingMockModel$new(c("hello ", "world"))
  session <- aisdk::create_chat_session(model = model)
  app_state <- aisdk:::create_console_app_state(session, view_mode = "inspect")
  aisdk:::console_app_start_turn(app_state, "Say hello")

  aisdk:::with_console_chat_display(app_state = app_state, code = {
    session$send_stream(
      "Say hello",
      callback = function(text, done) {
        if (!isTRUE(done)) {
          aisdk:::console_app_append_assistant_text(app_state, text)
        }
      }
    )
  })

  aisdk:::console_app_finish_turn(app_state)
  turn <- aisdk:::console_app_get_current_turn(app_state)

  expect_equal(turn$assistant_text, "hello world")
  expect_equal(turn$phase, "done")
  expect_equal(app_state$phase, "idle")
})

test_that("turn and tool inspector helpers expose structured details", {
  session <- aisdk::create_chat_session()
  app_state <- aisdk:::create_console_app_state(session, view_mode = "inspect")
  aisdk:::console_app_start_turn(app_state, "Run diagnostic code")
  aisdk:::console_app_append_assistant_text(app_state, "Version check completed.")

  raw_result <- structure(
    "Result: 4\nMessage: heads up\nWarning: careful",
    aisdk_messages = "heads up",
    aisdk_warnings = "careful"
  )

  aisdk:::console_app_record_tool_start(app_state, "execute_r_code", list(code = "2 + 2"))
  aisdk:::console_app_record_tool_result(app_state, "execute_r_code", raw_result, raw_result = raw_result)
  aisdk:::console_app_finish_turn(app_state)

  turn <- aisdk:::console_app_get_last_turn(app_state)
  turn_lines <- aisdk:::format_console_turn_detail(turn)
  tool_lines <- aisdk:::format_console_tool_detail(turn, 1)

  expect_true(any(grepl("^Turn:", turn_lines)))
  expect_true(any(grepl("^Assistant: Version check completed\\.", turn_lines)))
  expect_true(any(grepl("^Messages: heads up$", turn_lines)))
  expect_true(any(grepl("^Warnings: careful$", turn_lines)))
  expect_true(any(grepl("^Tool: execute_r_code$", tool_lines)))
  expect_true(any(grepl("^Args raw:", tool_lines)))
  expect_true(any(grepl("^Result raw:", tool_lines)))
})

test_that("overlay helpers build boxed inspector content", {
  session <- aisdk::create_chat_session()
  app_state <- aisdk:::create_console_app_state(session, view_mode = "inspect")
  aisdk:::console_app_start_turn(app_state, "Run diagnostic code")
  aisdk:::console_app_append_assistant_text(app_state, "Version check completed.")
  aisdk:::console_app_finish_turn(app_state)

  overlay <- aisdk:::console_app_open_turn_overlay(app_state)
  lines <- aisdk:::build_console_overlay_box(app_state, overlay)

  expect_equal(overlay$type, "inspector")
  expect_true(length(lines) >= 5)
  expect_true(any(grepl("Inspector Overlay", lines, fixed = TRUE)))
  expect_true(any(grepl("Close: /inspect close", lines, fixed = TRUE)))
})

test_that("console frame hides timeline outside inspect mode", {
  session <- aisdk::create_chat_session()
  app_state <- aisdk:::create_console_app_state(session, view_mode = "clean")
  aisdk:::console_app_start_turn(app_state, "Inspect tools")
  aisdk:::console_app_record_tool_start(app_state, "execute_r_code", list(code = "1 + 1"))
  aisdk:::console_app_record_tool_result(app_state, "execute_r_code", "2")
  aisdk:::console_app_finish_turn(app_state)

  frame <- aisdk:::build_console_frame(app_state)

  expect_length(frame$timeline$lines, 0)
  expect_false(isTRUE(frame$meta$has_overlay))
})

test_that("console_frame_section_changed detects unchanged status sections", {
  session <- aisdk::create_chat_session(model = "openai:gpt-5-mini")
  app_state <- aisdk:::create_console_app_state(session, view_mode = "clean")
  frame1 <- aisdk:::build_console_frame(app_state)
  frame2 <- aisdk:::build_console_frame(app_state)

  expect_true(aisdk:::console_frame_section_changed(NULL, frame1, "status", force = FALSE))
  expect_false(aisdk:::console_frame_section_changed(frame1, frame2, "status", force = FALSE))
  expect_true(aisdk:::console_frame_section_changed(frame1, frame2, "status", force = TRUE))
})

test_that("inspector lines include navigation hints", {
  session <- aisdk::create_chat_session()
  app_state <- aisdk:::create_console_app_state(session, view_mode = "inspect")
  aisdk:::console_app_start_turn(app_state, "Inspect tools")
  aisdk:::console_app_record_tool_start(app_state, "execute_r_code", list(code = "1 + 1"))
  aisdk:::console_app_record_tool_result(app_state, "execute_r_code", "2")
  aisdk:::console_app_finish_turn(app_state)

  turn <- aisdk:::console_app_get_last_turn(app_state)
  turn_lines <- aisdk:::build_console_inspector_lines(turn)
  tool_lines <- aisdk:::build_console_inspector_lines(turn, tool_index = 1)

  expect_true(any(grepl("/inspect next opens the first tool", turn_lines, fixed = TRUE)))
  expect_true(any(grepl("/inspect prev | /inspect next", tool_lines, fixed = TRUE)))
  expect_true(any(grepl("/inspect turn returns to the turn summary", tool_lines, fixed = TRUE)))
})

test_that("with_console_chat_display derives visibility from app state", {
  session <- aisdk::create_chat_session()
  app_state <- aisdk:::create_console_app_state(session, view_mode = "inspect")

  aisdk:::with_console_chat_display(app_state = app_state, code = {
    expect_equal(getOption("aisdk.tool_log_mode"), "compact")
    expect_false(getOption("aisdk.show_thinking"))
    expect_identical(getOption("aisdk.console_app_state"), app_state)
  })

  aisdk:::console_app_set_view_mode(app_state, "debug")

  aisdk:::with_console_chat_display(app_state = app_state, code = {
    expect_equal(getOption("aisdk.tool_log_mode"), "detailed")
    expect_true(getOption("aisdk.show_thinking"))
  })
})
