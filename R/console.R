#' @title Console Chat: Interactive REPL
#' @description
#' Interactive terminal chat interface for ChatSession.
#' Provides a REPL (Read-Eval-Print Loop) for conversing with LLMs.
#' By default, enables an intelligent terminal agent that can execute commands,
#' manage files, and run R code through natural language.
#' @name console
NULL

#' @title Start Console Chat
#' @description
#' Launch an interactive chat session in the R console. Supports streaming
#' output, slash commands, and colorful display using the cli package.
#'
#' The console UI has three presentation modes:
#' - `clean`: compact default output with a stable status bar
#' - `inspect`: keeps the compact transcript but adds a per-turn tool timeline
#'   and an overlay-backed inspector
#' - `debug`: shows detailed tool logs and thinking output for troubleshooting
#'
#' In agent mode, `console_chat()` can execute shell and R tools, summarize tool
#' progress inline, and open an inspector overlay for the latest turn or a
#' specific tool. The current implementation uses a shared frame builder for the
#' status bar, tool timeline, and overlay surfaces, while preserving an
#' append-only terminal fallback.
#'
#' By default, the console operates in agent mode with tools for bash execution,
#' file operations, R code execution, and more. Set `agent = NULL` for simple
#' chat without tools.
#'
#' @param session A ChatSession object, a LanguageModelV1 object, or a model string ID to create a new session.
#' @param system_prompt Optional system prompt (merged with agent prompt if agent is used).
#' @param tools Optional list of additional Tool objects.
#' @param hooks Optional HookHandler object.
#' @param stream Whether to use streaming output. Default TRUE.
#' @param verbose Logical. If `TRUE`, show detailed tool calls, tool results, and
#'   thinking output. Defaults to `FALSE` for a cleaner console UI.
#' @param agent Agent configuration. Options:
#'   - `"auto"` (default): Use the built-in console agent with terminal tools
#'   - `NULL`: Simple chat mode without tools
#'   - An Agent object: Use the provided custom agent
#' @param skills Optional skill paths, `"auto"`, or a SkillRegistry object for
#'   the built-in console agent. Defaults to automatic skill discovery when
#'   `agent = "auto"`.
#' @param working_dir Working directory for sandboxed console tools. Defaults to `tempdir()`.
#' @param sandbox_mode Sandbox mode for the console agent: "strict", "permissive" (default), or "none".
#' @param show_thinking Logical. Whether to show model thinking blocks when the
#'   provider exposes them. Defaults to `verbose`.
#' @param startup_dir R session startup directory used for project-aware context. Defaults to `getwd()`.
#' @return The ChatSession object (invisibly) when chat ends.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # Start with default agent (intelligent terminal mode)
#'   console_chat("openai:gpt-4o")
#'
#'   # Start in debug mode with full tool logs
#'   console_chat("openai:gpt-4o", verbose = TRUE)
#'
#'   # Simple chat mode without tools
#'   console_chat("openai:gpt-4o", agent = NULL)
#'
#'   # Start with an existing session
#'   chat <- create_chat_session("anthropic:claude-3-5-sonnet-latest")
#'   console_chat(chat)
#'
#'   # Start with a custom agent
#'   agent <- create_agent("MathAgent", "Does math", system_prompt = "You are a math wizard.")
#'   console_chat("openai:gpt-4o", agent = agent)
#'
#'   # Available commands in the chat:
#'   # /quit or /exit - End the chat
#'   # /save [path]   - Save session to file
#'   # /load [path]   - Load session from file
#'   # /model         - Open the provider/model chooser
#'   # /model [id]    - Switch to a different model
#'   # /model current - Show the active model
#'   # /history       - Show conversation history
#'   # /stats         - Show token usage statistics
#'   # /clear         - Clear conversation history
#'   # /stream [on|off] - Toggle streaming mode
#'   # /inspect [on|off] - Toggle inspect mode
#'   # /inspect turn - Open overlay for the latest turn
#'   # /inspect tool <index> - Open overlay for a tool in the latest turn
#'   # /inspect next - Move inspector overlay to the next tool
#'   # /inspect prev - Move inspector overlay to the previous tool
#'   # /inspect close - Close the active inspect overlay
#'   # /debug [on|off]  - Toggle detailed tool/thinking output
#'   # /local [on|off]- Toggle local execution mode (Global Environment)
#'   # /help          - Show available commands
#'   # /agent [on|off] - Toggle agent mode
#' }
#' }
console_chat <- function(session = NULL,
                         system_prompt = NULL,
                         tools = NULL,
                         hooks = NULL,
                         stream = TRUE,
                         verbose = FALSE,
                         agent = "auto",
                         skills = NULL,
                         working_dir = tempdir(),
                         sandbox_mode = "permissive",
                         show_thinking = verbose,
                         startup_dir = getwd()) {
  # Ensure cli package is available
  if (!requireNamespace("cli", quietly = TRUE)) {
    rlang::abort("Package 'cli' is required for console_chat(). Install with: install.packages('cli')")
  }

  working_dir <- if (missing(working_dir) && inherits(session, "ChatSession")) {
    console_session_directory(session, key = "console_working_dir", default = tempdir())
  } else {
    console_resolve_directory(working_dir, fallback = tempdir())
  }
  startup_dir <- if (missing(startup_dir) && inherits(session, "ChatSession")) {
    console_session_directory(session, key = "console_startup_dir", default = getwd())
  } else {
    console_resolve_directory(startup_dir, fallback = getwd())
  }

  verbose <- isTRUE(verbose)
  show_thinking <- isTRUE(show_thinking)

  # Resolve agent
  agent_mode <- FALSE
  if (is.character(agent) && agent == "auto") {
    agent <- create_console_agent(
      working_dir = working_dir,
      startup_dir = startup_dir,
      sandbox_mode = sandbox_mode,
      skills = skills %||% "auto",
      additional_tools = tools
    )
    agent_mode <- TRUE
    tools <- NULL # Tools are now in the agent
  } else if (inherits(agent, "Agent")) {
    agent_mode <- TRUE
  }

  # Create or use existing session
  if (is.null(session)) {
    startup_model <- resolve_console_startup_model()
    model_id <- startup_model$model_id %||% NULL

    if (is.null(model_id) || !nzchar(model_id)) {
      if (identical(startup_model$source %||% "", "invalid_default")) {
        cli::cli_alert_info("Saved default model is unavailable. Reopening model setup.")
      }
      model_id <- prompt_console_provider_profile()
    }

    if (!is.null(model_id) && nzchar(model_id)) {
      session <- tryCatch(
        create_chat_session(
          model = model_id,
          system_prompt = system_prompt,
          tools = tools,
          hooks = hooks,
          agent = agent
        ),
        error = function(e) {
          cli::cli_alert_danger("Failed to set model: {conditionMessage(e)}")
          fallback_model_id <- prompt_console_provider_profile()
          if (!is.null(fallback_model_id) && nzchar(fallback_model_id)) {
            return(create_chat_session(
              model = fallback_model_id,
              system_prompt = system_prompt,
              tools = tools,
              hooks = hooks,
              agent = agent
            ))
          }
          cli::cli_alert_info("Use {.code /model <id>} to set a model later.")
          create_chat_session(
            system_prompt = system_prompt,
            tools = tools,
            hooks = hooks,
            agent = agent
          )
        }
      )
    } else {
      cli::cli_alert_info("Use {.code /model <id>} to set a model later.")
      session <- create_chat_session(
        system_prompt = system_prompt,
        tools = tools,
        hooks = hooks,
        agent = agent
      )
    }
  } else if (is.character(session)) {
    session <- create_chat_session(
      model = session,
      system_prompt = system_prompt,
      tools = tools,
      hooks = hooks,
      agent = agent
    )
  } else if (inherits(session, "LanguageModelV1")) {
    session <- create_chat_session(
      model = session,
      system_prompt = system_prompt,
      tools = tools,
      hooks = hooks,
      agent = agent
    )
  } else if (!inherits(session, "ChatSession")) {
    rlang::abort("session must be a ChatSession object, LanguageModelV1 object, or model string ID")
  }

  session$merge_metadata(list(
    console_working_dir = working_dir,
    console_startup_dir = startup_dir
  ))
  assign(".console_working_dir", working_dir, envir = session$get_envir())
  assign(".console_startup_dir", startup_dir, envir = session$get_envir())
  assign(".session_model_id", session$get_model_id() %||% "", envir = session$get_envir())

  view_mode <- if (isTRUE(verbose)) "debug" else "clean"

  # Welcome message
  app_state <- create_console_app_state(
    session = session,
    agent_enabled = agent_mode,
    sandbox_mode = sandbox_mode,
    stream_enabled = stream,
    local_execution_enabled = isTRUE(session$get_envir()$.local_mode),
    view_mode = view_mode
  )

  cli::cli_h1("R AI SDK Console Chat")
  if (agent_mode) {
    n_tools <- length(session$.__enclos_env__$private$.tools)
    cli::cli_text("Agent mode: {.val enabled} ({n_tools} tools)")
  } else {
    cli::cli_text("Agent mode: {.val disabled} (simple chat)")
  }
  render_console_frame(build_console_frame(app_state), state = app_state, force = TRUE)
  cli::cli_text("Type {.code /help} for commands, {.code /quit} to exit.")

  input_state <- console_create_input_state(session)

  # Main REPL loop
  while (TRUE) {
    # Read user input
    input <- tryCatch(
      readline_multiline(input_state),
      interrupt = function(e) {
        cli::cli_alert_info("Use /quit to exit.")
        return("")
      },
      error = function(e) {
        return(NULL)
      }
    )

    # Handle EOF or error
    if (is.null(input)) {
      cli::cli_alert_info("Goodbye!")
      break
    }

    # Skip empty input
    if (!nzchar(trimws(input))) {
      next
    }

    # Check for commands
    if (startsWith(input, "/")) {
      result <- handle_command(
        input,
        session,
        stream,
        verbose,
        show_thinking,
        app_state = app_state
      )
      if (result$exit) {
        break
      }
      session <- result$session
      stream <- result$stream
      verbose <- result$verbose
      show_thinking <- result$show_thinking
      console_app_sync_session(app_state, session)
      console_app_set_stream_enabled(app_state, stream)
      if (isTRUE(result$refresh_status)) {
        render_console_frame(build_console_frame(app_state), state = app_state, force = TRUE)
      }
      next
    }

    # Check if model is set
    if (is.null(session$get_model_id())) {
      cli::cli_alert_danger("No model set. Use {.code /model <id>} first.")
      cli::cli_alert_info("Example: {.code /model openai:gpt-4o}")
      next
    }

    # Send message to model
    turn_system_prompt <- console_build_turn_system_prompt(session, input)
    console_app_sync_session(app_state, session)
    console_app_start_turn(app_state, input)
    cli::cli_text("")
    cli::cli_text(cli::col_green(cli::symbol$pointer), " ", cli::col_green("Assistant:"))

    tryCatch(
      {
        with_console_chat_display(
          app_state = app_state,
          code = {
            if (stream) {
              md_renderer <- create_markdown_stream_renderer()
              session$send_stream(
                input,
                turn_system_prompt = turn_system_prompt,
                callback = function(text, done) {
                  if (isTRUE(done)) {
                    md_renderer$process_chunk(NULL, TRUE)
                  } else {
                    console_app_append_assistant_text(app_state, text)
                    md_renderer$process_chunk(text, FALSE)
                  }
                }
              )
            } else {
              # Non-streaming output
              md_renderer <- create_markdown_stream_renderer()
              result <- session$send(input, turn_system_prompt = turn_system_prompt)
              if (!is.null(result$text)) {
                console_app_append_assistant_text(app_state, result$text)
                # Render as markdown
                md_renderer$process_chunk(result$text, FALSE)
                md_renderer$process_chunk(NULL, TRUE)
              }

              # Show tool calls if any in debug mode
              if (isTRUE(verbose) && !is.null(result$tool_calls) && length(result$tool_calls) > 0) {
                cli::cli_alert_info("Tool calls made: {.val {length(result$tool_calls)}}")
              }
            }
          }
        )
        console_app_finish_turn(app_state, failed = FALSE)
      },
      error = function(e) {
        console_app_finish_turn(app_state, failed = TRUE)
        cli::cli_alert_danger("Error: {conditionMessage(e)}")
      }
    )

    render_console_frame(
      build_console_frame(app_state),
      state = app_state,
      sections = c("timeline", "overlay"),
      force = FALSE
    )
    cli::cli_text("")
  }

  invisible(session)
}

#' @keywords internal
readline_multiline <- function(input_state = NULL,
                               readline_fn = NULL,
                               quiet = FALSE,
                               paste_output_dir = tempdir(),
                               clipboard_fn = console_read_clipboard) {
  input_state <- input_state %||% console_create_input_state()
  draining_paste <- console_has_queued_paste_drain(input_state)
  if (!isTRUE(quiet) && !draining_paste) {
    cli::cli_text(cli::col_blue(cli::symbol$pointer), " ", cli::col_blue("You:"))
  }

  input_event <- console_read_input_event(
    prompt = if (draining_paste) "" else "  ",
    readline_fn = readline_fn
  )
  if (identical(input_event$type, "eof")) {
    return(NULL)
  }
  if (identical(input_event$type, "paste")) {
    paste_ref <- console_save_paste_event(input_event$text, output_dir = paste_output_dir)
    if (nzchar(paste_ref$message %||% "")) {
      input_state$pending_paste <- paste_ref
      if (!isTRUE(quiet)) {
        console_show_paste_notice(paste_ref)
      }
    }
    return("")
  }

  input <- input_event$text %||% ""
  if (console_consume_queued_paste_line(input_state, input)) {
    if (!isTRUE(quiet)) {
      console_clear_readline_echo(input, has_label = !draining_paste)
    }
    console_maybe_show_pending_paste_notice(input_state, quiet = quiet)
    return("")
  }

  if (!is.null(input_state$pending_paste)) {
    if (startsWith(trimws(input), "/")) {
      console_input_history_add(input_state, input)
      return(input)
    }

    input <- console_consume_pending_paste(input_state, input)
    console_input_history_add(input_state, input)
    return(input)
  }

  if (console_should_auto_paste(input) || console_should_rstudio_clipboard_paste(input, clipboard_fn)) {
    paste_ref <- console_read_paste_to_file(
      input_state,
      readline_fn = readline_fn,
      quiet = quiet,
      initial_lines = input,
      output_dir = paste_output_dir,
      clipboard_fn = clipboard_fn
    )
    if (nzchar(paste_ref$message %||% "")) {
      input_state$pending_paste <- paste_ref
    }
    return("")
  }

  console_input_history_add(input_state, input)
  input
}

#' @keywords internal
console_read_input_event <- function(prompt = "  ", readline_fn = NULL) {
  if (!is.null(readline_fn)) {
    return(list(type = "line", text = readline_fn(prompt)))
  }

  event <- console_read_bracketed_input(prompt)
  if (!is.null(event)) {
    return(event)
  }

  list(type = "line", text = readline(prompt))
}

#' @keywords internal
console_read_bracketed_input <- function(prompt = "  ") {
  if (!console_can_use_raw_input()) {
    return(NULL)
  }

  old_stty <- tryCatch(system2("stty", "-g", stdout = TRUE, stderr = FALSE), error = function(e) character(0))
  if (length(old_stty) == 0L || !nzchar(old_stty[[1]])) {
    return(NULL)
  }

  con <- tryCatch(file("stdin", open = "rb"), error = function(e) NULL)
  if (is.null(con)) {
    return(NULL)
  }
  chars <- character(0)
  char_bytes <- raw(0)
  paste_bytes <- raw(0)
  in_paste <- FALSE

  restore <- function() {
    cat("\033[?2004l")
    tryCatch(close(con), error = function(e) NULL)
    tryCatch(system2("stty", old_stty[[1]], stdout = FALSE, stderr = FALSE), error = function(e) NULL)
    utils::flush.console()
  }

  cat(prompt)
  utils::flush.console()
  ok <- tryCatch({
    system2("stty", c("raw", "-echo"), stdout = FALSE, stderr = FALSE)
    cat("\033[?2004h")
    utils::flush.console()

    repeat {
      byte <- console_read_raw_byte(con)
      if (identical(byte, as.raw(0x1b))) {
        seq <- console_read_escape_sequence(con)
        if (identical(seq, "[200~")) {
          in_paste <- TRUE
          paste_bytes <- raw(0)
          next
        }
        if (identical(seq, "[201~") && isTRUE(in_paste)) {
          cat("\r\n")
          return(list(type = "paste", text = rawToChar(paste_bytes)))
        }
        next
      }

      if (isTRUE(in_paste)) {
        paste_bytes <- c(paste_bytes, byte)
        next
      }

      if (identical(byte, as.raw(0x0d)) || identical(byte, as.raw(0x0a))) {
        cat("\r\n")
        return(list(type = "line", text = paste0(chars, collapse = "")))
      }

      if (identical(byte, as.raw(0x03))) {
        stop(structure(list(message = "interrupt"), class = c("interrupt", "condition")))
      }

      if (identical(byte, as.raw(0x04))) {
        return(list(type = "eof", text = NULL))
      }

      if (identical(byte, as.raw(0x7f)) || identical(byte, as.raw(0x08))) {
        if (length(chars) > 0L) {
          chars <- chars[-length(chars)]
          cat("\b \b")
          utils::flush.console()
        }
        next
      }

      char_bytes <- c(char_bytes, byte)
      ch <- console_try_decode_utf8(char_bytes)
      if (!is.null(ch)) {
        chars <- c(chars, ch)
        cat(ch)
        utils::flush.console()
        char_bytes <- raw(0)
      }
    }
  }, error = function(e) {
    if (inherits(e, "interrupt")) {
      stop(e)
    }
    NULL
  }, finally = restore())

  ok
}

#' @keywords internal
console_read_escape_sequence <- function(con) {
  bytes <- raw(0)
  repeat {
    byte <- console_read_raw_byte(con)
    bytes <- c(bytes, byte)
    seq <- rawToChar(bytes)
    if (grepl("~$", seq) || length(bytes) >= 8L) {
      return(seq)
    }
  }
}

#' @keywords internal
console_read_raw_byte <- function(con) {
  byte <- readBin(con, what = "raw", n = 1L)
  if (length(byte) == 0L) {
    stop("No input available")
  }
  byte
}

#' @keywords internal
console_try_decode_utf8 <- function(bytes) {
  text <- tryCatch(rawToChar(bytes), error = function(e) NULL)
  if (is.null(text)) {
    return(NULL)
  }
  decoded <- iconv(text, from = "UTF-8", to = "UTF-8", sub = NA_character_)
  if (is.na(decoded)) {
    return(NULL)
  }
  decoded
}

#' @keywords internal
console_can_use_raw_input <- function() {
  if (!interactive() || .Platform$OS.type == "windows") {
    return(FALSE)
  }
  status <- tryCatch(system2("test", c("-t", "0"), stdout = FALSE, stderr = FALSE), error = function(e) 1L)
  identical(status, 0L)
}

#' @keywords internal
console_save_paste_event <- function(text, output_dir = tempdir()) {
  text <- gsub("\r\n", "\n", text %||% "", fixed = TRUE)
  text <- gsub("\r", "\n", text, fixed = TRUE)
  if (!nzchar(text)) {
    return(console_create_paste_ref("", 0L))
  }
  path <- console_write_paste_text(text, output_dir = output_dir)
  chars <- nchar(text, type = "chars", allowNA = FALSE, keepNA = FALSE)
  console_create_paste_ref(path, chars)
}

#' @keywords internal
console_read_paste_to_file <- function(input_state = NULL,
                                       readline_fn = function(prompt) readline(prompt),
                                       quiet = FALSE,
                                       initial_lines = character(0),
                                       output_dir = tempdir(),
                                       clipboard_fn = console_read_clipboard) {
  initial_lines <- console_normalize_input_lines(initial_lines)
  text <- console_clipboard_paste_text(initial_lines, clipboard_fn = clipboard_fn)
  used_clipboard <- !is.null(text)
  if (!used_clipboard) {
    if (!isTRUE(quiet)) {
      cli::cli_alert_info("Detected pasted content. Continue paste, then type {.code /endpaste} on its own line.")
    }
    lines <- initial_lines
    repeat {
      line <- readline_fn("  ")
      if (identical(trimws(line), "/endpaste")) {
        break
      }
      lines <- c(lines, line)
    }
    text <- paste(lines, collapse = "\n")
  }

  if (!nzchar(text)) {
    return(console_create_paste_ref("", 0L))
  }

  path <- console_write_paste_text(text, output_dir = output_dir)
  chars <- nchar(text, type = "chars", allowNA = FALSE, keepNA = FALSE)
  paste_ref <- console_create_paste_ref(path, chars)
  if (used_clipboard) {
    console_queue_paste_drain(input_state, text, initial_lines, paste_ref)
  }
  if (!isTRUE(quiet)) {
    if (used_clipboard) {
      # In clipboard mode only initial_lines have been echoed by readline() so far.
      # The remaining lines are still in the stdin buffer and must not be cleared.
      console_clear_paste_echo(initial_lines = initial_lines)
    } else {
      # In manual /endpaste mode all lines have been echoed interactively.
      console_clear_paste_echo(text, initial_lines)
    }
  }
  if (!isTRUE(quiet) && console_pending_paste_drain_empty(input_state)) {
    console_show_paste_notice(paste_ref)
  }

  paste_ref
}

#' @keywords internal
console_create_paste_ref <- function(path, chars) {
  message <- ""
  if (nzchar(path)) {
    message <- paste0(
      "[Pasted Content ", chars, " chars]\n",
      "The pasted content was saved to: ", path, "\n",
      "Please use this file as the content for my request."
    )
  }
  structure(
    list(
      path = path,
      chars = chars,
      message = message
    ),
    class = "aisdk_console_paste_ref"
  )
}

#' @keywords internal
console_consume_pending_paste <- function(input_state, input = "") {
  paste_ref <- input_state$pending_paste
  input_state$pending_paste <- NULL

  paste_message <- paste_ref$message %||% ""
  input <- trimws(input %||% "")
  if (!nzchar(input)) {
    return(paste_message)
  }

  paste0(input, "\n\n", paste_message)
}

#' @keywords internal
console_queue_paste_drain <- function(input_state,
                                      text,
                                      initial_lines = character(0),
                                      paste_ref = NULL) {
  if (is.null(input_state)) {
    return(invisible(character(0)))
  }
  lines <- console_submitted_paste_tail_lines(text, initial_lines)
  input_state$pending_paste_drain <- lines
  input_state$pending_paste_notice <- if (length(lines) > 0L) paste_ref else NULL
  invisible(lines)
}

#' @keywords internal
console_submitted_paste_tail_lines <- function(text, initial_lines = character(0)) {
  initial_lines <- console_normalize_input_lines(initial_lines)
  parts <- strsplit(text %||% "", "\n", fixed = TRUE)[[1]] %||% character(0)
  initial_count <- length(initial_lines %||% character(0))
  if (length(parts) <= initial_count) {
    return(character(0))
  }

  tail_indexes <- seq.int(initial_count + 1L, length(parts))
  parts[tail_indexes]
}

#' @keywords internal
console_consume_queued_paste_line <- function(input_state, input) {
  if (is.null(input_state)) {
    return(FALSE)
  }

  queued <- input_state$pending_paste_drain %||% character(0)
  if (length(queued) == 0L) {
    return(FALSE)
  }

  matched_index <- console_match_queued_paste_line(input, queued)
  if (!is.na(matched_index)) {
    input_state$pending_paste_drain <- queued[-seq_len(matched_index)]
    return(TRUE)
  }

  input_state$pending_paste_drain <- character(0)
  FALSE
}

#' @keywords internal
console_match_queued_paste_line <- function(input, queued) {
  if (length(queued) == 0L) {
    return(NA_integer_)
  }

  input_norm <- trimws(input %||% "")
  queued_norm <- trimws(queued %||% "")
  matches <- which(identical(input, queued[[1]]) | input_norm == queued_norm)
  if (length(matches) == 0L) {
    return(NA_integer_)
  }
  matches[[1]]
}

#' @keywords internal
console_has_queued_paste_drain <- function(input_state) {
  if (is.null(input_state)) {
    return(FALSE)
  }
  length(input_state$pending_paste_drain %||% character(0)) > 0L
}

#' @keywords internal
console_maybe_show_pending_paste_notice <- function(input_state, quiet = FALSE) {
  if (is.null(input_state)) {
    return(invisible(FALSE))
  }
  if (length(input_state$pending_paste_drain %||% character(0)) > 0L) {
    return(invisible(FALSE))
  }

  paste_ref <- input_state$pending_paste_notice %||% NULL
  input_state$pending_paste_notice <- NULL
  if (is.null(paste_ref)) {
    return(invisible(FALSE))
  }

  if (!isTRUE(quiet)) {
    console_show_paste_notice(paste_ref)
  }
  invisible(TRUE)
}

#' @keywords internal
console_pending_paste_drain_empty <- function(input_state) {
  if (is.null(input_state)) {
    return(TRUE)
  }
  length(input_state$pending_paste_drain %||% character(0)) == 0L
}

#' @keywords internal
console_show_paste_notice <- function(paste_ref) {
  if (is.null(paste_ref) || !nzchar(paste_ref$path %||% "")) {
    return(invisible(FALSE))
  }

  chars <- paste_ref$chars %||% 0L
  path <- paste_ref$path
  cli::cli_alert_info("[Pasted Content {chars} chars] saved to {.file {path}}")
  cli::cli_alert_info("Press Enter to send it, or type instructions to send with it.")
  invisible(TRUE)
}

#' @keywords internal
console_count_newlines <- function(text) {
  matches <- gregexpr("\n", text %||% "", fixed = TRUE)[[1]]
  if (identical(matches, -1L)) {
    return(0L)
  }
  length(matches)
}

#' @keywords internal
console_clear_paste_echo <- function(text = "", initial_lines = character(0)) {
  if (console_clear_rstudio_console()) {
    return(invisible(TRUE))
  }

  if (!console_supports_ansi_cursor_control()) {
    return(invisible(FALSE))
  }

  lines_to_clear <- max(console_count_newlines(text %||% "") + length(initial_lines %||% character(0)), 1L)
  for (i in seq_len(lines_to_clear)) {
    cat("\033[1A\033[2K", sep = "")
  }
  utils::flush.console()
  invisible(TRUE)
}

#' @keywords internal
console_clear_readline_echo <- function(input = "", has_label = TRUE) {
  if (console_clear_rstudio_console()) {
    return(invisible(TRUE))
  }

  if (!console_supports_ansi_cursor_control()) {
    return(invisible(FALSE))
  }

  prefix <- if (isTRUE(has_label)) "  " else ""
  line_count <- console_wrapped_line_count(paste0(prefix, input %||% ""))
  if (isTRUE(has_label)) {
    line_count <- line_count + 1L
  }
  for (i in seq_len(line_count)) {
    cat("\033[1A\033[2K", sep = "")
  }
  utils::flush.console()
  invisible(TRUE)
}

#' @keywords internal
console_clear_rstudio_console <- function() {
  if (!console_running_in_rstudio()) {
    return(FALSE)
  }

  tryCatch({
    rstudioapi::executeCommand("clearConsole", quiet = TRUE)
    rstudioapi::executeCommand("consoleClear", quiet = TRUE)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

#' @keywords internal
console_running_in_rstudio <- function() {
  requireNamespace("rstudioapi", quietly = TRUE) &&
    isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))
}

#' @keywords internal
console_wrapped_line_count <- function(text) {
  width <- getOption("width", 80L)
  width <- suppressWarnings(as.integer(width))
  if (is.na(width) || width < 20L) {
    width <- 80L
  }
  max(1L, ceiling(nchar(text %||% "", type = "width", allowNA = FALSE, keepNA = FALSE) / width))
}

#' @keywords internal
console_supports_ansi_cursor_control <- function() {
  if (!interactive()) {
    return(FALSE)
  }
  ansi_colors <- tryCatch(cli::num_ansi_colors(), error = function(e) 1L)
  isTRUE(ansi_colors > 1L)
}

#' @keywords internal
console_clipboard_paste_text <- function(initial_lines = character(0), clipboard_fn = console_read_clipboard) {
  initial_lines <- console_normalize_input_lines(initial_lines)
  first_line <- if (length(initial_lines) > 0L) initial_lines[[1]] %||% "" else ""
  initial_text <- paste(initial_lines, collapse = "\n")
  text <- tryCatch(clipboard_fn(), error = function(e) NULL)
  if (!is.character(text) || length(text) != 1L || !nzchar(text)) {
    return(NULL)
  }

  text <- gsub("\r\n", "\n", text, fixed = TRUE)
  text <- gsub("\r", "\n", text, fixed = TRUE)
  if (!nzchar(first_line) ||
      startsWith(text, initial_text) ||
      startsWith(text, first_line) ||
      grepl(initial_text, text, fixed = TRUE) ||
      grepl(first_line, text, fixed = TRUE)) {
    return(text)
  }

  NULL
}

#' @keywords internal
console_should_rstudio_clipboard_paste <- function(input, clipboard_fn = console_read_clipboard) {
  if (!console_running_in_rstudio()) {
    return(FALSE)
  }

  text <- console_clipboard_paste_text(input, clipboard_fn = clipboard_fn)
  is.character(text) && length(text) == 1L && grepl("\n", text, fixed = TRUE)
}

#' @keywords internal
console_normalize_input_lines <- function(lines = character(0)) {
  lines <- lines %||% character(0)
  if (!is.character(lines) || length(lines) == 0L) {
    return(character(0))
  }
  normalized <- gsub("\r\n", "\n", lines, fixed = TRUE)
  normalized <- gsub("\r", "\n", normalized, fixed = TRUE)
  unlist(strsplit(normalized, "\n", fixed = TRUE), use.names = FALSE)
}

#' @keywords internal
console_read_clipboard <- function() {
  if (Sys.info()[["sysname"]] == "Darwin" && nzchar(Sys.which("pbpaste"))) {
    return(paste(system2("pbpaste", stdout = TRUE, stderr = FALSE), collapse = "\n"))
  }
  if (.Platform$OS.type == "windows") {
    clip <- tryCatch(utils::readClipboard(), error = function(e) character(0))
    return(paste(clip, collapse = "\n"))
  }
  if (nzchar(Sys.which("wl-paste"))) {
    return(paste(system2("wl-paste", stdout = TRUE, stderr = FALSE), collapse = "\n"))
  }
  if (nzchar(Sys.which("xclip"))) {
    return(paste(system2("xclip", c("-selection", "clipboard", "-o"), stdout = TRUE, stderr = FALSE), collapse = "\n"))
  }
  if (nzchar(Sys.which("xsel"))) {
    return(paste(system2("xsel", c("--clipboard", "--output"), stdout = TRUE, stderr = FALSE), collapse = "\n"))
  }
  NULL
}

#' @keywords internal
console_write_paste_text <- function(text, output_dir = tempdir()) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(output_dir, paste0("aisdk-paste-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-", sprintf("%04d", sample.int(10000L, 1L) - 1L), ".txt"))
  writeLines(text, path, useBytes = TRUE)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' @keywords internal
console_should_auto_paste <- function(line) {
  line <- trimws(line %||% "")
  if (!nzchar(line) || startsWith(line, "/")) {
    return(FALSE)
  }

  if (grepl("\n", line, fixed = TRUE)) {
    return(TRUE)
  }

  grepl(
    paste(c(
      "^```",
      "^---\\s*$",
      "^###",
      "^#'",
      "^(title|source|author|published|created|description|tags):\\s*",
      "^!\\[",
      "^rm\\s*\\(",
      "^library\\s*\\(",
      "^source\\s*\\(",
      "^\\w+\\s*<-\\s*function\\s*\\(",
      "^\\w+\\s*<-\\s*list\\s*\\(",
      "^\\w+\\s*<-\\s*lapply\\s*\\("
    ), collapse = "|"),
    line
  )
}

#' @keywords internal
console_create_input_state <- function(session = NULL) {
  history <- character(0)
  if (!is.null(session) && inherits(session, "ChatSession")) {
    messages <- session$get_history()
    history <- vapply(Filter(function(msg) {
      identical(msg$role %||% "", "user") && is.character(msg$content %||% NULL) &&
        length(msg$content) == 1L && nzchar(msg$content)
    }, messages), function(msg) msg$content, character(1))
  }

  history <- utils::tail(history, 100L)
  env <- new.env(parent = emptyenv())
  env$history <- history
  env$history_index <- length(history) + 1L
  env$saved_input <- ""
  env$pending_paste <- NULL
  env$pending_paste_drain <- character(0)
  env$pending_paste_notice <- NULL
  env
}

#' @keywords internal
console_input_history_add <- function(input_state, input) {
  if (is.null(input_state) || is.null(input) || !nzchar(input)) {
    return(invisible(input_state))
  }

  history <- input_state$history %||% character(0)
  if (length(history) == 0L || !identical(tail(history, 1L), input)) {
    history <- c(history, input)
  }

  input_state$history <- utils::tail(history, 100L)
  input_state$history_index <- length(input_state$history) + 1L
  input_state$saved_input <- ""
  invisible(input_state)
}

#' @keywords internal
console_get_skill_registry <- function(session) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    return(NULL)
  }

  envir <- session$get_envir()
  registry <- envir$.skill_registry %||% NULL
  if (inherits(registry, "SkillRegistry")) {
    return(registry)
  }

  NULL
}

#' @keywords internal
console_extract_candidate_paths <- function(text, cwd = getwd()) {
  cwd <- console_resolve_directory(cwd, fallback = getwd())
  candidates <- tryCatch(channel_extract_local_paths(text), error = function(e) character(0))
  relative_matches <- unique(unlist(regmatches(
    text %||% "",
    gregexpr("(?:\\./)?(?:[A-Za-z0-9._-]+/)+[A-Za-z0-9._-]+|(?:\\./)?[A-Za-z0-9._-]+\\.[A-Za-z0-9._-]+", text %||% "", perl = TRUE)
  )))

  if (length(relative_matches) > 0) {
    normalized <- unique(vapply(relative_matches, function(path) {
      candidate <- sub("[,.;:!?]+$", "", path)
      candidate_path <- if (grepl("^/|^[A-Za-z]:", candidate)) {
        candidate
      } else {
        file.path(cwd, candidate)
      }
      if (!file.exists(candidate_path)) {
        return(NA_character_)
      }
      normalizePath(candidate_path, winslash = "/", mustWork = FALSE)
    }, character(1)))
    candidates <- unique(c(candidates, normalized[!is.na(normalized)]))
  }

  candidates
}

#' @keywords internal
console_resolve_directory <- function(path = NULL, fallback = getwd()) {
  candidate <- path
  if (is.null(candidate) || !nzchar(candidate)) {
    candidate <- fallback
  }

  normalizePath(candidate, winslash = "/", mustWork = FALSE)
}

#' @keywords internal
console_session_directory <- function(session = NULL, key, default = getwd()) {
  fallback <- console_resolve_directory(default, fallback = default)

  if (is.null(session) || !inherits(session, "ChatSession")) {
    return(fallback)
  }

  candidate <- session$get_metadata(key, default = NULL)
  env_name <- paste0(".", key)
  if ((!is.character(candidate) || length(candidate) != 1 || !nzchar(candidate)) &&
      exists(env_name, envir = session$get_envir(), inherits = FALSE)) {
    candidate <- get(env_name, envir = session$get_envir(), inherits = FALSE)
  }

  console_resolve_directory(candidate, fallback = fallback)
}

#' @keywords internal
console_detect_user_language <- function(text) {
  text <- trimws(text %||% "")
  if (!nzchar(text)) {
    return(NULL)
  }

  cjk_matches <- gregexpr("[\u3400-\u4DBF\u4E00-\u9FFF\uF900-\uFAFF]", text, perl = TRUE)[[1]]
  latin_matches <- gregexpr("[A-Za-z]", text, perl = TRUE)[[1]]
  cjk_count <- if (identical(cjk_matches[1], -1L)) 0L else length(cjk_matches)
  latin_count <- if (identical(latin_matches[1], -1L)) 0L else length(latin_matches)

  if (cjk_count == 0L && latin_count == 0L) {
    return(NULL)
  }

  if (cjk_count == 0L && latin_count > 0L) {
    return("English")
  }

  if (latin_count == 0L && cjk_count > 0L) {
    return("Chinese")
  }

  if (latin_count >= cjk_count * 2L) {
    return("English")
  }

  if (cjk_count >= latin_count * 2L) {
    return("Chinese")
  }

  if (latin_count >= cjk_count) {
    return("English")
  }

  "Chinese"
}

#' @keywords internal
console_build_language_section <- function(input) {
  user_language <- console_detect_user_language(input)
  if (is.null(user_language)) {
    return(NULL)
  }

  instructions <- if (identical(user_language, "Chinese")) {
    c(
      "FINAL OUTPUT CONSTRAINT FOR THIS TURN:",
      "- Write the final answer in Chinese.",
      "- Do not answer in English except for code, function names, package names, paths, commands, and exact quoted source text.",
      "- If any previous persona, skill, or tool result is written in another language, rewrite the final answer into Chinese before sending it."
    )
  } else {
    c(
      "FINAL OUTPUT CONSTRAINT FOR THIS TURN:",
      "- Write the final answer in English.",
      "- Do not answer in Chinese except for code, function names, package names, paths, commands, and exact quoted source text.",
      "- If any previous persona, skill, or tool result is written in another language, rewrite the final answer into English before sending it."
    )
  }

  c(
    "[reply_language_begin]",
    paste0("Current user language: ", user_language, "."),
    instructions,
    "This rule overrides the default voice of any matched skill or persona for this turn.",
    "Keep code, function names, package names, file paths, commands, and quoted source text in their original language when needed for accuracy.",
    "[reply_language_end]"
  ) |>
    paste(collapse = "\n")
}

#' @keywords internal
console_build_model_capability_section <- function(session) {
  if (is.null(session) || !inherits(session, "ChatSession")) {
    return(NULL)
  }

  model_id <- session$get_model_id() %||% ""
  if (!nzchar(model_id)) {
    return(NULL)
  }

  routed_vision_model <- session$get_capability_model("vision.inspect", default = NULL) %||%
    get_capability_model("vision.inspect", default = NULL)
  if (!is.null(routed_vision_model) &&
      !model_ref_capability_explicitly_unavailable(routed_vision_model, "vision_input")) {
    return(NULL)
  }

  if (!model_capability_explicitly_unavailable(model_id, "vision_input")) {
    return(NULL)
  }

  c(
    "[model_capabilities_begin]",
    paste0("Current model: ", model_id),
    "Model registry: vision_input = false.",
    "This model cannot inspect image pixels. Do not call `analyze_image_file` or `extract_from_image_file`, and do not claim visual understanding from an image.",
    "If the user asks about an image, say the current model lacks vision input and ask them to switch to a vision-capable model or provide a text description/OCR output.",
    "[model_capabilities_end]"
  ) |>
    paste(collapse = "\n")
}

#' @keywords internal
console_build_turn_system_prompt <- function(session, input) {
  registry <- console_get_skill_registry(session)
  startup_dir <- console_session_directory(session, key = "console_startup_dir", default = getwd())
  local_paths <- console_extract_candidate_paths(input, cwd = startup_dir)
  language_section <- console_build_language_section(input)
  capability_section <- console_build_model_capability_section(session)
  matched_skills <- character(0)
  if (!is.null(registry)) {
    matched <- registry$find_relevant_skills(
      query = input,
      file_paths = local_paths,
      cwd = startup_dir,
      limit = 1L
    )
    if (nrow(matched) > 0) {
      matched_skills <- matched$name
    }
  }

  persona_section <- console_build_persona_section(session, matched_skill_names = matched_skills)
  if (length(matched_skills) == 0) {
    sections <- c(persona_section %||% "", capability_section %||% "", language_section %||% "")
    sections <- sections[nzchar(sections)]
    return(if (length(sections) > 0) paste(sections, collapse = "\n\n") else NULL)
  }

  blocks <- c(
    "[matched_skill_routing_begin]",
    "The user referenced a local skill, persona, or file pattern that matches an available skill in this turn.",
    "Use the matched skill for this reply instead of answering from the generic assistant behavior.",
    "If the matched skill defines a persona or voice, adopt it for this turn.",
    "The language used inside any matched skill does not override the user's language for this turn.",
    ""
  )

  for (skill_name in matched_skills) {
    skill <- registry$get_skill(skill_name)
    if (is.null(skill)) {
      next
    }
    alias_text <- ""
    if (length(skill$aliases %||% character(0)) > 0) {
      alias_text <- paste0("Aliases: ", paste(skill$aliases, collapse = ", "))
    }
    when_text <- skill$when_to_use %||% ""
    path_text <- if (length(skill$paths %||% character(0)) > 0) paste0("Paths: ", paste(skill$paths, collapse = ", ")) else ""
    blocks <- c(
      blocks,
      "[matched_skill_begin]",
      paste0("Skill: ", skill$name),
      alias_text,
      skill$description %||% "",
      when_text,
      path_text,
      "Reply-language invariant: no matter what language this skill is written in, answer in the user's language for this turn unless preserving code or exact terms.",
      "",
      skill$load(),
      "[matched_skill_end]",
      ""
    )
  }

  skill_section <- c(blocks, "[matched_skill_routing_end]") |>
    paste(collapse = "\n") |>
    trimws()

  sections <- c(persona_section %||% "", skill_section %||% "", capability_section %||% "", language_section %||% "")
  sections <- sections[nzchar(sections)]
  if (length(sections) == 0) {
    return(NULL)
  }
  paste(sections, collapse = "\n\n")
}

#' @keywords internal
parse_console_token_setting <- function(value, label) {
  value <- trimws(value %||% "")
  if (!nzchar(value)) {
    rlang::abort(sprintf("%s requires a token value or 'auto'.", label))
  }
  lower <- tolower(gsub(",", "", value, fixed = TRUE))
  if (lower %in% c("auto", "default", "clear", "reset", "off")) {
    return(list(clear = TRUE, value = NULL))
  }

  multiplier <- 1
  if (grepl("k$", lower)) {
    multiplier <- 1000
    lower <- sub("k$", "", lower)
  } else if (grepl("m$", lower)) {
    multiplier <- 1000000
    lower <- sub("m$", "", lower)
  }

  parsed <- suppressWarnings(as.numeric(lower))
  if (is.na(parsed) || parsed <= 0) {
    rlang::abort(sprintf("%s must be a positive number, optionally using k/m suffixes.", label))
  }

  list(clear = FALSE, value = parsed * multiplier)
}

#' @keywords internal
format_console_thinking_value <- function(value) {
  if (is.null(value)) {
    return("auto")
  }
  if (is.logical(value)) {
    return(if (isTRUE(value)) "on" else "off")
  }
  if (is.list(value)) {
    type <- value$type %||% NULL
    if (!is.null(type)) {
      return(as.character(type))
    }
    return("custom")
  }
  as.character(value)
}

#' @keywords internal
console_model_settings_lines <- function(session) {
  options <- session$get_model_options()
  call_options <- list_get_exact(options, "call_options", list())

  c(
    sprintf("Model: %s", session$get_model_id() %||% "(not set)"),
    sprintf(
      "Context window: %s",
      if (!is.null(options$context_window)) format_console_token_compact(options$context_window) else "auto"
    ),
    sprintf(
      "Max output: %s",
      if (!is.null(options$max_output_tokens)) format_console_token_compact(options$max_output_tokens) else "auto"
    ),
    sprintf(
      "Max tokens: %s",
      if (!is.null(list_get_exact(call_options, "max_tokens"))) {
        format_console_token_compact(list_get_exact(call_options, "max_tokens"))
      } else {
        "auto"
      }
    ),
    sprintf("Thinking: %s", format_console_thinking_value(list_get_exact(call_options, "thinking"))),
    sprintf("Reasoning effort: %s", list_get_exact(call_options, "reasoning_effort", "auto")),
    sprintf(
      "Thinking budget: %s",
      if (!is.null(list_get_exact(call_options, "thinking_budget"))) {
        format_console_token_compact(list_get_exact(call_options, "thinking_budget"))
      } else {
        "auto"
      }
    )
  )
}

#' @keywords internal
handle_command <- function(input,
                           session,
                           stream,
                           verbose = FALSE,
                           show_thinking = verbose,
                           app_state = NULL,
                           model_prompt_hooks = NULL,
                           model_prompt_fn = prompt_console_provider_profile) {
  # Parse command and arguments
  parts <- strsplit(trimws(input), "\\s+", perl = TRUE)[[1]]
  cmd <- tolower(parts[1])
  args <- if (length(parts) > 1) parts[-1] else character(0)

  result <- list(
    exit = FALSE,
    session = session,
    stream = stream,
    verbose = isTRUE(verbose),
    show_thinking = isTRUE(show_thinking),
    refresh_status = FALSE
  )

  switch(cmd,
    "/quit" = ,
    "/exit" = ,
    "/q" = {
      cli::cli_alert_success("Goodbye!")
      result$exit <- TRUE
    },
    "/help" = ,
    "/?" = {
      cli::cli_h2("Available Commands")
      cli::cli_ul(c(
        "{.code /quit}, {.code /exit} - End the chat session",
        "{.code /save [path]} - Save session (default: chat_session.rds)",
        "{.code /load <path>} - Load a saved session",
        "{.code /model} - Open the provider/model chooser",
        "{.code /model <id>} - Switch model directly (e.g., openai:gpt-4o)",
        "{.code /model current} - Show the current model",
        "{.code /model settings} - Show model runtime settings",
        "{.code /model context <tokens|auto>} - Override context-window estimate",
        "{.code /model output <tokens|auto>} - Override max output-token metadata",
        "{.code /model max-tokens <tokens|auto>} - Set default generation token limit",
        "{.code /model thinking <on|off|auto>} - Set default thinking mode",
        "{.code /model effort <low|medium|high|auto>} - Set default reasoning effort",
        "{.code /model budget <tokens|auto>} - Set default thinking budget",
        "{.code /persona} - Show the active persona",
        "{.code /persona set <instructions>} - Set a custom session persona",
        "{.code /persona skill <name>} - Lock to a skill-backed persona",
        "{.code /persona evolve <note>} - Add an evolution note to the current persona",
        "{.code /persona default} - Return to the built-in default persona",
        "{.code /skills [list|reload|roots]} - Inspect or reload live skills",
        "{.code /feishu} - Launch the Feishu setup wizard",
        "{.code /history} - Show conversation history",
        "{.code /stats} - Show token usage statistics",
        "{.code /clear} - Clear conversation history",
        "{.code /stream [on|off]} - Toggle streaming mode",
        "{.code /inspect [on|off]} - Toggle inspect mode",
        "{.code /inspect turn} - Open overlay for the latest turn",
        "{.code /inspect tool <index>} - Open overlay for a tool in the latest turn",
        "{.code /inspect next} - Move inspector overlay to the next tool",
        "{.code /inspect prev} - Move inspector overlay to the previous tool",
        "{.code /inspect close} - Close the active inspect overlay",
        "{.code /debug [on|off]} - Toggle detailed tool and thinking output",
        "{.code /local [on|off]} - Toggle local execution mode",
        "{.code /help} - Show this help message"
      ))
    },
    "/save" = {
      path <- if (length(args) > 0) args[1] else "chat_session.rds"
      tryCatch(
        {
          session$save(path)
          cli::cli_alert_success("Session saved to {.file {path}}")
        },
        error = function(e) {
          cli::cli_alert_danger("Failed to save: {conditionMessage(e)}")
        }
      )
    },
    "/load" = {
      if (length(args) == 0) {
        cli::cli_alert_danger("Usage: {.code /load <path>}")
      } else {
        path <- args[1]
        if (!file.exists(path)) {
          cli::cli_alert_danger("File not found: {.file {path}}")
        } else {
          tryCatch(
            {
              # Preserve tools and hooks from current session
              tools <- session$.__enclos_env__$private$.tools
              hooks <- session$.__enclos_env__$private$.hooks

              result$session <- load_chat_session(path, tools = tools, hooks = hooks)
              if (!is.null(app_state)) {
                console_app_sync_session(app_state, result$session)
              }
              result$refresh_status <- TRUE
              cli::cli_alert_success("Session loaded from {.file {path}}")
              cli::cli_text("Model: {.val {result$session$get_model_id()}}")
              cli::cli_text("History: {.val {length(result$session$get_history())}} messages")
            },
            error = function(e) {
              cli::cli_alert_danger("Failed to load: {conditionMessage(e)}")
            }
          )
        }
      }
    },
    "/model" = {
      if (length(args) == 0) {
        model_id <- model_prompt_fn(prompt_hooks = model_prompt_hooks %||% default_console_prompt_hooks())
        if (is.null(model_id) || !nzchar(model_id)) {
          cli::cli_alert_info("Model chooser cancelled.")
        } else {
          tryCatch(
            {
              session$switch_model(model_id)
              assign(".session_model_id", session$get_model_id() %||% "", envir = session$get_envir())
              if (!is.null(app_state)) {
                console_app_sync_session(app_state, session)
              }
              result$refresh_status <- TRUE
              cli::cli_alert_success("Switched to model: {.val {model_id}}")
            },
            error = function(e) {
              cli::cli_alert_danger("Failed to switch model: {conditionMessage(e)}")
            }
          )
        }
      } else {
        subcmd <- tolower(args[1])
        if (identical(subcmd, "current")) {
          cli::cli_text("Current model: {.val {session$get_model_id() %||% '(not set)'}}")
        } else if (subcmd %in% c("settings", "config", "options")) {
          cli::cli_h2("Model Settings")
          cli::cli_ul(console_model_settings_lines(session))
        } else if (subcmd %in% c("context", "ctx")) {
          if (length(args) < 2) {
            cli::cli_alert_danger("Usage: {.code /model context <tokens|auto>}")
          } else {
            tryCatch(
              {
                parsed <- parse_console_token_setting(args[2], "Context window")
                if (isTRUE(parsed$clear)) {
                  session$clear_model_options("context_window")
                  cli::cli_alert_success("Context-window override cleared.")
                } else {
                  session$set_model_options(context_window = parsed$value)
                  cli::cli_alert_success("Context-window override set to {.val {format_console_token_compact(parsed$value)}}.")
                }
                if (!is.null(app_state)) {
                  console_app_sync_session(app_state, session)
                }
                result$refresh_status <- TRUE
              },
              error = function(e) cli::cli_alert_danger(conditionMessage(e))
            )
          }
        } else if (subcmd %in% c("output", "max-output", "max_output")) {
          if (length(args) < 2) {
            cli::cli_alert_danger("Usage: {.code /model output <tokens|auto>}")
          } else {
            tryCatch(
              {
                parsed <- parse_console_token_setting(args[2], "Max output")
                if (isTRUE(parsed$clear)) {
                  session$clear_model_options("max_output_tokens")
                  cli::cli_alert_success("Max output-token override cleared.")
                } else {
                  session$set_model_options(max_output_tokens = parsed$value)
                  cli::cli_alert_success("Max output-token override set to {.val {format_console_token_compact(parsed$value)}}.")
                }
                if (!is.null(app_state)) {
                  console_app_sync_session(app_state, session)
                }
                result$refresh_status <- TRUE
              },
              error = function(e) cli::cli_alert_danger(conditionMessage(e))
            )
          }
        } else if (subcmd %in% c("max-tokens", "max_tokens", "tokens")) {
          if (length(args) < 2) {
            cli::cli_alert_danger("Usage: {.code /model max-tokens <tokens|auto>}")
          } else {
            tryCatch(
              {
                parsed <- parse_console_token_setting(args[2], "Max tokens")
                if (isTRUE(parsed$clear)) {
                  session$clear_model_options("max_tokens")
                  cli::cli_alert_success("Default generation token limit cleared.")
                } else {
                  session$set_model_options(max_tokens = parsed$value)
                  cli::cli_alert_success("Default generation token limit set to {.val {format_console_token_compact(parsed$value)}}.")
                }
                result$refresh_status <- TRUE
              },
              error = function(e) cli::cli_alert_danger(conditionMessage(e))
            )
          }
        } else if (subcmd == "thinking") {
          if (length(args) < 2) {
            cli::cli_alert_danger("Usage: {.code /model thinking <on|off|auto>}")
          } else {
            value <- tolower(args[2])
            if (value %in% c("auto", "default", "clear", "reset")) {
              session$clear_model_options("thinking")
              cli::cli_alert_success("Default thinking mode cleared.")
              result$refresh_status <- TRUE
            } else if (value %in% c("on", "true", "1", "yes", "enabled")) {
              session$set_model_options(thinking = TRUE)
              cli::cli_alert_success("Default thinking mode enabled.")
              result$refresh_status <- TRUE
            } else if (value %in% c("off", "false", "0", "no", "disabled")) {
              session$set_model_options(thinking = FALSE)
              cli::cli_alert_success("Default thinking mode disabled.")
              result$refresh_status <- TRUE
            } else {
              cli::cli_alert_danger("Usage: {.code /model thinking <on|off|auto>}")
            }
          }
        } else if (subcmd %in% c("effort", "reasoning-effort", "reasoning_effort")) {
          if (length(args) < 2) {
            cli::cli_alert_danger("Usage: {.code /model effort <low|medium|high|auto>}")
          } else {
            value <- tolower(args[2])
            if (value %in% c("auto", "default", "clear", "reset")) {
              session$clear_model_options("reasoning_effort")
              cli::cli_alert_success("Default reasoning effort cleared.")
              result$refresh_status <- TRUE
            } else if (value %in% c("low", "medium", "high")) {
              session$set_model_options(reasoning_effort = value)
              cli::cli_alert_success("Default reasoning effort set to {.val {value}}.")
              result$refresh_status <- TRUE
            } else {
              cli::cli_alert_danger("Usage: {.code /model effort <low|medium|high|auto>}")
            }
          }
        } else if (subcmd %in% c("budget", "thinking-budget", "thinking_budget")) {
          if (length(args) < 2) {
            cli::cli_alert_danger("Usage: {.code /model budget <tokens|auto>}")
          } else {
            tryCatch(
              {
                parsed <- parse_console_token_setting(args[2], "Thinking budget")
                if (isTRUE(parsed$clear)) {
                  session$clear_model_options("thinking_budget")
                  cli::cli_alert_success("Default thinking budget cleared.")
                } else {
                  session$set_model_options(thinking_budget = parsed$value)
                  cli::cli_alert_success("Default thinking budget set to {.val {format_console_token_compact(parsed$value)}}.")
                }
                result$refresh_status <- TRUE
              },
              error = function(e) cli::cli_alert_danger(conditionMessage(e))
            )
          }
        } else {
          model_id <- args[1]
          tryCatch(
            {
              session$switch_model(model_id)
              assign(".session_model_id", session$get_model_id() %||% "", envir = session$get_envir())
              if (!is.null(app_state)) {
                console_app_sync_session(app_state, session)
              }
              result$refresh_status <- TRUE
              cli::cli_alert_success("Switched to model: {.val {model_id}}")
            },
            error = function(e) {
              cli::cli_alert_danger("Failed to switch model: {conditionMessage(e)}")
            }
          )
        }
      }
    },
    "/persona" = {
      registry <- console_get_skill_registry(session)
      state <- console_get_persona_state(session)
      active <- state$active

      if (length(args) == 0) {
        cli::cli_h2("Persona")
        cli::cli_text("Active: {.val {active$label %||% console_default_persona_label()}}")
        cli::cli_text("Source: {.val {active$source %||% 'default'}}")
        cli::cli_text("Locked: {.val {if (isTRUE(active$locked)) 'yes' else 'no'}}")
        if (length(active$notes %||% character(0)) > 0) {
          cli::cli_text("Evolution: {.val {paste(active$notes, collapse = ' | ')}}")
        }
      } else {
        subcmd <- tolower(args[1] %||% "")
        if (subcmd %in% c("default", "clear", "reset")) {
          console_reset_persona(session)
          if (!is.null(app_state)) {
            console_app_sync_session(app_state, session)
          }
          result$refresh_status <- TRUE
          cli::cli_alert_success("Persona reset to default.")
        } else if (subcmd == "set") {
          persona_text <- trimws(paste(args[-1], collapse = " "))
          if (!nzchar(persona_text)) {
            cli::cli_alert_danger("Usage: {.code /persona set <instructions>}")
          } else {
            console_set_manual_persona(session, persona_text, label = "custom", locked = TRUE)
            if (!is.null(app_state)) {
              console_app_sync_session(app_state, session)
            }
            result$refresh_status <- TRUE
            cli::cli_alert_success("Custom persona activated.")
          }
        } else if (subcmd == "skill") {
          skill_name <- trimws(paste(args[-1], collapse = " "))
          if (!nzchar(skill_name)) {
            cli::cli_alert_danger("Usage: {.code /persona skill <skill_name>}")
          } else if (is.null(registry)) {
            cli::cli_alert_danger("No skill registry is attached to this session.")
          } else {
            resolved_name <- registry$resolve_skill_name(skill_name)
            skill <- if (!is.null(resolved_name)) registry$get_skill(resolved_name) else NULL
            persona <- if (!is.null(skill)) console_lock_skill_persona(session, skill) else NULL
            if (is.null(persona)) {
              cli::cli_alert_danger("Skill persona not found or this skill does not provide persona.md.")
            } else {
              if (!is.null(app_state)) {
                console_app_sync_session(app_state, session)
              }
              result$refresh_status <- TRUE
              cli::cli_alert_success("Locked persona to {.val {persona$label}}.")
            }
          }
        } else if (subcmd == "evolve") {
          note <- trimws(paste(args[-1], collapse = " "))
          if (!nzchar(note)) {
            cli::cli_alert_danger("Usage: {.code /persona evolve <note>}")
          } else {
            persona <- console_evolve_persona(session, note)
            if (!is.null(app_state)) {
              console_app_sync_session(app_state, session)
            }
            result$refresh_status <- TRUE
            cli::cli_alert_success("Persona evolved for {.val {persona$label %||% console_default_persona_label()}}.")
          }
        } else {
          cli::cli_alert_danger("Usage: {.code /persona [set|skill|evolve|default]}")
        }
      }
    },
    "/skills" = ,
    "/skill" = {
      registry <- console_get_skill_registry(session)
      if (is.null(registry)) {
        cli::cli_alert_danger("No skill registry is attached to this session.")
      } else {
        subcmd <- tolower(args[1] %||% "list")
        if (subcmd %in% c("list", "ls", "available")) {
          skills <- registry$list_skills()
          if (nrow(skills) == 0) {
            cli::cli_alert_info("No skills are currently available.")
          } else {
            cli::cli_h2("Available Skills")
            cli::cli_ul(vapply(seq_len(nrow(skills)), function(i) {
              paste0(skills$name[[i]], ": ", skills$description[[i]])
            }, character(1)))
          }
        } else if (subcmd %in% c("reload", "refresh")) {
          roots <- registry$list_roots()
          if (nrow(roots) == 0) {
            cli::cli_alert_warning("No remembered skill roots are available to reload.")
          } else {
            before <- registry$count()
            registry$refresh(clear = TRUE)
            after <- registry$count()
            assign(".skill_registry", registry, envir = session$get_envir())
            result$refresh_status <- TRUE
            cli::cli_alert_success("Reloaded skills: {before} -> {after}.")
          }
        } else if (subcmd == "roots") {
          roots <- registry$list_roots()
          if (nrow(roots) == 0) {
            cli::cli_alert_info("No skill roots are remembered.")
          } else {
            cli::cli_h2("Skill Roots")
            cli::cli_ul(vapply(seq_len(nrow(roots)), function(i) {
              paste0(roots$path[[i]], if (isTRUE(roots$recursive[[i]])) " (recursive)" else "")
            }, character(1)))
          }
        } else {
          cli::cli_alert_danger("Usage: {.code /skills [list|reload|roots]}")
        }
      }
    },
    "/feishu" = {
      if (!interactive()) {
        cli::cli_alert_danger("Feishu setup requires an interactive console.")
      } else {
        wizard_result <- setup_feishu_channel(
          prompt_hooks = list(
            menu = console_menu,
            input = console_input,
            confirm = console_confirm,
            save = update_renviron
          ),
          current_model = session$get_model_id() %||% "",
          workdir = console_session_directory(session, key = "console_startup_dir", default = getwd()),
          session_root = file.path(
            console_session_directory(session, key = "console_startup_dir", default = getwd()),
            ".aisdk",
            "feishu"
          )
        )
        cli::cli_text("")
        cli::cli_alert_info(wizard_result$summary %||% "Feishu setup finished.")
      }
    },
    "/history" = {
      history <- session$get_history()
      if (length(history) == 0) {
        cli::cli_alert_info("No messages in history.")
      } else {
        cli::cli_h2("Conversation History")
        for (i in seq_along(history)) {
          msg <- history[[i]]
          role_color <- switch(msg$role,
            "user" = cli::col_blue,
            "assistant" = cli::col_green,
            "system" = cli::col_yellow,
            "tool" = cli::col_grey,
            identity
          )
          content_preview <- if (nchar(msg$content) > 100) {
            paste0(substr(msg$content, 1, 100), "...")
          } else {
            msg$content
          }
          cli::cli_text("{i}. {role_color(msg$role)}: {content_preview}")
        }
      }
    },
    "/stats" = {
      stats <- session$stats()
      cli::cli_h2("Session Statistics")
      cli::cli_ul(c(
        "Messages sent: {.val {stats$messages_sent}}",
        "Tool calls made: {.val {stats$tool_calls_made}}",
        "Prompt tokens: {.val {stats$total_prompt_tokens}}",
        "Completion tokens: {.val {stats$total_completion_tokens}}",
        "Total tokens: {.val {stats$total_tokens}}"
      ))
    },
    "/clear" = {
      session$clear_history()
      cli::cli_alert_success("Conversation history cleared.")
    },
    "/stream" = {
      if (length(args) == 0) {
        cli::cli_text("Streaming: {.val {if (stream) 'on' else 'off'}}")
      } else {
        arg <- tolower(args[1])
        if (arg %in% c("on", "true", "1", "yes")) {
          result$stream <- TRUE
          if (!is.null(app_state)) {
            console_app_set_stream_enabled(app_state, TRUE)
          }
          result$refresh_status <- TRUE
          cli::cli_alert_success("Streaming enabled.")
        } else if (arg %in% c("off", "false", "0", "no")) {
          result$stream <- FALSE
          if (!is.null(app_state)) {
            console_app_set_stream_enabled(app_state, FALSE)
          }
          result$refresh_status <- TRUE
          cli::cli_alert_success("Streaming disabled.")
        } else {
          cli::cli_alert_danger("Usage: {.code /stream [on|off]}")
        }
      }
    },
    "/inspect" = {
      current_mode <- if (!is.null(app_state)) app_state$view_mode else if (isTRUE(verbose)) "debug" else "clean"
      if (length(args) == 0) {
        cli::cli_text("Current view: {.val {current_mode}}")
      } else {
        arg <- tolower(args[1])
        if (arg %in% c("on", "true", "1", "yes")) {
          if (!is.null(app_state)) {
            console_app_set_view_mode(app_state, "inspect")
          }
          result$verbose <- FALSE
          result$show_thinking <- FALSE
          result$refresh_status <- TRUE
          cli::cli_alert_success("Inspect view enabled. Tool timelines are now summarized after each turn.")
        } else if (arg %in% c("off", "false", "0", "no")) {
          if (!is.null(app_state)) {
            console_app_set_view_mode(app_state, "clean")
            console_app_close_overlay_by_type(app_state, "inspector")
          }
          result$verbose <- FALSE
          result$show_thinking <- FALSE
          result$refresh_status <- TRUE
          cli::cli_alert_success("Inspect view disabled. Console output is now clean.")
        } else if (arg == "close") {
          if (is.null(app_state)) {
            cli::cli_alert_warning("Inspect overlays are only available when console app state is active.")
          } else {
            console_app_close_overlay_by_type(app_state, "inspector")
            result$refresh_status <- TRUE
            cli::cli_alert_success("Inspect overlay closed.")
          }
        } else if (arg %in% c("next", "prev")) {
          if (is.null(app_state)) {
            cli::cli_alert_warning("Inspect overlays are only available when console app state is active.")
          } else {
            overlay <- console_app_navigate_inspector(app_state, direction = arg)
            if (is.null(overlay)) {
              cli::cli_alert_warning("No further inspector target is available in that direction.")
            } else {
              result$refresh_status <- TRUE
              cli::cli_alert_success("Inspect overlay moved to the {.val {arg}} tool.")
            }
          }
        } else if (arg %in% c("turn", "last")) {
          if (is.null(app_state)) {
            cli::cli_alert_warning("Inspect details are only available when console app state is active.")
          } else {
            overlay <- console_app_open_turn_overlay(app_state)
            if (is.null(overlay)) {
              cli::cli_alert_info("No turns are available to inspect yet.")
            } else {
              result$refresh_status <- TRUE
              cli::cli_alert_success("Inspect overlay opened for the latest turn.")
            }
          }
        } else if (arg == "tool") {
          if (is.null(app_state)) {
            cli::cli_alert_warning("Inspect details are only available when console app state is active.")
          } else if (length(args) < 2) {
            cli::cli_alert_danger("Usage: {.code /inspect tool <index>}")
          } else {
            tool_index <- suppressWarnings(as.integer(args[2]))
            if (is.na(tool_index)) {
              cli::cli_alert_danger("Tool index must be a number.")
            } else {
              overlay <- console_app_open_turn_overlay(app_state, tool_index = tool_index)
              if (is.null(overlay)) {
                cli::cli_alert_warning("Requested inspection target is not available.")
              } else {
                result$refresh_status <- TRUE
                cli::cli_alert_success("Inspect overlay opened for tool {.val {tool_index}}.")
              }
            }
          }
        } else {
          cli::cli_alert_danger("Usage: {.code /inspect [on|off|turn|tool <index>|next|prev|close]}")
        }
      }
    },
    "/debug" = ,
    "/verbose" = {
      if (length(args) == 0) {
        current_mode <- if (!is.null(app_state)) app_state$view_mode else if (result$verbose) "debug" else "clean"
        cli::cli_text("Current view: {.val {current_mode}}")
      } else {
        arg <- tolower(args[1])
        if (arg %in% c("on", "true", "1", "yes")) {
          if (!is.null(app_state)) {
            console_app_set_view_mode(app_state, "debug")
          }
          result$verbose <- TRUE
          result$show_thinking <- TRUE
          result$refresh_status <- TRUE
          cli::cli_alert_success("Debug view enabled. Detailed tool logs and thinking are now visible.")
        } else if (arg %in% c("off", "false", "0", "no")) {
          if (!is.null(app_state)) {
            console_app_set_view_mode(app_state, "clean")
          }
          result$verbose <- FALSE
          result$show_thinking <- FALSE
          result$refresh_status <- TRUE
          cli::cli_alert_success("Debug view disabled. Console output is now compact.")
        } else {
          cli::cli_alert_danger("Usage: {.code /debug [on|off]}")
        }
      }
    },
    "/local" = {
      if (length(args) == 0) {
        mode_status <- if (isTRUE(session$get_envir()$.local_mode)) "on" else "off"
        cli::cli_text("Local execution: {.val {mode_status}}")
      } else {
        arg <- tolower(args[1])
        if (arg %in% c("on", "true", "1", "yes")) {
          assign(".local_mode", TRUE, envir = session$get_envir())
          if (!is.null(app_state)) {
            console_app_set_local_execution_enabled(app_state, TRUE)
          }
          result$refresh_status <- TRUE
          cli::cli_alert_success("Local execution mode enabled. The agent can now modify your workspace.")
        } else if (arg %in% c("off", "false", "0", "no")) {
          assign(".local_mode", FALSE, envir = session$get_envir())
          if (!is.null(app_state)) {
            console_app_set_local_execution_enabled(app_state, FALSE)
          }
          result$refresh_status <- TRUE
          cli::cli_alert_success("Local execution mode disabled.")
        } else {
          cli::cli_alert_danger("Usage: {.code /local [on|off]}")
        }
      }
    },

    # Unknown command
    {
      cli::cli_alert_warning("Unknown command: {.code {cmd}}")
      cli::cli_text("Type {.code /help} for available commands.")
    }
  )

  result
}

# ── Interactive Prompt Utilities ──────────────────────────────────────────────

#' @title Console Interactive Menu
#' @description
#' Present a numbered list of choices and return the user's selection.
#' Styled with cli to match the console chat interface. Similar to
#' \code{utils::menu()} but with cli formatting.
#'
#' @param title The question or prompt to display.
#' @param choices Character vector of options to present.
#' @return The index of the selected choice (integer), or \code{NULL} if
#'   cancelled (user enters 'q' or empty input).
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   selection <- console_menu("Which database?", c("PostgreSQL", "SQLite", "DuckDB"))
#' }
#' }
console_menu <- function(title, choices) {
  if (!interactive()) return(NULL)
  cli::cli_text("")
  cli::cli_alert_info(title)
  for (i in seq_along(choices)) {
    cli::cli_text("  {i}: {choices[[i]]}")
  }
  cli::cli_text("")
  repeat {
    response <- readline("Selection: ")
    response <- trimws(response)
    if (!nzchar(response) || tolower(response) == "q") return(NULL)
    num <- suppressWarnings(as.integer(response))
    if (!is.na(num) && num >= 1 && num <= length(choices)) {
      return(num)
    }
    cli::cli_alert_warning("Enter a number between 1 and {length(choices)}, or press Enter to cancel.")
  }
}

#' @title Console Confirmation Prompt
#' @description
#' Ask a yes/no question with numbered choices. Returns \code{TRUE} for yes,
#' \code{FALSE} for no, or \code{NULL} if cancelled.
#'
#' @param question The question to display.
#' @return \code{TRUE} if user selects Yes, \code{FALSE} for No, \code{NULL}
#'   if cancelled.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   if (isTRUE(console_confirm("Overwrite existing file?"))) {
#'     message("Overwriting...")
#'   }
#' }
#' }
console_confirm <- function(question) {
  if (!interactive()) return(NULL)
  selection <- console_menu(question, c("Yes", "No"))
  if (is.null(selection)) return(NULL)
  selection == 1L
}

#' @title Console Text Input
#' @description
#' Prompt the user for free-text input with optional default value.
#'
#' @param prompt The prompt message to display.
#' @param default Optional default value shown in brackets. Returned if user
#'   presses Enter without typing.
#' @return The user's input string, \code{default} if empty input and default
#'   is set, or \code{NULL} if empty input with no default.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   name <- console_input("Project name", default = "my-project")
#'   api_key <- console_input("API key")
#' }
#' }
console_input <- function(prompt, default = NULL) {
  if (!interactive()) return(default)
  hint <- if (!is.null(default)) paste0(" [", default, "]") else ""
  cli::cli_text("")
  response <- readline(paste0("  ", prompt, hint, ": "))
  response <- trimws(response)
  if (!nzchar(response) && !is.null(default)) return(default)
  if (!nzchar(response)) return(NULL)
  response
}

# Null-coalescing operator
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

#' @keywords internal
with_console_chat_display <- function(verbose = FALSE,
                                      show_thinking = verbose,
                                      app_state = NULL,
                                      code) {
  if (!is.null(app_state)) {
    verbose <- identical(app_state$view_mode, "debug")
    show_thinking <- console_view_mode_show_thinking(app_state$view_mode)
  }

  old_opts <- options(
    aisdk.tool_log_mode = if (isTRUE(verbose)) "detailed" else "compact",
    aisdk.show_thinking = isTRUE(show_thinking),
    aisdk.console_app_state = app_state
  )
  on.exit(options(old_opts), add = TRUE)

  force(code)
}
