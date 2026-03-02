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
#' By default, the console operates in agent mode with tools for bash execution,
#' file operations, R code execution, and more. Set `agent = NULL` for simple
#' chat without tools.
#'
#' @param session A ChatSession object, a LanguageModelV1 object, or a model string ID to create a new session.
#' @param system_prompt Optional system prompt (merged with agent prompt if agent is used).
#' @param tools Optional list of additional Tool objects.
#' @param hooks Optional HookHandler object.
#' @param stream Whether to use streaming output. Default TRUE.
#' @param agent Agent configuration. Options:
#'   - `"auto"` (default): Use the built-in console agent with terminal tools
#'   - `NULL`: Simple chat mode without tools
#'   - An Agent object: Use the provided custom agent
#' @param working_dir Working directory for the console agent. Defaults to getwd() interactively, tempdir() otherwise.
#' @param sandbox_mode Sandbox mode for the console agent: "strict", "permissive" (default), or "none".
#' @return The ChatSession object (invisibly) when chat ends.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # Start with default agent (intelligent terminal mode)
#'   console_chat("openai:gpt-4o")
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
#'   # /model [id]    - Switch to a different model
#'   # /history       - Show conversation history
#'   # /stats         - Show token usage statistics
#'   # /clear         - Clear conversation history
#'   # /stream [on|off] - Toggle streaming mode
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
                         agent = "auto",
                         working_dir = if (interactive()) getwd() else tempdir(),
                         sandbox_mode = "permissive") {
  # Ensure cli package is available
  if (!requireNamespace("cli", quietly = TRUE)) {
    rlang::abort("Package 'cli' is required for console_chat(). Install with: install.packages('cli')")
  }

  # Resolve agent
  agent_mode <- FALSE
  if (is.character(agent) && agent == "auto") {
    agent <- create_console_agent(
      working_dir = working_dir,
      sandbox_mode = sandbox_mode,
      additional_tools = tools
    )
    agent_mode <- TRUE
    tools <- NULL # Tools are now in the agent
  } else if (inherits(agent, "Agent")) {
    agent_mode <- TRUE
  }

  # Create or use existing session
  if (is.null(session)) {
    cli::cli_alert_warning("No model specified.")
    model_id <- console_input(
      "Enter model ID (e.g., openai:gpt-4o), or press Enter to skip"
    )
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

  # Welcome message
  cli::cli_h1("R AI SDK Console Chat")
  cli::cli_text("Model: {.val {session$get_model_id() %||% '(not set)'}}")
  if (agent_mode) {
    n_tools <- length(session$.__enclos_env__$private$.tools)
    cli::cli_text("Agent mode: {.val enabled} ({n_tools} tools)")
  } else {
    cli::cli_text("Agent mode: {.val disabled} (simple chat)")
  }
  cli::cli_text("Type {.code /help} for commands, {.code /quit} to exit.")
  cli::cli_rule()

  # Main REPL loop
  while (TRUE) {
    # Read user input
    input <- tryCatch(
      readline_multiline(),
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
      result <- handle_command(input, session, stream)
      if (result$exit) {
        break
      }
      session <- result$session
      stream <- result$stream
      next
    }

    # Check if model is set
    if (is.null(session$get_model_id())) {
      cli::cli_alert_danger("No model set. Use {.code /model <id>} first.")
      cli::cli_alert_info("Example: {.code /model openai:gpt-4o}")
      next
    }

    # Send message to model
    cli::cli_text("")
    cli::cli_text(cli::col_green(cli::symbol$pointer), " ", cli::col_green("Assistant:"))

    tryCatch(
      {
        if (stream) {
          # Streaming output - let stream_text handle rendering with its built-in renderer
          # By not providing a callback, stream_text will use create_stream_renderer()
          # which provides proper markdown formatting AND streaming output
          session$send_stream(input, callback = NULL)
        } else {
          # Non-streaming output
          md_renderer <- create_markdown_stream_renderer()
          result <- session$send(input)
          if (!is.null(result$text)) {
            # Render as markdown
            md_renderer$process_chunk(result$text, FALSE)
            md_renderer$process_chunk(NULL, TRUE)
          }

          # Show tool calls if any
          if (!is.null(result$tool_calls) && length(result$tool_calls) > 0) {
            cli::cli_alert_info("Tool calls made: {.val {length(result$tool_calls)}}")
          }
        }
      },
      error = function(e) {
        cli::cli_alert_danger("Error: {conditionMessage(e)}")
      }
    )

    cli::cli_text("")
  }

  invisible(session)
}

#' @keywords internal
readline_multiline <- function() {
  # Simple single-line input for now
  # Could be extended for multi-line with special handling
  cli::cli_text(cli::col_blue(cli::symbol$pointer), " ", cli::col_blue("You:"))
  input <- readline("  ")
  input
}

#' @keywords internal
handle_command <- function(input, session, stream) {
  # Parse command and arguments
  parts <- strsplit(trimws(input), "\\s+", perl = TRUE)[[1]]
  cmd <- tolower(parts[1])
  args <- if (length(parts) > 1) parts[-1] else character(0)

  result <- list(exit = FALSE, session = session, stream = stream)

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
        "{.code /model <id>} - Switch model (e.g., openai:gpt-4o)",
        "{.code /history} - Show conversation history",
        "{.code /stats} - Show token usage statistics",
        "{.code /clear} - Clear conversation history",
        "{.code /stream [on|off]} - Toggle streaming mode",
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
        cli::cli_text("Current model: {.val {session$get_model_id() %||% '(not set)'}}")
      } else {
        model_id <- args[1]
        tryCatch(
          {
            session$switch_model(model_id)
            cli::cli_alert_success("Switched to model: {.val {model_id}}")
          },
          error = function(e) {
            cli::cli_alert_danger("Failed to switch model: {conditionMessage(e)}")
          }
        )
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
          cli::cli_alert_success("Streaming enabled.")
        } else if (arg %in% c("off", "false", "0", "no")) {
          result$stream <- FALSE
          cli::cli_alert_success("Streaming disabled.")
        } else {
          cli::cli_alert_danger("Usage: {.code /stream [on|off]}")
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
          cli::cli_alert_success("Local execution mode enabled. The agent can now modify your workspace.")
        } else if (arg %in% c("off", "false", "0", "no")) {
          assign(".local_mode", FALSE, envir = session$get_envir())
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
