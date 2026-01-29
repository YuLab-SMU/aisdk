#' @title Console Chat: Interactive REPL
#' @description
#' Interactive terminal chat interface for ChatSession.
#' Provides a REPL (Read-Eval-Print Loop) for conversing with LLMs.
#' @name console
NULL

#' @title Start Console Chat
#' @description
#' Launch an interactive chat session in the R console. Supports streaming
#' output, slash commands, and colorful display using the cli package.
#'
#' @param session A ChatSession object, a LanguageModelV1 object, or a model string ID to create a new session.
#' @param system_prompt Optional system prompt (only used if creating a new session).
#' @param tools Optional list of Tool objects (only used if creating a new session).
#' @param hooks Optional HookHandler object.
#' @param stream Whether to use streaming output. Default TRUE.
#' @return The ChatSession object (invisibly) when chat ends.
#' @export
#' @examples
#' \dontrun{
#' # Start with a model ID
#' console_chat("openai:gpt-4o")
#'
#' # Start with an existing session
#' chat <- create_chat_session("anthropic:claude-3-5-sonnet-latest")
#' console_chat(chat)
#'
#' # Available commands in the chat:
#' # /quit or /exit - End the chat
#' # /save [path]   - Save session to file
#' # /load [path]   - Load session from file
#' # /model [id]    - Switch to a different model
#' # /history       - Show conversation history
#' # /stats         - Show token usage statistics
#' # /clear         - Clear conversation history
#' # /help          - Show available commands
#' }
console_chat <- function(session = NULL,
                         system_prompt = NULL,
                         tools = NULL,
                         hooks = NULL,
                         stream = TRUE) {
  # Ensure cli package is available
 if (!requireNamespace("cli", quietly = TRUE)) {
    rlang::abort("Package 'cli' is required for console_chat(). Install with: install.packages('cli')")
  }

  # Create or use existing session
  if (is.null(session)) {
    cli::cli_alert_warning("No model specified. Use /model <id> to set a model.")
    session <- ChatSession$new()
  } else if (is.character(session)) {
    session <- create_chat_session(
      model = session,
      system_prompt = system_prompt,
      tools = tools,
      hooks = hooks
    )
  } else if (inherits(session, "LanguageModelV1")) {
    session <- create_chat_session(
      model = session,
      system_prompt = system_prompt,
      tools = tools,
      hooks = hooks
    )
  } else if (!inherits(session, "ChatSession")) {
    rlang::abort("session must be a ChatSession object, LanguageModelV1 object, or model string ID")
  }

  # Welcome message
  cli::cli_h1("R AI SDK Console Chat")
  cli::cli_text("Model: {.val {session$get_model_id() %||% '(not set)'}}")
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

    # Show thinking indicator
    cat(cli::col_grey("(Thinking...)"))
    flush.console()

    first_chunk <- TRUE

    tryCatch({
      if (stream) {
        # Streaming output
        session$send_stream(input, callback = function(text, done) {
          if (first_chunk && !done && !is.null(text) && nzchar(text)) {
            # Clear thinking text (assuming length of "(Thinking...)" is ~13 chars)
            cat("\r             \r") 
            first_chunk <<- FALSE
          }
          
          if (!done) {
            cat(text)
            utils::flush.console()
          } else {
            cat("\n")
          }
        })
      } else {
        # Non-streaming output
        result <- session$send(input)
        if (!is.null(result$text)) {
          cat(result$text, "\n")
        }

        # Show tool calls if any
        if (!is.null(result$tool_calls) && length(result$tool_calls) > 0) {
          cli::cli_alert_info("Tool calls made: {.val {length(result$tool_calls)}}")
        }
      }
    }, error = function(e) {
      cli::cli_alert_danger("Error: {conditionMessage(e)}")
    })

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
        "{.code /help} - Show this help message"
      ))
    },

    "/save" = {
      path <- if (length(args) > 0) args[1] else "chat_session.rds"
      tryCatch({
        session$save(path)
        cli::cli_alert_success("Session saved to {.file {path}}")
      }, error = function(e) {
        cli::cli_alert_danger("Failed to save: {conditionMessage(e)}")
      })
    },

    "/load" = {
      if (length(args) == 0) {
        cli::cli_alert_danger("Usage: {.code /load <path>}")
      } else {
        path <- args[1]
        if (!file.exists(path)) {
          cli::cli_alert_danger("File not found: {.file {path}}")
        } else {
          tryCatch({
            # Preserve tools and hooks from current session
            tools <- session$.__enclos_env__$private$.tools
            hooks <- session$.__enclos_env__$private$.hooks

            result$session <- load_chat_session(path, tools = tools, hooks = hooks)
            cli::cli_alert_success("Session loaded from {.file {path}}")
            cli::cli_text("Model: {.val {result$session$get_model_id()}}")
            cli::cli_text("History: {.val {length(result$session$get_history())}} messages")
          }, error = function(e) {
            cli::cli_alert_danger("Failed to load: {conditionMessage(e)}")
          })
        }
      }
    },

    "/model" = {
      if (length(args) == 0) {
        cli::cli_text("Current model: {.val {session$get_model_id() %||% '(not set)'}}")
      } else {
        model_id <- args[1]
        tryCatch({
          session$switch_model(model_id)
          cli::cli_alert_success("Switched to model: {.val {model_id}}")
        }, error = function(e) {
          cli::cli_alert_danger("Failed to switch model: {conditionMessage(e)}")
        })
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

    # Unknown command
    {
      cli::cli_alert_warning("Unknown command: {.code {cmd}}")
      cli::cli_text("Type {.code /help} for available commands.")
    }
  )

  result
}

# Null-coalescing operator
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
