#' @title CLI Utils: Markdown and Tool Rendering
#' @description
#' Utilities for rendering Markdown text and tool execution status in the console.
#' @name utils_cli
#' @keywords internal
NULL

#' Check if thinking content should be shown
#' @keywords internal
should_show_thinking <- function() {
  opt <- getOption("aisdk.show_thinking", TRUE)
  if (is.logical(opt)) return(opt)
  if (is.character(opt)) return(tolower(opt) %in% c("true", "yes", "1", "on"))
  TRUE
}

#' @keywords internal
get_tool_log_mode <- function() {
  mode <- getOption("aisdk.tool_log_mode", "detailed")
  if (!is.character(mode) || length(mode) == 0 || !nzchar(mode[[1]])) {
    return("detailed")
  }

  mode <- tolower(mode[[1]])
  if (!mode %in% c("compact", "detailed")) {
    return("detailed")
  }

  mode
}

#' @keywords internal
tool_log_is_compact <- function() {
  identical(get_tool_log_mode(), "compact")
}

#' @keywords internal
get_console_app_state <- function() {
  state <- getOption("aisdk.console_app_state", NULL)
  if (is.environment(state)) {
    return(state)
  }
  NULL
}

#' @keywords internal
tool_spinner_frame <- local({
  frames <- c("\u280b", "\u2819", "\u2839", "\u2838", "\u283c", "\u2834", "\u2826", "\u2827", "\u2807", "\u280f")
  state <- new.env(parent = emptyenv())
  state$idx <- 0L

  function() {
    state$idx <- state$idx + 1L
    frames[((state$idx - 1L) %% length(frames)) + 1L]
  }
})

#' @keywords internal
compact_text_preview <- function(text, width = 60) {
  if (is.null(text) || !length(text)) {
    return("")
  }

  text <- paste(text, collapse = " ")
  text <- gsub("[\r\n\t]+", " ", text)
  text <- gsub("\\s+", " ", trimws(text))

  if (!nzchar(text)) {
    return("")
  }

  if (nchar(text) > width) {
    paste0(substr(text, 1, width - 3L), "...")
  } else {
    text
  }
}

#' @keywords internal
compact_path_preview <- function(path) {
  path <- compact_text_preview(path, width = 50)
  if (!nzchar(path)) {
    return("file")
  }

  parts <- strsplit(path, "/|\\\\", perl = TRUE)[[1]]
  leaf <- tail(parts[nzchar(parts)], 1)

  if (length(leaf) == 1 && nzchar(leaf) && nchar(leaf) <= 30) {
    return(leaf)
  }

  path
}

#' @keywords internal
compact_tool_start_label <- function(name, arguments) {
  args <- if (is.null(arguments)) list() else arguments

  switch(name,
    "execute_r_code" = {
      snippet <- compact_text_preview(args$code, width = 52)
      if (nzchar(snippet)) {
        paste0("Running R code: ", snippet)
      } else {
        "Running R code"
      }
    },
    "execute_r_code_local" = {
      snippet <- compact_text_preview(args$code, width = 48)
      if (nzchar(snippet)) {
        paste0("Running local R code: ", snippet)
      } else {
        "Running local R code"
      }
    },
    "read_file" = paste0("Reading ", compact_path_preview(args$path)),
    "write_file" = paste0("Writing ", compact_path_preview(args$path)),
    "bash" = {
      snippet <- compact_text_preview(args$command, width = 52)
      if (nzchar(snippet)) {
        paste0("Running shell command: ", snippet)
      } else {
        "Running shell command"
      }
    },
    "bash_execute" = {
      snippet <- compact_text_preview(args$command, width = 52)
      if (nzchar(snippet)) {
        paste0("Running shell command: ", snippet)
      } else {
        "Running shell command"
      }
    },
    "ask_user" = "Waiting for your input",
    paste0("Running ", name)
  )
}

#' @keywords internal
tool_result_failed <- function(result, success = TRUE) {
  if (!isTRUE(success)) {
    return(TRUE)
  }

  if (is.null(result)) {
    return(FALSE)
  }

  text <- if (is.character(result)) {
    paste(result, collapse = " ")
  } else {
    tryCatch(safe_to_json(result, auto_unbox = TRUE), error = function(e) "")
  }

  text <- trimws(text)
  grepl("^(Error|Error executing tool:|Tool execution denied|Sandbox violation:)", text, ignore.case = TRUE)
}

#' @keywords internal
compact_tool_result_label <- function(name, result, success = TRUE) {
  failed <- tool_result_failed(result, success)

  base <- switch(name,
    "execute_r_code" = "R code",
    "execute_r_code_local" = "Local R code",
    "read_file" = "File read",
    "write_file" = "File write",
    "bash" = "Shell command",
    "bash_execute" = "Shell command",
    "ask_user" = "Prompt",
    name
  )

  if (failed) {
    paste0(base, " failed")
  } else {
    paste0(base, " completed")
  }
}

#' @keywords internal
create_markdown_stream_renderer <- function() {
  state <- new.env(parent = emptyenv())
  state$buffer <- ""
  state$in_code_block <- FALSE
  state$in_thinking_block <- FALSE
  state$thinking_line_count <- 0
  state$thinking_lines_printed <- 0  # Track how many lines we've actually printed
  has_cli <- requireNamespace("cli", quietly = TRUE)
  show_thinking <- should_show_thinking()

  # For compact mode (show_thinking = FALSE): track current thinking text for typewriter effect
  state$current_thinking_text <- ""
  typing_delay <- 8  # milliseconds per character (adjust for desired speed)
  
  # ANSI escape codes for line manipulation
  ansi_up <- "\033[1A"     # Move cursor up one line
  ansi_clear <- "\033[2K"  # Clear entire line
  
  # Detect if ANSI escape codes are supported
  supports_ansi <- (Sys.info()["sysname"] != "Windows") ||
                   grepl("^(ansi|cygwin|mintty)", Sys.getenv("TERM"), ignore.case = TRUE) ||
                   grepl("windows terminal", Sys.getenv("WT_SESSION"), ignore.case = TRUE) ||
                   isTRUE(getOption("cli.unicode", FALSE))
  
  # Helper to apply inline formatting (bold)
  format_inline <- function(text) {
    parts <- strsplit(text, "\\*\\*")[[1]]
    if (length(parts) < 2) return(text)
    out <- character(0)
    for (i in seq_along(parts)) {
      if (i %% 2 == 0) {
        out <- c(out, cli::style_bold(parts[i]))
      } else {
        out <- c(out, parts[i])
      }
    }
    paste(out, collapse = "")
  }
  
  # Helper to clear N lines from terminal (move up and clear)
  clear_lines <- function(n) {
    if (n <= 0) return()
    
    if (supports_ansi) {
      # Use ANSI escape codes
      for (i in seq_len(n)) {
        cat(ansi_up, ansi_clear, sep = "")
      }
      utils::flush.console()
    } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      # RStudio: can only clear entire console, not specific lines
      # We'll skip clearing to avoid wiping everything
      invisible(NULL)
    } else {
      # Other terminals without ANSI: cannot clear
      invisible(NULL)
    }
  }

  render_line_cli <- function(line) {
    # Check for thinking block tags
    if (grepl("<think>", line, fixed = TRUE)) {
      state$in_thinking_block <- TRUE
      state$thinking_line_count <- 0
      state$thinking_lines_printed <- 0

      if (show_thinking) {
        # Full mode: Print thinking header
        cat(cli::col_grey(paste0(cli::symbol$line, cli::symbol$line, " ", cli::symbol$pointer, " Thinking...")), "\n")
        cat(cli::col_grey(cli::symbol$line), "\n")
        state$thinking_lines_printed <- 2  # Header + separator line
        utils::flush.console()
      } else {
        # Compact mode: Initialize typewriter mode
        state$current_thinking_text <- ""
        cat(cli::col_grey(paste0(cli::symbol$ellipsis, " Thinking (", state$thinking_line_count, " lines)...")))
        utils::flush.console()
      }

      line <- sub("<think>", "", line, fixed = TRUE)
      if (!nzchar(trimws(line))) return()
    }

    if (grepl("</think>", line, fixed = TRUE)) {
      parts <- strsplit(line, "</think>", fixed = TRUE)[[1]]
      if (length(parts) > 0 && nchar(parts[1]) > 0) {
        state$thinking_line_count <- state$thinking_line_count + 1
        if (show_thinking) {
          cat(cli::col_grey(paste0(cli::symbol$line, "  ", parts[1])), "\n")
          state$thinking_lines_printed <- state$thinking_lines_printed + 1
        }
      }
      state$in_thinking_block <- FALSE

      if (show_thinking) {
        # Print footer
        cat(cli::col_grey(cli::symbol$line), "\n")
        cat(cli::col_grey(paste0(cli::symbol$line, cli::symbol$line, " ", cli::symbol$tick, " Done thinking (", state$thinking_line_count, " lines)")), "\n\n")
        state$thinking_lines_printed <- state$thinking_lines_printed + 3  # separator + footer + blank
        utils::flush.console()

        # Wait 0.3 seconds only if ANSI is supported
        if (supports_ansi) {
          Sys.sleep(0.3)
          # Clear all thinking output
          clear_lines(state$thinking_lines_printed)
          # Print compact completion message with hint
          cat(cli::col_grey(paste0(cli::symbol$tick, " Thinking complete (", state$thinking_line_count, " lines)")), "\n")
          cat(cli::col_grey(paste0("  (", cli::symbol$info, " Hide with options(aisdk.show_thinking = FALSE))")), "\n\n")
        } else {
          # If ANSI is not supported, thinking content stays visible
          # Add hint on the next line
          cat(cli::col_grey(paste0("  (", cli::symbol$info, " Hide with options(aisdk.show_thinking = FALSE))")), "\n\n")
        }
      } else {
        # Compact mode: Clear the single-line indicator completely
        cat("\r", paste(rep(" ", 200), collapse = ""), "\r", sep = "")
        # Print compact completion message
        cat(cli::col_grey(paste0(cli::symbol$tick, " Thinking complete (", state$thinking_line_count, " lines)")), "\n\n")
      }

      utils::flush.console()

      state$thinking_line_count <- 0
      state$thinking_lines_printed <- 0
      state$current_thinking_text <- ""
      return()
    }

    if (state$in_thinking_block) {
      state$thinking_line_count <- state$thinking_line_count + 1
      if (show_thinking) {
        # Full mode: print thinking content
        cat(cli::col_grey(paste0(cli::symbol$line, "  ", line)), "\n")
        state$thinking_lines_printed <- state$thinking_lines_printed + 1
        utils::flush.console()
      } else {
        # Compact mode: Typewriter effect - show each line with typing animation
        # Reset for new line
        state$current_thinking_text <- ""

        # Typewriter effect: print each character with delay
        for (i in seq_len(nchar(line))) {
          char <- substr(line, i, i)
          state$current_thinking_text <- paste0(state$current_thinking_text, char)

          # Truncate if too long for display
          display_text <- state$current_thinking_text
          if (nchar(display_text) > 70) {
            display_text <- paste0(substr(display_text, 1, 67), "...")
          }

          # Update the line (use more spaces to clear)
          cat("\r", paste(rep(" ", 150), collapse = ""), "\r", sep = "")
          cat(cli::col_grey(paste0(cli::symbol$ellipsis, " Thinking (", state$thinking_line_count, " lines): ", display_text)))
          utils::flush.console()
          
          # Small delay for typewriter effect
          Sys.sleep(typing_delay / 1000)
        }
      }
      return()
    }

    # Check for code blocks
    if (grepl("^```", line)) {
      state$in_code_block <- !state$in_code_block
      cat(cli::col_grey(line), "\n", sep = "")
      return()
    }

    if (state$in_code_block) {
      cat(cli::col_cyan(line), "\n", sep = "")
      return()
    }

    # Headers
    if (grepl("^#+ ", line)) {
      level <- nchar(strsplit(line, " ")[[1]][1])
      content <- substring(line, level + 2)
      content <- format_inline(content)

      if (level == 1) cli::cli_h1(content)
      else if (level == 2) cli::cli_h2(content)
      else cli::cli_h3(content)
      return()
    }

    # Lists
    if (grepl("^\\* ", line) || grepl("^- ", line)) {
      content <- substring(line, 2)
      content_fmt <- format_inline(content)
      cat(cli::col_blue(substring(line, 1, 1)), content_fmt, "\n", sep = "")
      return()
    }

    # Normal line
    cat(format_inline(line), "\n", sep = "")
  }

  render_lines <- function(lines) {
    for (line in lines) {
      if (has_cli) {
        render_line_cli(line)
      } else {
        cat(line, "\n", sep = "")
      }
    }
  }

  process_chunk <- function(text, done = FALSE) {
    # When done=TRUE, flush any remaining buffer content
    if (done) {
      if (nzchar(state$buffer)) {
        lines <- strsplit(state$buffer, "\n", fixed = TRUE)[[1]]
        render_lines(lines)
        state$buffer <- ""
      }
      return()
    }

    if (is.null(text) || !nzchar(text)) return()

    state$buffer <- paste0(state$buffer, text)

    # Split buffer into lines
    lines <- strsplit(state$buffer, "\n", fixed = TRUE)[[1]]

    ends_with_newline <- grepl("\n$", state$buffer)

    # Determine complete lines to render
    lines_to_render <- character(0)

    if (ends_with_newline) {
      lines_to_render <- lines
      state$buffer <- ""
    } else {
      if (length(lines) > 1) {
        lines_to_render <- lines[1:(length(lines) - 1)]
        state$buffer <- lines[length(lines)]
      }
    }

    if (length(lines_to_render) > 0) {
      render_lines(lines_to_render)
    }
  }

  reset <- function() {
    state$buffer <- ""
    state$in_code_block <- FALSE
    state$in_thinking_block <- FALSE
    state$thinking_line_count <- 0
    state$thinking_lines_printed <- 0
    state$current_thinking_text <- ""
  }

  list(process_chunk = process_chunk, reset = reset)
}


#' @keywords internal
cli_tool_start <- function(name, arguments) {
  app_state <- get_console_app_state()
  if (!is.null(app_state)) {
    console_app_record_tool_start(app_state, name, arguments)
  }

  if (tool_log_is_compact()) {
    if (!requireNamespace("cli", quietly = TRUE)) {
      message(sprintf("%s %s", tool_spinner_frame(), compact_tool_start_label(name, arguments)))
      return()
    }

    cli::cli_text("{cli::col_grey(tool_spinner_frame())} {cli::col_grey(compact_tool_start_label(name, arguments))}")
    return()
  }

  if (!requireNamespace("cli", quietly = TRUE)) {
    args_str <- tryCatch(safe_to_json(arguments, auto_unbox = TRUE), error = function(e) "...")
    message(sprintf("\u2139 Calling tool %s(%s)", name, args_str))
    return()
  }

  args_preview <- tryCatch({
    json_str <- safe_to_json(arguments, auto_unbox = TRUE)
    json_str
  }, error = function(e) "...")

  if (nchar(args_preview) > 150) {
    args_preview <- paste0(substr(args_preview, 1, 147), "...")
  }

  icon <- switch(name,
    "execute_skill_script" = cli::symbol$play,
    "write_file" = cli::symbol$save,
    "read_file" = cli::symbol$file,
    "bash_execute" = cli::symbol$terminal,
    cli::symbol$info
  )

  cli::cli_div(theme = list(span.toolname = list(color = "magenta", font_weight = "bold")))
  cli::cli_text("{cli::col_blue(icon)} Calling tool {.toolname {name}} {cli::col_grey(args_preview)}")
  cli::cli_end()
}

#' @keywords internal
cli_tool_result <- function(name, result, success = TRUE, raw_result = result) {
  app_state <- get_console_app_state()
  if (!is.null(app_state)) {
    console_app_record_tool_result(app_state, name, result, success = success, raw_result = raw_result)
  }

  failed <- tool_result_failed(result, success)

  if (tool_log_is_compact()) {
    label <- compact_tool_result_label(name, result, success = !failed)

    if (!requireNamespace("cli", quietly = TRUE)) {
      status <- if (failed) "\u2716" else "\u2714"
      message(sprintf("%s %s", status, label))
      return()
    }

    if (failed) {
      cli::cli_text("{cli::col_red(cli::symbol$cross)} {cli::col_red(label)}")
    } else {
      cli::cli_text("{cli::col_grey(cli::symbol$tick)} {cli::col_grey(label)}")
    }
    return()
  }

  if (!requireNamespace("cli", quietly = TRUE)) {
    res_str <- if (is.character(result)) result else safe_to_json(result, auto_unbox = TRUE)
    status <- if (failed) "\u2716" else "\u2714"
    message(sprintf("%s Tool %s returned: %s", status, name, res_str))
    return()
  }

  res_preview <- if (is.null(result)) {
    "NULL"
  } else {
    if (is.character(result)) result else safe_to_json(result, auto_unbox = TRUE)
  }
  if (length(res_preview) == 0) res_preview <- "NULL"

  if (nchar(res_preview) > 200) {
    res_preview <- paste0(substr(res_preview, 1, 197), "...")
  }

  if (!failed) {
    cli::cli_div(theme = list(span.emph = list(color = "blue")))
    cli::cli_text("{cli::col_green(cli::symbol$tick)} Tool {.emph {name}} returned: {.val {res_preview}}")
  } else {
    cli::cli_div(theme = list(span.emph = list(color = "red", font_weight = "bold")))
    cli::cli_text("{cli::col_red(cli::symbol$cross)} Tool {.emph {name}} failed: {.val {res_preview}}")
  }
  cli::cli_end()
}
