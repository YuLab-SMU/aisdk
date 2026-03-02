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
create_markdown_stream_renderer <- function() {
  buffer <- ""
  in_code_block <- FALSE
  in_thinking_block <- FALSE
  thinking_line_count <- 0
  thinking_lines_printed <- 0  # Track how many lines we've actually printed
  has_cli <- requireNamespace("cli", quietly = TRUE)
  show_thinking <- should_show_thinking()
  
  # For compact mode (show_thinking = FALSE): track current thinking text for typewriter effect
  current_thinking_text <- ""
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
      in_thinking_block <<- TRUE
      thinking_line_count <<- 0
      thinking_lines_printed <<- 0

      if (show_thinking) {
        # Full mode: Print thinking header
        cat(cli::col_grey(paste0(cli::symbol$line, cli::symbol$line, " ", cli::symbol$pointer, " Thinking...")), "\n")
        cat(cli::col_grey(cli::symbol$line), "\n")
        thinking_lines_printed <<- 2  # Header + separator line
        utils::flush.console()
      } else {
        # Compact mode: Initialize typewriter mode
        current_thinking_text <<- ""
        cat(cli::col_grey(paste0(cli::symbol$ellipsis, " Thinking (", thinking_line_count, " lines)...")))
        utils::flush.console()
      }

      line <- sub("<think>", "", line, fixed = TRUE)
      if (!nzchar(trimws(line))) return()
    }

    if (grepl("</think>", line, fixed = TRUE)) {
      parts <- strsplit(line, "</think>", fixed = TRUE)[[1]]
      if (length(parts) > 0 && nchar(parts[1]) > 0) {
        thinking_line_count <<- thinking_line_count + 1
        if (show_thinking) {
          cat(cli::col_grey(paste0(cli::symbol$line, "  ", parts[1])), "\n")
          thinking_lines_printed <<- thinking_lines_printed + 1
        }
      }
      in_thinking_block <<- FALSE

      if (show_thinking) {
        # Print footer
        cat(cli::col_grey(cli::symbol$line), "\n")
        cat(cli::col_grey(paste0(cli::symbol$line, cli::symbol$line, " ", cli::symbol$tick, " Done thinking (", thinking_line_count, " lines)")), "\n\n")
        thinking_lines_printed <<- thinking_lines_printed + 3  # separator + footer + blank
        utils::flush.console()
        
        # Wait 0.3 seconds only if ANSI is supported
        if (supports_ansi) {
          Sys.sleep(0.3)
          # Clear all thinking output
          clear_lines(thinking_lines_printed)
          # Print compact completion message with hint
          cat(cli::col_grey(paste0(cli::symbol$tick, " Thinking complete (", thinking_line_count, " lines)")), "\n")
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
        cat(cli::col_grey(paste0(cli::symbol$tick, " Thinking complete (", thinking_line_count, " lines)")), "\n\n")
      }
      
      utils::flush.console()

      thinking_line_count <<- 0
      thinking_lines_printed <<- 0
      current_thinking_text <<- ""
      return()
    }

    if (in_thinking_block) {
      thinking_line_count <<- thinking_line_count + 1
      if (show_thinking) {
        # Full mode: print thinking content
        cat(cli::col_grey(paste0(cli::symbol$line, "  ", line)), "\n")
        thinking_lines_printed <<- thinking_lines_printed + 1
        utils::flush.console()
      } else {
        # Compact mode: Typewriter effect - show each line with typing animation
        # Reset for new line
        current_thinking_text <<- ""
        
        # Typewriter effect: print each character with delay
        for (i in seq_len(nchar(line))) {
          char <- substr(line, i, i)
          current_thinking_text <<- paste0(current_thinking_text, char)
          
          # Truncate if too long for display
          display_text <- current_thinking_text
          if (nchar(display_text) > 70) {
            display_text <- paste0(substr(display_text, 1, 67), "...")
          }
          
          # Update the line (use more spaces to clear)
          cat("\r", paste(rep(" ", 150), collapse = ""), "\r", sep = "")
          cat(cli::col_grey(paste0(cli::symbol$ellipsis, " Thinking (", thinking_line_count, " lines): ", display_text)))
          utils::flush.console()
          
          # Small delay for typewriter effect
          Sys.sleep(typing_delay / 1000)
        }
      }
      return()
    }

    # Check for code blocks
    if (grepl("^```", line)) {
      in_code_block <<- !in_code_block
      cat(cli::col_grey(line), "\n", sep = "")
      return()
    }

    if (in_code_block) {
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
      if (nzchar(buffer)) {
        lines <- strsplit(buffer, "\n", fixed = TRUE)[[1]]
        render_lines(lines)
        buffer <<- ""
      }
      return()
    }

    if (is.null(text) || !nzchar(text)) return()

    buffer <<- paste0(buffer, text)

    # Split buffer into lines
    lines <- strsplit(buffer, "\n", fixed = TRUE)[[1]]

    ends_with_newline <- grepl("\n$", buffer)

    # Determine complete lines to render
    lines_to_render <- character(0)

    if (ends_with_newline) {
      lines_to_render <- lines
      buffer <<- ""
    } else {
      if (length(lines) > 1) {
        lines_to_render <- lines[1:(length(lines) - 1)]
        buffer <<- lines[length(lines)]
      }
    }

    if (length(lines_to_render) > 0) {
      render_lines(lines_to_render)
    }
  }

  reset <- function() {
    buffer <<- ""
    in_code_block <<- FALSE
    in_thinking_block <<- FALSE
    thinking_line_count <<- 0
    thinking_lines_printed <<- 0
    current_thinking_text <<- ""
  }

  list(process_chunk = process_chunk, reset = reset)
}


#' @keywords internal
cli_tool_start <- function(name, arguments) {
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
cli_tool_result <- function(name, result, success = TRUE) {
  if (!requireNamespace("cli", quietly = TRUE)) {
    res_str <- if (is.character(result)) result else safe_to_json(result, auto_unbox = TRUE)
    status <- if (success) "\u2714" else "\u2716"
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

  if (success) {
    cli::cli_div(theme = list(span.emph = list(color = "blue")))
    cli::cli_text("{cli::col_green(cli::symbol$tick)} Tool {.emph {name}} returned: {.val {res_preview}}")
  } else {
    cli::cli_div(theme = list(span.emph = list(color = "red", font_weight = "bold")))
    cli::cli_text("{cli::col_red(cli::symbol$cross)} Tool {.emph {name}} failed: {.val {res_preview}}")
  }
  cli::cli_end()
}
