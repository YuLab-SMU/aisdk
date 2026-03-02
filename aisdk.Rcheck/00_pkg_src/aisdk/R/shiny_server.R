#' @title AI Chat Server
#' @description
#' Shiny module server for AI-powered chat, featuring non-blocking streaming
#' via background processes and tool execution bridge.
#'
#' @param id The namespace ID for the module.
#' @param model Either a LanguageModelV1 object, or a string ID like "openai:gpt-4o".
#' @param tools Optional list of Tool objects for function calling.
#' @param context Optional reactive expression that returns context data to inject
#'   into the system prompt. This is read with `isolate()` to avoid reactive loops.
#' @param system Optional system prompt.
#' @param system Optional system prompt.
#' @param debug Reactive expression or logical. If TRUE, shows raw debug output in UI.
#' @param on_message_complete Optional callback function called when a message is complete.
#'   Takes one argument: the complete assistant message text.
#' @return A reactive value containing the chat history.
#' @export
aiChatServer <- function(id,
                         model,
                         tools = NULL,
                         context = NULL,
                         system = NULL,
                         debug = FALSE,
                         on_message_complete = NULL) {
  # Check for suggested packages
  rlang::check_installed(c("shiny", "callr"))

  shiny::moduleServer(id, function(input, output, session) {
    # --- State ---
    chat_history <- shiny::reactiveVal(list())
    is_generating <- shiny::reactiveVal(FALSE)
    current_response <- shiny::reactiveVal("")
    
    # Chat manager for handling async generation
    manager <- ChatManager$new()

    # --- Send Message Handler ---
    shiny::observeEvent(input$send, {
      shiny::req(nzchar(trimws(input$user_input)))
      shiny::req(!is_generating())

      user_text <- trimws(input$user_input)

      # Clear input
      shiny::updateTextAreaInput(session, "user_input", value = "")

      # Add user message to history
      history <- chat_history()
      history <- c(history, list(list(role = "user", content = user_text)))
      chat_history(history)

      # Render user message in UI
      render_message(session, id, "user", user_text)

      # Build context string if provided
      context_str <- NULL
      if (!is.null(context)) {
        ctx_data <- shiny::isolate(context())
        if (!is.null(ctx_data)) {
          context_str <- format_context(ctx_data)
        }
      }

      # Build full system prompt
      full_system <- system
      if (!is.null(context_str)) {
        full_system <- if (is.null(full_system)) {
          paste0("Current application state:\n", context_str)
        } else {
          paste(full_system, "\n\nCurrent application state:\n", context_str, sep = "")
        }
      }

      # Start async generation
      is_generating(TRUE)
      current_response("")
      
      # Clear debug log if starting new message
      session$sendCustomMessage(paste0("append_debug_", id), list(clear = TRUE, visible = isTRUE(tryCatch(debug(), error = function(e) debug))))

      # Render empty assistant message container
      render_message(session, id, "assistant", "", streaming = TRUE)

      # Start background process
      manager$start_generation(
        model = model,
        messages = history,
        system = full_system,
        tools = tools
      )
    })

    # --- Polling Observer ---
    shiny::observe({
      shiny::req(is_generating())
      shiny::invalidateLater(100)

      poll_result <- manager$poll()

      if (!is.null(poll_result)) {
        # Handle new text chunks
        if (!is.null(poll_result$text) && nzchar(poll_result$text)) {
          new_text <- paste0(current_response(), poll_result$text)
          current_response(new_text)
          update_streaming_message(session, id, poll_result$text)
          
          # Debug output
          is_debug <- isTRUE(tryCatch(debug(), error = function(e) debug))
          if (is_debug) {
             session$sendCustomMessage(paste0("append_debug_", id), list(text = poll_result$text, visible = TRUE))
          }
        }

        # Handle tool calls
        if (isTRUE(poll_result$waiting_tool)) {
          tool_call <- poll_result$tool_call
          if (!is.null(tool_call) && !is.null(tools)) {
            # Render "Calling Tool" UI
            # We use append_html to add a block
            tool_args_str <- tryCatch(jsonlite::toJSON(tool_call$arguments, auto_unbox = TRUE), error = function(e) "{}")
            
            tool_ui_id <- paste0("tool_", tool_call$id)
            tool_html <- sprintf(
              '<details class="tool-execution" id="%s" open>
                 <summary>
                   <span>Running tool: %s</span>
                 </summary>
                 <div class="tool-execution-body">
                   <div style="margin-bottom: 0.5rem; color: #64748b; font-size: 0.85rem;">Input arguments:</div>
                   <pre>%s</pre>
                   <div id="%s_result"></div>
                 </div>
               </details>',
               tool_ui_id, tool_call$name, tool_args_str, tool_ui_id
            )
            
            # Send to UI (append to chat)
            session$sendCustomMessage("append_html", list(
              id = paste0("msg_", sprintf("%.0f", as.numeric(Sys.time())*1000)), # Trick: append after current? No, append to container
              # Actually append_html appends to container ID passed.
              # But we want to append it to the message stream.
              # The 'id' in append_html (shiny_ui line 199) refers to the CONTAINER if content_id is missing.
              id = session$ns("chat_messages"),
              html = tool_html
            ))
            
            # Execute tool in main thread
            tool_result <- execute_tool_in_context(tool_call, tools)
            
            # Render result
            # We can use update_element if we had an ID, or just append inside the result div
            res_str <- tool_result$result
            # Truncate if too long?
            if (nchar(res_str) > 500) res_str <- paste0(substr(res_str, 1, 500), "... (truncated)")
            
            res_html <- sprintf(
              '<div style="margin-top: 0.8rem; margin-bottom: 0.5rem; color: #64748b; font-size: 0.85rem;">Result:</div>
               <pre>%s</pre>',
               res_str
            )
            
            # Update the result container
            session$sendCustomMessage("update_element", list(
               id = paste0(tool_ui_id, "_result"),
               html = res_html
            ))
            
            # Collapse the details after done? Optional.
            # Maybe via JS: document.getElementById(id).removeAttribute("open");
            
            manager$resolve_tool(tool_result)
          }
        }

        # Handle completion
        if (isTRUE(poll_result$done)) {
          is_generating(FALSE)
          finalize_streaming_message(session, id, current_response())

          # Update history
          final_text <- current_response()
          history <- chat_history()
          history <- c(history, list(list(role = "assistant", content = final_text)))
          chat_history(history)

          # Callback
          if (!is.null(on_message_complete)) {
            on_message_complete(final_text)
          }

          # Cleanup
          manager$cleanup()
        }

        # Handle errors
        if (!is.null(poll_result$error)) {
          is_generating(FALSE)
          err_text <- if (nzchar(current_response())) current_response() else paste0("Error: ", poll_result$error)
          if (!nzchar(current_response())) {
            update_streaming_message(session, id, err_text)
          }
          finalize_streaming_message(session, id, err_text)
          manager$cleanup()
        }
      }
    })

    # Scroll chat on any history update
    shiny::observe({
      chat_history()
      session$sendCustomMessage(paste0("scroll_chat_", id), list())
    })

    # Return chat history
    chat_history
  })
}


# --- ChatManager R6 Class ---

#' @title Chat Manager
#' @description
#' Manages asynchronous chat generation using background processes.
#' Handles IPC for streaming tokens and tool call bridging.
#' @keywords internal
ChatManager <- R6::R6Class(
  "ChatManager",
  public = list(
    #' @field process The background callr process
    process = NULL,

    #' @field ipc_dir Temp directory for inter-process communication
    ipc_dir = NULL,

    #' @field last_read_pos Last position read from output file
    last_read_pos = 0,

    #' @description Initialize a new ChatManager
    initialize = function() {
      self$ipc_dir <- NULL
      self$process <- NULL
    },

    #' @description Start async text generation
    #' @param model The model (will be serialized for bg process)
    #' @param messages The message history
    #' @param system The system prompt
    #' @param tools The tools list
    start_generation = function(model, messages, system = NULL, tools = NULL) {
      # Create IPC directory
      self$ipc_dir <- tempfile("chat_ipc_")
      dir.create(self$ipc_dir, recursive = TRUE)

      # IPC files
      output_file <- file.path(self$ipc_dir, "output.txt")
      status_file <- file.path(self$ipc_dir, "status.txt")
      tool_call_file <- file.path(self$ipc_dir, "tool_call.rds")
      tool_result_file <- file.path(self$ipc_dir, "tool_result.rds")

      # Write initial status
      writeLines("RUNNING", status_file)

      # Reset read position
      self$last_read_pos <- 0

      model_config <- if (inherits(model, "LanguageModelV1")) {
        provider_type <- NULL
        if (inherits(model, "OpenAILanguageModel")) provider_type <- "openai"
        if (inherits(model, "AnthropicLanguageModel")) provider_type <- "anthropic"
        provider_config <- NULL
        if (!is.null(provider_type) && is.function(model$get_config)) {
          provider_config <- model$get_config()
        }
        list(
          provider = model$provider,
          model_id = model$model_id,
          provider_type = provider_type,
          provider_config = provider_config
        )
      } else if (is.character(model)) {
        parts <- strsplit(model, ":", fixed = TRUE)[[1]]
        list(
          provider = parts[1],
          model_id = parts[2],
          provider_type = NULL,
          provider_config = NULL
        )
      } else {
        stop("Invalid model specification")
      }

      # Serialize tool definitions (not the R functions)
      tool_defs <- if (!is.null(tools)) {
        lapply(tools, function(t) {
          list(
            name = t$name,
            description = t$description,
            parameters = t$parameters
          )
        })
      } else {
        NULL
      }

      # Start background process
      self$process <- callr::r_bg(
        func = function(model_config, messages, system, tool_defs,
                        output_file, status_file, tool_call_file, tool_result_file) {
          # Helper to append output
          write_output <- function(text) {
            cat(text, file = output_file, append = TRUE)
          }

          # Helper to get status
          get_status <- function() {
            if (file.exists(status_file)) readLines(status_file, n = 1) else "UNKNOWN"
          }

          # Helper to set status
          set_status <- function(status) {
            writeLines(status, status_file)
          }

          tryCatch({
            registry <- aisdk::get_default_registry()
            if (!is.null(model_config$provider_type) && !is.null(model_config$provider_config)) {
              if (model_config$provider_type == "openai") {
                provider_name <- model_config$provider_config$provider_name
                if (is.null(provider_name) || !nzchar(provider_name)) {
                  provider_name <- model_config$provider
                }
                provider <- aisdk::create_openai(
                  api_key = model_config$provider_config$api_key,
                  base_url = model_config$provider_config$base_url,
                  organization = model_config$provider_config$organization,
                  project = model_config$provider_config$project,
                  headers = model_config$provider_config$headers,
                  name = provider_name
                )
                registry$register(model_config$provider, provider)
              } else if (model_config$provider_type == "anthropic") {
                provider_name <- model_config$provider_config$provider_name
                if (is.null(provider_name) || !nzchar(provider_name)) {
                  provider_name <- model_config$provider
                }
                provider <- aisdk::create_anthropic(
                  api_key = model_config$provider_config$api_key,
                  base_url = model_config$provider_config$base_url,
                  api_version = model_config$provider_config$api_version,
                  headers = model_config$provider_config$headers,
                  name = provider_name
                )
                if (isTRUE(model_config$provider_config$enable_caching)) {
                  provider$enable_caching(TRUE)
                }
                registry$register(model_config$provider, provider)
              }
            }
            model <- registry$language_model(
              paste0(model_config$provider, ":", model_config$model_id)
            )

            # Recreate tools with placeholder execute functions
            # (actual execution happens in main thread)
            tools <- if (!is.null(tool_defs)) {
              lapply(tool_defs, function(td) {
                aisdk::tool(
                  name = td$name,
                  description = td$description,
                  parameters = td$parameters,
                  execute = function(...) {
                    # This should never be called in bg process
                    stop("Tool should be executed in main thread")
                  }
                )
              })
            } else {
              NULL
            }

            # Build messages with system
            # full_messages <- messages # (Should be managed in loop)
            current_messages <- messages
            
            step_count <- 0
            max_steps <- 10
            
            while (step_count < max_steps) {
              step_count <- step_count + 1
              
              full_messages <- current_messages
              if (!is.null(system)) {
                full_messages <- c(
                  list(list(role = "system", content = system)),
                  full_messages
                )
              }
  
              # Stream callback
              callback <- function(text, done) {
                write_output(text)
              }
              
              # Generate with streaming
              result <- model$do_stream(
                list(
                  messages = full_messages,
                  temperature = 0.7,
                  tools = tools
                ),
                callback
              )
              
              # Check for tool calls
              if (!is.null(result$tool_calls) && length(result$tool_calls) > 0) {
                # Handle first tool call
                tool_call <- result$tool_calls[[1]]
                saveRDS(tool_call, tool_call_file)
                set_status("WAITING_TOOL")
                
                # Poll for result
                timeout <- 300 # 5 min
                start_time <- Sys.time()
                tool_result_data <- NULL
                
                while (difftime(Sys.time(), start_time, units = "secs") < timeout) {
                  if (file.exists(tool_result_file)) {
                    tool_result_data <- readRDS(tool_result_file)
                    file.remove(tool_result_file)
                    break 
                  }
                  Sys.sleep(0.1)
                }
                
                if (is.null(tool_result_data)) {
                  set_status("ERROR")
                  write_output("\nError: Tool execution timeout")
                  return(invisible(NULL))
                }
                
                # Append to history
                current_messages <- c(current_messages, list(
                  list(
                    role = "assistant",
                    content = result$content %||% "",
                    tool_calls = result$tool_calls
                  ),
                  list(
                    role = "tool",
                    tool_call_id = tool_call$id,
                    content = tool_result_data$result
                  )
                ))
                
                # Continue loop
                set_status("RUNNING")
                if(file.exists(tool_call_file)) file.remove(tool_call_file)
                
              } else {
                # Done
                set_status("DONE")
                break
              }
            }
            
            if (step_count >= max_steps) {
               write_output("\n[System: Max steps reached]")
               set_status("DONE")
            }

          }, error = function(e) {
            set_status("ERROR")
            write_output(paste0("\nError: ", conditionMessage(e)))
          })

          invisible(NULL)
        },
        args = list(
          model_config = model_config,
          messages = messages,
          system = system,
          tool_defs = tool_defs,
          output_file = output_file,
          status_file = status_file,
          tool_call_file = tool_call_file,
          tool_result_file = tool_result_file
        ),
        supervise = TRUE
      )
    },

    #' @description Poll for new output and status
    #' @return List with text, done, waiting_tool, tool_call, error
    poll = function() {
      if (is.null(self$ipc_dir)) return(NULL)

      output_file <- file.path(self$ipc_dir, "output.txt")
      status_file <- file.path(self$ipc_dir, "status.txt")
      tool_call_file <- file.path(self$ipc_dir, "tool_call.rds")

      result <- list(
        text = NULL,
        done = FALSE,
        waiting_tool = FALSE,
        tool_call = NULL,
        error = NULL
      )

      # Read new output
      if (file.exists(output_file)) {
        con <- file(output_file, "r")
        on.exit(close(con), add = TRUE)

        # Read all content
        all_text <- paste(readLines(con, warn = FALSE), collapse = "\n")
        if (nchar(all_text) > self$last_read_pos) {
          result$text <- substr(all_text, self$last_read_pos + 1, nchar(all_text))
          self$last_read_pos <- nchar(all_text)
        }
      }

      # Check status
      if (file.exists(status_file)) {
        status <- readLines(status_file, n = 1, warn = FALSE)

        if (status == "DONE") {
          result$done <- TRUE
        } else if (status == "ERROR") {
          result$error <- "Generation failed"
          result$done <- TRUE
        } else if (status == "WAITING_TOOL") {
          result$waiting_tool <- TRUE
          if (file.exists(tool_call_file)) {
            result$tool_call <- readRDS(tool_call_file)
          }
        }
      }

      # Also check if process died unexpectedly
      if (!is.null(self$process) && !self$process$is_alive()) {
        if (!result$done) {
          result$error <- "Background process terminated unexpectedly"
          result$done <- TRUE
        }
      }

      result
    },

    #' @description Resolve a tool call with result
    #' @param result The tool execution result
    resolve_tool = function(result) {
      if (is.null(self$ipc_dir)) return()

      tool_result_file <- file.path(self$ipc_dir, "tool_result.rds")
      saveRDS(result, tool_result_file)
    },

    #' @description Cleanup resources
    cleanup = function() {
      if (!is.null(self$process)) {
        tryCatch({
          if (self$process$is_alive()) {
            self$process$kill()
          }
        }, error = function(e) {})
        self$process <- NULL
      }

      if (!is.null(self$ipc_dir) && dir.exists(self$ipc_dir)) {
        unlink(self$ipc_dir, recursive = TRUE)
        self$ipc_dir <- NULL
      }

      self$last_read_pos <- 0
    }
  )
)


# --- Helper Functions ---

#' @keywords internal
render_message <- function(session, id, role, content, streaming = FALSE) {
  ns <- shiny::NS(id)

  msg_id <- if (streaming) {
    paste0(ns("streaming_msg_"), sprintf("%.0f", as.numeric(Sys.time()) * 1000))
  } else {
    paste0("msg_", sprintf("%.0f", as.numeric(Sys.time()) * 1000))
  }
  if (streaming) {
    session$userData$streaming_msg_id <- msg_id
    session$userData$streaming_thinking <- FALSE
    session$userData$streaming_thinking_started <- FALSE
    session$userData$streaming_content_started <- FALSE
    session$userData$streaming_thinking_id <- paste0(msg_id, "_thinking")
    session$userData$streaming_content_id <- paste0(msg_id, "_content")
    session$userData$streaming_thinking_text <- ""
    session$userData$streaming_content_text <- ""
    msg_html <- sprintf(
      '<div id="%s" class="message %s"><div id="%s" class="message-thinking"></div><div id="%s" class="message-content"></div></div>',
      msg_id, role, session$userData$streaming_thinking_id, session$userData$streaming_content_id
    )
  } else {
    html_content <- if (nzchar(content)) commonmark::markdown_html(content) else ""
    msg_html <- sprintf(
      '<div id="%s" class="message %s">%s</div>',
      msg_id, role, html_content
    )
  }

  # Insert into chat
  shiny::insertUI(
    selector = paste0("#", ns("chat_messages")),
    where = "beforeEnd",
    ui = shiny::HTML(msg_html),
    session = session
  )
}

#' @keywords internal
update_streaming_message <- function(session, id, delta) {
  ns <- shiny::NS(id)
  msg_id <- session$userData$streaming_msg_id
  if (is.null(msg_id) || !nzchar(msg_id)) {
    msg_id <- ns("streaming_msg")
  }
  if (is.null(delta) || !nzchar(delta)) {
    return()
  }
  thinking_id <- session$userData$streaming_thinking_id
  content_id <- session$userData$streaming_content_id
  if (is.null(thinking_id) || !nzchar(thinking_id)) thinking_id <- paste0(msg_id, "_thinking")
  if (is.null(content_id) || !nzchar(content_id)) content_id <- paste0(msg_id, "_content")

  append_segment <- function(target_id, text, key) {
    if (!nzchar(text)) return()
    started_flag <- paste0("streaming_", key, "_started")
    started <- isTRUE(session$userData[[started_flag]])
    session$sendCustomMessage("append_text", list(
      id = target_id,
      text = text,
      clear = !started
    ))
    if (!started) {
      session$userData[[started_flag]] <- TRUE
    }
    text_flag <- paste0("streaming_", key, "_text")
    prev_text <- session$userData[[text_flag]] %||% ""
    session$userData[[text_flag]] <- paste0(prev_text, text)
  }

  remaining <- delta
  in_thinking <- isTRUE(session$userData$streaming_thinking)
  repeat {
    loc <- regexpr("<think>|</think>", remaining, perl = TRUE)
    if (loc[1] == -1) {
      if (in_thinking) {
        append_segment(thinking_id, remaining, "thinking")
      } else {
        append_segment(content_id, remaining, "content")
      }
      break
    }
    if (loc[1] > 1) {
      before <- substr(remaining, 1, loc[1] - 1)
      if (in_thinking) {
        append_segment(thinking_id, before, "thinking")
      } else {
        append_segment(content_id, before, "content")
      }
    }
    tag <- substr(remaining, loc[1], loc[1] + attr(loc, "match.length") - 1)
    if (tag == "<think>") {
      in_thinking <- TRUE
    } else if (tag == "</think>") {
      in_thinking <- FALSE
    }
    next_start <- loc[1] + attr(loc, "match.length")
    remaining <- if (next_start <= nchar(remaining)) substr(remaining, next_start, nchar(remaining)) else ""
    if (!nzchar(remaining)) break
  }
  session$userData$streaming_thinking <- in_thinking

  session$sendCustomMessage(paste0("scroll_chat_", id), list())
}

#' @keywords internal
finalize_streaming_message <- function(session, id, content) {
  ns <- shiny::NS(id)
  msg_id <- session$userData$streaming_msg_id
  if (is.null(msg_id) || !nzchar(msg_id)) {
    msg_id <- ns("streaming_msg")
  }
  thinking_text <- session$userData$streaming_thinking_text %||% ""
  content_text <- session$userData$streaming_content_text %||% ""
  html_content <- build_final_html(content_text, thinking_text, content)

  session$sendCustomMessage("update_element", list(
    id = msg_id,
    html = html_content
  ))

  session$sendCustomMessage(paste0("scroll_chat_", id), list())
  session$userData$streaming_msg_id <- NULL
  session$userData$streaming_thinking <- NULL
  session$userData$streaming_thinking_started <- NULL
  session$userData$streaming_content_started <- NULL
  session$userData$streaming_thinking_id <- NULL
  session$userData$streaming_content_id <- NULL
  session$userData$streaming_thinking_text <- NULL
  session$userData$streaming_content_text <- NULL
}

build_final_html <- function(content_text, thinking_text, full_content) {
  if (!nzchar(content_text) && !nzchar(thinking_text)) {
    return(format_thinking_html(full_content))
  }
  main_html <- if (nzchar(trimws(content_text))) commonmark::markdown_html(content_text) else ""
  thinking_html <- ""
  if (nzchar(trimws(thinking_text))) {
    thinking_html <- commonmark::markdown_html(thinking_text)
    thinking_html <- paste0('<details class="message-thinking-block"><summary>\u601d\u8003</summary><div>', thinking_html, "</div></details>")
  }
  paste0(main_html, thinking_html)
}

format_thinking_html <- function(content) {
  if (!nzchar(content)) return("(No response)")
  if (!grepl("<think>", content, fixed = TRUE)) {
    return(commonmark::markdown_html(content))
  }
  matches <- gregexpr("<think>[\\s\\S]*?</think>", content, perl = TRUE)
  blocks <- regmatches(content, matches)[[1]]
  thinking_text <- ""
  if (length(blocks) > 0) {
    thinking_text <- paste(vapply(blocks, function(x) sub("^<think>|</think>$", "", x), character(1)), collapse = "\n\n")
  }
  main_text <- gsub("<think>[\\s\\S]*?</think>", "", content, perl = TRUE)
  main_html <- if (nzchar(trimws(main_text))) commonmark::markdown_html(main_text) else ""
  thinking_html <- ""
  if (nzchar(trimws(thinking_text))) {
    thinking_html <- commonmark::markdown_html(thinking_text)
    thinking_html <- paste0('<details class="message-thinking-block"><summary>\u601d\u8003</summary><div>', thinking_html, "</div></details>")
  }
  paste0(main_html, thinking_html)
}

#' @keywords internal
format_context <- function(ctx_data) {
  if (is.list(ctx_data)) {
    lines <- vapply(names(ctx_data), function(nm) {
      val <- ctx_data[[nm]]
      if (is.atomic(val) && length(val) == 1) {
        sprintf("- %s: %s", nm, as.character(val))
      } else {
        sprintf("- %s: %s", nm, jsonlite::toJSON(val, auto_unbox = TRUE))
      }
    }, character(1))
    paste(lines, collapse = "\n")
  } else {
    as.character(ctx_data)
  }
}

#' @keywords internal
execute_tool_in_context <- function(tool_call, tools) {
  tool_name <- tool_call$name
  tool_args <- tool_call$arguments

  # Find the tool
  tool <- NULL
  for (t in tools) {
    if (t$name == tool_name) {
      tool <- t
      break
    }
  }

  if (is.null(tool)) {
    return(list(
      id = tool_call$id,
      name = tool_name,
      result = paste0("Error: Tool '", tool_name, "' not found")
    ))
  }

  # Execute
  result <- tryCatch({
    do.call(tool$execute, as.list(tool_args))
  }, error = function(e) {
    paste0("Error: ", conditionMessage(e))
  })

  list(
    id = tool_call$id,
    name = tool_name,
    result = if (is.character(result)) result else jsonlite::toJSON(result, auto_unbox = TRUE)
  )
}
