#' @title AI Chat Server
#' @description
#' Shiny module server for AI-powered chat. The module is backed by
#' [ChatSession], the same stateful execution path used by [console_chat()], so
#' tool calls, session environments, model runtime options, and streamed
#' thinking blocks behave consistently across terminal and Shiny interfaces.
#'
#' @param id The namespace ID for the module.
#' @param model A `LanguageModelV1` object or a string ID like
#'   `"openai:gpt-4o"`. Ignored when `session` is supplied.
#' @param tools Optional list of Tool objects for function calling.
#' @param context Optional reactive expression that returns context data to
#'   inject into the turn-specific system prompt. This is read with `isolate()`
#'   to avoid reactive loops.
#' @param system Optional base system prompt.
#' @param session Optional existing [ChatSession] object. Use this to share
#'   history, tools, memory, or the session environment with other interfaces
#'   such as [console_chat()].
#' @param hooks Optional [HookHandler] object. Shiny tool-call hooks are merged
#'   with these hooks so tool progress can be rendered in the chat UI.
#' @param show_thinking Reactive expression or logical. If `TRUE`, streamed
#'   `<think>` blocks are shown in a collapsible block in the assistant message.
#'   If `FALSE`, thinking text is hidden while preserving the final answer.
#' @param stream Whether to use streaming output. Default `TRUE`.
#' @param backend UI backend used by `aiChatUI()`. `"aisdk"` is the built-in UI.
#'   `"shinychat"` requires the optional `shinychat` package and currently uses
#'   the same `ChatSession` execution path with an adapter for rendering.
#' @param debug Reactive expression or logical. If `TRUE`, tool arguments and
#'   results are shown expanded in the UI.
#' @param models Optional character vector or list of selectable model IDs.
#' @param selected_model Optional initial model ID used for this chat session.
#' @param allow_model_switch Whether users may switch models between turns.
#'   Switching is ignored while a turn is generating.
#' @param max_upload_size Optional maximum image upload size in bytes.
#' @param on_message_complete Optional callback function called when a message
#'   is complete. Takes one argument: the complete assistant message text.
#' @return A reactive value containing the `ChatSession` history.
#' @export
aiChatServer <- function(id,
                         model = NULL,
                         tools = NULL,
                         context = NULL,
                         system = NULL,
                         session = NULL,
                         hooks = NULL,
                         show_thinking = TRUE,
                         stream = TRUE,
                         backend = c("aisdk", "shinychat"),
                         debug = FALSE,
                         models = NULL,
                         selected_model = NULL,
                         allow_model_switch = TRUE,
                         max_upload_size = NULL,
                         on_message_complete = NULL) {
  rlang::check_installed(c("shiny", "commonmark", "callr"))
  backend <- match.arg(backend)
  if (identical(backend, "shinychat")) {
    rlang::check_installed(
      "shinychat",
      reason = "to use `aiChatServer(backend = \"shinychat\")`. Install it or use `backend = \"aisdk\"`."
    )
  }

  chat_session <- normalize_shiny_chat_session(
    session = session,
    model = model,
    selected_model = selected_model,
    system = system,
    tools = tools,
    hooks = hooks
  )
  session_supplied <- !is.null(session)

  shiny::moduleServer(id, function(input, output, session) {
    chat_history <- shiny::reactiveVal(chat_session$get_history())
    is_generating <- shiny::reactiveVal(FALSE)
    manager <- ShinyChatManager$new()
    runtime <- ShinyChatRuntime$new(
      chat_session = chat_session,
      models = models,
      selected_model = if (isTRUE(session_supplied)) selected_model else NULL,
      allow_model_switch = allow_model_switch,
      max_upload_size = max_upload_size
    )
    renderer <- create_shiny_chat_renderer(
      backend = backend,
      session = session,
      id = id,
      show_thinking = show_thinking,
      debug = debug
    )
    session$userData$aisdk_chat_renderer <- renderer
    session$userData$aisdk_chat_runtime <- runtime
    session$userData$aisdk_chat_turn_index <- 0L
    session$userData$aisdk_chat_id <- id
    input_text <- if (identical(backend, "shinychat")) {
      shiny::reactive(input[[paste0(shinychat_control_id(), "_user_input")]])
    } else {
      shiny::reactive(input$user_input)
    }
    send_event <- if (identical(backend, "shinychat")) input_text else shiny::reactive(input$send)
    model_snapshot <- runtime$current_model_snapshot()

    if (identical(backend, "aisdk") && isTRUE(allow_model_switch)) {
      model_choices <- normalize_shiny_model_choices(models)
      current_model_id <- model_snapshot$id %||% ""
      if (nzchar(current_model_id) && !current_model_id %in% model_choices) {
        model_choices <- c(model_choices, stats::setNames(current_model_id, model_snapshot$label %||% current_model_id))
      }
      shiny::updateSelectizeInput(
        session,
        "model",
        choices = model_choices,
        selected = if (nzchar(current_model_id)) current_model_id else character(0),
        server = TRUE
      )
    }

    renderer$chat_init(
      model = model_snapshot,
      busy = FALSE,
      show_thinking = shiny_flag_value(show_thinking, default = TRUE)
    )

    shiny::observeEvent(input$model_apply, {
      if (!isTRUE(allow_model_switch) || is_generating()) {
        renderer$model_patch(runtime$current_model_snapshot())
        return()
      }
      shiny::req(nzchar(trimws(input$model %||% "")))
      ok <- tryCatch({
        runtime$switch_model(trimws(input$model))
        TRUE
      }, error = function(e) {
        renderer$error(turn_id = session$userData$streaming_turn_id, message = conditionMessage(e))
        FALSE
      })
      if (isTRUE(ok)) {
        renderer$model_patch(runtime$current_model_snapshot())
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$cancel, {
      shiny::req(is_generating())
      is_generating(FALSE)
      manager$cleanup()
      runtime$cancel_turn(session$userData$streaming_turn_id)
      renderer$turn_error(turn_id = session$userData$streaming_turn_id, message = "Canceled.", status = "canceled")
      renderer$set_busy(FALSE)
      clear_shiny_stream_state(session)
      chat_history(chat_session$get_history())
    }, ignoreInit = TRUE)

    shiny::observeEvent(send_event(), {
      shiny::req(nzchar(trimws(input_text())))
      shiny::req(!is_generating())

      user_text <- trimws(input_text())
      attachments_payload <- shiny_attachment_blocks(input$attachments, max_upload_size = max_upload_size)
      user_content <- shiny_user_content_blocks(user_text, attachments_payload)
      history_snapshot <- chat_session$get_history()
      turn_id <- next_shiny_turn_id(session)
      model_snapshot <- runtime$current_model_snapshot()

      if (!identical(backend, "shinychat")) {
        shiny::updateTextAreaInput(session, "user_input", value = "")
      }
      reset_shiny_stream_state(session, turn_id)
      renderer$set_busy(TRUE)
      runtime$start_turn(turn_id = turn_id, user_content = user_content, model = model_snapshot)
      renderer$user_message(turn_id = turn_id, content = user_content, model = model_snapshot)
      renderer$assistant_start(turn_id = turn_id, model = model_snapshot)

      turn_system_prompt <- shiny_turn_system_prompt(
        base = if (isTRUE(session_supplied)) system else NULL,
        context = context
      )
      effective_hooks <- merge_shiny_chat_hooks(
        hooks = hooks,
        shiny_session = session,
        id = id,
        debug = debug,
        backend = backend,
        renderer = renderer,
        turn_id = turn_id
      )

      is_generating(TRUE)
      session$userData$shiny_chat_history_snapshot <- history_snapshot

      if (isTRUE(stream)) {
        start_ok <- TRUE
        tryCatch(
          {
            validate_model_messages(chat_session$get_model(), list(list(role = "user", content = user_content)))
            chat_session$append_message("user", user_content)
            prompt_payload <- chat_session$assemble_messages(turn_system_prompt = turn_system_prompt)
            manager$start_generation(
              model = chat_session$get_model(),
              messages = prompt_payload$messages,
              system = prompt_payload$system,
              tools = chat_session$get_tools(),
              call_options = chat_session$get_model_call_options(),
              max_steps = chat_session$as_list()$max_steps %||% 10
            )
          },
          error = function(e) {
            start_ok <<- FALSE
            is_generating(FALSE)
            console_restore_session_history(chat_session, history_snapshot)
            renderer$error(turn_id = turn_id, message = conditionMessage(e))
            renderer$set_busy(FALSE)
            clear_shiny_stream_state(session)
            manager$cleanup()
            chat_history(chat_session$get_history())
          }
        )
        if (!isTRUE(start_ok)) {
          return()
        }
      } else {
        result <- NULL
        ok <- TRUE
        tryCatch(
          {
              validate_model_messages(chat_session$get_model(), list(list(role = "user", content = user_content)))
              result <- chat_session$send(
              user_content,
              turn_system_prompt = turn_system_prompt,
              hooks = effective_hooks
            )
            update_streaming_message(
              session = session,
              id = id,
              delta = shiny_result_stream_text(result),
              show_thinking = shiny_flag_value(show_thinking, default = TRUE),
              backend = backend,
              renderer = renderer,
              turn_id = turn_id
            )
          },
          error = function(e) {
            ok <<- FALSE
            console_restore_session_history(chat_session, history_snapshot)
            renderer$error(turn_id = turn_id, message = conditionMessage(e))
            renderer$set_busy(FALSE)
            clear_shiny_stream_state(session)
          }
        )

        is_generating(FALSE)
        final_text <- if (isTRUE(ok)) chat_session$get_last_response() %||% "" else session$userData$streaming_content_text %||% ""
        if (isTRUE(ok)) {
          finalize_streaming_message(
            session = session,
            id = id,
            content = final_text,
            show_thinking = shiny_flag_value(show_thinking, default = TRUE),
            backend = backend,
            renderer = renderer,
            turn_id = turn_id
          )
        }
        renderer$set_busy(FALSE)

        chat_history(chat_session$get_history())
        if (isTRUE(ok) && !is.null(on_message_complete)) {
          on_message_complete(final_text)
        }
      }
    })

    shiny::observe({
      shiny::req(is_generating())
      shiny::invalidateLater(100)

      tryCatch(
        {
          poll_result <- manager$poll()
          if (is.null(poll_result)) {
            return()
          }

          if (!is.null(poll_result$text) && nzchar(poll_result$text)) {
            update_streaming_message(
              session = session,
              id = id,
              delta = poll_result$text,
              show_thinking = shiny_flag_value(show_thinking, default = TRUE),
              backend = backend,
              renderer = renderer
            )
          }

          if (!is.null(poll_result$tool_request)) {
            tool_request <- poll_result$tool_request
            tool_id <- next_shiny_tool_id(session, tool_request)
            renderer$tool_start(
              turn_id = session$userData$streaming_turn_id,
              tool_id = tool_id,
              name = tool_request$name,
              arguments = tool_request$arguments
            )
            tool_result <- tryCatch(
              execute_shiny_tool_request(
                tool_request = tool_request,
                tools = chat_session$get_tools(),
                hooks = hooks,
                envir = chat_session$get_envir()
              ),
              error = function(e) {
                list(
                  id = tool_request$id %||% tool_id,
                  name = tool_request$name %||% "tool",
                  result = paste0("Error executing tool: ", conditionMessage(e)),
                  raw_result = NULL,
                  is_error = TRUE
                )
              }
            )
            renderer$tool_result(
              turn_id = session$userData$streaming_turn_id,
              tool_id = tool_id,
              name = tool_result$name,
              result = tool_result$result,
              success = !isTRUE(tool_result$is_error),
              error = NULL
            )
            manager$resolve_tool(tool_result)
          }

          if (!is.null(poll_result$error)) {
            is_generating(FALSE)
            history_snapshot <- session$userData$shiny_chat_history_snapshot %||% list()
            console_restore_session_history(chat_session, history_snapshot)
            renderer$error(
              turn_id = session$userData$streaming_turn_id,
              message = poll_result$error
            )
            renderer$set_busy(FALSE)
            clear_shiny_stream_state(session)
            manager$cleanup()
            chat_history(chat_session$get_history())
            return()
          }

          if (isTRUE(poll_result$done)) {
            is_generating(FALSE)
            result <- poll_result$result
            append_result_reasoning_if_needed(
              session = session,
              id = id,
              result = result,
              show_thinking = shiny_flag_value(show_thinking, default = TRUE),
              backend = backend,
              renderer = renderer
            )
            final_text <- result$text %||% session$userData$streaming_content_text %||% ""
            finalize_streaming_message(
              session = session,
              id = id,
              content = final_text,
              show_thinking = shiny_flag_value(show_thinking, default = TRUE),
              backend = backend,
              renderer = renderer
            )
            renderer$set_busy(FALSE)
            apply_shiny_generation_result(chat_session, result)
            manager$cleanup()
            chat_history(chat_session$get_history())
            if (!is.null(on_message_complete)) {
              on_message_complete(chat_session$get_last_response() %||% final_text)
            }
          }
        },
        error = function(e) {
          is_generating(FALSE)
          history_snapshot <- session$userData$shiny_chat_history_snapshot %||% list()
          console_restore_session_history(chat_session, history_snapshot)
          renderer$error(
            turn_id = session$userData$streaming_turn_id,
            message = conditionMessage(e)
          )
          renderer$set_busy(FALSE)
          clear_shiny_stream_state(session)
          manager$cleanup()
          chat_history(chat_session$get_history())
        }
      )
    })

    chat_history
  })
}

#' @keywords internal
normalize_shiny_chat_session <- function(session = NULL,
                                         model = NULL,
                                         selected_model = NULL,
                                         system = NULL,
                                         tools = NULL,
                                         hooks = NULL) {
  if (!is.null(session)) {
    if (!inherits(session, "ChatSession")) {
      rlang::abort("`session` must be a ChatSession object.")
    }
    return(session)
  }

  model <- resolve_shiny_startup_model(model = model, selected_model = selected_model)

  create_chat_session(
    model = model,
    system_prompt = system,
    tools = tools,
    hooks = hooks
  )
}

#' @keywords internal
resolve_shiny_startup_model <- function(model = NULL, selected_model = NULL) {
  if (!is.null(model)) {
    return(model)
  }
  if (!is.null(selected_model) && nzchar(trimws(selected_model))) {
    return(trimws(selected_model))
  }

  startup_model <- tryCatch(
    resolve_console_startup_model(),
    error = function(e) NULL
  )
  model_id <- trimws(startup_model$model_id %||% "")
  if (nzchar(model_id)) {
    return(model_id)
  }

  NULL
}

#' @keywords internal
ShinyChatRuntime <- R6::R6Class(
  "ShinyChatRuntime",
  public = list(
    chat_session = NULL,
    turns = NULL,
    models = NULL,
    allow_model_switch = TRUE,
    max_upload_size = NULL,

    initialize = function(chat_session,
                          models = NULL,
                          selected_model = NULL,
                          allow_model_switch = TRUE,
                          max_upload_size = NULL) {
      self$chat_session <- chat_session
      self$turns <- list()
      self$models <- normalize_shiny_model_choices(models)
      self$allow_model_switch <- isTRUE(allow_model_switch)
      self$max_upload_size <- max_upload_size
      if (!is.null(selected_model) && nzchar(selected_model)) {
        self$switch_model(selected_model)
      }
    },

    current_model_snapshot = function() {
      id <- self$chat_session$get_model_id() %||% ""
      pos <- match(id, self$models)
      label <- if (!is.na(pos)) names(self$models)[[pos]] else id
      list(
        id = id,
        label = label,
        choices = as.list(self$models),
        allow_switch = isTRUE(self$allow_model_switch)
      )
    },

    switch_model = function(model) {
      if (!isTRUE(self$allow_model_switch)) {
        rlang::abort("Model switching is disabled for this chat.")
      }
      if (is.character(model) && length(model) == 1 && length(self$models) > 0 && model %in% names(self$models)) {
        model <- self$models[[model]]
      }
      self$chat_session$switch_model(model)
      invisible(self)
    },

    start_turn = function(turn_id, user_content, model) {
      self$turns[[turn_id]] <- list(
        turn_id = turn_id,
        seq = 0L,
        status = "thinking",
        model = model,
        user_content = user_content,
        blocks = list()
      )
      invisible(self$turns[[turn_id]])
    },

    cancel_turn = function(turn_id) {
      if (!is.null(turn_id) && !is.null(self$turns[[turn_id]])) {
        self$turns[[turn_id]]$status <- "canceled"
      }
      invisible(self)
    },

    retry_turn = function(turn_id = NULL) {
      # Server-side retry is intentionally conservative for v2: the front-end
      # can request it, but the old history mutation path remains untouched.
      invisible(NULL)
    }
  )
)

#' @keywords internal
shiny_turn_system_prompt <- function(base = NULL, context = NULL) {
  context_str <- NULL
  if (!is.null(context)) {
    ctx_data <- shiny::isolate(context())
    if (!is.null(ctx_data)) {
      context_str <- format_context(ctx_data)
    }
  }

  if (is.null(context_str) || !nzchar(context_str)) {
    return(base)
  }

  context_prompt <- paste0("Current application state:\n", context_str)
  if (is.null(base) || !nzchar(base)) {
    context_prompt
  } else {
    paste(base, context_prompt, sep = "\n\n")
  }
}

#' @keywords internal
merge_shiny_chat_hooks <- function(hooks = NULL,
                                   shiny_session,
                                   id,
                                   debug = FALSE,
                                      backend = "aisdk",
                                      renderer = NULL,
                                      turn_id = NULL) {
  base_hooks <- if (!is.null(hooks)) hooks$hooks %||% list() else list()
  user_on_tool_start <- base_hooks$on_tool_start
  user_on_tool_end <- base_hooks$on_tool_end

  base_hooks$on_tool_start <- function(tool, args) {
    if (!is.null(user_on_tool_start)) {
      user_on_tool_start(tool, args)
    }
    render_tool_start(
      session = shiny_session,
      id = id,
      tool_name = tool$name,
      arguments = args,
      debug = debug,
      backend = backend,
      renderer = renderer,
      turn_id = turn_id
    )
  }

  base_hooks$on_tool_end <- function(tool, result, success = TRUE, error = NULL, args = NULL) {
    if (!is.null(user_on_tool_end)) {
      call_hook_compat(
        user_on_tool_end,
        list(tool, result, success, error, args)
      )
    }
    render_tool_result(
      session = shiny_session,
      id = id,
      tool_name = tool$name,
      result = result,
      success = isTRUE(success) && is.null(error),
      error = error,
      debug = debug,
      backend = backend,
      renderer = renderer,
      turn_id = turn_id
    )
  }

  HookHandler$new(base_hooks)
}

#' @keywords internal
call_hook_compat <- function(fn, args) {
  fmls <- names(formals(fn)) %||% character(0)
  if ("..." %in% fmls) {
    return(do.call(fn, args))
  }
  n <- min(length(args), length(fmls))
  do.call(fn, args[seq_len(n)])
}

#' @keywords internal
ShinyChatManager <- R6::R6Class(
  "ShinyChatManager",
  public = list(
    proc = NULL,
    dir = NULL,
    chunk_file = NULL,
    status_file = NULL,
    tool_result_file = NULL,
    chunk_offset = 0L,
    delivered_tool_key = NULL,
    waiting_tool_since = NULL,

    start_generation = function(model,
                                messages,
                                system = NULL,
                                tools = NULL,
                                call_options = list(),
                                max_steps = 10) {
      self$cleanup()
      self$dir <- tempfile("aisdk-shiny-chat-")
      dir.create(self$dir, recursive = TRUE, showWarnings = FALSE)
      self$chunk_file <- file.path(self$dir, "chunks.txt")
      self$status_file <- file.path(self$dir, "status.rds")
      self$tool_result_file <- file.path(self$dir, "tool_result.rds")
      self$chunk_offset <- 0L
      self$delivered_tool_key <- NULL
      self$waiting_tool_since <- NULL

      write_shiny_status(self$status_file, list(status = "running"))
      file.create(self$chunk_file)

      model_tools <- lapply(tools %||% list(), function(x) {
        tool(
          name = x$name,
          description = x$description,
          parameters = x$parameters,
          execute = function(args) NULL,
          layer = x$layer %||% "llm",
          meta = x$meta
        )
      })

      self$proc <- callr::r_bg(
        func = shiny_chat_worker,
        args = list(
          model = model,
          messages = messages,
          system = system,
          tools = model_tools,
          call_options = call_options %||% list(),
          max_steps = max_steps,
          chunk_file = self$chunk_file,
          status_file = self$status_file,
          tool_result_file = self$tool_result_file
        ),
        supervise = TRUE
      )

      invisible(self)
    },

    poll = function() {
      text <- tryCatch(
        read_shiny_chunks(self$chunk_file, self$chunk_offset),
        error = function(e) {
          list(
            value = "",
            offset = self$chunk_offset,
            error = conditionMessage(e)
          )
        }
      )
      self$chunk_offset <- text$offset

      status <- read_shiny_status(self$status_file)
      if (is.null(status)) {
        if (nzchar(text$value)) {
          return(list(text = text$value))
        }
        return(NULL)
      }

      out <- list(text = text$value)
      if (!is.null(text$error)) {
        out$error <- text$error
      }

      if (identical(status$status, "waiting_tool")) {
        key <- paste0(status$tool_request$id %||% "", ":", status$tool_index %||% "")
        if (!identical(key, self$delivered_tool_key)) {
          self$delivered_tool_key <- key
          self$waiting_tool_since <- Sys.time()
          out$tool_request <- status$tool_request
        }
      } else {
        self$delivered_tool_key <- NULL
        self$waiting_tool_since <- NULL
      }

      if (identical(status$status, "error")) {
        out$error <- status$error %||% "Unknown Shiny chat worker error."
      }

      if (identical(status$status, "done")) {
        out$done <- TRUE
        out$result <- status$result %||% list()
      }

      if (!identical(status$status, "done") &&
          !identical(status$status, "error") &&
          !is.null(self$proc) &&
          !self$proc$is_alive()) {
        worker_error <- tryCatch(self$proc$get_result(), error = function(e) e)
        out$error <- if (inherits(worker_error, "condition")) {
          conditionMessage(worker_error)
        } else {
          "Shiny chat worker exited before completing the response."
        }
        if (!nzchar(out$error %||% "")) {
          out$error <- "Shiny chat worker exited before completing the response."
        }
      }

      if (identical(status$status, "waiting_tool") &&
          !is.null(self$waiting_tool_since) &&
          difftime(Sys.time(), self$waiting_tool_since, units = "secs") > 300) {
        out$error <- paste0(
          "Timed out waiting for Shiny tool result: ",
          status$tool_request$name %||% "tool"
        )
      }

      if (!nzchar(out$text %||% "") &&
          is.null(out$tool_request) &&
          is.null(out$error) &&
          !isTRUE(out$done)) {
        return(NULL)
      }

      out
    },

    resolve_tool = function(tool_result) {
      write_shiny_status(self$tool_result_file, tool_result)
      invisible(self)
    },

    cleanup = function() {
      if (!is.null(self$proc) && self$proc$is_alive()) {
        self$proc$kill()
      }
      self$proc <- NULL
      if (!is.null(self$dir) && dir.exists(self$dir)) {
        unlink(self$dir, recursive = TRUE, force = TRUE)
      }
      self$dir <- NULL
      self$chunk_file <- NULL
      self$status_file <- NULL
      self$tool_result_file <- NULL
      self$chunk_offset <- 0L
      self$delivered_tool_key <- NULL
      self$waiting_tool_since <- NULL
      invisible(self)
    }
  )
)

#' @keywords internal
shiny_chat_worker <- function(model,
                              messages,
                              system,
                              tools,
                              call_options,
                              max_steps,
                              chunk_file,
                              status_file,
                              tool_result_file) {
  `%||%` <- function(x, y) if (is.null(x)) y else x

  write_status <- function(value) {
    tmp <- paste0(status_file, ".tmp.", Sys.getpid(), ".", sample.int(1000000, 1))
    saveRDS(value, tmp)
    if (!file.rename(tmp, status_file)) {
      file.copy(tmp, status_file, overwrite = TRUE)
      unlink(tmp, force = TRUE)
    }
    invisible(status_file)
  }

  write_chunk <- function(chunk) {
    if (!is.null(chunk) && nzchar(chunk)) {
      frame <- base64enc::base64encode(charToRaw(enc2utf8(chunk)))
      cat(frame, "\n", file = chunk_file, append = TRUE, sep = "")
    }
  }

  safe_json <- function(x, auto_unbox = TRUE, ...) {
    tryCatch(
      jsonlite::toJSON(x, auto_unbox = auto_unbox, ..., null = "null"),
      error = function(e) {
        jsonlite::toJSON(
          list(
            error = "non_serializable_result",
            class = paste(class(x), collapse = ","),
            message = conditionMessage(e)
          ),
          auto_unbox = TRUE,
          null = "null"
        )
      }
    )
  }

  parse_tool_args <- function(args, tool_name = "tool") {
    empty_args <- function() stats::setNames(list(), character(0))
    if (is.null(args)) {
      return(empty_args())
    }
    if (is.list(args)) {
      return(if (length(args) == 0) empty_args() else args)
    }
    if (!is.character(args)) {
      return(list(value = args))
    }
    args <- trimws(args)
    if (!nzchar(args) || args %in% c("{}", "{ }", "null", "NULL", "undefined", "[]", "[ ]")) {
      return(empty_args())
    }
    parsed <- tryCatch(
      jsonlite::fromJSON(args, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) {
      return(empty_args())
    }
    if (is.list(parsed)) {
      if (length(parsed) == 0) empty_args() else parsed
    } else {
      list(value = parsed)
    }
  }

  recover_text_tool_calls_local <- function(result) {
    if (!is.null(result$tool_calls) && length(result$tool_calls) > 0) {
      return(result)
    }
    text <- result$text %||% ""
    if (!nzchar(text)) {
      return(result)
    }
    matches <- gregexpr("(?s)<tool_call>\\s*.*?\\s*</tool_call>", text, perl = TRUE)[[1]]
    if (length(matches) == 1L && identical(matches[[1]], -1L)) {
      return(result)
    }
    blocks <- regmatches(text, list(matches))[[1]]
    tool_calls <- list()
    for (i in seq_along(blocks)) {
      inner <- sub("(?s)^\\s*<tool_call>\\s*", "", blocks[[i]], perl = TRUE)
      inner <- sub("(?s)\\s*</tool_call>\\s*$", "", inner, perl = TRUE)
      parsed <- tryCatch(jsonlite::fromJSON(trimws(inner), simplifyVector = FALSE), error = function(e) NULL)
      if (is.null(parsed) || !is.list(parsed) || !nzchar(parsed$name %||% "")) {
        next
      }
      tool_calls[[length(tool_calls) + 1L]] <- list(
        id = parsed$id %||% sprintf("text_tool_call_%02d", i),
        name = parsed$name,
        arguments = parse_tool_args(parsed$arguments %||% list(), tool_name = parsed$name)
      )
    }
    if (length(tool_calls) == 0) {
      return(result)
    }
    cleaned_text <- text
    regmatches(cleaned_text, list(matches)) <- list(rep("", length(blocks)))
    result$text <- trimws(cleaned_text)
    result$tool_calls <- tool_calls
    if (is.null(result$finish_reason) || !nzchar(result$finish_reason %||% "")) {
      result$finish_reason <- "tool_calls"
    }
    result
  }

  build_messages_added_local <- function(messages, initial_len, final_text = NULL, final_reasoning = NULL) {
    messages_added <- list()
    if (length(messages) > initial_len) {
      messages_added <- messages[(initial_len + 1):length(messages)]
    }
    if (!is.null(final_text) && nzchar(final_text)) {
      final_message <- list(role = "assistant", content = final_text)
      if (!is.null(final_reasoning) && nzchar(final_reasoning)) {
        final_message$reasoning <- final_reasoning
      }
      messages_added <- c(messages_added, list(final_message))
    }
    messages_added
  }

  tryCatch(
    {
      base_params <- c(list(tools = tools), call_options %||% list())
      initial_messages_len <- length(messages)
      all_tool_calls <- list()
      all_tool_results <- list()
      step <- 0L
      result <- NULL

      while (step < max_steps) {
        step <- step + 1L
        params <- c(list(messages = messages), base_params)
        if (!is.null(system)) {
          params$system <- system
        }

        result <- model$do_stream(params, function(chunk, done) {
          write_chunk(chunk)
        })
        result <- recover_text_tool_calls_local(result)

        if (!is.null(result$tool_calls) && length(result$tool_calls) > 0 && length(tools) > 0) {
          all_tool_calls <- c(all_tool_calls, result$tool_calls)

          if (step >= max_steps) {
            result$finish_reason <- "tool_failure"
            break
          }

          tool_results <- list()
          for (i in seq_along(result$tool_calls)) {
            tc <- result$tool_calls[[i]]
            write_status(list(
              status = "waiting_tool",
              tool_index = i,
              tool_request = tc
            ))

            while (!file.exists(tool_result_file)) {
              Sys.sleep(0.05)
            }
            tool_result <- readRDS(tool_result_file)
            unlink(tool_result_file, force = TRUE)
            tool_results[[length(tool_results) + 1L]] <- tool_result
            write_status(list(status = "running"))
          }

          all_tool_results <- c(all_tool_results, tool_results)

          assistant_message <- list(role = "assistant", content = result$text %||% "")
          history_format <- model$get_history_format()
          if (identical(history_format, "openai")) {
            assistant_message$tool_calls <- lapply(result$tool_calls, function(tc) {
              list(
                id = tc$id,
                type = "function",
                `function` = list(
                  name = tc$name,
                  arguments = safe_json(tc$arguments, auto_unbox = TRUE)
                )
              )
            })
            if (isTRUE(model$capabilities$preserve_reasoning_content) &&
                !is.null(result$reasoning) &&
                nzchar(result$reasoning)) {
              assistant_message$reasoning_content <- result$reasoning
            }
          } else if (identical(history_format, "anthropic")) {
            assistant_message$content <- result$raw_response$content
          }

          messages <- c(messages, list(assistant_message))
          for (tr in tool_results) {
            messages <- c(messages, list(model$format_tool_result(tr$id, tr$name, tr$result)))
          }
        } else {
          break
        }
      }

      if (is.null(result)) {
        result <- list(text = "", finish_reason = "stop")
      }
      result$steps <- step
      result$all_tool_calls <- all_tool_calls
      result$all_tool_results <- all_tool_results
      result$messages_added <- build_messages_added_local(
        messages = messages,
        initial_len = initial_messages_len,
        final_text = result$text %||% NULL,
        final_reasoning = result$reasoning %||% NULL
      )

      write_status(list(status = "done", result = result))
    },
    error = function(e) {
      write_status(list(
        status = "error",
        error = conditionMessage(e)
      ))
    }
  )
}

#' @keywords internal
execute_shiny_tool_request <- function(tool_request, tools, hooks = NULL, envir = NULL) {
  results <- execute_tool_calls(
    list(tool_request),
    tools = tools,
    hooks = hooks,
    envir = envir
  )
  results[[1]]
}

#' @keywords internal
shiny_result_stream_text <- function(result) {
  text <- result$text %||% ""
  reasoning <- result$reasoning %||% ""
  if (!nzchar(reasoning)) {
    return(text)
  }
  if (grepl("<think>", text, fixed = TRUE)) {
    return(text)
  }
  paste0("<think>\n", reasoning, "\n</think>\n\n", text)
}

#' @keywords internal
append_result_reasoning_if_needed <- function(session,
                                             id,
                                             result,
                                             show_thinking = TRUE,
                                             backend = "aisdk",
                                             renderer = NULL,
                                             turn_id = NULL) {
  reasoning <- result$reasoning %||% ""
  if (!nzchar(reasoning)) {
    return(invisible(FALSE))
  }
  if (nzchar(session$userData$streaming_thinking_text %||% "")) {
    return(invisible(FALSE))
  }
  update_streaming_message(
    session = session,
    id = id,
    delta = paste0("<think>\n", reasoning, "\n</think>\n\n"),
    show_thinking = show_thinking,
    backend = backend,
    renderer = renderer,
    turn_id = turn_id
  )
  invisible(TRUE)
}

#' @keywords internal
apply_shiny_generation_result <- function(chat_session, result) {
  messages_added <- result$messages_added %||% list()
  if (length(messages_added) == 0) {
    return(invisible(chat_session))
  }

  current <- chat_session$as_list()
  current$history <- c(current$history %||% list(), messages_added)
  chat_session$restore_from_list(current)
  invisible(chat_session)
}

#' @keywords internal
write_shiny_status <- function(path, value) {
  tmp <- paste0(path, ".tmp.", Sys.getpid(), ".", sample.int(1000000, 1))
  saveRDS(value, tmp)
  file.rename(tmp, path)
  invisible(path)
}

#' @keywords internal
read_shiny_status <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(NULL)
  }
  tryCatch(readRDS(path), error = function(e) NULL)
}

#' @keywords internal
read_shiny_chunks <- function(path, offset = 0L) {
  if (is.null(path) || !file.exists(path)) {
    return(list(value = "", offset = offset))
  }
  size <- file.info(path)$size %||% 0L
  if (is.na(size) || size <= offset) {
    return(list(value = "", offset = offset))
  }
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  if (offset > 0) {
    seek(con, where = offset, origin = "start")
  }
  bytes <- readBin(con, what = "raw", n = size - offset)
  raw_text <- rawToChar(bytes)
  lines <- strsplit(raw_text, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) {
    return(list(value = "", offset = offset))
  }

  complete <- grepl("\n$", raw_text)
  if (!complete) {
    lines <- lines[-length(lines)]
  }
  if (length(lines) == 0) {
    return(list(value = "", offset = offset))
  }

  decoded <- vapply(lines, function(line) {
    if (!nzchar(line)) {
      return("")
    }
    rawToChar(base64enc::base64decode(line))
  }, character(1), USE.NAMES = FALSE)

  consumed <- sum(nchar(paste0(lines, "\n"), type = "bytes"))
  list(value = paste0(decoded, collapse = ""), offset = offset + consumed)
}

#' @keywords internal
shiny_flag_value <- function(value, default = FALSE) {
  out <- tryCatch(
    {
      if (is.function(value)) value() else value
    },
    error = function(e) default
  )
  if (is.null(out) || length(out) == 0) {
    return(isTRUE(default))
  }
  isTRUE(out[[1]])
}

#' @keywords internal
send_aisdk_chat_event <- function(session, id, event) {
  chat_id <- event$chat_id %||% if (is.function(session$ns)) session$ns("chat_root") else id
  event$chat_id <- chat_id
  session$sendCustomMessage(paste0("aisdk_chat_event_", chat_id), event)
  invisible(event)
}

#' @keywords internal
render_chat_markdown <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return("")
  }
  commonmark::markdown_html(
    text,
    extensions = c("table", "strikethrough", "autolink", "tasklist", "tagfilter")
  )
}

#' @keywords internal
markdown_to_html <- function(text) {
  render_chat_markdown(text)
}

#' @keywords internal
shiny_content_text <- function(content) {
  if (is.character(content)) {
    return(paste(content, collapse = "\n"))
  }
  blocks <- normalize_content_blocks(content)
  paste(vapply(blocks, function(block) {
    if (identical(block$type, "input_text")) block$text else ""
  }, character(1)), collapse = "\n")
}

#' @keywords internal
shiny_content_media_blocks <- function(content) {
  blocks <- tryCatch(normalize_content_blocks(content), error = function(e) list())
  Filter(function(block) identical(block$type, "input_image"), blocks)
}

#' @keywords internal
shiny_media_block_payload <- function(block, block_id = NULL) {
  value <- block$value %||% ""
  if (identical(block$source$kind %||% "", "file")) {
    bytes <- readBin(value, what = "raw", n = file.info(value)$size)
    value <- paste0("data:", block$media_type %||% "image/jpeg", ";base64,", base64enc::base64encode(bytes))
  }
  list(
    type = "media",
    block_id = block_id %||% paste0("media_", digest::digest(value)),
    media_type = block$media_type %||% "image/jpeg",
    url = value,
    html = sprintf(
      '<button type="button" class="aisdk-media-thumb" data-src="%s" data-media-type="%s"><img src="%s" alt="Attached image"></button>',
      html_escape(value),
      html_escape(block$media_type %||% "image/jpeg"),
      html_escape(value)
    )
  )
}

#' @keywords internal
shiny_attachment_blocks <- function(files, max_upload_size = NULL) {
  if (is.null(files) || NROW(files) == 0) {
    return(list())
  }
  rows <- split(files, seq_len(NROW(files)))
  lapply(rows, function(row) {
    if (!is.null(max_upload_size) && is.finite(max_upload_size) && row$size > max_upload_size) {
      rlang::abort(sprintf("Attachment `%s` exceeds the configured upload size limit.", row$name %||% "image"))
    }
    input_image(row$datapath, media_type = row$type %||% "auto")
  })
}

#' @keywords internal
shiny_user_content_blocks <- function(text, attachments = list()) {
  blocks <- list()
  if (nzchar(text %||% "")) {
    blocks <- c(blocks, list(input_text(text)))
  }
  blocks <- c(blocks, attachments %||% list())
  blocks <- unname(blocks)
  if (length(blocks) == 1L && identical(blocks[[1]]$type, "input_text")) {
    return(blocks[[1]]$text)
  }
  blocks
}

#' @keywords internal
format_tool_result_html <- function(tool_name,
                                    result,
                                    success = TRUE,
                                    error = NULL,
                                    debug = FALSE) {
  result_text <- if (!is.null(error)) {
    paste0("Error: ", error)
  } else if (is.character(result)) {
    paste(result, collapse = "\n")
  } else {
    safe_to_json(result, auto_unbox = TRUE, pretty = TRUE)
  }
  if (nchar(result_text) > 1000) {
    result_text <- paste0(substr(result_text, 1, 1000), "... (truncated)")
  }

  status <- if (isTRUE(success)) "Done" else "Error"
  status_class <- if (isTRUE(success)) "ok" else "error"
  if (shiny_flag_value(debug, default = FALSE)) {
    return(sprintf(
      '<div class="aisdk-tool-label">Result</div><pre>%s</pre>',
      html_escape(result_text)
    ))
  }

  sprintf(
    '<div class="aisdk-tool-summary %s">%s: %s</div>',
    status_class,
    html_escape(status),
    html_escape(tool_name)
  )
}

#' @keywords internal
create_shiny_chat_renderer <- function(backend = c("aisdk", "shinychat"),
                                       session,
                                       id,
                                       show_thinking = TRUE,
                                       debug = FALSE) {
  backend <- match.arg(backend)
  if (identical(backend, "shinychat")) {
    create_shinychat_chat_renderer(
      session = session,
      id = id,
      show_thinking = show_thinking,
      debug = debug
    )
  } else {
    create_aisdk_chat_renderer(
      session = session,
      id = id,
      show_thinking = show_thinking,
      debug = debug
    )
  }
}

#' @keywords internal
create_aisdk_chat_renderer <- function(session, id, show_thinking = TRUE, debug = FALSE) {
  renderer <- list()
  turn_store <- new.env(parent = emptyenv())

  next_seq <- function(turn_id) {
    key <- paste0("seq_", turn_id)
    value <- (turn_store[[key]] %||% 0L) + 1L
    turn_store[[key]] <- value
    value
  }

  get_turn <- function(turn_id, model = NULL) {
    if (is.null(turn_store[[turn_id]])) {
      turn_store[[turn_id]] <- list(
        turn_id = turn_id,
        status = "thinking",
        model = model %||% list(),
        blocks = list()
      )
    }
    turn_store[[turn_id]]
  }

  set_turn <- function(turn) {
    turn_store[[turn$turn_id]] <- turn
    invisible(turn)
  }

  block <- function(type, block_id, content = NULL, html = NULL, status = NULL, ...) {
    out <- c(list(
      type = type,
      block_id = block_id,
      content = content,
      html = html,
      status = status
    ), list(...))
    out[!vapply(out, is.null, logical(1))]
  }

  upsert_block <- function(turn, next_block) {
    ids <- vapply(turn$blocks, function(x) x$block_id %||% "", character(1))
    pos <- match(next_block$block_id, ids)
    if (is.na(pos)) {
      turn$blocks <- c(turn$blocks, list(next_block))
    } else {
      turn$blocks[[pos]] <- utils::modifyList(turn$blocks[[pos]], next_block, keep.null = TRUE)
    }
    turn
  }

  send_turn_patch <- function(turn_id, status = NULL, terminal = FALSE) {
    turn <- get_turn(turn_id)
    if (!is.null(status)) {
      turn$status <- status
      set_turn(turn)
    }
    type <- if (isTRUE(terminal) && identical(turn$status, "error")) {
      "turn_error"
    } else if (isTRUE(terminal)) {
      "turn_done"
    } else {
      "turn_patch"
    }
    send_aisdk_chat_event(session, id, list(
      type = type,
      turn_id = turn_id,
      seq = next_seq(turn_id),
      status = turn$status,
      model = turn$model %||% list(),
      blocks = turn$blocks %||% list()
    ))
  }

  renderer$chat_init <- function(model = list(), busy = FALSE, show_thinking = TRUE) {
    send_aisdk_chat_event(session, id, list(
      type = "chat_init",
      model = model,
      busy = isTRUE(busy),
      show_thinking = isTRUE(show_thinking)
    ))
  }

  renderer$model_patch <- function(model = list()) {
    send_aisdk_chat_event(session, id, list(
      type = "model_patch",
      model = model
    ))
  }

  renderer$user_message <- function(turn_id, content, html = markdown_to_html(shiny_content_text(content)), model = list()) {
    media <- lapply(seq_along(shiny_content_media_blocks(content)), function(i) {
      shiny_media_block_payload(shiny_content_media_blocks(content)[[i]], paste0(turn_id, "_user_media_", i))
    })
    send_aisdk_chat_event(session, id, list(
      type = "user_message",
      turn_id = turn_id,
      html = html,
      blocks = c(list(block("markdown", paste0(turn_id, "_user_text"), shiny_content_text(content), html)), media),
      model = model
    ))
  }

  renderer$assistant_start <- function(turn_id, status = "thinking", model = list()) {
    turn <- get_turn(turn_id, model = model)
    turn$status <- status
    turn$model <- model %||% turn$model %||% list()
    set_turn(turn)
    send_aisdk_chat_event(session, id, list(
      type = "turn_start",
      turn_id = turn_id,
      seq = next_seq(turn_id),
      status = status,
      model = turn$model,
      blocks = turn$blocks
    ))
  }

  renderer$content_replace <- function(turn_id,
                                       content,
                                       delta = NULL,
                                       html = markdown_to_html(content),
                                       status = "answering") {
    turn <- get_turn(turn_id)
    turn <- upsert_block(turn, block("markdown", "answer", content, html, status))
    turn$status <- status
    set_turn(turn)
    send_turn_patch(turn_id, status = status)
  }

  renderer$thinking_replace <- function(turn_id,
                                        content,
                                        delta = NULL,
                                        html = markdown_to_html(content),
                                        status = "thinking",
                                        close = FALSE) {
    if (!shiny_flag_value(show_thinking, default = TRUE)) {
      return(invisible())
    }
    turn <- get_turn(turn_id)
    if (!nzchar(content %||% "")) {
      turn$blocks <- Filter(function(x) !identical(x$type, "thinking"), turn$blocks)
    } else {
      turn <- upsert_block(turn, block("thinking", "thinking", content, html, status, collapsed = isTRUE(close)))
    }
    turn$status <- status
    set_turn(turn)
    send_turn_patch(turn_id, status = status)
  }

  renderer$tool_start <- function(turn_id, tool_id, name, arguments = list()) {
    args_str <- tryCatch(
      safe_to_json(arguments, auto_unbox = TRUE, pretty = TRUE),
      error = function(e) "{}"
    )
    turn <- get_turn(turn_id)
    turn <- upsert_block(turn, block("tool", tool_id, args_str, NULL, "running", name = name, arguments = args_str, debug = shiny_flag_value(debug, default = FALSE), open = shiny_flag_value(debug, default = FALSE)))
    set_turn(turn)
    send_turn_patch(turn_id, status = paste0("tool: ", name))
  }

  renderer$tool_result <- function(turn_id,
                                   tool_id,
                                   name,
                                   result,
                                   success = TRUE,
                                   error = NULL) {
    turn <- get_turn(turn_id)
    turn <- upsert_block(turn, block(
      "tool",
      tool_id,
      NULL,
      format_tool_result_html(
        tool_name = name,
        result = result,
        success = success,
        error = error,
        debug = shiny_flag_value(debug, default = FALSE)
      ),
      if (isTRUE(success)) "done" else "error",
      name = name,
      success = isTRUE(success),
      debug = shiny_flag_value(debug, default = FALSE),
      open = shiny_flag_value(debug, default = FALSE)
    ))
    set_turn(turn)
    send_turn_patch(turn_id, status = if (isTRUE(success)) "answering" else "tool error")
  }

  renderer$assistant_end <- function(turn_id, content, html = markdown_to_html(content), status = "done") {
    turn <- get_turn(turn_id)
    if (!is.null(html)) {
      turn <- upsert_block(turn, block("markdown", "answer", content, html, status))
    }
    turn$status <- status
    set_turn(turn)
    send_turn_patch(turn_id, status = status, terminal = TRUE)
  }

  renderer$error <- function(turn_id, message) {
    renderer$turn_error(turn_id = turn_id, message = message)
    renderer$set_busy(FALSE)
  }

  renderer$turn_error <- function(turn_id, message, status = "error") {
    turn <- get_turn(turn_id %||% "error")
    turn <- upsert_block(turn, block("error", "error", message, sprintf('<div class="aisdk-error">%s</div>', html_escape(message)), status))
    turn$status <- status
    set_turn(turn)
    send_turn_patch(turn$turn_id, status = status, terminal = TRUE)
  }

  renderer$set_busy <- function(busy = TRUE) {
    send_aisdk_chat_event(session, id, list(
      type = "busy_patch",
      busy = isTRUE(busy)
    ))
  }

  renderer
}

#' @keywords internal
create_shinychat_chat_renderer <- function(session, id, show_thinking = TRUE, debug = FALSE) {
  renderer <- list()

  renderer$chat_init <- function(model = list(), busy = FALSE, show_thinking = TRUE) {
    invisible()
  }

  renderer$model_patch <- function(model = list()) {
    invisible()
  }

  renderer$user_message <- function(turn_id, content, html = NULL) {
    invisible()
  }

  renderer$assistant_start <- function(turn_id, status = "thinking") {
    session$userData$streaming_shinychat_thinking_open <- FALSE
    shinychat_append_chunk(session, "", chunk = "start")
  }

  renderer$content_replace <- function(turn_id,
                                       content,
                                       delta = NULL,
                                       html = NULL,
                                       status = "answering") {
    if (!is.null(delta) && nzchar(delta)) {
      shinychat_append_chunk(session, delta, chunk = TRUE)
    }
  }

  renderer$thinking_replace <- function(turn_id,
                                        content,
                                        delta = NULL,
                                        html = NULL,
                                        status = "thinking",
                                        close = FALSE) {
    if (!shiny_flag_value(show_thinking, default = TRUE)) {
      return(invisible())
    }
    if (isTRUE(close)) {
      if (isTRUE(session$userData$streaming_shinychat_thinking_open)) {
        shinychat_append_chunk(session, "\n\n</details>\n\n", chunk = TRUE)
        session$userData$streaming_shinychat_thinking_open <- FALSE
      }
      return(invisible())
    }
    if (!is.null(delta) && nzchar(delta)) {
      if (!isTRUE(session$userData$streaming_shinychat_thinking_open)) {
        shinychat_append_chunk(session, "\n\n<details><summary>Thinking</summary>\n\n", chunk = TRUE)
        session$userData$streaming_shinychat_thinking_open <- TRUE
      }
      shinychat_append_chunk(session, delta, chunk = TRUE)
    }
  }

  renderer$tool_start <- function(turn_id, tool_id, name, arguments = list()) {
    session$userData$active_tool_text <- paste0("Running tool: ", name)
    invisible()
  }

  renderer$tool_result <- function(turn_id,
                                   tool_id,
                                   name,
                                   result,
                                   success = TRUE,
                                   error = NULL) {
    session$userData$active_tool_text <- NULL
    invisible()
  }

  renderer$assistant_end <- function(turn_id, content, html = NULL, status = "done") {
    thinking_text <- session$userData$streaming_thinking_text %||% ""
    content_text <- session$userData$streaming_content_text %||% ""
    if (isTRUE(session$userData$streaming_shinychat_thinking_open)) {
      shinychat_append_chunk(session, "\n\n</details>\n\n", chunk = TRUE)
      session$userData$streaming_shinychat_thinking_open <- FALSE
    }
    final_content <- build_final_text(
      content_text = content_text,
      thinking_text = thinking_text,
      full_content = content,
      show_thinking = shiny_flag_value(show_thinking, default = TRUE)
    )
    shinychat_append_chunk(session, final_content, chunk = "end", operation = "replace")
  }

  renderer$error <- function(turn_id, message) {
    if (isTRUE(session$userData$streaming_shinychat_thinking_open)) {
      shinychat_append_chunk(session, "\n\n</details>\n\n", chunk = TRUE)
      session$userData$streaming_shinychat_thinking_open <- FALSE
    }
    shinychat_append_chunk(session, paste0("Error: ", message), chunk = "end", operation = "replace")
    renderer$set_busy(FALSE)
  }

  renderer$turn_error <- function(turn_id, message, status = "error") {
    renderer$error(turn_id = turn_id, message = message)
  }

  renderer$set_busy <- function(busy = TRUE) {
    invisible()
  }

  renderer
}

#' @keywords internal
shinychat_control_id <- function() {
  "shinychat"
}

#' @keywords internal
shinychat_append_message_impl <- function(...) {
  rlang::check_installed("shinychat")
  getExportedValue("shinychat", "chat_append_message")(...)
}

#' @keywords internal
shinychat_append_message <- function(session, role, content) {
  shinychat_append_message_impl(
    id = shinychat_control_id(),
    msg = list(role = role, content = content),
    chunk = FALSE,
    session = session
  )
}

#' @keywords internal
shinychat_append_chunk <- function(session,
                                   content,
                                   chunk = TRUE,
                                   operation = c("append", "replace")) {
  operation <- match.arg(operation)
  shinychat_append_message_impl(
    id = shinychat_control_id(),
    msg = list(role = "assistant", content = content),
    chunk = chunk,
    operation = operation,
    session = session
  )
}

#' @keywords internal
reset_shiny_stream_state <- function(session, turn_id) {
  session$userData$streaming_turn_id <- turn_id
  session$userData$streaming_thinking <- FALSE
  session$userData$streaming_thinking_started <- FALSE
  session$userData$streaming_content_started <- FALSE
  session$userData$streaming_tool_index <- 0L
  session$userData$streaming_thinking_text <- ""
  session$userData$streaming_content_text <- ""
  session$userData$streaming_shinychat_thinking_open <- FALSE
  session$userData$active_tool_ui_id <- NULL
  invisible()
}

#' @keywords internal
clear_shiny_stream_state <- function(session) {
  session$userData$streaming_turn_id <- NULL
  session$userData$streaming_thinking <- NULL
  session$userData$streaming_thinking_started <- NULL
  session$userData$streaming_content_started <- NULL
  session$userData$streaming_tool_index <- NULL
  session$userData$streaming_thinking_text <- NULL
  session$userData$streaming_content_text <- NULL
  session$userData$streaming_shinychat_thinking_open <- NULL
  session$userData$active_tool_ui_id <- NULL
  invisible()
}

#' @keywords internal
next_shiny_turn_id <- function(session) {
  index <- (session$userData$aisdk_chat_turn_index %||% 0L) + 1L
  session$userData$aisdk_chat_turn_index <- index
  paste0("turn_", index)
}

#' @keywords internal
next_shiny_tool_id <- function(session, tool_request = list()) {
  raw_id <- tool_request$id %||% ""
  if (nzchar(raw_id)) {
    session$userData$active_tool_ui_id <- raw_id
    return(raw_id)
  }
  index <- (session$userData$streaming_tool_index %||% 0L) + 1L
  session$userData$streaming_tool_index <- index
  tool_id <- paste0(session$userData$streaming_turn_id %||% "turn", "_tool_", index)
  session$userData$active_tool_ui_id <- tool_id
  tool_id
}

#' @keywords internal
set_streaming_turn_status <- function(session, text) {
  renderer <- session$userData$aisdk_chat_renderer
  turn_id <- session$userData$streaming_turn_id
  if (!is.null(renderer) && !is.null(turn_id)) {
    renderer$content_replace(
      turn_id = turn_id,
      content = session$userData$streaming_content_text %||% "",
      status = text
    )
  }
  invisible()
}

#' @keywords internal
render_streaming_region <- function(session, target_id, text, key = c("content", "thinking")) {
  key <- match.arg(key)
  renderer <- session$userData$aisdk_chat_renderer
  turn_id <- session$userData$streaming_turn_id
  html <- markdown_to_html(text)
  if (!is.null(renderer) && !is.null(turn_id)) {
    if (identical(key, "thinking")) {
      renderer$thinking_replace(turn_id = turn_id, content = text, html = html)
    } else {
      renderer$content_replace(turn_id = turn_id, content = text, html = html)
    }
  }
  invisible(html)
}

#' @keywords internal
render_message <- function(session,
                           id,
                           role,
                           content,
                           streaming = FALSE,
                           backend = "aisdk",
                           renderer = NULL,
                           turn_id = NULL) {
  renderer <- renderer %||% session$userData$aisdk_chat_renderer %||%
    create_shiny_chat_renderer(backend, session, id)
  turn_id <- turn_id %||% session$userData$streaming_turn_id %||% next_shiny_turn_id(session)
  if (isTRUE(streaming)) {
    reset_shiny_stream_state(session, turn_id)
    renderer$assistant_start(turn_id = turn_id)
  } else if (identical(role, "user")) {
    renderer$user_message(turn_id = turn_id, content = content)
  } else {
    renderer$content_replace(turn_id = turn_id, content = content)
  }
  invisible()
}

#' @keywords internal
render_tool_start <- function(session,
                              id,
                              tool_name,
                              arguments,
                              debug = FALSE,
                              backend = "aisdk",
                              renderer = NULL,
                              turn_id = NULL,
                              tool_id = NULL) {
  renderer <- renderer %||% session$userData$aisdk_chat_renderer %||%
    create_shiny_chat_renderer(backend, session, id, debug = debug)
  turn_id <- turn_id %||% session$userData$streaming_turn_id
  tool_id <- tool_id %||% next_shiny_tool_id(session, list(name = tool_name))
  renderer$tool_start(turn_id = turn_id, tool_id = tool_id, name = tool_name, arguments = arguments)
  invisible(tool_id)
}

#' @keywords internal
render_tool_result <- function(session,
                               id = session$userData$aisdk_chat_id %||% "chat",
                               tool_name,
                               result,
                               success = TRUE,
                               error = NULL,
                               debug = FALSE,
                               backend = "aisdk",
                               renderer = NULL,
                               turn_id = NULL,
                               tool_id = NULL) {
  renderer <- renderer %||% session$userData$aisdk_chat_renderer %||%
    create_shiny_chat_renderer(backend, session, id, debug = debug)
  turn_id <- turn_id %||% session$userData$streaming_turn_id
  tool_id <- tool_id %||% session$userData$active_tool_ui_id %||% next_shiny_tool_id(session, list(name = tool_name))
  renderer$tool_result(
    turn_id = turn_id,
    tool_id = tool_id,
    name = tool_name,
    result = result,
    success = success,
    error = error
  )
  session$userData$active_tool_ui_id <- NULL
  invisible()
}

#' @keywords internal
update_streaming_message <- function(session,
                                     id,
                                     delta,
                                     show_thinking = TRUE,
                                     backend = "aisdk",
                                     renderer = NULL,
                                     turn_id = NULL) {
  if (is.null(delta) || !nzchar(delta)) {
    return(invisible())
  }
  renderer <- renderer %||% session$userData$aisdk_chat_renderer %||%
    create_shiny_chat_renderer(backend, session, id, show_thinking = show_thinking)
  turn_id <- turn_id %||% session$userData$streaming_turn_id
  if (is.null(turn_id) || !nzchar(turn_id)) {
    turn_id <- next_shiny_turn_id(session)
    reset_shiny_stream_state(session, turn_id)
    renderer$assistant_start(turn_id = turn_id)
  }

  append_segment <- function(text, key, visible = TRUE) {
    if (!nzchar(text)) {
      return(invisible())
    }
    started_flag <- paste0("streaming_", key, "_started")
    if (!isTRUE(session$userData[[started_flag]])) {
      session$userData[[started_flag]] <- TRUE
    }
    text_flag <- paste0("streaming_", key, "_text")
    prev_text <- session$userData[[text_flag]] %||% ""
    session$userData[[text_flag]] <- paste0(prev_text, text)
    if (!isTRUE(visible)) {
      return(invisible())
    }
    if (identical(key, "thinking")) {
      renderer$thinking_replace(
        turn_id = turn_id,
        content = session$userData[[text_flag]],
        delta = text
      )
    } else {
      renderer$content_replace(
        turn_id = turn_id,
        content = session$userData[[text_flag]],
        delta = text,
        status = "answering"
      )
    }
  }

  remaining <- delta
  in_thinking <- isTRUE(session$userData$streaming_thinking)
  repeat {
    loc <- regexpr("<think>|</think>", remaining, perl = TRUE)
    if (loc[1] == -1) {
      if (in_thinking) {
        append_segment(remaining, "thinking", visible = shiny_flag_value(show_thinking, default = TRUE))
      } else {
        append_segment(remaining, "content", visible = TRUE)
      }
      break
    }
    if (loc[1] > 1) {
      before <- substr(remaining, 1, loc[1] - 1)
      if (in_thinking) {
        append_segment(before, "thinking", visible = shiny_flag_value(show_thinking, default = TRUE))
      } else {
        append_segment(before, "content", visible = TRUE)
      }
    }
    tag <- substr(remaining, loc[1], loc[1] + attr(loc, "match.length") - 1)
    if (tag == "<think>") {
      in_thinking <- TRUE
      if (!identical(backend, "shinychat")) {
        renderer$thinking_replace(turn_id = turn_id, content = session$userData$streaming_thinking_text %||% "")
      }
    } else if (tag == "</think>") {
      in_thinking <- FALSE
      renderer$thinking_replace(
        turn_id = turn_id,
        content = session$userData$streaming_thinking_text %||% "",
        close = TRUE
      )
      if (nzchar(session$userData$streaming_content_text %||% "")) {
        renderer$content_replace(
          turn_id = turn_id,
          content = session$userData$streaming_content_text,
          status = "answering"
        )
      }
    }
    next_start <- loc[1] + attr(loc, "match.length")
    remaining <- if (next_start <= nchar(remaining)) substr(remaining, next_start, nchar(remaining)) else ""
    if (!nzchar(remaining)) {
      break
    }
  }
  session$userData$streaming_thinking <- in_thinking
  invisible()
}

#' @keywords internal
finalize_streaming_message <- function(session,
                                       id,
                                       content,
                                       show_thinking = TRUE,
                                       backend = "aisdk",
                                       renderer = NULL,
                                       turn_id = NULL) {
  renderer <- renderer %||% session$userData$aisdk_chat_renderer %||%
    create_shiny_chat_renderer(backend, session, id, show_thinking = show_thinking)
  turn_id <- turn_id %||% session$userData$streaming_turn_id
  thinking_text <- session$userData$streaming_thinking_text %||% ""
  content_text <- session$userData$streaming_content_text %||% ""
  parsed <- split_thinking_blocks(content %||% "")
  final_content_text <- content_text
  if (!nzchar(trimws(final_content_text %||% ""))) {
    final_content_text <- parsed$content %||% content %||% ""
  }
  html_content <- markdown_to_html(final_content_text)
  renderer$assistant_end(
    turn_id = turn_id,
    content = final_content_text,
    html = html_content,
    status = "done"
  )
  clear_shiny_stream_state(session)
  invisible()
}

#' @keywords internal
build_final_text <- function(content_text, thinking_text, full_content, show_thinking = TRUE) {
  content_text <- content_text %||% ""
  thinking_text <- thinking_text %||% ""
  full_content <- full_content %||% ""
  if (!nzchar(trimws(content_text))) {
    parsed <- split_thinking_blocks(full_content)
    content_text <- parsed$content %||% ""
    if (!nzchar(thinking_text)) {
      thinking_text <- parsed$thinking %||% ""
    }
  }
  if (!isTRUE(show_thinking) || !nzchar(trimws(thinking_text))) {
    return(content_text)
  }
  paste0(content_text, "\n\n<details><summary>Thinking</summary>\n\n", thinking_text, "\n\n</details>")
}

#' @keywords internal
build_final_html <- function(content_text, thinking_text, full_content, show_thinking = TRUE) {
  content_text <- content_text %||% ""
  thinking_text <- thinking_text %||% ""
  full_content <- full_content %||% ""
  if (!nzchar(trimws(content_text))) {
    parsed <- split_thinking_blocks(full_content)
    content_text <- parsed$content %||% ""
    if (!nzchar(thinking_text)) {
      thinking_text <- parsed$thinking %||% ""
    }
  }
  main_html <- markdown_to_html(content_text)
  thinking_html <- ""
  if (isTRUE(show_thinking) && nzchar(trimws(thinking_text))) {
    thinking_html <- paste0(
      '<details class="aisdk-thinking" open><summary>Thinking</summary><div class="aisdk-thinking-body">',
      markdown_to_html(thinking_text),
      "</div></details>"
    )
  }
  paste0(main_html, thinking_html)
}

#' @keywords internal
split_thinking_blocks <- function(content) {
  content <- content %||% ""
  if (!nzchar(content) || !grepl("<think>", content, fixed = TRUE)) {
    return(list(content = content, thinking = ""))
  }
  matches <- gregexpr("<think>[\\s\\S]*?</think>", content, perl = TRUE)
  blocks <- regmatches(content, matches)[[1]]
  thinking_text <- ""
  if (length(blocks) > 0) {
    thinking_text <- paste(
      vapply(
        blocks,
        function(x) sub("^<think>|</think>$", "", x),
        character(1)
      ),
      collapse = "\n\n"
    )
  }
  main_text <- gsub("<think>[\\s\\S]*?</think>", "", content, perl = TRUE)
  list(content = main_text, thinking = thinking_text)
}

#' @keywords internal
format_thinking_html <- function(content, show_thinking = TRUE) {
  if (!nzchar(content %||% "")) {
    return("(No response)")
  }
  parsed <- split_thinking_blocks(content)
  build_final_html(
    content_text = parsed$content,
    thinking_text = parsed$thinking,
    full_content = "",
    show_thinking = show_thinking
  )
}

#' @keywords internal
format_context <- function(ctx_data) {
  if (is.list(ctx_data)) {
    nms <- names(ctx_data) %||% rep("", length(ctx_data))
    lines <- vapply(seq_along(ctx_data), function(i) {
      nm <- nms[[i]]
      if (!nzchar(nm)) nm <- paste0("value_", i)
      val <- ctx_data[[i]]
      if (is.atomic(val) && length(val) == 1) {
        sprintf("- %s: %s", nm, as.character(val))
      } else {
        sprintf("- %s: %s", nm, safe_to_json(val, auto_unbox = TRUE))
      }
    }, character(1))
    paste(lines, collapse = "\n")
  } else {
    as.character(ctx_data)
  }
}

#' @keywords internal
html_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}
