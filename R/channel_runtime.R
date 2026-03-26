#' @title Channel Runtime
#' @description
#' Runtime orchestration layer for driving `ChatSession` objects from external
#' messaging channels.
#' @name channel_runtime
NULL

channel_default_session_policy <- function() {
  list(
    direct = "per_peer",
    group = "shared_chat",
    thread = "per_thread"
  )
}

channel_default_agent_system_prompt <- function() {
  paste(
    "You are a messaging-channel assistant.",
    "Users should not need to know what skills exist.",
    "When a request matches an available skill by description, proactively use that skill.",
    "For document, PDF, OCR, file, reporting, or workflow-automation requests, check relevant skills before answering from memory.",
    "If the user is effectively asking to add a durable new capability and the current skills do not cover it, prefer using the available skill-creation workflow instead of inventing an undocumented one-off process.",
    sep = " "
  )
}

channel_resolve_agent <- function(agent = NULL, skills = NULL, model = NULL) {
  if (!is.null(agent)) {
    return(agent)
  }

  if (is.null(skills)) {
    return(NULL)
  }

  create_agent(
    name = "ChannelSkillAgent",
    description = "Messaging channel assistant that proactively discovers and uses local skills.",
    system_prompt = channel_default_agent_system_prompt(),
    skills = skills,
    model = model
  )
}

channel_message_requests_skill_creation <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(FALSE)
  }

  patterns <- c(
    "帮我加个能力",
    "加个能力",
    "新增能力",
    "增加能力",
    "做成skill",
    "做成技能",
    "做成一个skill",
    "以后自动处理",
    "以后自动识别",
    "以后遇到.*自动",
    "这类.*自动处理",
    "教会你",
    "教你",
    "turn this into a skill",
    "make (this|that) a skill",
    "create a skill",
    "add a skill",
    "new capability",
    "reusable capability",
    "teach you to",
    "teach the bot to",
    "so you can do this automatically"
  )

  any(vapply(patterns, function(pattern) grepl(pattern, text, ignore.case = TRUE), logical(1)))
}

channel_preload_skill_text <- function(agent = NULL, skill_name, session = NULL) {
  if (is.null(agent) || !inherits(agent, "Agent")) {
    return(NULL)
  }

  load_skill_tool <- find_tool(agent$tools %||% list(), "load_skill")
  if (is.null(load_skill_tool)) {
    return(NULL)
  }

  tool_envir <- if (!is.null(session)) session$get_envir() else NULL
  loaded <- tryCatch(
    load_skill_tool$run(list(skill_name = skill_name), envir = tool_envir),
    error = function(e) NULL
  )

  if (!is.character(loaded) || length(loaded) != 1 || !nzchar(loaded)) {
    return(NULL)
  }
  if (grepl("^Skill not found:|^Error:", loaded)) {
    return(NULL)
  }

  loaded
}

channel_apply_skill_routing <- function(prompt, message, agent = NULL, session = NULL) {
  text <- trimws(message$text %||% "")
  if (!channel_message_requests_skill_creation(text)) {
    return(prompt)
  }

  skill_text <- channel_preload_skill_text(agent = agent, skill_name = "skill-creator", session = session)
  hint_lines <- c(
    "[routing_hint_begin]",
    "The user is asking for a reusable new capability rather than only a one-off answer.",
    "Use the `skill-creator` skill workflow first and prefer creating or updating a reusable skill.",
    "[routing_hint_end]"
  )

  if (!is.null(skill_text) && nzchar(skill_text)) {
    hint_lines <- c(
      hint_lines,
      "[skill_creator_context_begin]",
      skill_text,
      "[skill_creator_context_end]"
    )
  }

  paste(c(hint_lines, "", prompt), collapse = "\n")
}

channel_extract_local_paths <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(character(0))
  }

  matches <- gregexpr("/[^\\s`\"'<>]+", text, perl = TRUE)
  paths <- regmatches(text, matches)[[1]]
  if (length(paths) == 0) {
    return(character(0))
  }

  normalized <- unique(vapply(paths, function(path) {
    candidate <- sub("[,.;:!?]+$", "", path)
    if (!file.exists(candidate)) {
      return(NA_character_)
    }
    normalizePath(candidate, winslash = "/", mustWork = FALSE)
  }, character(1)))

  normalized[!is.na(normalized)]
}

channel_claims_attachment_sent <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(FALSE)
  }

  patterns <- c(
    "发给你了",
    "已发给你",
    "已经发送",
    "已发送",
    "sent to you",
    "i sent",
    "uploaded for you",
    "attached for you",
    "base64"
  )

  any(vapply(patterns, function(pattern) grepl(pattern, text, ignore.case = TRUE), logical(1)))
}

channel_extract_artifacts_from_tool_results <- function(tool_results) {
  if (is.null(tool_results) || length(tool_results) == 0) {
    return(character(0))
  }

  paths <- character(0)
  for (tool_result in tool_results) {
    raw_result <- tool_result$raw_result %||% NULL
    artifacts <- attr(raw_result, "aisdk_artifacts", exact = TRUE)
    if (is.null(artifacts) || length(artifacts) == 0) {
      next
    }
    extracted <- vapply(artifacts, function(entry) {
      path <- entry$path %||% NULL
      if (is.null(path) || !nzchar(path) || !file.exists(path)) {
        return(NA_character_)
      }
      normalizePath(path, winslash = "/", mustWork = FALSE)
    }, character(1))
    paths <- c(paths, extracted[!is.na(extracted)])
  }

  unique(paths)
}

channel_merge_participants <- function(existing = NULL, message) {
  current <- list()
  for (entry in existing %||% list()) {
    if (!is.list(entry)) {
      next
    }
    participant_id <- entry$sender_id %||% entry$id %||% NULL
    if (is.null(participant_id) || !nzchar(participant_id)) {
      next
    }
    current[[participant_id]] <- list(
      sender_id = participant_id,
      sender_name = entry$sender_name %||% entry$name %||% participant_id,
      last_seen_at = entry$last_seen_at %||% NULL
    )
  }

  participant_id <- message$sender_id %||% NULL
  if (is.null(participant_id) || !nzchar(participant_id)) {
    return(unname(current))
  }

  current[[participant_id]] <- list(
    sender_id = participant_id,
    sender_name = message$sender_name %||% participant_id,
    last_seen_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  )
  unname(current)
}

#' @title Channel Runtime
#' @description
#' Coordinates channel adapters, durable session state, and `ChatSession`
#' execution for external messaging integrations.
#' @export
ChannelRuntime <- R6::R6Class(
  "ChannelRuntime",
  public = list(
    #' @field session_store Durable store for channel sessions.
    session_store = NULL,

    #' @description Initialize a channel runtime.
    #' @param session_store File-backed session store.
    #' @param model Optional default model id.
    #' @param agent Optional default agent.
    #' @param tools Optional default tools.
    #' @param hooks Optional session hooks.
    #' @param registry Optional provider registry.
    #' @param max_steps Maximum tool execution steps.
    #' @param session_policy Session routing policy list.
    initialize = function(session_store,
                          model = NULL,
                          agent = NULL,
                          tools = NULL,
                          hooks = NULL,
                          registry = NULL,
                          max_steps = 10,
                          session_policy = channel_default_session_policy()) {
      if (missing(session_store) || !inherits(session_store, "ChannelSessionStore")) {
        channel_runtime_abort("ChannelRuntime requires a ChannelSessionStore.")
      }

      self$session_store <- session_store
      private$.model <- model
      private$.agent <- agent
      private$.tools <- tools %||% list()
      private$.hooks <- hooks
      private$.registry <- registry
      private$.max_steps <- max_steps
      private$.session_policy <- utils::modifyList(channel_default_session_policy(), session_policy %||% list())
      private$.adapters <- list()
    },

    #' @description Register a channel adapter.
    #' @param adapter Channel adapter instance.
    #' @return Invisible self.
    register_adapter = function(adapter) {
      if (!inherits(adapter, "ChannelAdapter")) {
        channel_runtime_abort("register_adapter() requires a ChannelAdapter instance.")
      }
      private$.adapters[[adapter$id]] <- adapter
      invisible(self)
    },

    #' @description Get a channel adapter.
    #' @param channel_id Adapter identifier.
    #' @return Adapter instance.
    get_adapter = function(channel_id) {
      adapter <- private$.adapters[[channel_id]]
      if (is.null(adapter)) {
        channel_runtime_abort(sprintf("No channel adapter registered for '%s'.", channel_id))
      }
      adapter
    },

    #' @description Handle a raw channel request.
    #' @param channel_id Adapter identifier.
    #' @param headers Request headers.
    #' @param body Raw or parsed body.
    #' @param ... Optional adapter-specific values.
    #' @return A normalized runtime response.
    handle_request = function(channel_id, headers = NULL, body = NULL, ...) {
      adapter <- self$get_adapter(channel_id)
      parsed <- adapter$parse_request(headers = headers, body = body, ...)

      parse_type <- parsed$type %||% "ignored"
      if (identical(parse_type, "challenge")) {
        return(list(type = "challenge", status = parsed$status %||% 200L, payload = parsed$payload))
      }
      if (!identical(parse_type, "inbound")) {
        return(list(type = "ignored", status = parsed$status %||% 200L, payload = parsed$payload %||% list()))
      }

      results <- lapply(parsed$messages %||% list(), function(message) {
        self$process_message(channel_id = channel_id, message = message)
      })

      list(
        type = "processed",
        status = parsed$status %||% 200L,
        results = results
      )
    },

    #' @description Process one normalized inbound message.
    #' @param channel_id Adapter identifier.
    #' @param message Normalized inbound message.
    #' @return Processing result list.
    process_message = function(channel_id, message) {
      adapter <- self$get_adapter(channel_id)
      if (!is.null(message$event_id) &&
          nzchar(message$event_id) &&
          self$session_store$has_processed_event(channel_id, message$event_id)) {
        return(list(
          session_key = NULL,
          prompt = NULL,
          reply_text = NULL,
          usage = NULL,
          duplicate = TRUE,
          event_id = message$event_id
        ))
      }

      session_key <- adapter$resolve_session_key(message, policy = private$.session_policy)
      session <- private$load_or_create_session(session_key)
      message <- adapter$prepare_inbound_message(session, message)

      private$update_session_metadata(session, message, session_key)
      prompt <- adapter$format_inbound_message(message)
      prompt <- channel_apply_skill_routing(
        prompt = prompt,
        message = message,
        agent = private$.agent,
        session = session
      )
      status_result <- tryCatch(
        adapter$send_status(message = message, status = "thinking"),
        error = function(e) NULL
      )

      result <- tryCatch(
        session$send(prompt),
        error = function(e) {
          err_text <- paste0("处理消息时发生错误：", conditionMessage(e))
          tryCatch(
            adapter$send_status(message = message, status = "error", text = err_text),
            error = function(e2) NULL
          )
          list(text = err_text, usage = NULL, .error = TRUE)
        }
      )
      reply_text <- trimws(result$text %||% "")
      if (!nzchar(reply_text)) {
        reply_text <- "我暂时没有生成有效回复。"
      }

      record <- private$build_record(session_key, message, session)
      self$session_store$save_session(session_key, session, record = record)
      self$session_store$mark_processed_event(
        channel_id = channel_id,
        event_id = message$event_id,
        payload = list(
          session_key = session_key,
          chat_id = message$chat_id,
          sender_id = message$sender_id
        )
      )

      attachment_results <- list()
      local_paths <- unique(c(
        channel_extract_artifacts_from_tool_results(result$all_tool_results %||% list()),
        channel_extract_local_paths(reply_text)
      ))
      if (length(local_paths) > 0) {
        for (path in local_paths) {
          attachment_result <- tryCatch(
            adapter$send_attachment(message = message, path = path),
            error = function(e) {
              list(error = TRUE, message = conditionMessage(e), path = path)
            }
          )
          attachment_results[[length(attachment_results) + 1L]] <- attachment_result
        }
      }

      successful_attachments <- Filter(function(x) {
        is.list(x) && !isTRUE(x$error)
      }, attachment_results)

      if (length(successful_attachments) == 0 && channel_claims_attachment_sent(reply_text)) {
        reply_text <- paste(
          reply_text,
          "",
          "\u26A0\ufe0f 系统未检测到实际发送成功的附件。",
          "如果你需要我发送图片或文件，请让我在最终回复里明确给出生成文件的绝对路径，系统才会自动上传并发送。",
          sep = "\n"
        )
      }

      if (nzchar(reply_text)) {
        adapter$send_text(message = message, text = reply_text)
      }

      list(
        session_key = session_key,
        prompt = prompt,
        reply_text = reply_text,
        usage = result$usage %||% NULL,
        duplicate = FALSE,
        event_id = message$event_id %||% NULL,
        status = status_result %||% NULL,
        attachments = attachment_results
      )
    },

    #' @description Create a child session linked to a parent session.
    #' @param parent_session_key Parent session key.
    #' @param child_session_key Optional child key. Generated if omitted.
    #' @param inherit_history Whether to copy parent state into the child.
    #' @param metadata Optional metadata to merge into the child session.
    #' @return The child session key.
    create_child_session = function(parent_session_key,
                                    child_session_key = NULL,
                                    inherit_history = TRUE,
                                    metadata = NULL) {
      parent <- private$load_or_create_session(parent_session_key)
      if (is.null(child_session_key) || !nzchar(child_session_key)) {
        child_session_key <- paste0(parent_session_key, ":child:", format(Sys.time(), "%Y%m%d%H%M%OS3"))
      }

      child <- private$create_session()
      if (isTRUE(inherit_history)) {
        child$restore_from_list(parent$as_list())
      }
      child$merge_metadata(c(list(parent_session_key = parent_session_key), metadata %||% list()))

      self$session_store$save_session(
        child_session_key,
        child,
        record = list(
          parent_session_key = parent_session_key,
          metadata = child$get_metadata("channel", default = list())
        )
      )
      self$session_store$link_child_session(parent_session_key, child_session_key)
      child_session_key
    }
  ),
  private = list(
    .adapters = NULL,
    .model = NULL,
    .agent = NULL,
    .tools = NULL,
    .hooks = NULL,
    .registry = NULL,
    .max_steps = 10,
    .session_policy = NULL,

    get_session_tools = function() {
      agent_tools <- if (!is.null(private$.agent)) private$.agent$tools else list()
      c(private$.tools %||% list(), agent_tools %||% list())
    },

    create_session = function() {
      ChatSession$new(
        model = private$.model,
        tools = private$.tools,
        hooks = private$.hooks,
        max_steps = private$.max_steps,
        registry = private$.registry,
        agent = private$.agent,
        metadata = list()
      )
    },

    load_or_create_session = function(session_key) {
      session <- self$session_store$load_session(
        session_key,
        tools = private$get_session_tools(),
        hooks = private$.hooks,
        registry = private$.registry
      )
      if (!is.null(session)) {
        return(session)
      }
      private$create_session()
    },

    update_session_metadata = function(session, message, session_key) {
      channel_meta <- session$get_metadata("channel", default = list())
      participants <- channel_merge_participants(channel_meta$participants, message)
      next_channel_meta <- channel_meta
      next_channel_meta$session_key <- session_key
      next_channel_meta$channel_id <- message$channel_id
      next_channel_meta$account_id <- message$account_id
      next_channel_meta$chat_id <- message$chat_id
      next_channel_meta$chat_type <- message$chat_type
      next_channel_meta$thread_id <- message$thread_id
      next_channel_meta$participants <- participants
      next_channel_meta$last_event_id <- message$event_id
      next_channel_meta$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

      session$merge_metadata(list(channel = next_channel_meta))
    },

    build_record = function(session_key, message, session) {
      channel_meta <- session$get_metadata("channel", default = list())
      list(
        session_key = session_key,
        channel_id = message$channel_id,
        account_id = message$account_id,
        chat_id = message$chat_id,
        chat_type = message$chat_type,
        thread_id = message$thread_id,
        participants = channel_meta$participants %||% list(),
        metadata = channel_meta,
        parent_session_key = session$get_metadata("parent_session_key", default = NULL)
      )
    }
  )
)

#' @title Create a Channel Runtime
#' @description
#' Helper for constructing a `ChannelRuntime`.
#' @param session_store File-backed session store.
#' @param model Optional default model id.
#' @param agent Optional default agent.
#' @param skills Optional skill paths or `"auto"`. Used only when `agent` is `NULL`.
#' @param tools Optional default tools.
#' @param hooks Optional session hooks.
#' @param registry Optional provider registry.
#' @param max_steps Maximum tool execution steps.
#' @param session_policy Session routing policy list.
#' @return A `ChannelRuntime`.
#' @export
create_channel_runtime <- function(session_store,
                                   model = NULL,
                                   agent = NULL,
                                   skills = NULL,
                                   tools = NULL,
                                   hooks = NULL,
                                   registry = NULL,
                                   max_steps = 10,
                                   session_policy = channel_default_session_policy()) {
  agent <- channel_resolve_agent(agent = agent, skills = skills, model = model)

  ChannelRuntime$new(
    session_store = session_store,
    model = model,
    agent = agent,
    tools = tools,
    hooks = hooks,
    registry = registry,
    max_steps = max_steps,
    session_policy = session_policy
  )
}
