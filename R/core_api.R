#' @title Core API: High-Level Functions
#' @description
#' User-facing high-level API functions for interacting with AI models.
#' @name core_api
NULL

#' @keywords internal
parse_tool_call_json_payload <- function(payload, fallback_index = 1L) {
  payload <- trimws(payload %||% "")
  if (!nzchar(payload)) {
    return(list())
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(payload, simplifyVector = FALSE),
    error = function(e) {
      repaired <- repair_json_string(payload)
      tryCatch(
        jsonlite::fromJSON(repaired, simplifyVector = FALSE),
        error = function(e2) NULL
      )
    }
  )

  normalize_one <- function(item, idx) {
    if (is.null(item) || !is.list(item)) {
      return(NULL)
    }

    fn <- item$`function` %||% NULL
    name <- item$name %||% item$tool_name %||% fn$name %||% ""
    if (!nzchar(name)) {
      return(NULL)
    }

    arguments <- item$arguments %||% fn$arguments %||% item$input %||% list()
    list(
      id = item$id %||% item$tool_call_id %||% sprintf("text_tool_call_%02d", idx),
      name = name,
      arguments = parse_tool_arguments(arguments, tool_name = name)
    )
  }

  normalize_many <- function(value, offset = fallback_index) {
    if (is.null(value) || !is.list(value)) {
      return(list())
    }

    if (!is.null(value$tool_calls) && is.list(value$tool_calls)) {
      return(normalize_many(value$tool_calls, offset = offset))
    }

    if (!is.null(value$name) || !is.null(value$tool_name) || !is.null(value$`function`$name)) {
      one <- normalize_one(value, offset)
      return(if (is.null(one)) list() else list(one))
    }

    calls <- list()
    for (i in seq_along(value)) {
      item <- normalize_one(value[[i]], offset + i - 1L)
      if (!is.null(item)) {
        calls[[length(calls) + 1L]] <- item
      }
    }
    calls
  }

  normalize_many(parsed, offset = fallback_index)
}

#' @keywords internal
parse_tool_call_blocks <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(list(tool_calls = NULL, text = text))
  }

  parse_tag_blocks <- function(current_text, tag, parse_block) {
    pattern <- sprintf("(?is)<%s>\\s*.*?\\s*</%s>", tag, tag)
    matches <- gregexpr(pattern, current_text, perl = TRUE)[[1]]
    if (length(matches) == 1L && identical(matches[[1]], -1L)) {
      return(list(tool_calls = list(), text = current_text))
    }

    blocks <- regmatches(current_text, list(matches))[[1]]
    calls <- list()
    for (i in seq_along(blocks)) {
      inner <- sub(sprintf("(?is)^\\s*<%s>\\s*", tag), "", blocks[[i]], perl = TRUE)
      inner <- sub(sprintf("(?is)\\s*</%s>\\s*$", tag), "", inner, perl = TRUE)
      block_calls <- parse_block(trimws(inner), length(calls) + 1L)
      if (length(block_calls) > 0) {
        calls <- c(calls, block_calls)
      }
    }

    cleaned <- current_text
    regmatches(cleaned, list(matches)) <- list(rep("", length(blocks)))
    list(tool_calls = calls, text = cleaned)
  }

  tool_calls <- list()
  cleaned_text <- text

  plural <- parse_tag_blocks(cleaned_text, "tool_calls", function(inner, fallback_index) {
    nested <- parse_tool_call_blocks(inner)
    if (!is.null(nested$tool_calls) && length(nested$tool_calls) > 0) {
      return(nested$tool_calls)
    }
    parse_tool_call_json_payload(inner, fallback_index = fallback_index)
  })
  tool_calls <- c(tool_calls, plural$tool_calls)
  cleaned_text <- plural$text

  singular <- parse_tag_blocks(cleaned_text, "tool_call", function(inner, fallback_index) {
    parse_tool_call_json_payload(inner, fallback_index = fallback_index)
  })
  tool_calls <- c(tool_calls, singular$tool_calls)
  cleaned_text <- singular$text

  cleaned_text <- gsub("(?is)<tool_calls>\\s*</tool_calls>", "", cleaned_text, perl = TRUE)
  cleaned_text <- trimws(cleaned_text)

  if (length(tool_calls) == 0) {
    return(list(tool_calls = NULL, text = text))
  }

  list(tool_calls = tool_calls, text = cleaned_text)
}

#' @keywords internal
recover_text_tool_calls <- function(result) {
  if (!is.null(result$tool_calls) && length(result$tool_calls) > 0) {
    return(result)
  }

  original_text <- result$text %||% ""
  parsed <- parse_tool_call_blocks(result$text %||% "")
  if (is.null(parsed$tool_calls) || length(parsed$tool_calls) == 0) {
    return(result)
  }

  result$tool_calls <- parsed$tool_calls
  result$text <- parsed$text
  result$text_tool_call_explicit <- TRUE
  result$text_tool_call_extra_text <- parsed$text
  result$text_tool_call_protocol_text <- original_text
  if (is.null(result$finish_reason) || !nzchar(result$finish_reason %||% "")) {
    result$finish_reason <- "tool_calls"
  }
  result
}

#' @keywords internal
parse_final_answer_block <- function(text) {
  if (is.null(text) || !nzchar(text)) {
    return(list(final_answer = NULL, text = text, explicit = FALSE))
  }

  matches <- gregexpr("(?is)<final_answer>\\s*.*?\\s*</final_answer>", text, perl = TRUE)[[1]]
  if (length(matches) == 1L && identical(matches[[1]], -1L)) {
    return(list(final_answer = NULL, text = text, explicit = FALSE))
  }

  blocks <- regmatches(text, list(matches))[[1]]
  answers <- vapply(blocks, function(block) {
    inner <- sub("(?is)^\\s*<final_answer>\\s*", "", block, perl = TRUE)
    inner <- sub("(?is)\\s*</final_answer>\\s*$", "", inner, perl = TRUE)
    trimws(inner)
  }, character(1))
  answers <- answers[nzchar(answers)]

  if (length(answers) == 0) {
    return(list(final_answer = NULL, text = text, explicit = FALSE))
  }

  cleaned <- text
  regmatches(cleaned, list(matches)) <- list(rep("", length(blocks)))
  cleaned <- trimws(cleaned)

  list(
    final_answer = paste(answers, collapse = "\n\n"),
    text = cleaned,
    explicit = TRUE
  )
}

#' @keywords internal
recover_text_final_answer <- function(result) {
  original_text <- result$text %||% ""
  parsed <- parse_final_answer_block(result$text %||% "")
  result$final_answer_explicit <- isTRUE(parsed$explicit)
  result$final_answer_extra_text <- parsed$text %||% ""
  if (!isTRUE(parsed$explicit)) {
    return(result)
  }

  result$final_answer_protocol_text <- original_text
  result$text <- parsed$final_answer
  result
}

#' @keywords internal
text_tool_protocol_missing <- function(result, awaiting_protocol = FALSE) {
  if (!isTRUE(awaiting_protocol)) {
    return(FALSE)
  }

  has_tool_call <- length(result$tool_calls %||% list()) > 0
  has_final_answer <- isTRUE(result$final_answer_explicit)
  if (isTRUE(has_tool_call) && isTRUE(has_final_answer)) {
    return(TRUE)
  }
  FALSE
}

#' Create a Text-Tool-Protocol Markup Stream Filter
#'
#' Returns a stateful stream filter that strips text-based tool-protocol
#' markup (`<tool_calls>`, `<tool_call>`) from streamed model output and
#' unwraps `<final_answer>` content, so raw protocol tags never reach the
#' display. Part of the package-author extension API.
#'
#' @return An environment with a `process(text, done = FALSE)` function
#'   returning display-safe text chunks.
#' @keywords internal
#' @export
new_tool_protocol_markup_filter <- function() {
  tags <- c("tool_calls", "tool_call", "final_answer")
  start_patterns <- paste0("<", tags)
  hidden_tags <- c("tool_calls", "tool_call")
  state <- new.env(parent = emptyenv())
  state$buffer <- ""
  state$current_tag <- NULL

  keep_suffix <- function(text, n) {
    len <- nchar(text, type = "chars")
    if (len <= n) {
      text
    } else {
      substr(text, len - n + 1L, len)
    }
  }

  find_next_start <- function(buffer) {
    positions <- vapply(start_patterns, function(pattern) {
      regexpr(pattern, buffer, fixed = TRUE)[[1]]
    }, integer(1))
    valid <- which(positions > 0)
    if (length(valid) == 0) {
      return(NULL)
    }
    idx <- valid[[which.min(positions[valid])]]
    list(pos = positions[[idx]], tag = tags[[idx]], pattern = start_patterns[[idx]])
  }

  detect_open_tag <- function(buffer, tag, pattern) {
    gt_pos <- regexpr(">", buffer, fixed = TRUE)[[1]]
    if (identical(gt_pos, -1L)) {
      return(NULL)
    }

    open_text <- substr(buffer, 1L, gt_pos)
    if (!grepl(sprintf("^<%s\\s*>$", tag), open_text, perl = TRUE)) {
      return(FALSE)
    }

    list(after = gt_pos + 1L)
  }

  state$process <- function(text, done = FALSE) {
    if (!is.null(text) && nzchar(text)) {
      state$buffer <- paste0(state$buffer, text)
    }

    out <- ""

    repeat {
      if (!is.null(state$current_tag)) {
        end_tag <- paste0("</", state$current_tag, ">")
        end_pos <- regexpr(end_tag, state$buffer, fixed = TRUE)[[1]]
        if (identical(end_pos, -1L)) {
          if (state$current_tag %in% hidden_tags) {
            state$buffer <- keep_suffix(state$buffer, nchar(end_tag) - 1L)
          } else {
            keep <- nchar(end_tag) - 1L
            len <- nchar(state$buffer, type = "chars")
            if (len > keep) {
              safe_len <- len - keep
              out <- paste0(out, substr(state$buffer, 1L, safe_len))
              state$buffer <- substr(state$buffer, safe_len + 1L, len)
            }
          }
          break
        }

        if (!state$current_tag %in% hidden_tags && end_pos > 1L) {
          out <- paste0(out, substr(state$buffer, 1L, end_pos - 1L))
        }
        end_after <- end_pos + nchar(end_tag) - 1L
        state$buffer <- substr(state$buffer, end_after + 1L, nchar(state$buffer))
        state$current_tag <- NULL
        next
      }

      start <- find_next_start(state$buffer)
      if (!is.null(start)) {
        if (start$pos > 1L) {
          out <- paste0(out, substr(state$buffer, 1L, start$pos - 1L))
        }
        state$buffer <- substr(state$buffer, start$pos, nchar(state$buffer))

        open <- detect_open_tag(state$buffer, start$tag, start$pattern)
        if (is.null(open)) {
          break
        }
        if (identical(open, FALSE)) {
          out <- paste0(out, substr(state$buffer, 1L, nchar(start$pattern)))
          state$buffer <- substr(state$buffer, nchar(start$pattern) + 1L, nchar(state$buffer))
          next
        }

        state$buffer <- substr(state$buffer, open$after, nchar(state$buffer))
        state$current_tag <- start$tag
        next
      }

      if (isTRUE(done)) {
        out <- paste0(out, state$buffer)
        state$buffer <- ""
      } else {
        keep <- max(nchar(start_patterns)) - 1L
        len <- nchar(state$buffer, type = "chars")
        if (len > keep) {
          safe_len <- len - keep
          out <- paste0(out, substr(state$buffer, 1L, safe_len))
          state$buffer <- substr(state$buffer, safe_len + 1L, len)
        }
      }
      break
    }

    if (isTRUE(done)) {
      state$buffer <- ""
      state$current_tag <- NULL
    }

    out
  }

  state
}

#' @keywords internal
post_tool_protocol_tool_call_instruction <- function(use_text_tool_fallback = FALSE) {
  if (isTRUE(use_text_tool_fallback)) {
    return(paste(
      "Continue with another tool call:",
      "<tool_call>",
      "{\"name\":\"tool_name\",\"arguments\":{}}",
      "</tool_call>",
      sep = "\n"
    ))
  }

  "Continue with another tool call by using the provider's native/API tool-call interface. Do not write prose while doing so."
}

#' @keywords internal
post_tool_protocol_final_answer_instruction <- function() {
  paste(
    "Or finish the task for the user:",
    "<final_answer>",
    "Your final answer to the user.",
    "</final_answer>",
    sep = "\n"
  )
}

#' @keywords internal
post_tool_protocol_system_prompt <- function(use_text_tool_fallback = FALSE) {
  paste(
    "Post-tool response protocol:",
    "After tool results are provided, choose exactly one next action:",
    "If more tool work is needed, call a tool immediately.",
    "If the task is complete, answer the user directly.",
    "Do not narrate that you will continue unless you also call a tool.",
    post_tool_protocol_tool_call_instruction(use_text_tool_fallback = use_text_tool_fallback),
    "To finish, write the final user-visible answer. You may wrap it in `<final_answer>...</final_answer>`, but plain final text is also valid.",
    sep = "\n\n"
  )
}

#' @keywords internal
append_post_tool_protocol_message <- function(messages, use_text_tool_fallback = FALSE) {
  content <- paste(
    "Post-tool response protocol:",
    "Return exactly one next action:",
    post_tool_protocol_tool_call_instruction(use_text_tool_fallback = use_text_tool_fallback),
    "Or finish by writing the final answer directly. Optional compatibility form:",
    "<final_answer>",
    "Your final answer to the user.",
    "</final_answer>",
    sep = "\n\n"
  )
  c(messages, list(list(role = "user", content = content)))
}

#' @keywords internal
text_tool_protocol_correction_message <- function(result, use_text_tool_fallback = TRUE) {
  preview_source <- result$final_answer_protocol_text %||%
    result$text_tool_call_protocol_text %||%
    result$text %||%
    ""
  preview <- compact_text_preview(preview_source, width = 800)
  content <- paste(
    "Your previous response after tool results did not follow the required post-tool protocol.",
    "Do not explain the protocol. Re-emit exactly one next action.",
    "Use a tool call if more tool work is needed, or give the final answer directly if the task is complete.",
    "",
    post_tool_protocol_tool_call_instruction(use_text_tool_fallback = use_text_tool_fallback),
    "",
    "Optional final-answer compatibility form:",
    post_tool_protocol_final_answer_instruction(),
    if (nzchar(preview)) paste0("\nPrevious non-protocol response was:\n", preview) else NULL,
    sep = "\n"
  )
  list(role = "user", content = content)
}

#' @keywords internal
native_tool_calling_enabled <- function(model) {
  !identical(model$capabilities$native_tool_calling %||% TRUE, FALSE)
}

#' @keywords internal
format_tools_for_text_fallback <- function(tools) {
  if (is.null(tools) || length(tools) == 0) {
    return("")
  }

  rendered <- vapply(tools, function(tool_obj) {
    if (!inherits(tool_obj, "Tool")) {
      return("")
    }
    schema_json <- tryCatch(
      schema_to_json(tool_obj$parameters, pretty = TRUE),
      error = function(e) "{}"
    )
    paste0(
      "- ", tool_obj$name, ": ", tool_obj$description, "\n",
      "  Parameters JSON schema:\n",
      gsub("(?m)^", "  ", schema_json, perl = TRUE)
    )
  }, character(1))

  rendered <- rendered[nzchar(rendered)]
  paste(rendered, collapse = "\n\n")
}

#' @keywords internal
build_text_tool_system_prompt <- function(tools) {
  tool_defs <- format_tools_for_text_fallback(tools)
  if (!nzchar(tool_defs)) {
    return("")
  }

  paste(
    "Native API tool calling is unavailable for this model/provider.",
    "When you need a tool, emit one or more tool-call blocks in plain text using exactly this format:",
    "<tool_call>\n{\"name\":\"tool_name\",\"arguments\":{}}\n</tool_call>",
    "Do not wrap ordinary prose in `<tool_call>` blocks.",
    "After tool results are provided, emit exactly one next-action block: either another `<tool_call>` block or a `<final_answer>...</final_answer>` block. Do not write prose outside those tags.",
    paste0("Available tools:\n\n", tool_defs),
    sep = "\n\n"
  )
}

#' @keywords internal
append_text_tool_result_messages <- function(messages, result, tool_results) {
  assistant_text <- result$text %||% ""
  if (nzchar(assistant_text) && length(result$tool_calls %||% list()) == 0) {
    messages <- c(messages, list(list(role = "assistant", content = assistant_text)))
  }

  lines <- c("Tool execution results:")
  for (tr in tool_results) {
    status <- if (isTRUE(tr$is_error)) "error" else "ok"
    lines <- c(
      lines,
      paste0("- ", tr$name, " [", status, "]"),
      tr$result %||% ""
    )
  }
  lines <- c(
    lines,
    "",
    "Post-tool response protocol:",
    "Return exactly one next action:",
    "1. Continue with another tool call:",
    "<tool_call>",
    "{\"name\":\"tool_name\",\"arguments\":{}}",
    "</tool_call>",
    "2. Finish the task for the user by writing the final answer directly.",
    "Optional compatibility form:",
    "<final_answer>",
    "Your final answer to the user.",
    "</final_answer>"
  )

  messages <- c(messages, list(list(role = "user", content = paste(lines, collapse = "\n"))))
  messages
}

#' @title Generate Text
#' @description
#' Generate text using a language model. This is the primary high-level function
#' for non-streaming text generation.
#'
#' When tools are provided, the function automatically executes tool calls and
#' feeds results back to the LLM in a task-state driven runtime. `max_steps`
#' controls one execution window; the runtime may open another window or
#' finalize from tool observations instead of silently stopping at the boundary.
#'
#' @param model Either a LanguageModelV1 object, or a string ID like "openai:gpt-4o".
#' @param prompt A character string prompt, or a list of messages.
#' @param system Optional system prompt.
#' @param temperature Sampling temperature (0-2). Default 0.7.
#' @param max_tokens Maximum tokens to generate.
#' @param tools Optional list of Tool objects for function calling.
#' @param max_steps Number of model/tool steps in one execution window.
#'   Default 1. The runtime treats this as a budget checkpoint, not as a hard
#'   task stop.
#' @param max_tool_result_errors Historical compatibility option. Tool result
#'   errors are recorded as task observations; runtime policy decides whether
#'   to continue, finalize, ask the user, or block.
#' @param require_post_tool_protocol Logical. If TRUE, after any tool results
#'   are returned the model must either make another tool call or wrap its final
#'   answer in a `<final_answer>...</final_answer>` block. This is enabled
#'   automatically for text-based tool fallback.
#' @param sandbox Logical. If TRUE, enables R-native programmatic sandbox mode.
#'   All tools are bound into an isolated R environment and replaced by a single
#'   `execute_r_code` meta-tool. The LLM writes R code to batch-invoke tools,
#'   filter data with dplyr/purrr, and return only summary results, dramatically
#'   reducing token usage and latency. Default FALSE.
#' @param skills Optional path to skills directory, or a SkillRegistry object.
#'   When provided, skill tools are auto-injected and skill summaries are added
#'   to the system prompt.
#' @param session Optional ChatSession object. When provided, tool executions
#'   run in the session's environment, enabling cross-agent data sharing.
#' @param hooks Optional HookHandler object for intercepting events.
#' @param registry Optional ProviderRegistry to use (defaults to global registry).
#' @param ... Additional arguments passed to the model.
#' @return A GenerateResult object with text and optionally tool_calls.
#'   When max_steps > 1 and tools are used, the result includes:
#'   \itemize{
#'     \item steps: Number of steps taken
#'     \item all_tool_calls: List of all tool calls made across all steps
#'   }
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # Using hooks
#'   my_hooks <- create_hooks(
#'     on_generation_start = function(model, prompt, tools) message("Starting..."),
#'     on_tool_start = function(tool, args) message("Calling tool ", tool$name)
#'   )
#'   result <- generate_text(model, "...", hooks = my_hooks)
#' }
#' }
generate_text <- function(model = NULL,
                          prompt,
                          system = NULL,
                          temperature = 0.7,
                          max_tokens = NULL,
                          tools = NULL,
                          max_steps = 1,
                          max_tool_result_errors = 2,
                          require_post_tool_protocol = FALSE,
                          sandbox = FALSE,
                          skills = NULL,
                          session = NULL,
                          hooks = NULL,
                          registry = NULL,
                          ...) {
  requested_model_id <- if (is.character(model) && length(model) == 1) model else NULL
  effective_model_id <- requested_model_id %||% if (is.null(model) && is.null(session)) get_model() else NULL
  default_call_options <- if (is.null(session)) {
    configured <- model_config_runtime_options(effective_model_id)$call_options %||% list()
    if (is.null(model)) {
      merge_call_options(configured, get_default_model_runtime_options()$call_options %||% list())
    } else {
      configured
    }
  } else {
    list()
  }

  # Resolve model from string ID if needed
  model <- resolve_model(model, registry, type = "language")

  # Handle skills parameter
  skill_registry <- NULL
  if (!is.null(skills)) {
    skill_registry <- coerce_skill_registry(skills, recursive = TRUE, project_dir = getwd())

    # Inject skill summaries into system prompt
    skill_prompt <- skill_registry$generate_prompt_section()
    if (nzchar(skill_prompt)) {
      system <- if (is.null(system)) skill_prompt else paste(system, "\n\n", skill_prompt, sep = "")
    }

    # Add skill tools to the tools list
    skill_tools <- create_skill_tools(skill_registry)
    tools <- if (is.null(tools)) skill_tools else c(tools, skill_tools)
  }

  tools <- filter_tools_for_model_capabilities(tools, model, session = session)
  use_text_tool_fallback <- !native_tool_calling_enabled(model)
  require_post_tool_protocol <- isTRUE(require_post_tool_protocol) || isTRUE(use_text_tool_fallback)

  # Handle sandbox mode: bind tools into SandboxManager, replace with meta-tool
  if (isTRUE(sandbox) && !is.null(tools) && length(tools) > 0) {
    parent_env <- if (!is.null(session)) session$get_envir() else NULL
    sandbox_mgr <- SandboxManager$new(
      tools = tools,
      parent_env = parent_env
    )
    # Inject sandbox usage instructions into system prompt
    sandbox_prompt <- create_sandbox_system_prompt(sandbox_mgr)
    system <- if (is.null(system)) sandbox_prompt else paste(system, "\n\n", sandbox_prompt, sep = "")
    # Replace all tools with the single execute_r_code meta-tool
    tools <- list(create_r_code_tool(sandbox_mgr))
  }

  # Trigger on_generation_start
  if (!is.null(hooks)) {
    hooks$trigger_generation_start(model, prompt, tools)
  }

  if (isTRUE(use_text_tool_fallback) && !is.null(tools) && length(tools) > 0) {
    tool_prompt <- build_text_tool_system_prompt(tools)
    if (nzchar(tool_prompt)) {
      system <- if (is.null(system)) tool_prompt else paste(system, "\n\n", tool_prompt, sep = "")
    }
  }
  if (isTRUE(require_post_tool_protocol) && !is.null(tools) && length(tools) > 0) {
    protocol_prompt <- post_tool_protocol_system_prompt(use_text_tool_fallback = use_text_tool_fallback)
    system <- if (is.null(system)) protocol_prompt else paste(system, "\n\n", protocol_prompt, sep = "")
  }

  # Build initial messages
  messages <- build_messages(prompt, system)
  validate_model_messages(model, messages)

  # Build base params (tools stay constant across steps)
  base_params <- merge_call_options(
    default_call_options,
    list(
      temperature = temperature,
      max_tokens = max_tokens,
      tools = if (isTRUE(use_text_tool_fallback)) NULL else tools,
      ...
    )
  )

  initial_messages_len <- length(messages)
  run_id <- paste0("run_", generate_stable_id("generate_text", Sys.time(), stats::runif(1)))

  result <- run_agent_runtime(
    model = model,
    messages = messages,
    base_params = base_params,
    tools = tools,
    session = session,
    hooks = hooks,
    stream = FALSE,
    run_id = run_id,
    max_steps = max_steps,
    max_tool_result_errors = max_tool_result_errors,
    require_post_tool_protocol = require_post_tool_protocol,
    use_text_tool_fallback = use_text_tool_fallback,
    initial_messages_len = initial_messages_len
  )

  # Trigger on_generation_end
  if (!is.null(hooks)) {
    hooks$trigger_generation_end(result)
  }

  result
}

#' @title Stream Text
#' @description
#' Generate text using a language model with streaming output.
#' This function provides a real-time stream of tokens through a callback.
#'
#' @param model Either a LanguageModelV1 object, or a string ID like "openai:gpt-4o".
#' @param prompt A character string prompt, or a list of messages.
#' @param callback A function called for each text chunk: \code{callback(text, done)}.
#' @param system Optional system prompt.
#' @param temperature Sampling temperature (0-2). Default 0.7.
#' @param max_tokens Maximum tokens to generate.
#' @param tools Optional list of Tool objects for function calling.
#' @param max_steps Number of model/tool steps in one execution window.
#'   Default 1. The runtime treats this as a budget checkpoint, not as a hard
#'   task stop.
#' @param max_tool_result_errors Historical compatibility option. Tool result
#'   errors are recorded as task observations; runtime policy decides whether
#'   to continue, finalize, ask the user, or block.
#' @param require_post_tool_protocol Logical. If TRUE, after any tool results
#'   are returned the model must either make another tool call or wrap its final
#'   answer in a `<final_answer>...</final_answer>` block. This is enabled
#'   automatically for text-based tool fallback.
#' @param sandbox Logical. If TRUE, enables R-native programmatic sandbox mode.
#'   See \code{generate_text} for details. Default FALSE.
#' @param skills Optional path to skills directory, or a SkillRegistry object.
#' @param session Optional ChatSession object for shared state.
#' @param hooks Optional HookHandler object.
#' @param registry Optional ProviderRegistry to use.
#' @param renderer Optional [Renderer] for agent output. Defaults to the cli
#'   terminal backend ([create_stream_renderer()]); pass any Renderer-conforming
#'   object (e.g. from a web UI, [create_capture_renderer()], or
#'   [create_null_renderer()]) to render agent output elsewhere.
#' @param .stream_event_callback Internal callback for typed stream events.
#' @param ... Additional arguments passed to the model.
#' @return A GenerateResult object (accumulated from the stream).
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   model <- create_openai()$language_model("gpt-4o")
#'   stream_text(model, "Tell me a story", callback = function(text, done) {
#'     if (!done) cat(text)
#'   })
#' }
#' }
stream_text <- function(model = NULL,
                        prompt,
                        callback = NULL,
                        system = NULL,
                        temperature = 0.7,
                        max_tokens = NULL,
                        tools = NULL,
                        max_steps = 1,
                        max_tool_result_errors = 2,
                        require_post_tool_protocol = FALSE,
                        sandbox = FALSE,
                        skills = NULL,
                        session = NULL,
                        hooks = NULL,
                        registry = NULL,
                        renderer = NULL,
                        .stream_event_callback = NULL,
                        ...) {
  requested_model_id <- if (is.character(model) && length(model) == 1) model else NULL
  effective_model_id <- requested_model_id %||% if (is.null(model) && is.null(session)) get_model() else NULL
  default_call_options <- if (is.null(session)) {
    configured <- model_config_runtime_options(effective_model_id)$call_options %||% list()
    if (is.null(model)) {
      merge_call_options(configured, get_default_model_runtime_options()$call_options %||% list())
    } else {
      configured
    }
  } else {
    list()
  }

  model <- resolve_model(model, registry, type = "language")

  # Handle skills parameter
  skill_registry <- NULL
  if (!is.null(skills)) {
    skill_registry <- coerce_skill_registry(skills, recursive = TRUE, project_dir = getwd())

    # Inject skill summaries into system prompt
    skill_prompt <- skill_registry$generate_prompt_section()
    if (nzchar(skill_prompt)) {
      system <- if (is.null(system)) skill_prompt else paste(system, "\n\n", skill_prompt, sep = "")
    }

    # Add skill tools to the tools list
    skill_tools <- create_skill_tools(skill_registry)
    tools <- if (is.null(tools)) skill_tools else c(tools, skill_tools)
  }

  tools <- filter_tools_for_model_capabilities(tools, model, session = session)
  use_text_tool_fallback <- !native_tool_calling_enabled(model)
  require_post_tool_protocol <- isTRUE(require_post_tool_protocol) || isTRUE(use_text_tool_fallback)

  # Handle sandbox mode: bind tools into SandboxManager, replace with meta-tool
  if (isTRUE(sandbox) && !is.null(tools) && length(tools) > 0) {
    parent_env <- if (!is.null(session)) session$get_envir() else NULL
    sandbox_mgr <- SandboxManager$new(
      tools = tools,
      parent_env = parent_env
    )
    # Inject sandbox usage instructions into system prompt
    sandbox_prompt <- create_sandbox_system_prompt(sandbox_mgr)
    system <- if (is.null(system)) sandbox_prompt else paste(system, "\n\n", sandbox_prompt, sep = "")
    # Replace all tools with the single execute_r_code meta-tool
    tools <- list(create_r_code_tool(sandbox_mgr))
  }

  # Trigger on_generation_start
  if (!is.null(hooks)) {
    hooks$trigger_generation_start(model, prompt, tools)
  }

  if (isTRUE(use_text_tool_fallback) && !is.null(tools) && length(tools) > 0) {
    tool_prompt <- build_text_tool_system_prompt(tools)
    if (nzchar(tool_prompt)) {
      system <- if (is.null(system)) tool_prompt else paste(system, "\n\n", tool_prompt, sep = "")
    }
  }
  if (isTRUE(require_post_tool_protocol) && !is.null(tools) && length(tools) > 0) {
    protocol_prompt <- post_tool_protocol_system_prompt(use_text_tool_fallback = use_text_tool_fallback)
    system <- if (is.null(system)) protocol_prompt else paste(system, "\n\n", protocol_prompt, sep = "")
  }

  messages <- build_messages(prompt, system)
  validate_model_messages(model, messages)

  # Build base params
  base_params <- merge_call_options(
    default_call_options,
    list(
      temperature = temperature,
      max_tokens = max_tokens,
      tools = if (isTRUE(use_text_tool_fallback)) NULL else tools,
      ...
    )
  )

  initial_messages_len <- length(messages)
  run_id <- paste0("run_", generate_stable_id("stream_text", Sys.time(), stats::runif(1)))

  # Agent output is rendered through the UI-agnostic Renderer contract. Default
  # to the built-in cli/terminal backend; callers (e.g. aisdk.shiny, a custom UI,
  # or a capture/null renderer) can inject any Renderer-conforming object.
  if (is.null(renderer)) {
    renderer <- create_stream_renderer()
  }

  result <- run_agent_runtime(
    model = model,
    messages = messages,
    base_params = base_params,
    tools = tools,
    session = session,
    hooks = hooks,
    stream = TRUE,
    callback = callback,
    renderer = renderer,
    run_id = run_id,
    max_steps = max_steps,
    max_tool_result_errors = max_tool_result_errors,
    require_post_tool_protocol = require_post_tool_protocol,
    use_text_tool_fallback = use_text_tool_fallback,
    initial_messages_len = initial_messages_len,
    stream_event_callback = .stream_event_callback
  )

  # Trigger on_generation_end
  if (!is.null(hooks)) {
    hooks$trigger_generation_end(result)
  }

  result
}

#' @title Create Embeddings
#' @description
#' Generate embeddings for text using an embedding model.
#'
#' @param model Either an EmbeddingModelV1 object, or a string ID like "openai:text-embedding-3-small".
#' @param value A character string or vector to embed.
#' @param registry Optional ProviderRegistry to use.
#' @return A list with embeddings and usage information.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   model <- create_openai()$embedding_model("text-embedding-3-small")
#'   result <- create_embeddings(model, "Hello, world!")
#'   print(length(result$embeddings[[1]]))
#' }
#' }
create_embeddings <- function(model, value, registry = NULL) {
  model <- resolve_model(model, registry, type = "embedding")
  model$do_embed(value)
}

# --- Internal Helper Functions ---

#' @keywords internal
handle_network_error <- function(e, rethrow = TRUE) {
  # Check for common network error patterns
  is_network_error <- is_network_error_condition(e)

  if (is_network_error) {
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_alert_danger("Network Connection Interrupted")
      cli::cli_alert_info("Don't worry! This process is designed to be resilient.")
      cli::cli_ul()
      cli::cli_li("If you were running a task, it is safe to re-run.")
      cli::cli_li("Option 1: Simply run the last command again.")
      cli::cli_li("Option 2: Ask the agent to 'Continue where you left off'.")
      cli::cli_end()
    } else {
      message("\n!!! Network Connection Interrupted !!!")
      message("Don't worry! It is safe to re-run this task.")
      message("Option 1: Simply run the last command again.")
      message("Option 2: Ask the agent to 'Continue where you left off'.\n")
    }
  }

  if (isTRUE(rethrow)) {
    rlang::cnd_signal(e)
  }

  invisible(is_network_error)
}

#' @keywords internal
resolve_model <- function(model, registry = NULL, type = c("language", "embedding", "image")) {
  type <- match.arg(type)

  if (is.null(model)) {
    if (type == "language") {
      model <- get_model()
    } else if (type == "image") {
      rlang::abort("No image model configured. Please supply `model` explicitly.")
    } else {
      rlang::abort("No embedding model configured. Please supply `model` explicitly.")
    }
  }

  if (is.character(model)) {
    # Model is a string ID, resolve from registry
    reg <- registry %||% get_default_registry()
    if (type == "language") {
      model <- reg$language_model(model)
    } else if (type == "image") {
      model <- reg$image_model(model)
    } else {
      model <- reg$embedding_model(model)
    }
  }

  # Validate model type
  expected_class <- switch(type,
    language = "LanguageModelV1",
    embedding = "EmbeddingModelV1",
    image = "ImageModelV1"
  )
  if (!inherits(model, expected_class)) {
    rlang::abort(paste0("Expected a ", expected_class, " object."))
  }

  if (identical(type, "language")) {
    model <- enrich_language_model_capabilities(model)
  }

  model
}

#' @keywords internal
enrich_language_model_capabilities <- function(model) {
  if (!inherits(model, "LanguageModelV1")) {
    return(model)
  }

  provider <- model$provider %||% NULL
  model_id <- model$model_id %||% NULL
  if (is.null(provider) || is.null(model_id) || !nzchar(provider) || !nzchar(model_id)) {
    return(model)
  }

  info <- tryCatch(
    get_model_info(provider, model_id),
    error = function(e) NULL
  )
  config_caps <- info$capabilities %||% list()
  if (length(config_caps) == 0) {
    return(model)
  }

  model$capabilities <- utils::modifyList(
    config_caps,
    model$capabilities %||% list(),
    keep.null = TRUE
  )
  model
}

#' @keywords internal
model_capability_value <- function(model, capability, registry = NULL) {
  if (inherits(model, "LanguageModelV1")) {
    model <- enrich_language_model_capabilities(model)
    caps <- model$capabilities %||% list()
    return(caps[[capability]] %||% NULL)
  }

  if (!is.character(model) || length(model) == 0 || !nzchar(model[[1]])) {
    return(NULL)
  }

  model_id <- model[[1]]
  sep_pos <- regexpr(":", model_id, fixed = TRUE)
  if (sep_pos < 1) {
    return(NULL)
  }

  provider <- substr(model_id, 1, sep_pos - 1)
  provider_model <- substr(model_id, sep_pos + 1, nchar(model_id))
  info <- tryCatch(
    get_model_info(provider, provider_model),
    error = function(e) NULL
  )
  caps <- info$capabilities %||% list()
  caps[[capability]] %||% NULL
}

#' Does a Model Explicitly Lack a Capability?
#'
#' Looks up the model's metadata and returns `TRUE` only when the capability
#' flag is explicitly `FALSE` (unknown capabilities are not treated as
#' unavailable). Part of the package-author extension API.
#'
#' @param model A model id string (`"provider:model"`) or model object.
#' @param capability Capability flag name, e.g. `"native_tools"`.
#' @param registry Optional provider registry to resolve against.
#' @return `TRUE` if the capability is explicitly marked unavailable.
#' @keywords internal
#' @export
model_capability_explicitly_unavailable <- function(model, capability, registry = NULL) {
  identical(model_capability_value(model, capability, registry = registry), FALSE)
}

#' @keywords internal
tool_required_model_capabilities <- function(tool_obj) {
  if (is.null(tool_obj) || is.null(tool_obj$meta) || !is.list(tool_obj$meta)) {
    return(character(0))
  }

  req <- tool_obj$meta$required_model_capabilities %||%
    tool_obj$meta$requires_model_capabilities %||%
    character(0)
  unique(as.character(req))
}

#' @keywords internal
tool_model_capability_route <- function(tool_obj) {
  if (is.null(tool_obj) || is.null(tool_obj$meta) || !is.list(tool_obj$meta)) {
    return(NULL)
  }

  route <- tool_obj$meta$model_capability_route %||%
    tool_obj$meta$capability_model_route %||%
    tool_obj$meta$model_route %||%
    NULL

  if (is.null(route) || !is.character(route) || length(route) != 1 || !nzchar(trimws(route))) {
    return(NULL)
  }
  normalize_capability_name(route)
}

#' @keywords internal
tool_has_compatible_capability_route <- function(tool_obj, required_model_capabilities, session = NULL) {
  route <- tool_model_capability_route(tool_obj)
  if (is.null(route)) {
    return(FALSE)
  }

  selected <- select_model_ref_for_capability(
    capability = route,
    session = session,
    fallback_model = NULL,
    default_model = NULL
  )
  if (is.null(selected$model)) {
    return(FALSE)
  }

  !any(vapply(
    required_model_capabilities,
    function(capability) model_ref_capability_explicitly_unavailable(selected$model, capability),
    logical(1)
  ))
}

#' @keywords internal
filter_tools_for_model_capabilities <- function(tools, model, session = NULL) {
  if (is.null(tools) || length(tools) == 0) {
    return(tools)
  }

  filtered <- Filter(function(tool_obj) {
    req <- tool_required_model_capabilities(tool_obj)
    if (length(req) == 0) {
      return(TRUE)
    }

    unavailable <- vapply(
      req,
      function(capability) model_capability_explicitly_unavailable(model, capability),
      logical(1)
    )

    if (!any(unavailable)) {
      return(TRUE)
    }

    tool_has_compatible_capability_route(tool_obj, req, session = session)
  }, tools)

  if (length(filtered) == 0) {
    return(list())
  }
  filtered
}

#' @keywords internal
build_messages <- function(prompt, system = NULL) {
  if (is.list(prompt) && all(sapply(prompt, function(x) is.list(x) && "role" %in% names(x)))) {
    # prompt is already a list of messages
    messages <- prompt
    if (!is.null(system)) {
      messages <- c(list(list(role = "system", content = system)), messages)
    }
  } else if (is.character(prompt)) {
    # prompt is a string
    messages <- list()
    if (!is.null(system)) {
      messages <- c(messages, list(list(role = "system", content = system)))
    }
    messages <- c(messages, list(list(role = "user", content = prompt)))
  } else {
    rlang::abort("prompt must be a character string or a list of message objects.")
  }

  messages
}

#' @keywords internal
build_messages_added <- function(messages, initial_len, final_text = NULL, final_reasoning = NULL) {
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

# Null-coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

#' @keywords internal
print_tool_execution <- function(name, arguments) {
  cli_tool_start(name, arguments)
}

#' @keywords internal
print_tool_result <- function(name, result, success = TRUE, raw_result = result) {
  cli_tool_result(name, result, success = success, raw_result = raw_result)
}
