#' @title Content Translation Helpers
#' @description
#' Internal helpers for translating provider-neutral content blocks into
#' provider-specific message payloads.
#' @name content_translation
#' @keywords internal
NULL

#' Translate content blocks into a provider-specific payload
#'
#' Part of the companion-package extension API (used by \pkg{aisdk.shiny}).
#' Translates provider-neutral content blocks into the message payload shape
#' expected by a given provider family.
#' @param content Provider-neutral content (a string or a list of content blocks).
#' @param target Target provider format.
#' @return The translated, provider-specific content.
#' @keywords internal
#' @export
translate_message_content <- function(content, target = c("openai_chat", "openai_responses", "gemini", "anthropic")) {
  target <- match.arg(target)

  if (is.null(content)) {
    return(content)
  }

  if (is.character(content)) {
    blocks <- normalize_content_blocks(content)
  } else if (is_content_block(content) && is_supported_content_block(content)) {
    blocks <- normalize_content_blocks(list(content))
  } else if (is_supported_block_payload(content)) {
    blocks <- normalize_content_blocks(content)
  } else {
    return(content)
  }

  unname(switch(target,
    openai_chat = translate_blocks_openai_chat(blocks),
    openai_responses = translate_blocks_openai_responses(blocks),
    gemini = translate_blocks_gemini(blocks),
    anthropic = translate_blocks_anthropic(blocks)
  ))
}

#' @keywords internal
is_supported_block_payload <- function(content) {
  is_content_block_list(content)
}

#' @keywords internal
translate_blocks_openai_chat <- function(blocks) {
  lapply(blocks, function(block) {
    if (identical(block$type, "input_text")) {
      return(list(type = "text", text = block$text))
    }

    image_payload <- list(
      type = "image_url",
      image_url = list(url = block_to_url(block))
    )
    if (!is.null(block$detail)) {
      image_payload$image_url$detail <- block$detail
    }
    image_payload
  })
}

#' @keywords internal
translate_blocks_openai_responses <- function(blocks) {
  lapply(blocks, function(block) {
    if (identical(block$type, "input_text")) {
      return(list(type = "input_text", text = block$text))
    }

    image_payload <- list(
      type = "input_image",
      image_url = block_to_url(block)
    )
    if (!is.null(block$detail)) {
      image_payload$detail <- block$detail
    }
    image_payload
  })
}

#' @keywords internal
translate_blocks_gemini <- function(blocks) {
  lapply(blocks, function(block) {
    if (identical(block$type, "input_text")) {
      return(list(text = block$text))
    }

    if (identical(block$source$kind, "url")) {
      return(list(
        fileData = list(
          mimeType = block$media_type,
          fileUri = block$value
        )
      ))
    }

    data <- block_to_base64(block)
    list(
      inlineData = list(
        mimeType = block$media_type,
        data = data
      )
    )
  })
}

#' @keywords internal
translate_blocks_anthropic <- function(blocks) {
  lapply(blocks, function(block) {
    if (identical(block$type, "input_text")) {
      return(list(type = "text", text = block$text))
    }

    if (identical(block$source$kind, "url")) {
      return(list(
        type = "image",
        source = list(
          type = "url",
          url = block$value
        )
      ))
    }

    list(
      type = "image",
      source = list(
        type = "base64",
        media_type = block$media_type,
        data = block_to_base64(block)
      )
    )
  })
}

#' @keywords internal
block_to_url <- function(block) {
  if (identical(block$source$kind, "url") || identical(block$source$kind, "data_uri")) {
    return(block$value)
  }

  paste0("data:", block$media_type, ";base64,", block_to_base64(block))
}

#' @keywords internal
block_to_base64 <- function(block) {
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    rlang::abort("Package `base64enc` is required for local multimodal image support.")
  }

  if (identical(block$source$kind, "file")) {
    return(base64enc::base64encode(block$value))
  }

  if (identical(block$source$kind, "data_uri")) {
    return(sub("^data:[^;]+;base64,", "", block$value, ignore.case = TRUE))
  }

  rlang::abort(paste0("Cannot convert image source kind `", block$source$kind, "` to base64."))
}

#' Normalize a portable `tool_choice` into a provider-specific shape
#'
#' A unified `tool_choice` (Vercel-AI-SDK style) that behaves the same across
#' providers. Each wire API spells this differently — OpenAI uses a top-level
#' `tool_choice` string/object, Anthropic a `{type: ...}` object, Gemini a
#' `toolConfig.functionCallingConfig.mode` — so hand-written provider-native
#' values do not port. This maps the unified forms to the target's shape and
#' passes anything it does not recognize through unchanged (so existing code
#' that already writes a provider-native value keeps working).
#'
#' Unified forms:
#' \itemize{
#'   \item `"auto"` — the model decides (provider default)
#'   \item `"required"` / `"any"` — force the model to call some tool
#'   \item `"none"` — forbid tool calls
#'   \item `list(type = "tool", name = "X")` — force a specific tool
#'     (also accepts `toolName =` or a bare `list(tool = "X")`)
#' }
#'
#' @param choice The unified value (or a provider-native value to pass through).
#' @param target One of "openai_chat", "openai_responses", "anthropic", "gemini".
#' @param parallel Optional logical: when `FALSE`, request that the model not
#'   emit parallel tool calls. Only Anthropic carries this inside `tool_choice`
#'   (`disable_parallel_tool_use`); OpenAI uses a separate top-level
#'   `parallel_tool_calls` and Gemini has no equivalent.
#' @return The provider-shaped value to place at the provider's tool-choice
#'   slot, or `NULL` when there is nothing to set.
#' @keywords internal
normalize_tool_choice <- function(choice, target, parallel = NULL) {
  target <- match.arg(target, c("openai_chat", "openai_responses", "anthropic", "gemini"))
  no_parallel <- isFALSE(parallel)

  # Anthropic is the only target that expresses "no parallel tool calls" inside
  # tool_choice, so `parallel = FALSE` alone (no explicit choice) still needs a
  # carrier there — default it to auto.
  if (is.null(choice)) {
    if (identical(target, "anthropic") && no_parallel) {
      return(list(type = "auto", disable_parallel_tool_use = TRUE))
    }
    return(NULL)
  }

  # Parse the unified value into (mode, tool_name). A value we do not recognize
  # is assumed to already be provider-native and returned unchanged.
  mode <- NULL
  tool_name <- NULL
  if (is.character(choice) && length(choice) == 1) {
    key <- tolower(trimws(choice))
    if (key %in% c("auto", "none")) {
      mode <- key
    } else if (key %in% c("required", "any")) {
      mode <- "required"
    }
  } else if (is.list(choice)) {
    ctype <- tolower(as.character(choice$type %||% ""))
    if (identical(ctype, "tool") || !is.null(choice$toolName) ||
        (!is.null(choice$tool) && is.null(choice$type))) {
      mode <- "tool"
      tool_name <- choice$name %||% choice$toolName %||% choice$tool
    }
  }

  if (is.null(mode)) {
    return(choice) # already provider-native (or unrecognized): pass through
  }

  switch(target,
    openai_chat = {
      if (mode == "tool") {
        list(type = "function", `function` = list(name = tool_name))
      } else if (mode == "required") {
        "required"
      } else {
        mode
      }
    },
    openai_responses = {
      if (mode == "tool") {
        list(type = "function", name = tool_name)
      } else if (mode == "required") {
        "required"
      } else {
        mode
      }
    },
    anthropic = {
      tc <- switch(mode,
        auto = list(type = "auto"),
        none = list(type = "none"),
        required = list(type = "any"),
        tool = list(type = "tool", name = tool_name)
      )
      if (no_parallel && mode != "none") {
        tc$disable_parallel_tool_use <- TRUE
      }
      tc
    },
    gemini = {
      fcc <- switch(mode,
        auto = list(mode = "AUTO"),
        none = list(mode = "NONE"),
        required = list(mode = "ANY"),
        tool = list(mode = "ANY", allowedFunctionNames = list(tool_name))
      )
      list(functionCallingConfig = fcc)
    }
  )
}
