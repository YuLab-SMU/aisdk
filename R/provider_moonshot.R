#' @name provider_moonshot
#' @title Moonshot / Kimi Provider
#' @description
#' Implementation for Moonshot AI Kimi models. The public Kimi Open Platform
#' uses `https://api.moonshot.cn/v1`; Kimi Code membership API uses
#' `https://api.kimi.com/coding/v1`. These are separate account systems and
#' their API keys are not interchangeable.
#' @keywords internal
NULL

moonshot_base_url_kind <- function(base_url) {
  base <- tolower(base_url %||% "")
  if (grepl("api\\.kimi\\.com/coding", base)) {
    return("coding")
  }
  if (grepl("api\\.moonshot\\.cn", base)) {
    return("platform")
  }
  "unknown"
}

moonshot_key_kind <- function(api_key) {
  key <- api_key %||% ""
  if (!nzchar(key)) {
    return("unknown")
  }
  if (grepl("^sk-kimi-", key)) {
    return("coding")
  }
  "platform"
}

moonshot_first_nonempty <- function(...) {
  values <- list(...)
  for (value in values) {
    if (!is.null(value) && length(value) == 1 && nzchar(value)) {
      return(value)
    }
  }
  ""
}

moonshot_normalize_thinking <- function(thinking) {
  if (is.null(thinking)) {
    return(NULL)
  }
  if (is.logical(thinking)) {
    if (isTRUE(thinking)) {
      return(list(type = "enabled"))
    }
    return(list(type = "disabled"))
  }
  thinking
}

#' @title Moonshot Language Model Class
#' @description Language model implementation for Moonshot / Kimi Chat Completions APIs.
#' @keywords internal
MoonshotLanguageModel <- R6::R6Class(
  "MoonshotLanguageModel",
  inherit = OpenAILanguageModel,
  private = list(
    is_temperature_locked_model = function() {
      grepl("^kimi-k2|^kimi-for-coding$", self$model_id, ignore.case = TRUE)
    },

    finalize_payload = function(payload, params) {
      if (!is.null(payload$body$max_tokens) && is.null(payload$body$max_completion_tokens)) {
        payload$body$max_completion_tokens <- payload$body$max_tokens
        payload$body$max_tokens <- NULL
      }

      thinking <- list_get_exact(params, "thinking")
      if (!is.null(thinking)) {
        payload$body$thinking <- moonshot_normalize_thinking(thinking)
      }

      if (
        identical(private$config$api_kind, "coding") &&
          is.null(payload$body$prompt_cache_key) &&
          nzchar(private$config$prompt_cache_key %||% "")
      ) {
        payload$body$prompt_cache_key <- private$config$prompt_cache_key
      }

      if (private$is_temperature_locked_model() && !is.null(payload$body$temperature)) {
        payload$body$temperature <- 1
      }

      payload
    }
  ),
  public = list(
    #' @description Initialize the Moonshot language model.
    #' @param model_id The model ID.
    #' @param config Provider configuration.
    initialize = function(model_id, config) {
      private$config <- config
      is_kimi_k2 <- grepl("^kimi-k2", model_id, ignore.case = TRUE)
      has_vision <- is_kimi_k2 ||
        grepl("^moonshot-v1-.*vision", model_id, ignore.case = TRUE)
      has_reasoning <- is_kimi_k2 ||
        grepl("thinking", model_id, ignore.case = TRUE)

      super$initialize(
        model_id = model_id,
        config = config,
        capabilities = list(
          is_reasoning_model = TRUE,
          reasoning = has_reasoning,
          preserve_reasoning_content = TRUE,
          vision_input = has_vision,
          function_call = TRUE,
          structured_output = TRUE
        )
      )
    },

    #' @description Build the request payload for non-streaming generation.
    #' @param params A list of call options.
    #' @return A list with url, headers, and body.
    build_payload = function(params) {
      private$finalize_payload(super$build_payload(params), params)
    },

    #' @description Build the request payload for streaming generation.
    #' @param params A list of call options.
    #' @return A list with url, headers, and body.
    build_stream_payload = function(params) {
      private$finalize_payload(super$build_stream_payload(params), params)
    },

    #' @description Parse the API response into a GenerateResult.
    #' @param response The parsed API response.
    #' @return A GenerateResult object.
    parse_response = function(response) {
      result <- super$parse_response(response)
      choice <- response$choices[[1]]
      if (!is.null(choice$message$reasoning_content)) {
        result$reasoning <- choice$message$reasoning_content
      }
      result
    }
  )
)

#' @title Moonshot / Kimi Provider Class
#' @description Provider class for Moonshot AI Kimi models.
#' @export
MoonshotProvider <- R6::R6Class(
  "MoonshotProvider",
  inherit = OpenAIProvider,
  public = list(
    #' @description Initialize the Moonshot provider.
    #' @param api_key API key. Defaults to MOONSHOT_API_KEY for the Kimi Open
    #'   Platform, or KIMI_API_KEY / KIMI_CODE_API_KEY for Kimi Code.
    #' @param base_url Base URL.
    #' @param platform API platform: "auto", "platform", or "coding".
    #' @param headers Optional additional headers.
    #' @param prompt_cache_key Default prompt cache key for Kimi Code requests.
    #' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
    #' @param total_timeout_seconds Optional total request timeout in seconds.
    #' @param first_byte_timeout_seconds Optional time-to-first-byte timeout.
    #' @param connect_timeout_seconds Optional connection-establishment timeout.
    #' @param idle_timeout_seconds Optional stall timeout.
    initialize = function(api_key = NULL,
                          base_url = NULL,
                          platform = c("auto", "platform", "coding"),
                          headers = NULL,
                          prompt_cache_key = NULL,
                          timeout_seconds = NULL,
                          total_timeout_seconds = NULL,
                          first_byte_timeout_seconds = NULL,
                          connect_timeout_seconds = NULL,
                          idle_timeout_seconds = NULL) {
      platform <- match.arg(platform)
      env_base_url <- Sys.getenv("MOONSHOT_BASE_URL", unset = "")
      env_kimi_base_url <- Sys.getenv("KIMI_BASE_URL", unset = "")
      env_kimi_code_base_url <- Sys.getenv("KIMI_CODE_BASE_URL", unset = "")
      base_url_input <- moonshot_first_nonempty(base_url, env_base_url, env_kimi_base_url, env_kimi_code_base_url)

      if (identical(platform, "auto")) {
        inferred_from_base <- moonshot_base_url_kind(base_url_input)
        if (!identical(inferred_from_base, "unknown")) {
          platform <- inferred_from_base
        } else if (
          !nzchar(Sys.getenv("MOONSHOT_API_KEY", unset = "")) &&
            (nzchar(Sys.getenv("KIMI_API_KEY", unset = "")) ||
              nzchar(Sys.getenv("KIMI_CODE_API_KEY", unset = "")))
        ) {
          platform <- "coding"
        } else {
          platform <- "platform"
        }
      }

      if (identical(platform, "coding")) {
        if (is.null(base_url)) {
          base_url <- if (nzchar(env_kimi_base_url)) {
            env_kimi_base_url
          } else if (nzchar(env_kimi_code_base_url)) {
            env_kimi_code_base_url
          } else {
            "https://api.kimi.com/coding/v1"
          }
        }
        api_key <- moonshot_first_nonempty(
          api_key,
          Sys.getenv("KIMI_API_KEY", unset = ""),
          Sys.getenv("KIMI_CODE_API_KEY", unset = "")
        )
        prompt_cache_key <- moonshot_first_nonempty(
          prompt_cache_key,
          Sys.getenv("KIMI_PROMPT_CACHE_KEY", unset = ""),
          Sys.getenv("KIMI_CODE_PROMPT_CACHE_KEY", unset = ""),
          "aisdk-session"
        )
        headers <- headers %||% list()
        if (is.null(headers[["User-Agent"]]) && is.null(headers[["user-agent"]])) {
          version <- tryCatch(as.character(utils::packageVersion("aisdk")), error = function(...) "dev")
          headers[["User-Agent"]] <- paste0("aisdk/", version)
        }
      } else {
        base_url <- moonshot_first_nonempty(base_url, env_base_url, "https://api.moonshot.cn/v1")
        api_key <- moonshot_first_nonempty(api_key, Sys.getenv("MOONSHOT_API_KEY", unset = ""))
      }

      url_kind <- moonshot_base_url_kind(base_url)
      key_kind <- moonshot_key_kind(api_key)
      if (!identical(url_kind, "unknown") && !identical(url_kind, platform)) {
        rlang::warn(paste0(
          "Moonshot platform/base_url mismatch: platform = '", platform,
          "' but base_url looks like '", url_kind, "'. ",
          "Kimi Code uses https://api.kimi.com/coding/v1; the Kimi Open Platform uses https://api.moonshot.cn/v1."
        ))
      }
      if (nzchar(api_key %||% "") && !identical(key_kind, "unknown") &&
          !identical(url_kind, "unknown") && !identical(key_kind, url_kind)) {
        rlang::warn(paste0(
          "The API key appears to belong to the ", key_kind,
          " account system, but the base_url points to ", url_kind,
          ". Kimi Code and the Kimi Open Platform use separate keys."
        ))
      }

      suppressWarnings(
        super$initialize(
          api_key = api_key,
          base_url = base_url,
          headers = headers,
          name = if (identical(platform, "coding")) "kimi" else "moonshot",
          timeout_seconds = timeout_seconds,
          total_timeout_seconds = total_timeout_seconds,
          first_byte_timeout_seconds = first_byte_timeout_seconds,
          connect_timeout_seconds = connect_timeout_seconds,
          idle_timeout_seconds = idle_timeout_seconds
        )
      )
      private$config$api_kind <- platform
      private$config$prompt_cache_key <- prompt_cache_key

      if (nchar(private$config$api_key) == 0) {
        if (identical(platform, "coding")) {
          rlang::warn("Kimi Code API key not set. Set KIMI_API_KEY / KIMI_CODE_API_KEY or pass api_key.")
        } else {
          rlang::warn("Moonshot API key not set. Set MOONSHOT_API_KEY or pass api_key.")
        }
      }
    },

    #' @description Create a language model.
    #' @param model_id The model ID.
    #' @return A MoonshotLanguageModel object.
    language_model = function(model_id = NULL) {
      if (is.null(model_id) || !nzchar(model_id)) {
        if (identical(private$config$api_kind, "coding")) {
          model_id <- Sys.getenv("KIMI_MODEL_NAME", unset = "")
          if (!nzchar(model_id)) {
            model_id <- Sys.getenv("KIMI_CODE_MODEL", "kimi-for-coding")
          }
        } else {
          model_id <- Sys.getenv("MOONSHOT_MODEL", "kimi-k2.6")
        }
      }
      MoonshotLanguageModel$new(model_id, private$config)
    }
  )
)

#' @title Kimi Code Anthropic Provider Class
#' @description Anthropic-compatible provider wrapper for Kimi Code.
#' @keywords internal
KimiCodeAnthropicProvider <- R6::R6Class(
  "KimiCodeAnthropicProvider",
  inherit = AnthropicProvider,
  public = list(
    #' @description Create a Kimi Code Anthropic-compatible language model.
    #' @param model_id The model ID. Defaults to `kimi-for-coding`.
    #' @return An AnthropicLanguageModel object.
    language_model = function(model_id = NULL) {
      model_id <- moonshot_first_nonempty(
        model_id,
        Sys.getenv("KIMI_MODEL_NAME", unset = ""),
        Sys.getenv("KIMI_CODE_MODEL", unset = ""),
        "kimi-for-coding"
      )
      super$language_model(model_id)
    }
  )
)

#' @title Create Moonshot / Kimi Provider
#' @description
#' Factory function to create a Moonshot provider. Use `platform = "platform"`
#' for the pay-as-you-go Kimi Open Platform (`https://api.moonshot.cn/v1`) and
#' `platform = "coding"` for Kimi Code membership API
#' (`https://api.kimi.com/coding/v1`). The two platforms use separate API keys.
#'
#' @eval generate_model_docs("moonshot")
#'
#' @param api_key API key. Defaults to MOONSHOT_API_KEY for the Kimi Open
#'   Platform, or KIMI_API_KEY / KIMI_CODE_API_KEY for Kimi Code.
#' @param base_url Base URL for API calls.
#' @param platform API platform: "auto", "platform", or "coding".
#' @param headers Optional additional headers.
#' @param prompt_cache_key Default prompt cache key for Kimi Code requests.
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total request timeout in seconds.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout.
#' @param connect_timeout_seconds Optional connection-establishment timeout.
#' @param idle_timeout_seconds Optional stall timeout.
#' @return A MoonshotProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#' moonshot <- create_moonshot()
#' model <- moonshot$language_model("kimi-k2.6")
#' result <- generate_text(model, "Hello", temperature = 1)
#'
#' kimi_code <- create_moonshot(platform = "coding")
#' coding_model <- kimi_code$language_model()
#' result <- generate_text(coding_model, "Review this function", prompt_cache_key = "task-1")
#' }
#' }
create_moonshot <- function(api_key = NULL,
                            base_url = NULL,
                            platform = c("auto", "platform", "coding"),
                            headers = NULL,
                            prompt_cache_key = NULL,
                            timeout_seconds = NULL,
                            total_timeout_seconds = NULL,
                            first_byte_timeout_seconds = NULL,
                            connect_timeout_seconds = NULL,
                            idle_timeout_seconds = NULL) {
  MoonshotProvider$new(
    api_key = api_key,
    base_url = base_url,
    platform = platform,
    headers = headers,
    prompt_cache_key = prompt_cache_key,
    timeout_seconds = timeout_seconds,
    total_timeout_seconds = total_timeout_seconds,
    first_byte_timeout_seconds = first_byte_timeout_seconds,
    connect_timeout_seconds = connect_timeout_seconds,
    idle_timeout_seconds = idle_timeout_seconds
  )
}

#' @title Create Kimi Code Provider
#' @description
#' Convenience wrapper for Kimi Code membership API. By default this uses the
#' Anthropic-compatible endpoint because it works for self-built coding agents
#' with their real User-Agent. Set `api_format = "openai"` when integrating with
#' OpenAI-compatible tools that Kimi Code recognizes as coding agents.
#' @param api_format API protocol to use: "anthropic" or "openai".
#' @inheritParams create_moonshot
#' @return A provider object configured for Kimi Code.
#' @export
create_kimi_code <- function(api_key = NULL,
                             base_url = NULL,
                             api_format = c("anthropic", "openai"),
                             headers = NULL,
                             prompt_cache_key = NULL,
                             timeout_seconds = NULL,
                             total_timeout_seconds = NULL,
                             first_byte_timeout_seconds = NULL,
                             connect_timeout_seconds = NULL,
                             idle_timeout_seconds = NULL) {
  api_format <- match.arg(api_format)
  if (identical(api_format, "anthropic")) {
    return(create_kimi_code_anthropic(
      api_key = api_key,
      base_url = base_url,
      headers = headers,
      timeout_seconds = timeout_seconds,
      total_timeout_seconds = total_timeout_seconds,
      first_byte_timeout_seconds = first_byte_timeout_seconds,
      connect_timeout_seconds = connect_timeout_seconds,
      idle_timeout_seconds = idle_timeout_seconds
    ))
  }

  create_moonshot(
    api_key = api_key,
    base_url = base_url,
    platform = "coding",
    headers = headers,
    prompt_cache_key = prompt_cache_key,
    timeout_seconds = timeout_seconds,
    total_timeout_seconds = total_timeout_seconds,
    first_byte_timeout_seconds = first_byte_timeout_seconds,
    connect_timeout_seconds = connect_timeout_seconds,
    idle_timeout_seconds = idle_timeout_seconds
  )
}

#' @title Create Kimi Code Provider (Anthropic API Format)
#' @description
#' Convenience wrapper for Kimi Code's Anthropic-compatible endpoint. Use model
#' ID `kimi-for-coding`. The public Kimi docs list the Anthropic base as
#' `https://api.kimi.com/coding/`; aisdk's Anthropic provider appends
#' `/messages` directly, so this wrapper normalizes to
#' `https://api.kimi.com/coding/v1`.
#' @inheritParams create_kimi_code
#' @return An AnthropicProvider object configured for Kimi Code.
#' @export
create_kimi_code_anthropic <- function(api_key = NULL,
                                       base_url = NULL,
                                       headers = NULL,
                                       timeout_seconds = NULL,
                                       total_timeout_seconds = NULL,
                                       first_byte_timeout_seconds = NULL,
                                       connect_timeout_seconds = NULL,
                                       idle_timeout_seconds = NULL) {
  api_key <- moonshot_first_nonempty(
    api_key,
    Sys.getenv("KIMI_API_KEY", unset = ""),
    Sys.getenv("KIMI_CODE_API_KEY", unset = "")
  )
  base_url <- moonshot_first_nonempty(base_url, Sys.getenv("KIMI_ANTHROPIC_BASE_URL", unset = ""), "https://api.kimi.com/coding/v1")
  if (moonshot_key_kind(api_key) != "coding" && nzchar(api_key %||% "")) {
    rlang::warn("Kimi Code Anthropic endpoint requires a Kimi Code API key, not a Kimi Open Platform key.")
  }

  KimiCodeAnthropicProvider$new(
    api_key = api_key,
    base_url = base_url,
    api_version = NULL,
    name = "kimi",
    headers = headers,
    timeout_seconds = timeout_seconds,
    total_timeout_seconds = total_timeout_seconds,
    first_byte_timeout_seconds = first_byte_timeout_seconds,
    connect_timeout_seconds = connect_timeout_seconds,
    idle_timeout_seconds = idle_timeout_seconds
  )
}
