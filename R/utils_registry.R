#' @title Utilities: Provider Registry
#' @description
#' A registry for managing AI model providers.
#' Supports the `provider:model` syntax for accessing models.
#' @name utils_registry
NULL

# Private environment to store the default registry (avoids locked binding issues)
.registry_env <- new.env(parent = emptyenv())

# Extra provider factories registered by companion packages (e.g. aisdk.providers).
# Namespace functions in aisdk cannot see `create_*` exports that live in an
# attached companion package, so those packages push their factories here from
# their `.onLoad` hook via `register_provider()`.
.provider_extras <- new.env(parent = emptyenv())

#' @keywords internal
reset_default_registry <- function() {
  .registry_env$default <- NULL
  invisible(TRUE)
}

#' @title Register a Provider Factory
#' @description
#' Register an additional provider factory so it can be resolved through the
#' default registry's `provider:model` syntax. Intended for companion packages
#' (such as \pkg{aisdk.providers}) that ship OpenAI-compatible providers and
#' register them from their `.onLoad` hook, e.g.
#' `aisdk::register_provider("deepseek", function() create_deepseek())`.
#'
#' Registration is load-order independent: the factory is replayed into the
#' default registry whether it is registered before or after the registry is
#' first built.
#' @param id The provider ID (e.g. "deepseek").
#' @param factory A zero-argument function returning a provider object, or a
#'   function of one argument (`model_id`) returning a language model.
#' @return Invisibly `TRUE`.
#' @export
register_provider <- function(id, factory) {
  if (!is.character(id) || length(id) != 1L || !nzchar(id)) {
    rlang::abort("Provider ID must be a non-empty string.")
  }
  if (!is.function(factory)) {
    rlang::abort("Provider factory must be a function.")
  }
  .provider_extras[[id]] <- factory
  # If the default registry has already been built, register immediately so the
  # provider becomes resolvable without forcing a registry reset.
  if (!is.null(.registry_env$default)) {
    .registry_env$default$register(id, factory)
  }
  invisible(TRUE)
}

# Providers that are not shipped in aisdk core but in companion packages,
# mapped to the package that supplies them. Used to offer an install prompt
# (or a clear hint) when a user requests one of these providers but the
# companion package is not installed. Keep in sync with the companion's
# `.onLoad` registration list.
# A named list (not a character vector) so that a lookup of an unknown provider
# id returns NULL rather than erroring with "subscript out of bounds".
.companion_providers <- list(
  deepseek = "aisdk.providers",
  xai = "aisdk.providers",
  volcengine = "aisdk.providers",
  nvidia = "aisdk.providers",
  stepfun = "aisdk.providers",
  bailian = "aisdk.providers",
  openrouter = "aisdk.providers",
  aihubmix = "aisdk.providers",
  moonshot = "aisdk.providers",
  kimi = "aisdk.providers"
)

# If `provider_id` is supplied by a companion package, make sure that package is
# loaded so it can register itself. When the package is installed we simply load
# it; when it is missing we offer to install it, but only in interactive
# sessions so that scripts, `R CMD check`, and CRAN runs are never interrupted
# or made to perform installs. Returns TRUE if the provider should now be
# resolvable, FALSE otherwise.
#' @keywords internal
ensure_companion_provider <- function(provider_id) {
  pkg <- .companion_providers[[provider_id]]
  if (is.null(pkg)) {
    return(FALSE)
  }
  ensure_companion_package(
    sub("^aisdk\\.", "", pkg),
    reason = sprintf("to use the '%s' provider", provider_id)
  )
}

# Build the rlang::abort() message body for an unresolved provider, appending a
# hint pointing at the companion package when the provider is known to live in
# one.
#' @keywords internal
provider_not_found_message <- function(provider_id, available) {
  msg <- c(
    paste0("Provider not found: ", provider_id),
    "i" = if (nchar(available) > 0) {
      paste0("Available providers: ", available)
    } else {
      "No providers registered."
    }
  )
  pkg <- .companion_providers[[provider_id]]
  if (!is.null(pkg)) {
    msg <- c(
      msg,
      "i" = sprintf(
        "The '%s' provider is supplied by the '%s' package. Install it with install.packages(\"%s\").",
        provider_id, pkg, pkg
      )
    )
  }
  msg
}

#' @keywords internal
create_env_custom_provider <- function() {
  base_url <- Sys.getenv("AISDK_CUSTOM_BASE_URL", unset = "")
  backup_base_urls <- Sys.getenv("AISDK_CUSTOM_BASE_URLS", unset = "")
  if (nzchar(backup_base_urls)) {
    base_url <- paste(c(base_url, backup_base_urls), collapse = ",")
  }
  if (!nzchar(base_url)) {
    rlang::abort("Custom provider is not configured. Set AISDK_CUSTOM_BASE_URL first.")
  }

  api_format <- Sys.getenv("AISDK_CUSTOM_API_FORMAT", unset = "chat_completions")
  if (!api_format %in% c("chat_completions", "responses", "anthropic_messages")) {
    api_format <- "chat_completions"
  }

  create_custom_provider(
    provider_name = "custom",
    base_url = base_url,
    api_key = Sys.getenv("AISDK_CUSTOM_API_KEY", unset = ""),
    api_format = api_format,
    use_max_completion_tokens = tolower(Sys.getenv("AISDK_CUSTOM_USE_MAX_COMPLETION_TOKENS", unset = "false")) %in% c("true", "1", "yes"),
    disable_stream_options = !tolower(Sys.getenv("AISDK_CUSTOM_ENABLE_STREAM_OPTIONS", unset = "false")) %in% c("true", "1", "yes"),
    supports_native_tools = tolower(Sys.getenv("AISDK_CUSTOM_SUPPORTS_NATIVE_TOOLS", unset = "false")) %in% c("true", "1", "yes"),
    responses_state_mode = responses_normalize_state_mode(Sys.getenv("AISDK_CUSTOM_RESPONSES_STATE_MODE", unset = "stateless"))
  )
}

#' @title Provider Registry
#' @description
#' Manages registered providers and allows accessing models by ID.
#' @export
ProviderRegistry <- R6::R6Class(
  "ProviderRegistry",
  private = list(
    providers = list(),
    separator = ":"
  ),
  public = list(
    #' @description Initialize the registry.
    #' @param separator The separator between provider and model IDs (default: ":").
    initialize = function(separator = ":") {
      private$providers <- list()
      private$separator <- separator
    },

    #' @description Register a provider.
    #' @param id The provider ID (e.g., "openai").
    #' @param provider The provider object (must have `language_model` method).
    register = function(id, provider) {
      if (!is.character(id) || nchar(id) == 0) {
        rlang::abort("Provider ID must be a non-empty string.")
      }
      private$providers[[id]] <- provider
      invisible(self)
    },

    #' @description Get a language model by ID.
    #' @param id Model ID in the format "provider:model" (e.g., "openai:gpt-4o").
    #' @return A LanguageModelV1 object.
    language_model = function(id) {
      sep_pos <- regexpr(private$separator, id, fixed = TRUE)
      if (sep_pos < 1) {
        rlang::abort(c(
          paste0("Invalid model ID format: ", id),
          "i" = paste0("Expected format: provider", private$separator, "model"),
          "i" = "Example: openai:gpt-4o"
        ))
      }
      provider_id <- substr(id, 1, sep_pos - 1)
      model_id <- substr(id, sep_pos + 1, nchar(id))

      provider <- private$providers[[provider_id]]
      if (is.null(provider) && ensure_companion_provider(provider_id)) {
        provider <- private$providers[[provider_id]]
      }
      if (is.null(provider)) {
        available <- paste(names(private$providers), collapse = ", ")
        rlang::abort(provider_not_found_message(provider_id, available))
      }

      # Evaluate lazy factories
      if (is.function(provider) && length(formals(provider)) == 0) {
        provider <- provider()
      }

      if (is.function(provider)) {
        return(provider(model_id))
      } else if (inherits(provider, "R6") && !is.null(provider$language_model)) {
        return(provider$language_model(model_id))
      } else {
        rlang::abort(paste0("Provider '", provider_id, "' does not support language models."))
      }
    },

    #' @description Get an embedding model by ID.
    #' @param id Model ID in the format "provider:model".
    #' @return An EmbeddingModelV1 object.
    embedding_model = function(id) {
      sep_pos <- regexpr(private$separator, id, fixed = TRUE)
      if (sep_pos < 1) {
        rlang::abort(paste0("Invalid model ID format: ", id))
      }
      provider_id <- substr(id, 1, sep_pos - 1)
      model_id <- substr(id, sep_pos + 1, nchar(id))

      provider <- private$providers[[provider_id]]
      if (is.null(provider) && ensure_companion_provider(provider_id)) {
        provider <- private$providers[[provider_id]]
      }
      if (is.null(provider)) {
        available <- paste(names(private$providers), collapse = ", ")
        rlang::abort(provider_not_found_message(provider_id, available))
      }

      # Evaluate lazy factories
      if (is.function(provider) && length(formals(provider)) == 0) {
        provider <- provider()
      }

      if (inherits(provider, "R6") && !is.null(provider$embedding_model)) {
        return(provider$embedding_model(model_id))
      } else {
        rlang::abort(paste0("Provider '", provider_id, "' does not support embedding models."))
      }
    },

    #' @description Get an image model by ID.
    #' @param id Model ID in the format "provider:model".
    #' @return An ImageModelV1 object.
    image_model = function(id) {
      sep_pos <- regexpr(private$separator, id, fixed = TRUE)
      if (sep_pos < 1) {
        rlang::abort(c(
          paste0("Invalid model ID format: ", id),
          "i" = paste0("Expected format: provider", private$separator, "model")
        ))
      }
      provider_id <- substr(id, 1, sep_pos - 1)
      model_id <- substr(id, sep_pos + 1, nchar(id))

      provider <- private$providers[[provider_id]]
      if (is.null(provider) && ensure_companion_provider(provider_id)) {
        provider <- private$providers[[provider_id]]
      }
      if (is.null(provider)) {
        available <- paste(names(private$providers), collapse = ", ")
        rlang::abort(provider_not_found_message(provider_id, available))
      }

      if (is.function(provider) && length(formals(provider)) == 0) {
        provider <- provider()
      }

      if (inherits(provider, "R6") && !is.null(provider$image_model)) {
        return(provider$image_model(model_id))
      }

      rlang::abort(paste0("Provider '", provider_id, "' does not support image models."))
    },

    #' @description List all registered provider IDs.
    #' @return A character vector of provider IDs.
    list_providers = function() {
      names(private$providers)
    }
  )
)

#' @title Get Default Registry
#' @description
#' Returns the global default provider registry, creating it if necessary.
#' @return A ProviderRegistry object.
#' @export
get_default_registry <- function() {
  if (is.null(.registry_env$default)) {
    reg <- ProviderRegistry$new()
    # Auto-register default providers lazily to ensure fresh environment variables
    tryCatch(
      {
        # Built-in providers shipped with aisdk core.
        if (exists("create_openai", mode = "function")) reg$register("openai", function() suppressWarnings(create_openai()))
        if (exists("create_anthropic", mode = "function")) reg$register("anthropic", function() suppressWarnings(create_anthropic()))
        if (exists("create_gemini", mode = "function")) reg$register("gemini", function() suppressWarnings(create_gemini()))
        if (exists("create_custom_provider", mode = "function")) reg$register("custom", function() suppressWarnings(create_env_custom_provider()))
        # Long-tail providers (deepseek, xai, volcengine, nvidia, stepfun,
        # bailian, openrouter, aihubmix, moonshot, kimi) live in the companion
        # package aisdk.providers and register themselves via its .onLoad hook
        # through register_provider(); they are replayed below.
      },
      error = function(e) {}
    )
    # Replay provider factories registered by companion packages (e.g.
    # aisdk.providers) so they survive a fresh registry build regardless of
    # whether their .onLoad ran before or after this point.
    for (extra_id in ls(.provider_extras)) {
      reg$register(extra_id, .provider_extras[[extra_id]])
    }
    tryCatch(
      register_configured_model_providers(reg),
      error = function(e) {
        rlang::warn(paste0("Failed to load aisdk model config: ", conditionMessage(e)))
      }
    )

    .registry_env$default <- reg
  }
  .registry_env$default
}
