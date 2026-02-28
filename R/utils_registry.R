#' @title Utilities: Provider Registry
#' @description
#' A registry for managing AI model providers.
#' Supports the `provider:model` syntax for accessing models.
#' @name utils_registry
NULL

# Private environment to store the default registry (avoids locked binding issues)
.registry_env <- new.env(parent = emptyenv())

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
      if (is.null(provider)) {
        available <- paste(names(private$providers), collapse = ", ")
        rlang::abort(c(
          paste0("Provider not found: ", provider_id),
          "i" = if (nchar(available) > 0) paste0("Available providers: ", available) else "No providers registered."
        ))
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
      if (is.null(provider)) {
        rlang::abort(paste0("Provider not found: ", provider_id))
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
        if (exists("create_openai", mode = "function")) reg$register("openai", function() suppressWarnings(create_openai()))
        if (exists("create_anthropic", mode = "function")) reg$register("anthropic", function() suppressWarnings(create_anthropic()))
        if (exists("create_gemini", mode = "function")) reg$register("gemini", function() suppressWarnings(create_gemini()))
        if (exists("create_deepseek", mode = "function")) reg$register("deepseek", function() suppressWarnings(create_deepseek()))
        if (exists("create_xai", mode = "function")) reg$register("xai", function() suppressWarnings(create_xai()))
        if (exists("create_volcengine", mode = "function")) reg$register("volcengine", function() suppressWarnings(create_volcengine()))
        if (exists("create_stepfun", mode = "function")) reg$register("stepfun", function() suppressWarnings(create_stepfun()))
        if (exists("create_bailian", mode = "function")) reg$register("bailian", function() suppressWarnings(create_bailian()))
        if (exists("create_openrouter", mode = "function")) reg$register("openrouter", function() suppressWarnings(create_openrouter()))
      },
      error = function(e) {}
    )

    .registry_env$default <- reg
  }
  .registry_env$default
}
