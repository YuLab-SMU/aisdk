# ============================================
# Test Environment Configuration Helper
# ============================================
# This helper manages project-level API keys for testing
# It ensures that API tests only run when keys are available
# and prevents accidental commits of sensitive information

#' Skip test if API tests are not enabled
#'
#' @param provider Provider name for the message
#' @export
#' Skip test if offline
#'
#' @export
skip_if_offline <- function() {
  if (!curl::has_internet()) {
    testthat::skip("Offline: curl::has_internet() is FALSE. Skipping internet-dependent test.")
  }
}

#' Create a test evaluation environment with package namespace fallback
#'
#' @param parent Parent environment. Defaults to the installed aisdk namespace
#'   when available, otherwise the global environment.
#' @return A new environment used by source-based tests.
#' @keywords internal
aisdk_test_env <- function(parent = NULL) {
  ns <- NULL
  if ("aisdk" %in% loadedNamespaces()) {
    ns <- asNamespace("aisdk")
    parent <- ns
  } else if (is.null(parent)) {
    parent <- globalenv()
  }

  env <- new.env(parent = parent)
  if (!is.null(ns)) {
    ns_names <- ls(ns, all.names = TRUE)
    ns_values <- mget(ns_names, envir = ns, inherits = FALSE)
    if (".engine_env" %in% names(ns_values)) {
      ns_values[[".engine_env"]] <- new.env(parent = emptyenv())
    }
    ns_values <- lapply(ns_values, function(value) {
      if (is.function(value) && !is.primitive(value)) {
        environment(value) <- env
      }
      value
    })
    list2env(ns_values, envir = env)
  }
  env$`%||%` <- rlang::`%||%`
  env
}

#' Source a package file from the local source tree when available
#'
#' @param env Environment to source into.
#' @param path File path relative to the package `R/` directory.
#' @return Invisibly returns `TRUE` when a local source file was found and
#'   sourced, otherwise `FALSE`.
#' @keywords internal
source_local_aisdk_file <- function(env, path) {
  candidates <- c(
    file.path("R", path),
    file.path("..", "R", path),
    file.path("..", "..", "R", path)
  )

  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    return(invisible(FALSE))
  }

  sys.source(existing[[1]], envir = env)
  invisible(TRUE)
}

#' Skip test if API tests are not enabled
#'
#' @param provider Provider name for the message
#' @export
skip_if_no_api_key <- function(provider = "API") {
  # Standardize provider name
  provider_key <- tolower(provider)

  skip_if_offline()

  # has_api_key is now in R/utils_env.R
  if (!aisdk::has_api_key(provider_key)) {
    testthat::skip(paste0(provider, " tests skipped: Set ", toupper(provider), "_API_KEY in project .Renviron or set ENABLE_API_TESTS=TRUE"))
  }
}

#' Get provider with warning suppression
#'
#' Creates a provider instance and suppresses API key warnings
#' @param provider Provider function (create_openai or create_anthropic)
#' @param ... Additional arguments to pass to provider
#' @return Provider instance
#' @export
safe_create_provider <- function(provider, ...) {
  suppressWarnings(provider(...))
}

#' Print API test configuration status
#'
#' @export
print_api_test_config <- function() {
  cat("\n========================================\n")
  cat("AISDK API Test Configuration\n")
  cat("========================================\n")
  # Functions now in R/utils_env.R
  cat("API Tests Enabled:", aisdk::enable_api_tests(), "\n")
  cat("OpenAI Key Available:", aisdk::has_api_key("openai"), "\n")
  cat("OpenAI Model:", aisdk::get_openai_model(), "\n")
  cat("Anthropic Key Available:", aisdk::has_api_key("anthropic"), "\n")
  cat("Anthropic Model:", aisdk::get_anthropic_model(), "\n")
  cat("DeepSeek Key Available:", aisdk::has_api_key("deepseek"), "\n")

  if (!aisdk::enable_api_tests()) {
    cat("\nTo enable API tests:\n")
    cat("Option 1 (Recommended - Persistent):")
    cat("\n  1. Run: usethis::edit_r_environ(scope = 'project')\n")
    cat("  2. Copy contents from .Renviron.example\n")
    cat("  3. Fill in your API keys and model names\n")
    cat("  4. Run: reload_env() to apply changes without restarting\n")
    cat("\nOption 2 (Temporary - Session only):")
    cat("\n  1. Run: options(OPENAI_API_KEY = 'your-key')\n")
    cat("  2. Use: getOption('OPENAI_API_KEY')\n")
    cat("\nOption 3 (Modern - dotenv package):")
    cat("\n  1. Create .env file (not .Renviron)\n")
    cat("  2. Run: dotenv::load_dot_env()\n")
    cat("  3. Use: Sys.getenv('OPENAI_API_KEY')\n")
  }
  cat("========================================\n\n")
}

# Print configuration on package load (only during development)
if (interactive() && getwd() == file.path(system.file(package = "aisdk"), "..")) {
  # Only print if running in development mode
  print_api_test_config()
}
