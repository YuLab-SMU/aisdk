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
skip_if_no_api_key <- function(provider = "API") {
  # Standardize provider name
  provider_key <- tolower(provider)
  
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
