# ============================================
# Test Environment Configuration Helper
# ============================================
# This helper manages project-level API keys for testing
# It ensures that API tests only run when keys are available
# and prevents accidental commits of sensitive information

#' Check if API tests should be enabled
#'
#' @return TRUE if API tests are enabled and keys are available, FALSE otherwise
#' @export
enable_api_tests <- function() {
  # Check environment variable
  env_flag <- Sys.getenv("ENABLE_API_TESTS")
  
  if (env_flag == "") {
    # Default: only enable if API keys are present
    openai_key <- Sys.getenv("OPENAI_API_KEY")
    anthropic_key <- Sys.getenv("ANTHROPIC_API_KEY")
    
    has_openai <- nzchar(openai_key) && !grepl("^your_|^example_", openai_key)
    has_anthropic <- nzchar(anthropic_key) && !grepl("^your_|^example_", anthropic_key)
    
    return(has_openai || has_anthropic)
  }
  
  return(toupper(env_flag) == "TRUE")
}

#' Skip test if API tests are not enabled
#'
#' @param provider Provider name for the message
#' @export
skip_if_no_api_key <- function(provider = "API") {
  if (!enable_api_tests()) {
    testthat::skip(paste0(provider, " tests skipped: Set API keys in project .Renviron or set ENABLE_API_TESTS=TRUE"))
  }
}

#' Check if specific provider key is available
#'
#' @param provider Provider name ("openai" or "anthropic")
#' @return TRUE if key is available and valid
#' @export
has_api_key <- function(provider) {
  if (provider == "openai") {
    key <- Sys.getenv("OPENAI_API_KEY")
    return(nzchar(key) && !grepl("^your_|^example_", key))
  } else if (provider == "anthropic") {
    key <- Sys.getenv("ANTHROPIC_API_KEY")
    return(nzchar(key) && !grepl("^your_|^example_", key))
  }
  return(FALSE)
}

get_openai_base_url <- function() {
  value <- Sys.getenv("OPENAI_BASE_URL")
  if (value == "") {
    return("https://api.openai.com/v1")
  }
  value
}

get_openai_model <- function() {
  value <- Sys.getenv("OPENAI_MODEL")
  if (value == "") {
    return("gpt-4o-mini")
  }
  value
}

get_openai_embedding_model <- function() {
  value <- Sys.getenv("OPENAI_EMBEDDING_MODEL")
  if (value == "") {
    return("text-embedding-3-small")
  }
  value
}

get_openai_model_id <- function() {
  value <- Sys.getenv("OPENAI_MODEL_ID")
  if (value == "") {
    return("openai:gpt-4o")
  }
  value
}

#' Get Anthropic base URL from environment
#'
#' @return Base URL for Anthropic API (default: official)
#' @export
get_anthropic_base_url <- function() {
  value <- Sys.getenv("ANTHROPIC_BASE_URL")
  if (value == "") {
    return("https://api.anthropic.com")
  }
  value
}

#' Get Anthropic model name from environment
#'
#' @return Model name (default: claude-sonnet-4-20250514)
#' @export
get_anthropic_model <- function() {
  value <- Sys.getenv("ANTHROPIC_MODEL")
  if (value == "") {
    return("claude-sonnet-4-20250514")
  }
  value
}

#' Get Anthropic model ID from environment
#'
#' @return Model ID (default: anthropic:claude-sonnet-4-20250514)
#' @export
get_anthropic_model_id <- function() {
  value <- Sys.getenv("ANTHROPIC_MODEL_ID")
  if (value == "") {
    return("anthropic:claude-sonnet-4-20250514")
  }
  value
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

#' Reload project-level environment variables
#'
#' Forces R to re-read the .Renviron file without restarting the session.
#' This is useful when you've modified .Renviron and don't want to restart R.
#'
#' @param path Path to .Renviron file (default: project root)
#' @return Invisible TRUE if successful
#' @export
#' @examples
#' \dontrun{
#' # Reload environment after modifying .Renviron
#' reload_env()
#' # Now use the new keys
#' Sys.getenv("OPENAI_API_KEY")
#' }
reload_env <- function(path = ".Renviron") {
  if (!file.exists(path)) {
    warning(paste0(".Renviron file not found at: ", path))
    return(invisible(FALSE))
  }
  
  # Read the environment file
  result <- readRenviron(path)
  
  message(paste0("✓ Environment reloaded from: ", path))
  message("✓ New values are now available in Sys.getenv()")
  
  invisible(TRUE)
}

#' Print API test configuration status
#'
#' @export
print_api_test_config <- function() {
  cat("\n========================================\n")
  cat("AISDK API Test Configuration\n")
  cat("========================================\n")
  cat("API Tests Enabled:", enable_api_tests(), "\n")
  cat("OpenAI Key Available:", has_api_key("openai"), "\n")
  cat("OpenAI Model:", get_openai_model(), "\n")
  cat("Anthropic Key Available:", has_api_key("anthropic"), "\n")
  cat("Anthropic Model:", get_anthropic_model(), "\n")
  
  if (!enable_api_tests()) {
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
