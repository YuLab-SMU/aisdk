#' @title Environment Configuration Utilities
#' @description
#' Utilities for managing API keys and environment variables.
#' @name utils_env
#' @keywords internal
NULL

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
  } else if (provider == "api") {
    # Generic check for ANY key
    return(enable_api_tests())
  }
  return(FALSE)
}

#' Get OpenAI Base URL from environment
#' @return Base URL string
#' @export
get_openai_base_url <- function() {
  value <- Sys.getenv("OPENAI_BASE_URL")
  if (value == "") {
    return("https://api.openai.com/v1")
  }
  value
}

#' Get OpenAI Model from environment
#' @return Model name string
#' @export
get_openai_model <- function() {
  value <- Sys.getenv("OPENAI_MODEL")
  if (value == "") {
    return("gpt-4o-mini")
  }
  value
}

#' Get OpenAI Embedding Model from environment
#' @return Model name string
#' @export
get_openai_embedding_model <- function() {
  value <- Sys.getenv("OPENAI_EMBEDDING_MODEL")
  if (value == "") {
    return("text-embedding-3-small")
  }
  value
}

#' Get OpenAI Model ID from environment
#' @return Model ID string
#' @export
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
  readRenviron(path)
  
  message(paste0("[OK] Environment reloaded from: ", path))
  message("[OK] New values are now available in Sys.getenv()")
  
  invisible(TRUE)
}

#' Update .Renviron with new values
#'
#' Updates or appends environment variables to the .Renviron file.
#'
#' @param updates A named list of key-value pairs to update.
#' @param path Path to .Renviron file (default: project root)
#' @return Invisible TRUE if successful
#' @export
update_renviron <- function(updates, path = ".Renviron") {
  lines <- if (file.exists(path)) readLines(path) else character(0)
  
  for (key in names(updates)) {
    value <- updates[[key]]
    if (is.null(value) || value == "") next
    
    # Check if key exists (ignore commented lines)
    pattern <- paste0("^\\s*", key, "\\s*=")
    idx <- grep(pattern, lines)
    
    new_line <- paste0(key, "=", value)
    
    if (length(idx) > 0) {
      lines[idx[1]] <- new_line
    } else {
      lines <- c(lines, new_line)
    }
  }
  
  # Ensure newline at end
  if (length(lines) > 0 && nchar(lines[length(lines)]) > 0) {
    # It's fine, writeLines adds newline
  }
  
  writeLines(lines, path)
  
  # Reload to apply changes immediately
  reload_env(path)
  
  invisible(TRUE)
}

#' Annotate model capabilities based on ID
#' @param df Data frame with 'id' column
#' @return Data frame with added logical columns
#' @keywords internal
annotate_model_capabilities <- function(df) {
  if (nrow(df) == 0) return(df)
  
  df$function_calling <- FALSE
  df$thinking <- FALSE
  df$vision <- FALSE
  
  # Heuristics
  id <- tolower(df$id)
  
  # Function calling: Most modern models support it
  # OpenAI: almost all recent ones
  # Anthropic: Claude 3+
  # DeepSeek: Chat models usually do
  # Llama 3: groq supports it
  
  df$function_calling <- grepl("gpt-|claude-3|deepseek|llama-3|mixtral", id)
  
  # Vision
  df$vision <- grepl("gpt-4o|vision|claude-3|gemini|llava", id)
  
  # Thinking / Reasoning
  df$thinking <- grepl("o1-|o3-|r1|reasoning", id)
  
  df
}


#' Fetch available models from API provider
#'
#' @param provider Provider name ("openai", "nvidia", "anthropic", etc.)
#' @param api_key API key
#' @param base_url Base URL
#' @return A data frame with 'id' column and capability flag columns
#' @export
fetch_api_models <- function(provider, api_key = NULL, base_url = NULL) {
  if (is.null(api_key) || nchar(api_key) == 0) {
    return(data.frame(id = character(0), function_calling = logical(0), thinking = logical(0), vision = logical(0)))
  }
  
  # Normalize provider name
  provider <- tolower(provider)
  models_list <- character(0)
  
  # OpenAI-compatible providers
  openai_compatible <- c("openai", "nvidia", "deepseek", "groq")
  
  if (provider %in% openai_compatible) {
    if (is.null(base_url) || nchar(base_url) == 0) {
       if (provider == "openai") base_url <- "https://api.openai.com/v1"
       if (provider == "nvidia") base_url <- "https://integrate.api.nvidia.com/v1"
       if (provider == "deepseek") base_url <- "https://api.deepseek.com"
       if (provider == "groq") base_url <- "https://api.groq.com/openai/v1"
    }
    
    url <- paste0(sub("/$", "", base_url), "/models")
    headers <- list(
      Authorization = paste("Bearer", api_key)
    )
    
    tryCatch({
      req <- httr2::request(url)
      req <- httr2::req_headers(req, !!!headers)
      req <- httr2::req_retry(req, max_tries = 2)
      
      resp <- httr2::req_perform(req)
      data <- httr2::resp_body_json(resp)
      
      models_list <- vapply(data$data, function(m) m$id, character(1))
      models_list <- sort(models_list)
      
    }, error = function(e) {
      warning("Failed to fetch models: ", e$message)
    })
  } else if (provider == "anthropic") {
    # Anthropic doesn't have a public models list API yet.
    # Return a curated list.
    models_list <- c(
      "claude-3-7-sonnet-20250219",
      "claude-3-5-sonnet-20241022",
      "claude-3-5-sonnet-20240620",
      "claude-3-5-haiku-20241022",
      "claude-3-opus-20240229",
      "claude-3-sonnet-20240229",
      "claude-3-haiku-20240307"
    )
  }
  
  if (length(models_list) > 0) {
    df <- data.frame(id = models_list, stringsAsFactors = FALSE)
    return(annotate_model_capabilities(df))
  } else {
    return(data.frame(id = character(0), function_calling = logical(0), thinking = logical(0), vision = logical(0)))
  }
}
