#' @name provider_deepseek
#' @title DeepSeek Provider
#' @description
#' Implementation for DeepSeek models.
#' DeepSeek API is OpenAI-compatible with support for reasoning models.
#' @keywords internal
NULL

#' @title DeepSeek Language Model Class
#' @description
#' Language model implementation for DeepSeek's chat completions API.
#' Inherits from OpenAI model but adds support for DeepSeek-specific features
#' like reasoning content extraction from `deepseek-reasoner` model.
#' @keywords internal
DeepSeekLanguageModel <- R6::R6Class(
    "DeepSeekLanguageModel",
    inherit = OpenAILanguageModel,
    public = list(
        #' @description Parse the API response into a GenerateResult.
        #' Overrides parent to extract DeepSeek-specific reasoning_content.
        #' @param response The parsed API response.
        #' @return A GenerateResult object.
        parse_response = function(response) {
            # Use parent's parsing for standard fields
            result <- super$parse_response(response)

            # Extract DeepSeek-specific reasoning content
            choice <- response$choices[[1]]
            result$reasoning <- choice$message$reasoning_content

            result
        }
    )
)

#' @title DeepSeek Provider Class
#' @description
#' Provider class for DeepSeek.
#' @export
DeepSeekProvider <- R6::R6Class(
    "DeepSeekProvider",
    inherit = OpenAIProvider,
    public = list(
        #' @description Initialize the DeepSeek provider.
        #' @param api_key DeepSeek API key. Defaults to DEEPSEEK_API_KEY env var.
        #' @param base_url Base URL. Defaults to https://api.deepseek.com.
        #' @param headers Optional additional headers.
        initialize = function(api_key = NULL,
                              base_url = NULL,
                              headers = NULL) {
            # Suppress parent class warning since we do our own check
            suppressWarnings(
                super$initialize(
                    api_key = api_key %||% Sys.getenv("DEEPSEEK_API_KEY"),
                    base_url = base_url %||% Sys.getenv("DEEPSEEK_BASE_URL", "https://api.deepseek.com"),
                    headers = headers,
                    name = "deepseek"
                )
            )

            if (nchar(private$config$api_key) == 0) {
                rlang::warn("DeepSeek API key not set. Set DEEPSEEK_API_KEY env var or pass api_key parameter.")
            }
        },

        #' @description Create a language model.
        #' @param model_id The model ID (e.g., "deepseek-chat", "deepseek-reasoner").
        #' @return A DeepSeekLanguageModel object.
        language_model = function(model_id = NULL) {
            model_id <- model_id %||% Sys.getenv("DEEPSEEK_MODEL", "deepseek-chat")
            DeepSeekLanguageModel$new(model_id, private$config)
        }
    )
)

#' @title Create DeepSeek Provider
#' @description
#' Factory function to create a DeepSeek provider.
#'
#' DeepSeek offers two main models:
#' \itemize{
#'   \item \strong{deepseek-chat}: Standard chat model (DeepSeek-V3.2 non-thinking mode)
#'   \item \strong{deepseek-reasoner}: Reasoning model with chain-of-thought (DeepSeek-V3.2 thinking mode)
#' }
#'
#' @param api_key DeepSeek API key. Defaults to DEEPSEEK_API_KEY env var.
#' @param base_url Base URL. Defaults to "https://api.deepseek.com".
#' @param headers Optional additional headers.
#' @return A DeepSeekProvider object.
#' @export
#' @examples
#' \donttest{
#' # Basic usage with deepseek-chat
#' deepseek <- create_deepseek()
#' model <- deepseek$language_model("deepseek-chat")
#' result <- generate_text(model, "Hello!")
#'
#' # Using deepseek-reasoner for chain-of-thought reasoning
#' model_reasoner <- deepseek$language_model("deepseek-reasoner")
#' result <- model_reasoner$generate(
#'     messages = list(list(role = "user", content = "Solve: What is 15 * 23?")),
#'     max_tokens = 500
#' )
#' print(result$text) # Final answer
#' print(result$reasoning) # Chain-of-thought reasoning
#'
#' # Streaming with reasoning
#' stream_text(model_reasoner, "Explain quantum entanglement step by step")
#' }
create_deepseek <- function(api_key = NULL,
                            base_url = NULL,
                            headers = NULL) {
    DeepSeekProvider$new(
        api_key = api_key,
        base_url = base_url,
        headers = headers
    )
}

#' @title Create DeepSeek Provider (Anthropic API Format)
#' @description
#' Factory function to create a DeepSeek provider using the Anthropic-compatible API.
#' This allows you to use DeepSeek models with the Anthropic API format.
#'
#' @details
#' DeepSeek provides an Anthropic-compatible endpoint at `https://api.deepseek.com/anthropic`.
#' This convenience function wraps `create_anthropic()` with DeepSeek-specific defaults.
#'
#' Note: When using an unsupported model name, the API backend will automatically
#' map it to `deepseek-chat`.
#'
#' @param api_key DeepSeek API key. Defaults to DEEPSEEK_API_KEY env var.
#' @param headers Optional additional headers.
#' @return An AnthropicProvider object configured for DeepSeek.
#' @export
#' @examples
#' \donttest{
#' # Use DeepSeek via Anthropic API format
#' deepseek <- create_deepseek_anthropic()
#' model <- deepseek$language_model("deepseek-chat")
#' result <- generate_text(model, "Hello!")
#'
#' # This is useful for tools that expect Anthropic API format
#' # such as Claude Code integration
#' }
create_deepseek_anthropic <- function(api_key = NULL,
                                      headers = NULL) {
    create_anthropic(
        api_key = api_key %||% Sys.getenv("DEEPSEEK_API_KEY"),
        base_url = "https://api.deepseek.com/anthropic",
        name = "deepseek",
        headers = headers
    )
}
