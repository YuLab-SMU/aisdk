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
        #' @description Generate text (non-streaming).
        #' @param params A list of call options including messages, temperature, etc.
        #' @return A GenerateResult object.
        do_generate = function(params) {
            url <- paste0(private$config$base_url, "/chat/completions")
            headers <- private$get_headers()

            body <- list(
                model = self$model_id,
                messages = params$messages,
                temperature = params$temperature %||% 0.7,
                stream = FALSE
            )

            # Handle max_tokens
            if (!is.null(params$max_tokens)) {
                body$max_tokens <- params$max_tokens
            }

            # Add tools if provided
            if (!is.null(params$tools) && length(params$tools) > 0) {
                tools_list <- unname(params$tools)
                body$tools <- lapply(tools_list, function(t) {
                    if (inherits(t, "Tool")) {
                        t$to_api_format("openai")
                    } else {
                        t # Assume already in correct format
                    }
                })
            }

            # Pass through extra parameters
            handled_params <- c("messages", "temperature", "max_tokens", "tools", "stream", "model")
            extra_params <- params[setdiff(names(params), handled_params)]
            if (length(extra_params) > 0) {
                body <- utils::modifyList(body, extra_params)
            }

            # Remove NULL entries
            body <- body[!sapply(body, is.null)]

            response <- post_to_api(url, headers, body)

            # Parse response
            choice <- response$choices[[1]]

            # Parse tool_calls if present
            tool_calls <- NULL
            if (!is.null(choice$message$tool_calls)) {
                tool_calls <- lapply(choice$message$tool_calls, function(tc) {
                    list(
                        id = tc$id,
                        name = tc$`function`$name,
                        arguments = parse_tool_arguments(tc$`function`$arguments, tool_name = tc$`function`$name)
                    )
                })
            }

            # Extract reasoning content (DeepSeek specific for deepseek-reasoner)
            reasoning <- choice$message$reasoning_content

            GenerateResult$new(
                text = choice$message$content,
                usage = response$usage,
                finish_reason = choice$finish_reason,
                raw_response = response,
                tool_calls = tool_calls,
                reasoning = reasoning
            )
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
#' \dontrun{
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
#' \dontrun{
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
