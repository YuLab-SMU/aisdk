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
#' like reasoning content extraction and DeepSeek thinking-mode parameters.
#' @keywords internal
DeepSeekLanguageModel <- R6::R6Class(
    "DeepSeekLanguageModel",
    inherit = OpenAILanguageModel,
    private = list(
        # Normalize thinking parameter to DeepSeek API format.
        # DeepSeek expects {"type": "enabled"} or {"type": "disabled"}.
        # We auto-convert logical values for convenience.
        normalize_thinking = function(thinking) {
            if (is.logical(thinking)) {
                if (isTRUE(thinking)) {
                    return(list(type = "enabled"))
                } else {
                    return(list(type = "disabled"))
                }
            }
            thinking
        }
    ),

    public = list(
        #' @description Initialize the DeepSeek language model.
        #' @param model_id The model ID.
        #' @param config Provider configuration.
        initialize = function(model_id, config) {
            is_reasoning_model <- grepl(
                "deepseek-v4|deepseek-reasoner|(^|[-_])r1()|(^|[-_])thinking()",
                model_id,
                ignore.case = TRUE
            )

            super$initialize(
                model_id = model_id,
                config = config,
                capabilities = list(
                    is_reasoning_model = is_reasoning_model,
                    reasoning = is_reasoning_model,
                    preserve_reasoning_content = TRUE,
                    function_call = TRUE,
                    structured_output = TRUE
                )
            )
        },

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
        },

        #' @description Build request payload with DeepSeek-specific reasoning params.
        #' @param params A list of call options.
        #' @return A list with url, headers, and body.
        build_payload = function(params) {
            payload <- super$build_payload(params)

            thinking <- list_get_exact(params, "thinking")
            thinking_budget <- list_get_exact(params, "thinking_budget")
            reasoning_effort <- list_get_exact(params, "reasoning_effort")

            if (!is.null(thinking)) {
                payload$body$thinking <- private$normalize_thinking(thinking)
            }
            if (!is.null(thinking_budget)) {
                payload$body$thinking_budget <- thinking_budget
            }
            if (!is.null(reasoning_effort)) {
                payload$body$reasoning_effort <- reasoning_effort
            }

            payload
        },

        #' @description Build stream payload with DeepSeek-specific reasoning params.
        #' @param params A list of call options.
        #' @return A list with url, headers, and body.
        build_stream_payload = function(params) {
            payload <- super$build_stream_payload(params)

            thinking <- list_get_exact(params, "thinking")
            thinking_budget <- list_get_exact(params, "thinking_budget")
            reasoning_effort <- list_get_exact(params, "reasoning_effort")

            if (!is.null(thinking)) {
                payload$body$thinking <- private$normalize_thinking(thinking)
            }
            if (!is.null(thinking_budget)) {
                payload$body$thinking_budget <- thinking_budget
            }
            if (!is.null(reasoning_effort)) {
                payload$body$reasoning_effort <- reasoning_effort
            }

            payload
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
        #' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
        #' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
        #' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
        #' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
        #' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
        initialize = function(api_key = NULL,
                              base_url = NULL,
                              headers = NULL,
                              timeout_seconds = NULL,
                              total_timeout_seconds = NULL,
                              first_byte_timeout_seconds = NULL,
                              connect_timeout_seconds = NULL,
                              idle_timeout_seconds = NULL) {
            # Suppress parent class warning since we do our own check
            suppressWarnings(
                super$initialize(
                    api_key = api_key %||% Sys.getenv("DEEPSEEK_API_KEY"),
                    base_url = base_url %||% Sys.getenv("DEEPSEEK_BASE_URL", "https://api.deepseek.com"),
                    headers = headers,
                    name = "deepseek",
                    timeout_seconds = timeout_seconds,
                    total_timeout_seconds = total_timeout_seconds,
                    first_byte_timeout_seconds = first_byte_timeout_seconds,
                    connect_timeout_seconds = connect_timeout_seconds,
                    idle_timeout_seconds = idle_timeout_seconds
                )
            )

            if (nchar(private$config$api_key) == 0) {
                rlang::warn("DeepSeek API key not set. Set DEEPSEEK_API_KEY env var or pass api_key parameter.")
            }
        },

        #' @description Create a language model.
        #' @param model_id The model ID (e.g., "deepseek-chat", "deepseek-reasoner", or a `deepseek-v4*` model).
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
#' DeepSeek supports classic aliases plus newer model families such as DeepSeek V4.
#'
#' Common model IDs include:
#' \itemize{
#'   \item \strong{deepseek-chat}: Chat alias provided by DeepSeek
#'   \item \strong{deepseek-reasoner}: Reasoning alias provided by DeepSeek
#'   \item \strong{deepseek-v4*}: DeepSeek V4 family model IDs exposed by the API
#' }
#'
#' Additional DeepSeek-specific request fields such as `thinking`,
#' `thinking_budget`, and `reasoning_effort` are passed through when supplied
#' to `$generate()` or `$stream()`.
#'
#' @param api_key DeepSeek API key. Defaults to DEEPSEEK_API_KEY env var.
#' @param base_url Base URL. Defaults to "https://api.deepseek.com".
#' @param headers Optional additional headers.
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
#' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
#' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
#' @return A DeepSeekProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#' # Basic usage with deepseek-chat
#' deepseek <- create_deepseek()
#' model <- deepseek$language_model("deepseek-chat")
#' result <- generate_text(model, "Hello!")
#'
#' # Using a reasoning-capable model
#' model_reasoner <- deepseek$language_model("deepseek-reasoner")
#' result <- model_reasoner$generate(
#'     messages = list(list(role = "user", content = "Solve: What is 15 * 23?")),
#'     max_tokens = 500,
#'     thinking = TRUE
#' )
#' print(result$text) # Final answer
#' print(result$reasoning) # Chain-of-thought reasoning
#'
#' # Streaming with reasoning
#' stream_text(model_reasoner, "Explain quantum entanglement step by step")
#' }
#' }
create_deepseek <- function(api_key = NULL,
                            base_url = NULL,
                            headers = NULL,
                            timeout_seconds = NULL,
                            total_timeout_seconds = NULL,
                            first_byte_timeout_seconds = NULL,
                            connect_timeout_seconds = NULL,
                            idle_timeout_seconds = NULL) {
    DeepSeekProvider$new(
        api_key = api_key,
        base_url = base_url,
        headers = headers,
        timeout_seconds = timeout_seconds,
        total_timeout_seconds = total_timeout_seconds,
        first_byte_timeout_seconds = first_byte_timeout_seconds,
        connect_timeout_seconds = connect_timeout_seconds,
        idle_timeout_seconds = idle_timeout_seconds
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
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
#' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
#' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
#' @return An AnthropicProvider object configured for DeepSeek.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#' # Use DeepSeek via Anthropic API format
#' deepseek <- create_deepseek_anthropic()
#' model <- deepseek$language_model("deepseek-chat")
#' result <- generate_text(model, "Hello!")
#'
#' # This is useful for tools that expect Anthropic API format
#' # such as Claude Code integration
#' }
#' }
create_deepseek_anthropic <- function(api_key = NULL,
                                      headers = NULL,
                                      timeout_seconds = NULL,
                                      total_timeout_seconds = NULL,
                                      first_byte_timeout_seconds = NULL,
                                      connect_timeout_seconds = NULL,
                                      idle_timeout_seconds = NULL) {
    create_anthropic(
        api_key = api_key %||% Sys.getenv("DEEPSEEK_API_KEY"),
        base_url = "https://api.deepseek.com/anthropic",
        name = "deepseek",
        headers = headers,
        timeout_seconds = timeout_seconds,
        total_timeout_seconds = total_timeout_seconds,
        first_byte_timeout_seconds = first_byte_timeout_seconds,
        connect_timeout_seconds = connect_timeout_seconds,
        idle_timeout_seconds = idle_timeout_seconds
    )
}
