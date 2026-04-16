#' @name provider_nvidia
#' @title NVIDIA Provider
#' @description
#' Implementation for NVIDIA NIM and other NVIDIA-hosted models.
#' @keywords internal
NULL

#' @title NVIDIA Language Model Class
#' @description
#' Language model implementation for NVIDIA's chat completions API.
#' Inherits from OpenAI model but adds support for NVIDIA-specific features
#' like "enable_thinking" and reasoning content extraction.
#' @keywords internal
NvidiaLanguageModel <- R6::R6Class(
  "NvidiaLanguageModel",
  inherit = OpenAILanguageModel,
  public = list(
    #' @description Parse the API response into a GenerateResult.
    #' Overrides parent to extract NVIDIA-specific reasoning_content.
    #' @param response The parsed API response.
    #' @return A GenerateResult object.
    parse_response = function(response) {
      # Use parent's parsing for standard fields
      result <- super$parse_response(response)

      # Extract NVIDIA-specific reasoning content
      choice <- response$choices[[1]]
      result$reasoning <- choice$message$reasoning_content

      result
    }
  )
)

#' @title NVIDIA Provider Class
#' @description
#' Provider class for NVIDIA.
#' @export
NvidiaProvider <- R6::R6Class(
  "NvidiaProvider",
  inherit = OpenAIProvider,
  public = list(
    #' @description Initialize the NVIDIA provider.
    #' @param api_key NVIDIA API key. Defaults to NVIDIA_API_KEY env var.
    #' @param base_url Base URL. Defaults to https://integrate.api.nvidia.com/v1.
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
      super$initialize(
        api_key = api_key %||% Sys.getenv("NVIDIA_API_KEY"),
        base_url = base_url %||% Sys.getenv("NVIDIA_BASE_URL", "https://integrate.api.nvidia.com/v1"),
        headers = headers,
        name = "nvidia",
        timeout_seconds = timeout_seconds,
        total_timeout_seconds = total_timeout_seconds,
        first_byte_timeout_seconds = first_byte_timeout_seconds,
        connect_timeout_seconds = connect_timeout_seconds,
        idle_timeout_seconds = idle_timeout_seconds
      )

      if (nchar(private$config$api_key) == 0) {
        rlang::warn("NVIDIA API key not set. Set NVIDIA_API_KEY env var or pass api_key parameter.")
      }
    },

    #' @description Create a language model.
    #' @param model_id The model ID (e.g., "z-ai/glm4.7").
    #' @return A NvidiaLanguageModel object.
    language_model = function(model_id = NULL) {
      model_id <- model_id %||% Sys.getenv("NVIDIA_MODEL")
      if (model_id == "") {
        rlang::abort("Model ID not provided and NVIDIA_MODEL environment variable not set.")
      }
      NvidiaLanguageModel$new(model_id, private$config)
    }
  )
)

#' @title Create NVIDIA Provider
#' @description
#' Factory function to create a NVIDIA provider.
#' @param api_key NVIDIA API key. Defaults to NVIDIA_API_KEY env var.
#' @param base_url Base URL. Defaults to "https://integrate.api.nvidia.com/v1".
#' @param headers Optional additional headers.
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
#' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
#' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
#' @return A NvidiaProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#' nvidia <- create_nvidia()
#' model <- nvidia$language_model("z-ai/glm4.7")
#'
#' # Enable thinking/reasoning
#' result <- generate_text(model, "Who are you?",
#'   chat_template_kwargs = list(enable_thinking = TRUE)
#' )
#' print(result$reasoning)
#' }
#' }
create_nvidia <- function(api_key = NULL,
                          base_url = NULL,
                          headers = NULL,
                          timeout_seconds = NULL,
                          total_timeout_seconds = NULL,
                          first_byte_timeout_seconds = NULL,
                          connect_timeout_seconds = NULL,
                          idle_timeout_seconds = NULL) {
  NvidiaProvider$new(
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
