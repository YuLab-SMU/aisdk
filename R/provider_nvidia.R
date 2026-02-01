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
    #' @description Generate text (non-streaming).
    #' @param params A list of call options including messages, temperature, etc.
    #' @return A GenerateResult object.
    do_generate = function(params) {
      # Add chat_template_kwargs if enable_thinking is requested
      # Check if this is a "thinking" model request
      
      # We just pass everything to super$do_generate first to handle the request
      # BUT super$do_generate doesn't know how to extract NVIDIA's reasoning_content 
      # from the non-standard location if it differs, or if it's just standard.
      # NVIDIA GLM-4.7 returns `reasoning_content` in the message object (standard-ish).
      
      # However, we need to intercept the response creation to populate `reasoning` field.
      # The easiest way is to copy most of OpenAILanguageModel$do_generate but adapt it.
      
      url <- paste0(private$config$base_url, "/chat/completions")
      headers <- private$get_headers()

      body <- list(
        model = self$model_id,
        messages = params$messages,
        temperature = params$temperature %||% 0.7,
        stream = FALSE
      )
      
      # Handle max_tokens mapping (NVIDIA seems to use standard max_tokens)
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
            t  # Assume already in correct format
          }
        })
      }

      # Pass through extra parameters (e.g., chat_template_kwargs)
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
      
      # Extract reasoning content (NVIDIA specific)
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
    initialize = function(api_key = NULL,
                          base_url = NULL,
                          headers = NULL) {
      super$initialize(
        api_key = api_key %||% Sys.getenv("NVIDIA_API_KEY"),
        base_url = base_url %||% Sys.getenv("NVIDIA_BASE_URL", "https://integrate.api.nvidia.com/v1"),
        headers = headers,
        name = "nvidia"
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
#' @return A NvidiaProvider object.
#' @export
#' @examples
#' \dontrun{
#' nvidia <- create_nvidia()
#' model <- nvidia$language_model("z-ai/glm4.7")
#' 
#' # Enable thinking/reasoning
#' result <- generate_text(model, "Who are you?", 
#'   chat_template_kwargs = list(enable_thinking = TRUE))
#' print(result$reasoning)
#' }
create_nvidia <- function(api_key = NULL,
                          base_url = NULL,
                          headers = NULL) {
  NvidiaProvider$new(
    api_key = api_key,
    base_url = base_url,
    headers = headers
  )
}
