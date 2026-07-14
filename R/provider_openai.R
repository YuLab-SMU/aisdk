#' @name provider_openai
#' @title OpenAI Provider
#' @description
#' Implementation for OpenAI models.
#' @keywords internal
NULL

#' @title OpenAI Provider Class
#' @description
#' Provider class for OpenAI. Can create language and embedding models.
#' @export
OpenAIProvider <- R6::R6Class(
  "OpenAIProvider",
  public = list(
    #' @field specification_version Provider spec version.
    specification_version = "v1",

    #' @description Initialize the OpenAI provider.
    #' @param api_key OpenAI API key. Defaults to OPENAI_API_KEY env var.
    #' @param base_url Base URL for API calls. Defaults to https://api.openai.com/v1.
    #' @param organization Optional OpenAI organization ID.
    #' @param project Optional OpenAI project ID.
    #' @param headers Optional additional headers.
    #' @param name Optional provider name override (for compatible APIs).
    #' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
    #' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
    #' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
    #' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
    #' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
    #' @param disable_stream_options Disable stream_options parameter (for providers that don't support it).
    #' @param api_format Default API surface for `smart_model()` / `model()`:
    #'   "auto" (route reasoning models to Responses, others to Chat — the
    #'   canonical OpenAI behavior), "chat" (always Chat Completions — useful
    #'   for proxies that don't expose /responses, or that surface reasoning
    #'   models like gpt-5.x via /chat/completions), or "responses" (always
    #'   Responses API). The explicit `language_model()` and `responses_model()`
    #'   methods continue to ignore this setting.
    #' @param responses_state_mode Server-side conversation state policy for the
    #'   Responses API: "auto"/"server" send `previous_response_id` to chain
    #'   turns (auto-degrading to full-history resend if the endpoint rejects
    #'   it), while "stateless" always resends the full transcript and never
    #'   sends `previous_response_id`. First-party OpenAI defaults to "auto";
    #'   HTTP-stateless Responses proxies should use "stateless".
    initialize = function(api_key = NULL,
                          base_url = NULL,
                          organization = NULL,
                          project = NULL,
                          headers = NULL,
                          name = NULL,
                          timeout_seconds = NULL,
                          total_timeout_seconds = NULL,
                          first_byte_timeout_seconds = NULL,
                          connect_timeout_seconds = NULL,
                          idle_timeout_seconds = NULL,
                          disable_stream_options = FALSE,
                          api_format = c("auto", "chat", "responses"),
                          responses_state_mode = c("auto", "stateless", "server")) {
      api_format <- match.arg(api_format)
      responses_state_mode <- match.arg(responses_state_mode)
      env_base_url <- Sys.getenv("OPENAI_BASE_URL", unset = "")
      raw_base_url <- base_url %||% c(
        if (nzchar(trimws(env_base_url))) env_base_url else "https://api.openai.com/v1",
        Sys.getenv("OPENAI_BASE_URLS", unset = "")
      )
      base_urls <- normalize_base_urls(raw_base_url)
      if (length(base_urls) == 0L) {
        base_urls <- "https://api.openai.com/v1"
      }
      private$config <- list(
        api_key = api_key %||% Sys.getenv("OPENAI_API_KEY"),
        base_url = base_urls[[1]],
        base_urls = base_urls,
        organization = organization,
        project = project,
        headers = headers,
        provider_name = name %||% "openai",
        timeout_seconds = timeout_seconds,
        total_timeout_seconds = total_timeout_seconds,
        first_byte_timeout_seconds = first_byte_timeout_seconds,
        connect_timeout_seconds = connect_timeout_seconds,
        idle_timeout_seconds = idle_timeout_seconds,
        disable_stream_options = disable_stream_options,
        api_format = api_format,
        responses_state_mode = responses_state_mode
      )

      if (nchar(private$config$api_key) == 0) {
        rlang::warn("OpenAI API key not set. Set OPENAI_API_KEY env var or pass api_key parameter.")
      }
    },

    #' @description Create a language model (always Chat Completions API).
    #' @param model_id The model ID (e.g., "gpt-4o", "gpt-4o-mini").
    #' @return An OpenAILanguageModel object.
    language_model = function(model_id = Sys.getenv("OPENAI_MODEL", "gpt-4o")) {
      OpenAILanguageModel$new(model_id, private$config)
    },

    #' @description Create a language model using the Responses API.
    #' @param model_id The model ID (e.g., "o1", "o3-mini", "gpt-4o").
    #' @return An OpenAIResponsesLanguageModel object.
    #' @details
    #' The Responses API is designed for:
    #' - Models with built-in reasoning (o1, o3 series)
    #' - Stateful multi-turn conversations (server maintains history)
    #' - Advanced features like structured outputs
    #'
    #' The model maintains conversation state internally via response IDs.
    #' Call `model$reset()` to start a fresh conversation.
    responses_model = function(model_id) {
      OpenAIResponsesLanguageModel$new(model_id, private$config)
    },

    #' @description Default-route factory. Picks chat vs responses based on the
    #' `api_format` set in `create_openai()`. Recommended entry point for
    #' callers that don't care which surface is used.
    #' @param model_id The model ID. Defaults to `OPENAI_MODEL` env var.
    #' @return A LanguageModelV1 instance.
    model = function(model_id = Sys.getenv("OPENAI_MODEL", "gpt-4o")) {
      self$smart_model(model_id, api_format = private$config$api_format %||% "auto")
    },

    #' @description Smart model factory that selects the API surface.
    #' @param model_id The model ID.
    #' @param api_format API format: "auto" (default — reasoning → Responses,
    #'   others → Chat), "chat", or "responses". Defaults to the
    #'   `api_format` passed to `create_openai()`.
    #' @return A language model object (either OpenAILanguageModel or OpenAIResponsesLanguageModel).
    #' @details
    #' When `api_format = "auto"`, the method picks:
    #' - Responses API for reasoning models (o1, o3, gpt-5, ...)
    #' - Chat Completions API for everything else
    #'
    #' Override per-call when the provider's default doesn't match the
    #' specific model you're about to use.
    smart_model = function(model_id,
                           api_format = private$config$api_format %||% "auto") {
      api_format <- match.arg(api_format, c("auto", "chat", "responses"))

      if (api_format == "auto") {
        # Reasoning models use Responses API
        # Pattern matches: o1, o3, o1-mini, o3-mini, o1-preview, gpt-5, gpt-5-mini, etc.
        is_reasoning_model <- grepl("^o[0-9]|^gpt-5", model_id, ignore.case = TRUE)

        if (is_reasoning_model) {
          api_format <- "responses"
        } else {
          api_format <- "chat"
        }
      }

      switch(api_format,
        "chat" = self$language_model(model_id),
        "responses" = self$responses_model(model_id),
        stop("Unknown api_format: ", api_format)
      )
    },

    #' @description Create an embedding model.
    #' @param model_id The model ID (e.g., "text-embedding-3-small").
    #' @return An OpenAIEmbeddingModel object.
    embedding_model = function(model_id = "text-embedding-3-small") {
      OpenAIEmbeddingModel$new(model_id, private$config)
    },

    #' @description Create an image model.
    #' @param model_id The model ID (e.g., "gpt-image-2", "gpt-image-1.5").
    #' @return An OpenAIImageModel object.
    image_model = function(model_id = Sys.getenv("OPENAI_IMAGE_MODEL", "gpt-image-2")) {
      OpenAIImageModel$new(model_id, private$config)
    },

    #' @description Create a server-side conversation object via
    #'   `POST /v1/conversations`. Returns the parsed response, including the
    #'   conversation `id` you can pass as `conversation = "conv_..."` to
    #'   `generate_text()` / `stream_text()` so OpenAI manages the message
    #'   history server-side instead of you sending the full transcript each
    #'   turn.
    #' @param items Optional list of initial conversation items, each shaped
    #'   like `list(type = "message", role = "user", content = "Hello!")`.
    #' @param metadata Optional named list (up to 16 keys, values stringified).
    #' @return Parsed response list with at least `id`, `object`, `created_at`,
    #'   `metadata`.
    create_conversation = function(items = NULL, metadata = NULL) {
      body <- list()
      if (!is.null(items))    body$items    <- items
      if (!is.null(metadata)) body$metadata <- metadata
      private$request_conversations_api("POST", path = "", body = body)
    },

    #' @description Retrieve a conversation object by id.
    #' @param conversation_id Conversation id returned from `create_conversation()`.
    #' @return Parsed response list.
    get_conversation = function(conversation_id) {
      if (!is.character(conversation_id) || !nzchar(conversation_id)) {
        rlang::abort("`conversation_id` must be a non-empty string.")
      }
      private$request_conversations_api("GET", path = conversation_id)
    },

    #' @description Delete a conversation object by id. Server-side history
    #'   is irrecoverable after this call.
    #' @param conversation_id Conversation id returned from `create_conversation()`.
    #' @return Parsed response list (typically `list(id, object, deleted)`).
    delete_conversation = function(conversation_id) {
      if (!is.character(conversation_id) || !nzchar(conversation_id)) {
        rlang::abort("`conversation_id` must be a non-empty string.")
      }
      private$request_conversations_api("DELETE", path = conversation_id)
    }
  ),
  private = list(
    config = NULL,
    request_conversations_api = function(method, path = "", body = NULL) {
      url <- api_endpoint_urls(private$config, paste0("/conversations", if (nzchar(path)) paste0("/", path) else ""))
      headers <- list(`Content-Type` = "application/json")
      if (nzchar(private$config$api_key %||% "")) {
        headers$Authorization <- paste("Bearer", private$config$api_key)
      }
      if (!is.null(private$config$organization)) {
        headers$`OpenAI-Organization` <- private$config$organization
      }
      if (!is.null(private$config$headers)) {
        headers <- c(headers, private$config$headers)
      }

      request_json_from_api(
        url,
        headers,
        method = method,
        body = body,
        timeout_seconds = private$config$timeout_seconds,
        total_timeout_seconds = private$config$total_timeout_seconds,
        first_byte_timeout_seconds = private$config$first_byte_timeout_seconds,
        connect_timeout_seconds = private$config$connect_timeout_seconds,
        idle_timeout_seconds = private$config$idle_timeout_seconds
      )
    }
  )
)

#' @title Create OpenAI Provider
#' @description
#' Factory function to create an OpenAI provider.
#'
#' @eval generate_model_docs("openai")
#'
#' @section Token Limit Parameters:
#' The SDK provides a unified `max_tokens` parameter that automatically maps to the
#' correct API field based on the model and API type:
#'
#' \itemize{
#'   \item **Chat API (standard models)**: `max_tokens` -> `max_tokens`
#'   \item **Chat API (o1/o3 models)**: `max_tokens` -> `max_completion_tokens`
#'   \item **Responses API**: `max_tokens` -> `max_output_tokens` (total: reasoning + answer)
#' }
#'
#' For advanced users who need fine-grained control:
#' \itemize{
#'   \item `max_completion_tokens`: Explicitly set completion tokens (Chat API, o1/o3)
#'   \item `max_output_tokens`: Explicitly set total output limit (Responses API)
#'   \item `max_answer_tokens`: Limit answer only, excluding reasoning (Responses API, Volcengine-specific)
#' }
#'
#' @param api_key OpenAI API key. Defaults to OPENAI_API_KEY env var.
#' @param base_url Base URL for API calls. Defaults to https://api.openai.com/v1.
#' @param organization Optional OpenAI organization ID.
#' @param project Optional OpenAI project ID.
#' @param headers Optional additional headers.
#' @param name Optional provider name override (for compatible APIs).
#' @param timeout_seconds Legacy alias for `total_timeout_seconds`.
#' @param total_timeout_seconds Optional total request timeout in seconds for API calls.
#' @param first_byte_timeout_seconds Optional time-to-first-byte timeout in seconds for API calls.
#' @param connect_timeout_seconds Optional connection-establishment timeout in seconds for API calls.
#' @param idle_timeout_seconds Optional stall timeout in seconds for API calls.
#' @param disable_stream_options Disable stream_options parameter (for providers like Volcengine that don't support it).
#' @param api_format Default API surface for `smart_model()` / `model()`: `"auto"` (default, picks Chat or Responses based on model), `"chat"` (always Chat Completions), or `"responses"` (always Responses API).
#' @param responses_state_mode Server-side conversation state policy for the Responses API: `"auto"`/`"server"` send `previous_response_id` to chain turns (auto-degrading to a full-history resend if the endpoint rejects it), while `"stateless"` always resends the full transcript. First-party OpenAI defaults to `"auto"`; HTTP-stateless Responses proxies should use `"stateless"`.
#' @return An OpenAIProvider object.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # Basic usage with Chat Completions API
#'   openai <- create_openai(api_key = "sk-...")
#'   model <- openai$language_model("gpt-4o")
#'   result <- generate_text(model, "Hello!")
#'
#'   # Using Responses API for reasoning models
#'   openai <- create_openai()
#'   model <- openai$responses_model("o1")
#'   result <- generate_text(model, "Solve this math problem...")
#'   print(result$reasoning) # Access chain-of-thought
#'
#'   # Smart model selection (auto-detects best API)
#'   model <- openai$smart_model("o3-mini") # Uses Responses API
#'   model <- openai$smart_model("gpt-4o") # Uses Chat Completions API
#'
#'   # Token limits - unified interface
#'   # For standard models: limits generated content
#'   result <- model$generate(messages = msgs, max_tokens = 1000)
#'
#'   # For o1/o3 models: automatically maps to max_completion_tokens
#'   model_o1 <- openai$language_model("o1")
#'   result <- model_o1$generate(messages = msgs, max_tokens = 2000)
#'
#'   # For Responses API: automatically maps to max_output_tokens (total limit)
#'   model_resp <- openai$responses_model("o1")
#'   result <- model_resp$generate(messages = msgs, max_tokens = 2000)
#'
#'   # Advanced: explicitly control answer-only limit (Volcengine Responses API)
#'   result <- model_resp$generate(messages = msgs, max_answer_tokens = 500)
#'
#'   # Multi-turn conversation with Responses API
#'   model <- openai$responses_model("o1")
#'   result1 <- generate_text(model, "What is 2+2?")
#'   result2 <- generate_text(model, "Now multiply that by 3") # Remembers context
#'   model$reset() # Start fresh conversation
#' }
#' }
create_openai <- function(api_key = NULL,
                          base_url = NULL,
                          organization = NULL,
                          project = NULL,
                          headers = NULL,
                          name = NULL,
                          timeout_seconds = NULL,
                          total_timeout_seconds = NULL,
                          first_byte_timeout_seconds = NULL,
                          connect_timeout_seconds = NULL,
                          idle_timeout_seconds = NULL,
                          disable_stream_options = FALSE,
                          api_format = c("auto", "chat", "responses"),
                          responses_state_mode = c("auto", "stateless", "server")) {
  api_format <- match.arg(api_format)
  responses_state_mode <- match.arg(responses_state_mode)
  OpenAIProvider$new(
    api_key = api_key,
    base_url = base_url,
    organization = organization,
    project = project,
    headers = headers,
    name = name,
    timeout_seconds = timeout_seconds,
    total_timeout_seconds = total_timeout_seconds,
    first_byte_timeout_seconds = first_byte_timeout_seconds,
    connect_timeout_seconds = connect_timeout_seconds,
    idle_timeout_seconds = idle_timeout_seconds,
    disable_stream_options = disable_stream_options,
    api_format = api_format,
    responses_state_mode = responses_state_mode
  )
}

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
