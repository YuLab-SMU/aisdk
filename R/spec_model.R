#' @title Specification Layer: Model Interfaces
#' @description Abstract base classes (interfaces) for AI models.
#' @name spec_model
NULL

#' @title Generate Result
#' @description Result object returned by model generation.
#' @details
#' This class uses `lock_objects = FALSE` to allow dynamic field addition.
#' This enables the ReAct loop and other components to attach additional
#' metadata (like `steps`, `all_tool_calls`) without modifying the class.
#'
#' For models that support reasoning/thinking (like OpenAI o1/o3, DeepSeek, Claude with extended thinking),
#' the `reasoning` field contains the model's chain-of-thought content.
#'
#' For Responses API models, `response_id` contains the server-side response ID
#' which can be used for multi-turn conversations without sending full history.
#' @export
GenerateResult <- R6::R6Class(
  "GenerateResult",
  lock_objects = FALSE,  # Allow dynamic field addition
  public = list(
    #' @field text The generated text content.
    text = NULL,
    #' @field usage Token usage information (list with prompt_tokens, completion_tokens, total_tokens).
    usage = NULL,
    #' @field finish_reason Reason the model stopped generating.
    finish_reason = NULL,
    #' @field warnings Any warnings from the model.
    warnings = NULL,
    #' @field raw_response The raw response from the API.
    raw_response = NULL,
    #' @field tool_calls List of tool calls requested by the model. Each item contains id, name, arguments.
    tool_calls = NULL,
    #' @field steps Number of ReAct loop steps taken (when max_steps > 1).
    steps = NULL,
    #' @field all_tool_calls Accumulated list of all tool calls made across all ReAct steps.
    all_tool_calls = NULL,
    #' @field reasoning Chain-of-thought/reasoning content from models that support it (o1, o3, DeepSeek, etc.).
    reasoning = NULL,
    #' @field response_id Server-side response ID for Responses API (used for stateful multi-turn conversations).
    response_id = NULL,

    #' @description Initialize a GenerateResult object.
    #' @param text Generated text.
    #' @param usage Token usage.
    #' @param finish_reason Reason for stopping.
    #' @param warnings Warnings.
    #' @param raw_response Raw API response.
    #' @param tool_calls Tool calls requested by the model.
    #' @param steps Number of ReAct steps taken.
    #' @param all_tool_calls All tool calls across steps.
    #' @param reasoning Chain-of-thought content.
    #' @param response_id Server-side response ID for Responses API.
    initialize = function(text = NULL, usage = NULL, finish_reason = NULL,
                          warnings = NULL, raw_response = NULL, tool_calls = NULL,
                          steps = NULL, all_tool_calls = NULL,
                          reasoning = NULL, response_id = NULL) {
      self$text <- text
      self$usage <- usage
      self$finish_reason <- finish_reason
      self$warnings <- warnings
      self$raw_response <- raw_response
      self$tool_calls <- tool_calls
      self$steps <- steps
      self$all_tool_calls <- all_tool_calls
      self$reasoning <- reasoning
      self$response_id <- response_id
    }
  )
)

#' @title Language Model V1 (Abstract Base Class)
#' @description
#' Abstract interface for language models. All LLM providers must implement this class.
#' Uses `do_` prefix for internal methods to prevent direct usage by end-users.
#' @export
LanguageModelV1 <- R6::R6Class(
  "LanguageModelV1",
  public = list(
    #' @field specification_version The version of this specification.
    specification_version = "v1",
    #' @field provider The provider identifier (e.g., "openai").
    provider = NULL,
    #' @field model_id The model identifier (e.g., "gpt-4o").
    model_id = NULL,

    #' @description Initialize the model with provider and model ID.
    #' @param provider Provider name.
    #' @param model_id Model ID.
    initialize = function(provider, model_id) {
      self$provider <- provider
      self$model_id <- model_id
    },

    #' @description Public generation method (wrapper for do_generate).
    #' @param ... Call options passed to do_generate.
    #' @return A GenerateResult object.
    generate = function(...) {
      self$do_generate(list(...))
    },

    #' @description Public streaming method (wrapper for do_stream).
    #' @param callback Function to call with each chunk.
    #' @param ... Call options passed to do_stream.
    #' @return A GenerateResult object.
    stream = function(callback, ...) {
      self$do_stream(list(...), callback)
    },

    #' @description Generate text (non-streaming). Abstract method.
    #' @param params A list of call options.
    #' @return A GenerateResult object.
    do_generate = function(params) {
      rlang::abort("LanguageModelV1$do_generate() must be implemented by subclass.")
    },

    #' @description Generate text (streaming). Abstract method.
    #' @param params A list of call options.
    #' @param callback A function called for each chunk (text, done).
    #' @return A GenerateResult object (accumulated from the stream).
    do_stream = function(params, callback) {
      rlang::abort("LanguageModelV1$do_stream() must be implemented by subclass.")
    },

    #' @description Format a tool execution result for the provider's API.
    #' @param tool_call_id The ID of the tool call.
    #' @param tool_name The name of the tool.
    #' @param result_content The result content from executing the tool.
    #' @return A list formatted as a message for this provider's API.
    format_tool_result = function(tool_call_id, tool_name, result_content) {
      rlang::abort("LanguageModelV1$format_tool_result() must be implemented by subclass.")
    },

    #' @description Get the message format used by this model's API for history.
    #' @return A character string ("openai" or "anthropic").
    get_history_format = function() {
       # Subclasses should override this.
       # Defaulting to "openai" as it's the most common standard for proxies.
       "openai"
    }
  )
)

#' @title Embedding Model V1 (Abstract Base Class)
#' @description
#' Abstract interface for embedding models.
#' @export
EmbeddingModelV1 <- R6::R6Class(
  "EmbeddingModelV1",
  public = list(
    #' @field specification_version The version of this specification.
    specification_version = "v1",
    #' @field provider The provider identifier.
    provider = NULL,
    #' @field model_id The model identifier.
    model_id = NULL,

    #' @description Initialize the embedding model.
    #' @param provider Provider name.
    #' @param model_id Model ID.
    initialize = function(provider, model_id) {
      self$provider <- provider
      self$model_id <- model_id
    },

    #' @description Generate embeddings for a value. Abstract method.
    #' @param value A character string or vector to embed.
    #' @return A list with embeddings.
    do_embed = function(value) {
      rlang::abort("EmbeddingModelV1$do_embed() must be implemented by subclass.")
    }
  )
)
