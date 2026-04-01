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
  lock_objects = FALSE,
  public = list(
    #' @field text The generated text content.
    text = NULL,
    #' @field usage Token usage information.
    usage = NULL,
    #' @field finish_reason Reason the model stopped generating.
    finish_reason = NULL,
    #' @field warnings Any warnings from the model.
    warnings = NULL,
    #' @field raw_response The raw response from the provider API.
    raw_response = NULL,
    #' @field tool_calls List of tool calls requested by the model.
    tool_calls = NULL,
    #' @field steps Number of ReAct loop steps taken.
    steps = NULL,
    #' @field all_tool_calls Accumulated list of all tool calls across steps.
    all_tool_calls = NULL,
    #' @field reasoning Optional chain-of-thought or reasoning content.
    reasoning = NULL,
    #' @field response_id Server-side response ID for Responses-style APIs.
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
    #' @param response_id Server-side response ID for Responses-style APIs.
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
    },

    #' @description Print method for GenerateResult.
    print = function() {
      if (!is.null(self$text) && nzchar(self$text)) {
        cat(self$text, "\n")
      } else if (!is.null(self$tool_calls) && length(self$tool_calls) > 0) {
        cat(sprintf("<GenerateResult: %d tool calls>\n", length(self$tool_calls)))
      } else {
        cat("<GenerateResult>\n")
      }
      invisible(self)
    }
  )
)

#' @title Generate Image Result
#' @description Result object returned by image generation and editing models.
#' @export
GenerateImageResult <- R6::R6Class(
  "GenerateImageResult",
  lock_objects = FALSE,
  public = list(
    #' @field images Generated image artifacts.
    images = NULL,
    #' @field text Optional textual companion output.
    text = NULL,
    #' @field usage Usage information from the provider.
    usage = NULL,
    #' @field finish_reason Reason the model stopped generating.
    finish_reason = NULL,
    #' @field warnings Any warnings from the model.
    warnings = NULL,
    #' @field raw_response The raw response from the provider API.
    raw_response = NULL,

    #' @description Initialize a GenerateImageResult object.
    #' @param images Generated image artifacts.
    #' @param text Optional text output.
    #' @param usage Usage information.
    #' @param finish_reason Finish reason.
    #' @param warnings Warnings.
    #' @param raw_response Raw provider response.
    initialize = function(images = NULL, text = NULL, usage = NULL,
                          finish_reason = NULL, warnings = NULL,
                          raw_response = NULL) {
      self$images <- images
      self$text <- text
      self$usage <- usage
      self$finish_reason <- finish_reason
      self$warnings <- warnings
      self$raw_response <- raw_response
    },

    #' @description Print method for GenerateImageResult.
    print = function() {
      n_images <- length(self$images %||% list())
      cat(sprintf("<GenerateImageResult: %d image%s>\n", n_images, if (n_images == 1) "" else "s"))
      invisible(self)
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
    #' @field capabilities Model capability flags.
    capabilities = list(),

    #' @description Initialize the model with provider and model ID.
    #' @param provider Provider name.
    #' @param model_id Model ID.
    #' @param capabilities Optional list of capability flags.
    initialize = function(provider, model_id, capabilities = list()) {
      self$provider <- provider
      self$model_id <- model_id
      self$capabilities <- capabilities
    },

    #' @description Check if the model has a specific capability.
    #' @param cap Capability name.
    #' @return Logical.
    has_capability = function(cap) {
      isTRUE(self$capabilities[[cap]])
    },

    #' @description Public generation method.
    #' @param ... Call options passed to `do_generate()`.
    #' @return A GenerateResult object.
    generate = function(...) {
      self$do_generate(list(...))
    },

    #' @description Public streaming method.
    #' @param callback Callback invoked with `(text, done)`.
    #' @param ... Call options passed to `do_stream()`.
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
    #' @param callback Callback invoked with `(text, done)`.
    #' @return A GenerateResult object.
    do_stream = function(params, callback) {
      rlang::abort("LanguageModelV1$do_stream() must be implemented by subclass.")
    },

    #' @description Format a tool execution result for the provider API.
    #' @param tool_call_id The tool call identifier.
    #' @param tool_name The tool name.
    #' @param result_content The tool execution result.
    #' @return A provider-specific tool result message.
    format_tool_result = function(tool_call_id, tool_name, result_content) {
      rlang::abort("LanguageModelV1$format_tool_result() must be implemented by subclass.")
    },

    #' @description Get the message history format used by this model.
    #' @return A character string such as `"openai"` or `"anthropic"`.
    get_history_format = function() {
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
    #' @return A provider-specific embedding result.
    do_embed = function(value) {
      rlang::abort("EmbeddingModelV1$do_embed() must be implemented by subclass.")
    }
  )
)

#' @title Image Model V1 (Abstract Base Class)
#' @description
#' Abstract interface for image generation and editing models.
#' @export
ImageModelV1 <- R6::R6Class(
  "ImageModelV1",
  public = list(
    #' @field specification_version The version of this specification.
    specification_version = "v1",
    #' @field provider The provider identifier.
    provider = NULL,
    #' @field model_id The model identifier.
    model_id = NULL,
    #' @field capabilities Model capability flags.
    capabilities = list(),

    #' @description Initialize the image model.
    #' @param provider Provider name.
    #' @param model_id Model ID.
    #' @param capabilities Optional list of capability flags.
    initialize = function(provider, model_id, capabilities = list()) {
      self$provider <- provider
      self$model_id <- model_id
      self$capabilities <- capabilities
    },

    #' @description Check if the image model has a specific capability.
    #' @param cap Capability name.
    #' @return Logical.
    has_capability = function(cap) {
      isTRUE(self$capabilities[[cap]])
    },

    #' @description Public image generation method.
    #' @param ... Call options passed to `do_generate_image()`.
    #' @return A GenerateImageResult object.
    generate_image = function(...) {
      self$do_generate_image(list(...))
    },

    #' @description Public image editing method.
    #' @param ... Call options passed to `do_edit_image()`.
    #' @return A GenerateImageResult object.
    edit_image = function(...) {
      self$do_edit_image(list(...))
    },

    #' @description Generate images. Abstract method.
    #' @param params A list of call options.
    #' @return A GenerateImageResult object.
    do_generate_image = function(params) {
      rlang::abort("ImageModelV1$do_generate_image() must be implemented by subclass.")
    },

    #' @description Edit images. Abstract method.
    #' @param params A list of call options.
    #' @return A GenerateImageResult object.
    do_edit_image = function(params) {
      rlang::abort("ImageModelV1$do_edit_image() must be implemented by subclass.")
    }
  )
)
