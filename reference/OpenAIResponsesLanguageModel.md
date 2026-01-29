# OpenAI Responses Language Model Class

Language model implementation for OpenAI's Responses API. This API is
designed for stateful multi-turn conversations where the server
maintains conversation history, and supports advanced features like:

- Built-in reasoning/thinking (for o1, o3 models)

- Server-side conversation state management via response IDs

- Structured output items (reasoning, message, tool calls)

The Responses API uses a different request/response format than Chat
Completions:

- Request: `input` field instead of `messages`, optional
  `previous_response_id`

- Response: `output` array with typed items instead of `choices`

## Super class

[`aisdk::LanguageModelV1`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.md)
-\> `OpenAIResponsesLanguageModel`

## Methods

### Public methods

- [`OpenAIResponsesLanguageModel$new()`](#method-OpenAIResponsesLanguageModel-new)

- [`OpenAIResponsesLanguageModel$get_config()`](#method-OpenAIResponsesLanguageModel-get_config)

- [`OpenAIResponsesLanguageModel$get_last_response_id()`](#method-OpenAIResponsesLanguageModel-get_last_response_id)

- [`OpenAIResponsesLanguageModel$reset()`](#method-OpenAIResponsesLanguageModel-reset)

- [`OpenAIResponsesLanguageModel$do_generate()`](#method-OpenAIResponsesLanguageModel-do_generate)

- [`OpenAIResponsesLanguageModel$do_stream()`](#method-OpenAIResponsesLanguageModel-do_stream)

- [`OpenAIResponsesLanguageModel$format_tool_result()`](#method-OpenAIResponsesLanguageModel-format_tool_result)

- [`OpenAIResponsesLanguageModel$get_history_format()`](#method-OpenAIResponsesLanguageModel-get_history_format)

- [`OpenAIResponsesLanguageModel$clone()`](#method-OpenAIResponsesLanguageModel-clone)

Inherited methods

- [`aisdk::LanguageModelV1$generate()`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.html#method-generate)
- [`aisdk::LanguageModelV1$stream()`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.html#method-stream)

------------------------------------------------------------------------

### Method `new()`

Initialize the OpenAI Responses language model.

#### Usage

    OpenAIResponsesLanguageModel$new(model_id, config)

#### Arguments

- `model_id`:

  The model ID (e.g., "o1", "o3-mini", "gpt-4o").

- `config`:

  Configuration list with api_key, base_url, headers, etc.

------------------------------------------------------------------------

### Method `get_config()`

Get the configuration list.

#### Usage

    OpenAIResponsesLanguageModel$get_config()

#### Returns

A list with provider configuration.

------------------------------------------------------------------------

### Method `get_last_response_id()`

Get the last response ID (for debugging/advanced use).

#### Usage

    OpenAIResponsesLanguageModel$get_last_response_id()

#### Returns

The last response ID or NULL.

------------------------------------------------------------------------

### Method `reset()`

Reset the conversation state (clear response ID). Call this to start a
fresh conversation.

#### Usage

    OpenAIResponsesLanguageModel$reset()

------------------------------------------------------------------------

### Method `do_generate()`

Generate text (non-streaming) using Responses API.

#### Usage

    OpenAIResponsesLanguageModel$do_generate(params)

#### Arguments

- `params`:

  A list of call options including messages, temperature, etc.

#### Returns

A GenerateResult object.

------------------------------------------------------------------------

### Method `do_stream()`

Generate text (streaming) using Responses API.

#### Usage

    OpenAIResponsesLanguageModel$do_stream(params, callback)

#### Arguments

- `params`:

  A list of call options.

- `callback`:

  A function called for each chunk: callback(text, done).

#### Returns

A GenerateResult object.

------------------------------------------------------------------------

### Method `format_tool_result()`

Format a tool execution result for Responses API.

#### Usage

    OpenAIResponsesLanguageModel$format_tool_result(
      tool_call_id,
      tool_name,
      result_content
    )

#### Arguments

- `tool_call_id`:

  The ID of the tool call.

- `tool_name`:

  The name of the tool.

- `result_content`:

  The result content from executing the tool.

#### Returns

A list formatted as a message for Responses API.

------------------------------------------------------------------------

### Method `get_history_format()`

Get the message format for Responses API.

#### Usage

    OpenAIResponsesLanguageModel$get_history_format()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OpenAIResponsesLanguageModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
