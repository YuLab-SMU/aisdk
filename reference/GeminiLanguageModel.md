# Gemini Language Model Class

Language model implementation for Gemini's generateContent API.

## Super class

[`aisdk::LanguageModelV1`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.md)
-\> `GeminiLanguageModel`

## Methods

### Public methods

- [`GeminiLanguageModel$new()`](#method-GeminiLanguageModel-new)

- [`GeminiLanguageModel$get_config()`](#method-GeminiLanguageModel-get_config)

- [`GeminiLanguageModel$build_payload_internal()`](#method-GeminiLanguageModel-build_payload_internal)

- [`GeminiLanguageModel$do_generate()`](#method-GeminiLanguageModel-do_generate)

- [`GeminiLanguageModel$do_stream()`](#method-GeminiLanguageModel-do_stream)

- [`GeminiLanguageModel$format_tool_result()`](#method-GeminiLanguageModel-format_tool_result)

- [`GeminiLanguageModel$get_history_format()`](#method-GeminiLanguageModel-get_history_format)

- [`GeminiLanguageModel$clone()`](#method-GeminiLanguageModel-clone)

Inherited methods

- [`aisdk::LanguageModelV1$generate()`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.html#method-generate)
- [`aisdk::LanguageModelV1$has_capability()`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.html#method-has_capability)
- [`aisdk::LanguageModelV1$stream()`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.html#method-stream)

------------------------------------------------------------------------

### Method `new()`

Initialize the Gemini language model.

#### Usage

    GeminiLanguageModel$new(model_id, config)

#### Arguments

- `model_id`:

  The model ID (e.g., "gemini-1.5-pro").

- `config`:

  Configuration list with api_key, base_url, headers, etc.

------------------------------------------------------------------------

### Method `get_config()`

Get the configuration list.

#### Usage

    GeminiLanguageModel$get_config()

#### Returns

A list with provider configuration.

------------------------------------------------------------------------

### Method `build_payload_internal()`

Build the request payload for generation

#### Usage

    GeminiLanguageModel$build_payload_internal(params, stream = FALSE)

#### Arguments

- `params`:

  A list of call options.

- `stream`:

  Whether to build for streaming

#### Returns

A list with url, headers, and body.

------------------------------------------------------------------------

### Method `do_generate()`

Generate text (non-streaming).

#### Usage

    GeminiLanguageModel$do_generate(params)

#### Arguments

- `params`:

  A list of call options including messages, temperature, etc.

#### Returns

A GenerateResult object.

------------------------------------------------------------------------

### Method `do_stream()`

Generate text (streaming).

#### Usage

    GeminiLanguageModel$do_stream(params, callback)

#### Arguments

- `params`:

  A list of call options.

- `callback`:

  A function called for each chunk: callback(text, done).

#### Returns

A GenerateResult object.

------------------------------------------------------------------------

### Method `format_tool_result()`

Format a tool execution result for Gemini's API.

#### Usage

    GeminiLanguageModel$format_tool_result(tool_call_id, tool_name, result_content)

#### Arguments

- `tool_call_id`:

  The ID of the tool call (unused in Gemini but present for interface
  compatibility).

- `tool_name`:

  The name of the tool.

- `result_content`:

  The result content from executing the tool.

#### Returns

A list formatted as a message for Gemini API.

------------------------------------------------------------------------

### Method `get_history_format()`

Get the message format for Gemini.

#### Usage

    GeminiLanguageModel$get_history_format()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GeminiLanguageModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
