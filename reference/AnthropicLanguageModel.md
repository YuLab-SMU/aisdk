# Anthropic Language Model Class

Language model implementation for Anthropic's Messages API.

## Super class

[`aisdk::LanguageModelV1`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.md)
-\> `AnthropicLanguageModel`

## Methods

### Public methods

- [`AnthropicLanguageModel$new()`](#method-AnthropicLanguageModel-new)

- [`AnthropicLanguageModel$get_config()`](#method-AnthropicLanguageModel-get_config)

- [`AnthropicLanguageModel$do_generate()`](#method-AnthropicLanguageModel-do_generate)

- [`AnthropicLanguageModel$do_stream()`](#method-AnthropicLanguageModel-do_stream)

- [`AnthropicLanguageModel$format_tool_result()`](#method-AnthropicLanguageModel-format_tool_result)

- [`AnthropicLanguageModel$get_history_format()`](#method-AnthropicLanguageModel-get_history_format)

- [`AnthropicLanguageModel$clone()`](#method-AnthropicLanguageModel-clone)

Inherited methods

- [`aisdk::LanguageModelV1$generate()`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.html#method-generate)
- [`aisdk::LanguageModelV1$stream()`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.html#method-stream)

------------------------------------------------------------------------

### Method `new()`

Initialize the Anthropic language model.

#### Usage

    AnthropicLanguageModel$new(model_id, config)

#### Arguments

- `model_id`:

  The model ID (e.g., "claude-sonnet-4-20250514").

- `config`:

  Configuration list with api_key, base_url, headers, etc.

------------------------------------------------------------------------

### Method `get_config()`

Get the configuration list.

#### Usage

    AnthropicLanguageModel$get_config()

#### Returns

A list with provider configuration.

------------------------------------------------------------------------

### Method `do_generate()`

Generate text (non-streaming).

#### Usage

    AnthropicLanguageModel$do_generate(params)

#### Arguments

- `params`:

  A list of call options including messages, temperature, etc.

#### Returns

A GenerateResult object.

------------------------------------------------------------------------

### Method `do_stream()`

Generate text (streaming).

#### Usage

    AnthropicLanguageModel$do_stream(params, callback)

#### Arguments

- `params`:

  A list of call options.

- `callback`:

  A function called for each chunk: callback(text, done).

#### Returns

A GenerateResult object.

------------------------------------------------------------------------

### Method `format_tool_result()`

Format a tool execution result for Anthropic's API.

#### Usage

    AnthropicLanguageModel$format_tool_result(
      tool_call_id,
      tool_name,
      result_content
    )

#### Arguments

- `tool_call_id`:

  The ID of the tool call (tool_use_id in Anthropic terms).

- `tool_name`:

  The name of the tool (not used by Anthropic but kept for interface
  consistency).

- `result_content`:

  The result content from executing the tool.

#### Returns

A list formatted as a message for Anthropic's API.

------------------------------------------------------------------------

### Method `get_history_format()`

Get the message format for Anthropic.

#### Usage

    AnthropicLanguageModel$get_history_format()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AnthropicLanguageModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
