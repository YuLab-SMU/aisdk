# OpenAI Language Model Class

Language model implementation for OpenAI's chat completions API.

## Super class

[`aisdk::LanguageModelV1`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.md)
-\> `OpenAILanguageModel`

## Methods

### Public methods

- [`OpenAILanguageModel$new()`](#method-OpenAILanguageModel-new)

- [`OpenAILanguageModel$get_config()`](#method-OpenAILanguageModel-get_config)

- [`OpenAILanguageModel$build_payload()`](#method-OpenAILanguageModel-build_payload)

- [`OpenAILanguageModel$execute_request()`](#method-OpenAILanguageModel-execute_request)

- [`OpenAILanguageModel$parse_response()`](#method-OpenAILanguageModel-parse_response)

- [`OpenAILanguageModel$do_generate()`](#method-OpenAILanguageModel-do_generate)

- [`OpenAILanguageModel$build_stream_payload()`](#method-OpenAILanguageModel-build_stream_payload)

- [`OpenAILanguageModel$do_stream()`](#method-OpenAILanguageModel-do_stream)

- [`OpenAILanguageModel$format_tool_result()`](#method-OpenAILanguageModel-format_tool_result)

- [`OpenAILanguageModel$get_history_format()`](#method-OpenAILanguageModel-get_history_format)

- [`OpenAILanguageModel$clone()`](#method-OpenAILanguageModel-clone)

Inherited methods

- [`aisdk::LanguageModelV1$generate()`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.html#method-generate)
- [`aisdk::LanguageModelV1$has_capability()`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.html#method-has_capability)
- [`aisdk::LanguageModelV1$stream()`](https://YuLab-SMU.github.io/aisdk/reference/LanguageModelV1.html#method-stream)

------------------------------------------------------------------------

### Method `new()`

Initialize the OpenAI language model.

#### Usage

    OpenAILanguageModel$new(model_id, config, capabilities = list())

#### Arguments

- `model_id`:

  The model ID (e.g., "gpt-4o").

- `config`:

  Configuration list with api_key, base_url, headers, etc.

- `capabilities`:

  Optional list of capability flags.

------------------------------------------------------------------------

### Method `get_config()`

Get the configuration list.

#### Usage

    OpenAILanguageModel$get_config()

#### Returns

A list with provider configuration.

------------------------------------------------------------------------

### Method `build_payload()`

Build the request payload for non-streaming generation. Subclasses can
override to customize payload construction.

#### Usage

    OpenAILanguageModel$build_payload(params)

#### Arguments

- `params`:

  A list of call options.

#### Returns

A list with url, headers, and body.

------------------------------------------------------------------------

### Method `execute_request()`

Execute the API request.

#### Usage

    OpenAILanguageModel$execute_request(url, headers, body)

#### Arguments

- `url`:

  The API endpoint URL.

- `headers`:

  A named list of HTTP headers.

- `body`:

  The request body.

#### Returns

The parsed API response.

------------------------------------------------------------------------

### Method `parse_response()`

Parse the API response into a GenerateResult. Subclasses can override to
extract provider-specific fields (e.g., reasoning_content).

#### Usage

    OpenAILanguageModel$parse_response(response)

#### Arguments

- `response`:

  The parsed API response.

#### Returns

A GenerateResult object.

------------------------------------------------------------------------

### Method `do_generate()`

Generate text (non-streaming). Uses template method pattern.

#### Usage

    OpenAILanguageModel$do_generate(params)

#### Arguments

- `params`:

  A list of call options including messages, temperature, etc.

#### Returns

A GenerateResult object.

------------------------------------------------------------------------

### Method `build_stream_payload()`

Build the request payload for streaming generation. Subclasses can
override to customize stream payload construction.

#### Usage

    OpenAILanguageModel$build_stream_payload(params)

#### Arguments

- `params`:

  A list of call options.

#### Returns

A list with url, headers, and body.

------------------------------------------------------------------------

### Method `do_stream()`

Generate text (streaming).

#### Usage

    OpenAILanguageModel$do_stream(params, callback)

#### Arguments

- `params`:

  A list of call options.

- `callback`:

  A function called for each chunk: callback(text, done).

#### Returns

A GenerateResult object.

------------------------------------------------------------------------

### Method `format_tool_result()`

Format a tool execution result for OpenAI's API.

#### Usage

    OpenAILanguageModel$format_tool_result(tool_call_id, tool_name, result_content)

#### Arguments

- `tool_call_id`:

  The ID of the tool call.

- `tool_name`:

  The name of the tool (not used by OpenAI but kept for interface
  consistency).

- `result_content`:

  The result content from executing the tool.

#### Returns

A list formatted as a message for OpenAI's API.

------------------------------------------------------------------------

### Method `get_history_format()`

Get the message format for OpenAI.

#### Usage

    OpenAILanguageModel$get_history_format()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OpenAILanguageModel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
