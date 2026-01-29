# Language Model V1 (Abstract Base Class)

Abstract interface for language models. All LLM providers must implement
this class. Uses `do_` prefix for internal methods to prevent direct
usage by end-users.

## Public fields

- `specification_version`:

  The version of this specification.

- `provider`:

  The provider identifier (e.g., "openai").

- `model_id`:

  The model identifier (e.g., "gpt-4o").

## Methods

### Public methods

- [`LanguageModelV1$new()`](#method-LanguageModelV1-new)

- [`LanguageModelV1$generate()`](#method-LanguageModelV1-generate)

- [`LanguageModelV1$stream()`](#method-LanguageModelV1-stream)

- [`LanguageModelV1$do_generate()`](#method-LanguageModelV1-do_generate)

- [`LanguageModelV1$do_stream()`](#method-LanguageModelV1-do_stream)

- [`LanguageModelV1$format_tool_result()`](#method-LanguageModelV1-format_tool_result)

- [`LanguageModelV1$get_history_format()`](#method-LanguageModelV1-get_history_format)

- [`LanguageModelV1$clone()`](#method-LanguageModelV1-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the model with provider and model ID.

#### Usage

    LanguageModelV1$new(provider, model_id)

#### Arguments

- `provider`:

  Provider name.

- `model_id`:

  Model ID.

------------------------------------------------------------------------

### Method `generate()`

Public generation method (wrapper for do_generate).

#### Usage

    LanguageModelV1$generate(...)

#### Arguments

- `...`:

  Call options passed to do_generate.

#### Returns

A GenerateResult object.

------------------------------------------------------------------------

### Method `stream()`

Public streaming method (wrapper for do_stream).

#### Usage

    LanguageModelV1$stream(callback, ...)

#### Arguments

- `callback`:

  Function to call with each chunk.

- `...`:

  Call options passed to do_stream.

#### Returns

A GenerateResult object.

------------------------------------------------------------------------

### Method `do_generate()`

Generate text (non-streaming). Abstract method.

#### Usage

    LanguageModelV1$do_generate(params)

#### Arguments

- `params`:

  A list of call options.

#### Returns

A GenerateResult object.

------------------------------------------------------------------------

### Method `do_stream()`

Generate text (streaming). Abstract method.

#### Usage

    LanguageModelV1$do_stream(params, callback)

#### Arguments

- `params`:

  A list of call options.

- `callback`:

  A function called for each chunk (text, done).

#### Returns

A GenerateResult object (accumulated from the stream).

------------------------------------------------------------------------

### Method `format_tool_result()`

Format a tool execution result for the provider's API.

#### Usage

    LanguageModelV1$format_tool_result(tool_call_id, tool_name, result_content)

#### Arguments

- `tool_call_id`:

  The ID of the tool call.

- `tool_name`:

  The name of the tool.

- `result_content`:

  The result content from executing the tool.

#### Returns

A list formatted as a message for this provider's API.

------------------------------------------------------------------------

### Method `get_history_format()`

Get the message format used by this model's API for history.

#### Usage

    LanguageModelV1$get_history_format()

#### Returns

A character string ("openai" or "anthropic").

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LanguageModelV1$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
