# SSEAggregator R6 Class

Accumulates streaming chunks into a final GenerateResult.

Handles two tool call formats:

- **OpenAI format**: Chunked deltas with index, id, function.name,
  function.arguments

- **Anthropic format**: content_block_start with id+name, then
  input_json_delta chunks

## Methods

### Public methods

- [`SSEAggregator$new()`](#method-SSEAggregator-new)

- [`SSEAggregator$on_text_delta()`](#method-SSEAggregator-on_text_delta)

- [`SSEAggregator$on_reasoning_delta()`](#method-SSEAggregator-on_reasoning_delta)

- [`SSEAggregator$on_reasoning_start()`](#method-SSEAggregator-on_reasoning_start)

- [`SSEAggregator$on_block_stop()`](#method-SSEAggregator-on_block_stop)

- [`SSEAggregator$on_tool_call_delta()`](#method-SSEAggregator-on_tool_call_delta)

- [`SSEAggregator$on_tool_start()`](#method-SSEAggregator-on_tool_start)

- [`SSEAggregator$on_tool_input_delta()`](#method-SSEAggregator-on_tool_input_delta)

- [`SSEAggregator$on_finish_reason()`](#method-SSEAggregator-on_finish_reason)

- [`SSEAggregator$on_usage()`](#method-SSEAggregator-on_usage)

- [`SSEAggregator$on_raw_response()`](#method-SSEAggregator-on_raw_response)

- [`SSEAggregator$on_done()`](#method-SSEAggregator-on_done)

- [`SSEAggregator$finalize()`](#method-SSEAggregator-finalize)

- [`SSEAggregator$clone()`](#method-SSEAggregator-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the aggregator.

#### Usage

    SSEAggregator$new(callback)

#### Arguments

- `callback`:

  User callback function: callback(text, done).

------------------------------------------------------------------------

### Method `on_text_delta()`

Handle a text content delta.

#### Usage

    SSEAggregator$on_text_delta(text)

#### Arguments

- `text`:

  The text chunk.

------------------------------------------------------------------------

### Method `on_reasoning_delta()`

Handle a reasoning/thinking content delta.

#### Usage

    SSEAggregator$on_reasoning_delta(text)

#### Arguments

- `text`:

  The reasoning text chunk.

------------------------------------------------------------------------

### Method `on_reasoning_start()`

Signal the start of a reasoning block (Anthropic thinking).

#### Usage

    SSEAggregator$on_reasoning_start()

------------------------------------------------------------------------

### Method `on_block_stop()`

Signal content block stop (closes reasoning if open).

#### Usage

    SSEAggregator$on_block_stop()

------------------------------------------------------------------------

### Method `on_tool_call_delta()`

Handle OpenAI-format tool call deltas.

#### Usage

    SSEAggregator$on_tool_call_delta(tool_calls)

#### Arguments

- `tool_calls`:

  List of tool call delta objects from the choices delta.

------------------------------------------------------------------------

### Method `on_tool_start()`

Handle Anthropic-format tool use block start.

#### Usage

    SSEAggregator$on_tool_start(index, id, name, input = NULL)

#### Arguments

- `index`:

  Block index (0-based from API, converted to 1-based internally).

- `id`:

  Tool call ID.

- `name`:

  Tool name.

- `input`:

  Initial input (usually NULL or empty).

------------------------------------------------------------------------

### Method `on_tool_input_delta()`

Handle Anthropic-format input_json_delta.

#### Usage

    SSEAggregator$on_tool_input_delta(index, partial_json)

#### Arguments

- `index`:

  Block index (0-based from API).

- `partial_json`:

  Partial JSON string.

------------------------------------------------------------------------

### Method `on_finish_reason()`

Store finish reason.

#### Usage

    SSEAggregator$on_finish_reason(reason)

#### Arguments

- `reason`:

  The finish reason string.

------------------------------------------------------------------------

### Method `on_usage()`

Store usage information.

#### Usage

    SSEAggregator$on_usage(usage)

#### Arguments

- `usage`:

  Usage list.

------------------------------------------------------------------------

### Method `on_raw_response()`

Store last raw response for diagnostics.

#### Usage

    SSEAggregator$on_raw_response(response)

#### Arguments

- `response`:

  The raw response data.

------------------------------------------------------------------------

### Method `on_done()`

Signal stream completion.

#### Usage

    SSEAggregator$on_done()

------------------------------------------------------------------------

### Method `finalize()`

Finalize accumulated state into a GenerateResult.

#### Usage

    SSEAggregator$finalize()

#### Returns

A GenerateResult object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SSEAggregator$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
