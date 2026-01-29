# Generate Result

Result object returned by model generation.

## Details

This class uses `lock_objects = FALSE` to allow dynamic field addition.
This enables the ReAct loop and other components to attach additional
metadata (like `steps`, `all_tool_calls`) without modifying the class.

For models that support reasoning/thinking (like OpenAI o1/o3, DeepSeek,
Claude with extended thinking), the `reasoning` field contains the
model's chain-of-thought content.

For Responses API models, `response_id` contains the server-side
response ID which can be used for multi-turn conversations without
sending full history.

## Public fields

- `text`:

  The generated text content.

- `usage`:

  Token usage information (list with prompt_tokens, completion_tokens,
  total_tokens).

- `finish_reason`:

  Reason the model stopped generating.

- `warnings`:

  Any warnings from the model.

- `raw_response`:

  The raw response from the API.

- `tool_calls`:

  List of tool calls requested by the model. Each item contains id,
  name, arguments.

- `steps`:

  Number of ReAct loop steps taken (when max_steps \> 1).

- `all_tool_calls`:

  Accumulated list of all tool calls made across all ReAct steps.

- `reasoning`:

  Chain-of-thought/reasoning content from models that support it (o1,
  o3, DeepSeek, etc.).

- `response_id`:

  Server-side response ID for Responses API (used for stateful
  multi-turn conversations).

## Methods

### Public methods

- [`GenerateResult$new()`](#method-GenerateResult-new)

- [`GenerateResult$clone()`](#method-GenerateResult-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a GenerateResult object.

#### Usage

    GenerateResult$new(
      text = NULL,
      usage = NULL,
      finish_reason = NULL,
      warnings = NULL,
      raw_response = NULL,
      tool_calls = NULL,
      steps = NULL,
      all_tool_calls = NULL,
      reasoning = NULL,
      response_id = NULL
    )

#### Arguments

- `text`:

  Generated text.

- `usage`:

  Token usage.

- `finish_reason`:

  Reason for stopping.

- `warnings`:

  Warnings.

- `raw_response`:

  Raw API response.

- `tool_calls`:

  Tool calls requested by the model.

- `steps`:

  Number of ReAct steps taken.

- `all_tool_calls`:

  All tool calls across steps.

- `reasoning`:

  Chain-of-thought content.

- `response_id`:

  Server-side response ID for Responses API.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GenerateResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
