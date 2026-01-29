# ChatSession Class

R6 class representing a stateful chat session. Automatically manages
conversation history, supports tool execution, and provides persistence.

## Methods

### Public methods

- [`ChatSession$new()`](#method-ChatSession-new)

- [`ChatSession$send()`](#method-ChatSession-send)

- [`ChatSession$send_stream()`](#method-ChatSession-send_stream)

- [`ChatSession$append_message()`](#method-ChatSession-append_message)

- [`ChatSession$get_history()`](#method-ChatSession-get_history)

- [`ChatSession$get_last_response()`](#method-ChatSession-get_last_response)

- [`ChatSession$clear_history()`](#method-ChatSession-clear_history)

- [`ChatSession$switch_model()`](#method-ChatSession-switch_model)

- [`ChatSession$get_model_id()`](#method-ChatSession-get_model_id)

- [`ChatSession$stats()`](#method-ChatSession-stats)

- [`ChatSession$save()`](#method-ChatSession-save)

- [`ChatSession$as_list()`](#method-ChatSession-as_list)

- [`ChatSession$restore_from_list()`](#method-ChatSession-restore_from_list)

- [`ChatSession$print()`](#method-ChatSession-print)

- [`ChatSession$get_memory()`](#method-ChatSession-get_memory)

- [`ChatSession$set_memory()`](#method-ChatSession-set_memory)

- [`ChatSession$list_memory()`](#method-ChatSession-list_memory)

- [`ChatSession$clear_memory()`](#method-ChatSession-clear_memory)

- [`ChatSession$get_envir()`](#method-ChatSession-get_envir)

- [`ChatSession$eval_in_session()`](#method-ChatSession-eval_in_session)

- [`ChatSession$list_envir()`](#method-ChatSession-list_envir)

- [`ChatSession$clone()`](#method-ChatSession-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new ChatSession.

#### Usage

    ChatSession$new(
      model = NULL,
      system_prompt = NULL,
      tools = NULL,
      hooks = NULL,
      history = NULL,
      max_steps = 10,
      registry = NULL,
      memory = NULL,
      envir = NULL
    )

#### Arguments

- `model`:

  A LanguageModelV1 object or model string ID (e.g., "openai:gpt-4o").

- `system_prompt`:

  Optional system prompt for the conversation.

- `tools`:

  Optional list of Tool objects for function calling.

- `hooks`:

  Optional HookHandler object for event hooks.

- `history`:

  Optional initial message history (list of message objects).

- `max_steps`:

  Maximum steps for tool execution loops. Default 10.

- `registry`:

  Optional ProviderRegistry for model resolution.

- `memory`:

  Optional initial shared memory (list). For multi-agent state sharing.

- `envir`:

  Optional shared R environment. For multi-agent data sharing.

------------------------------------------------------------------------

### Method `send()`

Send a message and get a response.

#### Usage

    ChatSession$send(prompt, ...)

#### Arguments

- `prompt`:

  The user message to send.

- `...`:

  Additional arguments passed to generate_text.

#### Returns

The GenerateResult object from the model.

------------------------------------------------------------------------

### Method `send_stream()`

Send a message with streaming output.

#### Usage

    ChatSession$send_stream(prompt, callback, ...)

#### Arguments

- `prompt`:

  The user message to send.

- `callback`:

  Function called for each chunk: callback(text, done).

- `...`:

  Additional arguments passed to stream_text.

#### Returns

Invisible NULL (output is via callback).

------------------------------------------------------------------------

### Method `append_message()`

Append a message to the history.

#### Usage

    ChatSession$append_message(role, content)

#### Arguments

- `role`:

  Message role: "user", "assistant", "system", or "tool".

- `content`:

  Message content.

------------------------------------------------------------------------

### Method `get_history()`

Get the conversation history.

#### Usage

    ChatSession$get_history()

#### Returns

A list of message objects.

------------------------------------------------------------------------

### Method `get_last_response()`

Get the last response from the assistant.

#### Usage

    ChatSession$get_last_response()

#### Returns

The content of the last assistant message, or NULL.

------------------------------------------------------------------------

### Method `clear_history()`

Clear the conversation history.

#### Usage

    ChatSession$clear_history(keep_system = TRUE)

#### Arguments

- `keep_system`:

  If TRUE, keeps the system prompt. Default TRUE.

------------------------------------------------------------------------

### Method `switch_model()`

Switch to a different model.

#### Usage

    ChatSession$switch_model(model)

#### Arguments

- `model`:

  A LanguageModelV1 object or model string ID.

------------------------------------------------------------------------

### Method `get_model_id()`

Get current model identifier.

#### Usage

    ChatSession$get_model_id()

#### Returns

Model ID string.

------------------------------------------------------------------------

### Method `stats()`

Get token usage statistics.

#### Usage

    ChatSession$stats()

#### Returns

A list with token counts and message stats.

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save session to a file.

#### Usage

    ChatSession$save(path, format = NULL)

#### Arguments

- `path`:

  File path (supports .rds or .json extension).

- `format`:

  Optional format override: "rds" or "json". Auto-detected from path.

------------------------------------------------------------------------

### Method `as_list()`

Export session state as a list (for serialization).

#### Usage

    ChatSession$as_list()

#### Returns

A list containing session state.

------------------------------------------------------------------------

### Method `restore_from_list()`

Restore session state from a list.

#### Usage

    ChatSession$restore_from_list(data)

#### Arguments

- `data`:

  A list exported by as_list().

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for ChatSession.

#### Usage

    ChatSession$print()

------------------------------------------------------------------------

### Method [`get_memory()`](https://YuLab-SMU.github.io/aisdk/reference/get_memory.md)

Get a value from shared memory.

#### Usage

    ChatSession$get_memory(key, default = NULL)

#### Arguments

- `key`:

  The key to retrieve.

- `default`:

  Default value if key not found. Default NULL.

#### Returns

The stored value or default.

------------------------------------------------------------------------

### Method `set_memory()`

Set a value in shared memory.

#### Usage

    ChatSession$set_memory(key, value)

#### Arguments

- `key`:

  The key to store.

- `value`:

  The value to store.

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method `list_memory()`

List all keys in shared memory.

#### Usage

    ChatSession$list_memory()

#### Returns

Character vector of memory keys.

------------------------------------------------------------------------

### Method `clear_memory()`

Clear shared memory.

#### Usage

    ChatSession$clear_memory(keys = NULL)

#### Arguments

- `keys`:

  Optional specific keys to clear. If NULL, clears all.

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method `get_envir()`

Get the shared R environment.

#### Usage

    ChatSession$get_envir()

#### Details

This environment is shared across all agents using this session. Agents
can store and retrieve data frames, models, and other R objects.

#### Returns

An environment object.

------------------------------------------------------------------------

### Method `eval_in_session()`

Evaluate an expression in the session environment.

#### Usage

    ChatSession$eval_in_session(expr)

#### Arguments

- `expr`:

  An expression to evaluate.

#### Returns

The result of the evaluation.

------------------------------------------------------------------------

### Method `list_envir()`

List objects in the session environment.

#### Usage

    ChatSession$list_envir()

#### Returns

Character vector of object names.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ChatSession$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
