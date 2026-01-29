# Chat Manager

Manages asynchronous chat generation using background processes. Handles
IPC for streaming tokens and tool call bridging.

## Public fields

- `process`:

  The background callr process

- `ipc_dir`:

  Temp directory for inter-process communication

- `last_read_pos`:

  Last position read from output file

## Methods

### Public methods

- [`ChatManager$new()`](#method-ChatManager-new)

- [`ChatManager$start_generation()`](#method-ChatManager-start_generation)

- [`ChatManager$poll()`](#method-ChatManager-poll)

- [`ChatManager$resolve_tool()`](#method-ChatManager-resolve_tool)

- [`ChatManager$cleanup()`](#method-ChatManager-cleanup)

- [`ChatManager$clone()`](#method-ChatManager-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new ChatManager

#### Usage

    ChatManager$new()

------------------------------------------------------------------------

### Method `start_generation()`

Start async text generation

#### Usage

    ChatManager$start_generation(model, messages, system = NULL, tools = NULL)

#### Arguments

- `model`:

  The model (will be serialized for bg process)

- `messages`:

  The message history

- `system`:

  The system prompt

- `tools`:

  The tools list

------------------------------------------------------------------------

### Method `poll()`

Poll for new output and status

#### Usage

    ChatManager$poll()

#### Returns

List with text, done, waiting_tool, tool_call, error

------------------------------------------------------------------------

### Method `resolve_tool()`

Resolve a tool call with result

#### Usage

    ChatManager$resolve_tool(result)

#### Arguments

- `result`:

  The tool execution result

------------------------------------------------------------------------

### Method `cleanup()`

Cleanup resources

#### Usage

    ChatManager$cleanup()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ChatManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
