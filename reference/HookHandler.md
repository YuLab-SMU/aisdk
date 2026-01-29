# Hook Handler

R6 class to manage and execute hooks.

## Public fields

- `hooks`:

  List of hook functions.

## Methods

### Public methods

- [`HookHandler$new()`](#method-HookHandler-new)

- [`HookHandler$trigger_generation_start()`](#method-HookHandler-trigger_generation_start)

- [`HookHandler$trigger_generation_end()`](#method-HookHandler-trigger_generation_end)

- [`HookHandler$trigger_tool_start()`](#method-HookHandler-trigger_tool_start)

- [`HookHandler$trigger_tool_end()`](#method-HookHandler-trigger_tool_end)

- [`HookHandler$clone()`](#method-HookHandler-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize HookHandler

#### Usage

    HookHandler$new(hooks_list = list())

#### Arguments

- `hooks_list`:

  A list of hook functions. Supported hooks:

  - on_generation_start(model, prompt, tools)

  - on_generation_end(result)

  - on_tool_start(tool, args)

  - on_tool_end(tool, result)

  - on_tool_approval(tool, args) - Return TRUE to approve, FALSE to
    deny.

------------------------------------------------------------------------

### Method `trigger_generation_start()`

Trigger on_generation_start

#### Usage

    HookHandler$trigger_generation_start(model, prompt, tools)

#### Arguments

- `model`:

  The language model object.

- `prompt`:

  The prompt being sent.

- `tools`:

  The list of tools provided.

------------------------------------------------------------------------

### Method `trigger_generation_end()`

Trigger on_generation_end

#### Usage

    HookHandler$trigger_generation_end(result)

#### Arguments

- `result`:

  The generation result object.

------------------------------------------------------------------------

### Method `trigger_tool_start()`

Trigger on_tool_start

#### Usage

    HookHandler$trigger_tool_start(tool, args)

#### Arguments

- `tool`:

  The tool object.

- `args`:

  The arguments for the tool.

------------------------------------------------------------------------

### Method `trigger_tool_end()`

Trigger on_tool_end

#### Usage

    HookHandler$trigger_tool_end(tool, result)

#### Arguments

- `tool`:

  The tool object.

- `result`:

  The result from the tool execution.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    HookHandler$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
