# SharedSession Class

R6 class representing an enhanced session for multi-agent systems.
Extends ChatSession with:

- Execution context tracking (call stack, delegation history)

- Sandboxed code execution with safety guardrails

- Variable scoping and access control

- Comprehensive tracing and observability

## Super class

[`aisdk::ChatSession`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.md)
-\> `SharedSession`

## Methods

### Public methods

- [`SharedSession$new()`](#method-SharedSession-new)

- [`SharedSession$push_context()`](#method-SharedSession-push_context)

- [`SharedSession$pop_context()`](#method-SharedSession-pop_context)

- [`SharedSession$get_context()`](#method-SharedSession-get_context)

- [`SharedSession$set_global_task()`](#method-SharedSession-set_global_task)

- [`SharedSession$execute_code()`](#method-SharedSession-execute_code)

- [`SharedSession$get_var()`](#method-SharedSession-get_var)

- [`SharedSession$set_var()`](#method-SharedSession-set_var)

- [`SharedSession$list_vars()`](#method-SharedSession-list_vars)

- [`SharedSession$summarize_vars()`](#method-SharedSession-summarize_vars)

- [`SharedSession$create_scope()`](#method-SharedSession-create_scope)

- [`SharedSession$delete_scope()`](#method-SharedSession-delete_scope)

- [`SharedSession$trace_event()`](#method-SharedSession-trace_event)

- [`SharedSession$get_trace()`](#method-SharedSession-get_trace)

- [`SharedSession$clear_trace()`](#method-SharedSession-clear_trace)

- [`SharedSession$trace_summary()`](#method-SharedSession-trace_summary)

- [`SharedSession$set_access_control()`](#method-SharedSession-set_access_control)

- [`SharedSession$check_permission()`](#method-SharedSession-check_permission)

- [`SharedSession$get_sandbox_mode()`](#method-SharedSession-get_sandbox_mode)

- [`SharedSession$set_sandbox_mode()`](#method-SharedSession-set_sandbox_mode)

- [`SharedSession$print()`](#method-SharedSession-print)

- [`SharedSession$clone()`](#method-SharedSession-clone)

Inherited methods

- [`aisdk::ChatSession$append_message()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-append_message)
- [`aisdk::ChatSession$as_list()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-as_list)
- [`aisdk::ChatSession$clear_history()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-clear_history)
- [`aisdk::ChatSession$clear_memory()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-clear_memory)
- [`aisdk::ChatSession$eval_in_session()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-eval_in_session)
- [`aisdk::ChatSession$get_envir()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-get_envir)
- [`aisdk::ChatSession$get_history()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-get_history)
- [`aisdk::ChatSession$get_last_response()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-get_last_response)
- [`aisdk::ChatSession$get_memory()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-get_memory)
- [`aisdk::ChatSession$get_model_id()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-get_model_id)
- [`aisdk::ChatSession$list_envir()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-list_envir)
- [`aisdk::ChatSession$list_memory()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-list_memory)
- [`aisdk::ChatSession$restore_from_list()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-restore_from_list)
- [`aisdk::ChatSession$save()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-save)
- [`aisdk::ChatSession$send()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-send)
- [`aisdk::ChatSession$send_stream()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-send_stream)
- [`aisdk::ChatSession$set_memory()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-set_memory)
- [`aisdk::ChatSession$stats()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-stats)
- [`aisdk::ChatSession$switch_model()`](https://YuLab-SMU.github.io/aisdk/reference/ChatSession.html#method-switch_model)

------------------------------------------------------------------------

### Method `new()`

Initialize a new SharedSession.

#### Usage

    SharedSession$new(
      model = NULL,
      system_prompt = NULL,
      tools = NULL,
      hooks = NULL,
      max_steps = 10,
      registry = NULL,
      sandbox_mode = "strict",
      trace_enabled = TRUE
    )

#### Arguments

- `model`:

  A LanguageModelV1 object or model string ID.

- `system_prompt`:

  Optional system prompt for the conversation.

- `tools`:

  Optional list of Tool objects.

- `hooks`:

  Optional HookHandler object.

- `max_steps`:

  Maximum steps for tool execution loops. Default 10.

- `registry`:

  Optional ProviderRegistry for model resolution.

- `sandbox_mode`:

  Sandbox mode: "strict", "permissive", or "none". Default "strict".

- `trace_enabled`:

  Enable execution tracing. Default TRUE.

------------------------------------------------------------------------

### Method `push_context()`

Push an agent onto the execution stack.

#### Usage

    SharedSession$push_context(agent_name, task, parent_agent = NULL)

#### Arguments

- `agent_name`:

  Name of the agent being activated.

- `task`:

  The task being delegated.

- `parent_agent`:

  Name of the delegating agent (or NULL for root).

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method `pop_context()`

Pop the current agent from the execution stack.

#### Usage

    SharedSession$pop_context(result = NULL)

#### Arguments

- `result`:

  Optional result from the completed agent.

#### Returns

The popped context, or NULL if stack was empty.

------------------------------------------------------------------------

### Method `get_context()`

Get the current execution context.

#### Usage

    SharedSession$get_context()

#### Returns

A list with current_agent, depth, and delegation_stack.

------------------------------------------------------------------------

### Method `set_global_task()`

Set the global task (user's original request).

#### Usage

    SharedSession$set_global_task(task)

#### Arguments

- `task`:

  The global task description.

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method `execute_code()`

Execute R code in a sandboxed environment.

#### Usage

    SharedSession$execute_code(
      code,
      scope = "global",
      timeout_ms = 30000,
      capture_output = TRUE
    )

#### Arguments

- `code`:

  R code to execute (character string).

- `scope`:

  Variable scope: "global", "agent", or a custom scope name.

- `timeout_ms`:

  Execution timeout in milliseconds. Default 30000.

- `capture_output`:

  Capture stdout/stderr. Default TRUE.

#### Returns

A list with result, output, error, and duration_ms.

------------------------------------------------------------------------

### Method `get_var()`

Get a variable from a specific scope.

#### Usage

    SharedSession$get_var(name, scope = "global", default = NULL)

#### Arguments

- `name`:

  Variable name.

- `scope`:

  Scope name. Default "global".

- `default`:

  Default value if not found.

#### Returns

The variable value or default.

------------------------------------------------------------------------

### Method `set_var()`

Set a variable in a specific scope.

#### Usage

    SharedSession$set_var(name, value, scope = "global")

#### Arguments

- `name`:

  Variable name.

- `value`:

  Variable value.

- `scope`:

  Scope name. Default "global".

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method `list_vars()`

List variables in a scope.

#### Usage

    SharedSession$list_vars(scope = "global", pattern = NULL)

#### Arguments

- `scope`:

  Scope name. Default "global".

- `pattern`:

  Optional pattern to filter names.

#### Returns

Character vector of variable names.

------------------------------------------------------------------------

### Method `summarize_vars()`

Get a summary of all variables in a scope.

#### Usage

    SharedSession$summarize_vars(scope = "global")

#### Arguments

- `scope`:

  Scope name. Default "global".

#### Returns

A data frame with name, type, and size information.

------------------------------------------------------------------------

### Method `create_scope()`

Create a new variable scope.

#### Usage

    SharedSession$create_scope(scope_name, parent_scope = "global")

#### Arguments

- `scope_name`:

  Name for the new scope.

- `parent_scope`:

  Parent scope name. Default "global".

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method `delete_scope()`

Delete a variable scope.

#### Usage

    SharedSession$delete_scope(scope_name)

#### Arguments

- `scope_name`:

  Name of the scope to delete.

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method `trace_event()`

Record a trace event.

#### Usage

    SharedSession$trace_event(event_type, data = list())

#### Arguments

- `event_type`:

  Type of event (e.g., "context_push", "code_execution").

- `data`:

  Event data as a list.

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method `get_trace()`

Get the execution trace.

#### Usage

    SharedSession$get_trace(event_types = NULL, agent = NULL)

#### Arguments

- `event_types`:

  Optional filter by event types.

- `agent`:

  Optional filter by agent name.

#### Returns

A list of trace events.

------------------------------------------------------------------------

### Method `clear_trace()`

Clear the execution trace.

#### Usage

    SharedSession$clear_trace()

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method `trace_summary()`

Get trace summary statistics.

#### Usage

    SharedSession$trace_summary()

#### Returns

A list with event counts, agent activity, and timing info.

------------------------------------------------------------------------

### Method `set_access_control()`

Set access control for an agent.

#### Usage

    SharedSession$set_access_control(agent_name, permissions)

#### Arguments

- `agent_name`:

  Agent name.

- `permissions`:

  List of permissions (read_scopes, write_scopes, tools).

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method `check_permission()`

Check if an agent has permission for an action.

#### Usage

    SharedSession$check_permission(agent_name, action, target)

#### Arguments

- `agent_name`:

  Agent name.

- `action`:

  Action type: "read", "write", or "tool".

- `target`:

  Target scope or tool name.

#### Returns

TRUE if permitted, FALSE otherwise.

------------------------------------------------------------------------

### Method `get_sandbox_mode()`

Get sandbox mode.

#### Usage

    SharedSession$get_sandbox_mode()

#### Returns

The current sandbox mode.

------------------------------------------------------------------------

### Method `set_sandbox_mode()`

Set sandbox mode.

#### Usage

    SharedSession$set_sandbox_mode(mode)

#### Arguments

- `mode`:

  Sandbox mode: "strict", "permissive", or "none".

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for SharedSession.

#### Usage

    SharedSession$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SharedSession$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
