# Flow Class

R6 class representing an orchestration layer for multi-agent systems.
Features:

- Comprehensive delegation tracing

- Automatic delegate_task tool generation

- Depth and context limits with guardrails

- Result aggregation and summarization

## Methods

### Public methods

- [`Flow$new()`](#method-Flow-new)

- [`Flow$depth()`](#method-Flow-depth)

- [`Flow$current()`](#method-Flow-current)

- [`Flow$session()`](#method-Flow-session)

- [`Flow$set_global_context()`](#method-Flow-set_global_context)

- [`Flow$global_context()`](#method-Flow-global_context)

- [`Flow$delegate()`](#method-Flow-delegate)

- [`Flow$generate_delegate_tool()`](#method-Flow-generate_delegate_tool)

- [`Flow$run()`](#method-Flow-run)

- [`Flow$get_delegation_history()`](#method-Flow-get_delegation_history)

- [`Flow$delegation_stats()`](#method-Flow-delegation_stats)

- [`Flow$clear_history()`](#method-Flow-clear_history)

- [`Flow$print()`](#method-Flow-print)

- [`Flow$clone()`](#method-Flow-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new Flow.

#### Usage

    Flow$new(
      session,
      model,
      registry = NULL,
      max_depth = 5,
      max_steps_per_agent = 10,
      max_context_tokens = 4000,
      enable_guardrails = TRUE
    )

#### Arguments

- `session`:

  A ChatSession object.

- `model`:

  The default model ID to use.

- `registry`:

  Optional AgentRegistry for agent lookup.

- `max_depth`:

  Maximum delegation depth. Default 5.

- `max_steps_per_agent`:

  Maximum ReAct steps per agent. Default 10.

- `max_context_tokens`:

  Maximum context tokens per delegation. Default 4000.

- `enable_guardrails`:

  Enable safety guardrails. Default TRUE.

------------------------------------------------------------------------

### Method `depth()`

Get the current call stack depth.

#### Usage

    Flow$depth()

#### Returns

Integer depth.

------------------------------------------------------------------------

### Method `current()`

Get the current active agent.

#### Usage

    Flow$current()

#### Returns

The currently active Agent, or NULL.

------------------------------------------------------------------------

### Method [`session()`](https://YuLab-SMU.github.io/aisdk/reference/session.md)

Get the shared session.

#### Usage

    Flow$session()

#### Returns

The ChatSession object.

------------------------------------------------------------------------

### Method `set_global_context()`

Set the global context (the user's original goal).

#### Usage

    Flow$set_global_context(context)

#### Arguments

- `context`:

  Character string describing the overall goal.

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method `global_context()`

Get the global context.

#### Usage

    Flow$global_context()

#### Returns

The global context string.

------------------------------------------------------------------------

### Method `delegate()`

Delegate a task to another agent with enhanced tracking.

#### Usage

    Flow$delegate(agent, task, context = NULL, priority = "normal")

#### Arguments

- `agent`:

  The Agent to delegate to.

- `task`:

  The task instruction.

- `context`:

  Optional additional context.

- `priority`:

  Task priority: "high", "normal", "low". Default "normal".

#### Returns

The text result from the delegate agent.

------------------------------------------------------------------------

### Method `generate_delegate_tool()`

Generate the delegate_task tool for manager agents.

#### Usage

    Flow$generate_delegate_tool()

#### Details

Creates a single unified tool that can delegate to any registered agent.
This is more efficient than generating separate tools per agent.

#### Returns

A Tool object for delegation.

------------------------------------------------------------------------

### Method `run()`

Run a primary agent with enhanced orchestration.

#### Usage

    Flow$run(agent, task, use_unified_delegate = TRUE)

#### Arguments

- `agent`:

  The primary/manager Agent to run.

- `task`:

  The user's task/input.

- `use_unified_delegate`:

  Use single delegate_task tool. Default TRUE.

#### Returns

The final result from the primary agent.

------------------------------------------------------------------------

### Method `get_delegation_history()`

Get delegation history.

#### Usage

    Flow$get_delegation_history(agent_name = NULL, limit = NULL)

#### Arguments

- `agent_name`:

  Optional filter by agent name.

- `limit`:

  Maximum number of records to return.

#### Returns

A list of delegation records.

------------------------------------------------------------------------

### Method `delegation_stats()`

Get delegation statistics.

#### Usage

    Flow$delegation_stats()

#### Returns

A list with counts, timing, and success rates.

------------------------------------------------------------------------

### Method `clear_history()`

Clear delegation history.

#### Usage

    Flow$clear_history()

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for Flow.

#### Usage

    Flow$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Flow$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
