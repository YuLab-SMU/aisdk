# FlowStack Class

R6 class representing an enhanced orchestration layer for multi-agent
systems. Extends Flow with:

- Comprehensive delegation tracing

- Automatic delegate_task tool generation

- Depth and context limits with guardrails

- Result aggregation and summarization

## Super class

[`aisdk::Flow`](https://YuLab-SMU.github.io/aisdk/reference/flow.md) -\>
`FlowStack`

## Methods

### Public methods

- [`FlowStack$new()`](#method-FlowStack-new)

- [`FlowStack$delegate()`](#method-FlowStack-delegate)

- [`FlowStack$generate_delegate_tool()`](#method-FlowStack-generate_delegate_tool)

- [`FlowStack$run()`](#method-FlowStack-run)

- [`FlowStack$get_delegation_history()`](#method-FlowStack-get_delegation_history)

- [`FlowStack$delegation_stats()`](#method-FlowStack-delegation_stats)

- [`FlowStack$clear_history()`](#method-FlowStack-clear_history)

- [`FlowStack$print()`](#method-FlowStack-print)

- [`FlowStack$clone()`](#method-FlowStack-clone)

Inherited methods

- [`aisdk::Flow$current()`](https://YuLab-SMU.github.io/aisdk/reference/Flow.html#method-current)
- [`aisdk::Flow$depth()`](https://YuLab-SMU.github.io/aisdk/reference/Flow.html#method-depth)
- [`aisdk::Flow$global_context()`](https://YuLab-SMU.github.io/aisdk/reference/Flow.html#method-global_context)
- [`aisdk::Flow$session()`](https://YuLab-SMU.github.io/aisdk/reference/Flow.html#method-session)
- [`aisdk::Flow$set_global_context()`](https://YuLab-SMU.github.io/aisdk/reference/Flow.html#method-set_global_context)

------------------------------------------------------------------------

### Method `new()`

Initialize a new FlowStack.

#### Usage

    FlowStack$new(
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

  A SharedSession or ChatSession object.

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

### Method `delegate()`

Delegate a task to another agent with enhanced tracking.

#### Usage

    FlowStack$delegate(agent, task, context = NULL, priority = "normal")

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

    FlowStack$generate_delegate_tool()

#### Details

Creates a single unified tool that can delegate to any registered agent.
This is more efficient than generating separate tools per agent.

#### Returns

A Tool object for delegation.

------------------------------------------------------------------------

### Method `run()`

Run a primary agent with enhanced orchestration.

#### Usage

    FlowStack$run(agent, task, use_unified_delegate = TRUE)

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

    FlowStack$get_delegation_history(agent_name = NULL, limit = NULL)

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

    FlowStack$delegation_stats()

#### Returns

A list with counts, timing, and success rates.

------------------------------------------------------------------------

### Method `clear_history()`

Clear delegation history.

#### Usage

    FlowStack$clear_history()

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for FlowStack.

#### Usage

    FlowStack$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FlowStack$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
