# Flow Class

R6 class representing the orchestration layer for multi-agent systems.
Manages the call stack, context switching, and delegation between
agents.

The Flow acts as the "conductor" - it suspends the calling agent,
activates the delegate agent, and resumes the caller with the result.

## Methods

### Public methods

- [`Flow$new()`](#method-Flow-new)

- [`Flow$depth()`](#method-Flow-depth)

- [`Flow$current()`](#method-Flow-current)

- [`Flow$session()`](#method-Flow-session)

- [`Flow$set_global_context()`](#method-Flow-set_global_context)

- [`Flow$global_context()`](#method-Flow-global_context)

- [`Flow$delegate()`](#method-Flow-delegate)

- [`Flow$run()`](#method-Flow-run)

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
      max_steps_per_agent = 10
    )

#### Arguments

- `session`:

  A ChatSession object for shared state between agents.

- `model`:

  The default model ID to use (e.g., "openai:gpt-4o").

- `registry`:

  Optional AgentRegistry for agent lookup.

- `max_depth`:

  Maximum delegation depth (prevents infinite loops). Default 5.

- `max_steps_per_agent`:

  Maximum ReAct steps per agent. Default 10.

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

Delegate a task to another agent.

#### Usage

    Flow$delegate(agent, task, context = NULL)

#### Arguments

- `agent`:

  The Agent to delegate to.

- `task`:

  The task instruction.

- `context`:

  Optional additional context.

#### Details

This is the core orchestration method. It:

1.  Checks depth limit

2.  Pushes current agent to stack (if any)

3.  Builds recursive context

4.  Executes the delegate agent

5.  Pops and returns result

#### Returns

The text result from the delegate agent.

------------------------------------------------------------------------

### Method `run()`

Run a primary agent (the Manager).

#### Usage

    Flow$run(agent, task)

#### Arguments

- `agent`:

  The primary/manager Agent to run.

- `task`:

  The user's task/input.

#### Details

Entry point for a multi-agent flow. The primary agent is run with
delegation tools automatically injected from the registry.

#### Returns

The final result from the primary agent.

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
