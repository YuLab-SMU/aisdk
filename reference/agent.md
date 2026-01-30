# Agent Class

R6 class representing an AI agent. Agents are the worker units in the
multi-agent architecture. Each agent has a name, description (for
semantic routing), system prompt (persona), and a set of tools it can
use.

Key design principle: Agents are stateless regarding conversation
history. The ChatSession holds the shared state (history, memory,
environment).

## Public fields

- `name`:

  Unique identifier for this agent.

- `description`:

  Description of the agent's capability. This is the "API" that the LLM
  Manager uses for semantic routing.

- `system_prompt`:

  The agent's persona/instructions.

- `tools`:

  List of Tool objects this agent can use.

## Methods

### Public methods

- [`Agent$new()`](#method-Agent-new)

- [`Agent$run()`](#method-Agent-run)

- [`Agent$stream()`](#method-Agent-stream)

- [`Agent$as_tool()`](#method-Agent-as_tool)

- [`Agent$create_session()`](#method-Agent-create_session)

- [`Agent$print()`](#method-Agent-print)

- [`Agent$clone()`](#method-Agent-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new Agent.

#### Usage

    Agent$new(name, description, system_prompt = NULL, tools = NULL, skills = NULL)

#### Arguments

- `name`:

  Unique name for this agent (e.g., "DataCleaner", "Visualizer").

- `description`:

  A clear description of what this agent does. This is used by the
  Manager LLM to decide which agent to delegate to.

- `system_prompt`:

  Optional system prompt defining the agent's persona.

- `tools`:

  Optional list of Tool objects the agent can use.

- `skills`:

  Optional character vector of skill paths or "auto" to discover skills.
  When provided, this automatically loads skills, creates tools, and
  updates the system prompt.

#### Returns

An Agent object.

------------------------------------------------------------------------

### Method `run()`

Run the agent with a given task.

#### Usage

    Agent$run(
      task,
      session = NULL,
      context = NULL,
      model = NULL,
      max_steps = 10,
      ...
    )

#### Arguments

- `task`:

  The task instruction (natural language).

- `session`:

  Optional ChatSession for shared state. If NULL, a temporary session is
  created.

- `context`:

  Optional additional context to inject (e.g., from parent agent).

- `model`:

  Optional model override. Uses session's model if not provided.

- `max_steps`:

  Maximum ReAct loop iterations. Default 10.

- `...`:

  Additional arguments passed to generate_text.

#### Returns

A GenerateResult object from generate_text.

------------------------------------------------------------------------

### Method `stream()`

Run the agent with streaming output.

#### Usage

    Agent$stream(
      task,
      callback = NULL,
      session = NULL,
      context = NULL,
      model = NULL,
      max_steps = 10,
      ...
    )

#### Arguments

- `task`:

  The task instruction (natural language).

- `callback`:

  Function to handle streaming chunks: callback(text, done).

- `session`:

  Optional ChatSession for shared state.

- `context`:

  Optional additional context to inject.

- `model`:

  Optional model override.

- `max_steps`:

  Maximum ReAct loop iterations. Default 10.

- `...`:

  Additional arguments passed to stream_text.

#### Returns

A GenerateResult object (accumulated).

------------------------------------------------------------------------

### Method `as_tool()`

Convert this agent to a Tool.

#### Usage

    Agent$as_tool()

#### Details

This allows the agent to be used as a delegate target by a Manager
agent. The tool wraps the agent's run() method and uses the agent's
description for semantic routing.

#### Returns

A Tool object that wraps this agent.

------------------------------------------------------------------------

### Method [`create_session()`](https://YuLab-SMU.github.io/aisdk/reference/create_session.md)

Create a stateful ChatSession from this agent.

#### Usage

    Agent$create_session(model = NULL, ...)

#### Arguments

- `model`:

  Optional model override.

- `...`:

  Additional arguments passed to ChatSession\$new.

#### Returns

A ChatSession object initialized with this agent's config.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for Agent.

#### Usage

    Agent$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Agent$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
