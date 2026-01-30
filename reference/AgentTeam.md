# AgentTeam Class

R6 class representing a team of agents.

## Public fields

- `name`:

  Name of the team.

- `members`:

  List of registered agents (workers).

- `manager`:

  The manager agent (created automatically).

## Methods

### Public methods

- [`AgentTeam$new()`](#method-AgentTeam-new)

- [`AgentTeam$register_agent()`](#method-AgentTeam-register_agent)

- [`AgentTeam$run()`](#method-AgentTeam-run)

- [`AgentTeam$print()`](#method-AgentTeam-print)

- [`AgentTeam$clone()`](#method-AgentTeam-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new AgentTeam.

#### Usage

    AgentTeam$new(name = "AgentTeam")

#### Arguments

- `name`:

  Name of the team.

#### Returns

A new AgentTeam object.

------------------------------------------------------------------------

### Method `register_agent()`

Register an agent to the team.

#### Usage

    AgentTeam$register_agent(
      name,
      description,
      skills = NULL,
      tools = NULL,
      system_prompt = NULL
    )

#### Arguments

- `name`:

  Name of the agent.

- `description`:

  Description of the agent's capabilities.

- `skills`:

  Character vector of skills to load for this agent.

- `tools`:

  List of explicit Tool objects.

- `system_prompt`:

  Optional system prompt override.

#### Returns

Self (for chaining).

------------------------------------------------------------------------

### Method `run()`

Run the team on a task.

#### Usage

    AgentTeam$run(task, model = NULL)

#### Arguments

- `task`:

  The task instruction.

- `model`:

  Model ID to use for the Manager.

#### Returns

The result from the Manager agent.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print team info.

#### Usage

    AgentTeam$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AgentTeam$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
