# AgentRegistry Class

R6 class for managing a collection of Agent objects. Provides storage,
lookup, and automatic delegation tool generation for multi-agent
systems.

## Methods

### Public methods

- [`AgentRegistry$new()`](#method-AgentRegistry-new)

- [`AgentRegistry$register()`](#method-AgentRegistry-register)

- [`AgentRegistry$get()`](#method-AgentRegistry-get)

- [`AgentRegistry$has()`](#method-AgentRegistry-has)

- [`AgentRegistry$list_agents()`](#method-AgentRegistry-list_agents)

- [`AgentRegistry$get_all()`](#method-AgentRegistry-get_all)

- [`AgentRegistry$unregister()`](#method-AgentRegistry-unregister)

- [`AgentRegistry$generate_delegate_tools()`](#method-AgentRegistry-generate_delegate_tools)

- [`AgentRegistry$generate_prompt_section()`](#method-AgentRegistry-generate_prompt_section)

- [`AgentRegistry$print()`](#method-AgentRegistry-print)

- [`AgentRegistry$clone()`](#method-AgentRegistry-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new AgentRegistry.

#### Usage

    AgentRegistry$new(agents = NULL)

#### Arguments

- `agents`:

  Optional list of Agent objects to register immediately.

------------------------------------------------------------------------

### Method `register()`

Register an agent.

#### Usage

    AgentRegistry$register(agent)

#### Arguments

- `agent`:

  An Agent object to register.

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method [`get()`](https://rdrr.io/r/base/get.html)

Get an agent by name.

#### Usage

    AgentRegistry$get(name)

#### Arguments

- `name`:

  The agent name.

#### Returns

The Agent object, or NULL if not found.

------------------------------------------------------------------------

### Method `has()`

Check if an agent is registered.

#### Usage

    AgentRegistry$has(name)

#### Arguments

- `name`:

  The agent name.

#### Returns

TRUE if registered, FALSE otherwise.

------------------------------------------------------------------------

### Method `list_agents()`

List all registered agent names.

#### Usage

    AgentRegistry$list_agents()

#### Returns

Character vector of agent names.

------------------------------------------------------------------------

### Method `get_all()`

Get all registered agents.

#### Usage

    AgentRegistry$get_all()

#### Returns

List of Agent objects.

------------------------------------------------------------------------

### Method `unregister()`

Unregister an agent.

#### Usage

    AgentRegistry$unregister(name)

#### Arguments

- `name`:

  The agent name to remove.

#### Returns

Invisible self for chaining.

------------------------------------------------------------------------

### Method `generate_delegate_tools()`

Generate delegation tools for all registered agents.

#### Usage

    AgentRegistry$generate_delegate_tools(
      flow = NULL,
      session = NULL,
      model = NULL
    )

#### Arguments

- `flow`:

  Optional Flow object for context-aware execution.

- `session`:

  Optional ChatSession for shared state.

- `model`:

  Optional model ID for agent execution.

#### Details

Creates a list of Tool objects that wrap each agent's run() method.
These tools can be given to a Manager agent for semantic routing.

#### Returns

A list of Tool objects.

------------------------------------------------------------------------

### Method `generate_prompt_section()`

Generate a prompt section describing available agents.

#### Usage

    AgentRegistry$generate_prompt_section()

#### Details

Creates a formatted string listing all agents and their descriptions.
Useful for injecting into a Manager's system prompt.

#### Returns

A character string.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for AgentRegistry.

#### Usage

    AgentRegistry$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AgentRegistry$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
