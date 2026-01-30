# Agent Discovery Library

Agent Discovery Library

Agent Discovery Library

## Details

AgentLibrary provides automatic discovery and instantiation of agents
from skill directories. It scans SKILL.md files for agent metadata and
enables zero-configuration team assembly.

## Methods

### Public methods

- [`AgentLibrary$new()`](#method-AgentLibrary-new)

- [`AgentLibrary$scan_from_skills()`](#method-AgentLibrary-scan_from_skills)

- [`AgentLibrary$get_capabilities_summary()`](#method-AgentLibrary-get_capabilities_summary)

- [`AgentLibrary$instantiate_agents()`](#method-AgentLibrary-instantiate_agents)

- [`AgentLibrary$list_roles()`](#method-AgentLibrary-list_roles)

- [`AgentLibrary$print()`](#method-AgentLibrary-print)

- [`AgentLibrary$clone()`](#method-AgentLibrary-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new AgentLibrary

#### Usage

    AgentLibrary$new()

------------------------------------------------------------------------

### Method `scan_from_skills()`

Scan skills directory and extract agent metadata

#### Usage

    AgentLibrary$scan_from_skills(skill_paths = "auto", recursive = TRUE)

#### Arguments

- `skill_paths`:

  Character vector of paths to scan, or "auto" for default locations

- `recursive`:

  Logical, whether to scan recursively (default: TRUE)

#### Returns

Self (invisibly) for method chaining

------------------------------------------------------------------------

### Method `get_capabilities_summary()`

Get agent capabilities summary for Architect

#### Usage

    AgentLibrary$get_capabilities_summary()

#### Returns

Data frame with role, description, capabilities, skills columns

------------------------------------------------------------------------

### Method `instantiate_agents()`

Instantiate agents by role names (lazy loading)

#### Usage

    AgentLibrary$instantiate_agents(
      role_names,
      model = "claude-3-5-sonnet-20241022"
    )

#### Arguments

- `role_names`:

  Character vector of agent roles to instantiate

- `model`:

  Model to use for agents

#### Returns

Named list of Agent objects

------------------------------------------------------------------------

### Method `list_roles()`

List all available agent roles

#### Usage

    AgentLibrary$list_roles()

#### Returns

Character vector of role names

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print summary of discovered agents

#### Usage

    AgentLibrary$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AgentLibrary$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
