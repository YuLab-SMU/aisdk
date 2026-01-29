# Create Standard Agent Registry

Creates an AgentRegistry pre-populated with standard library agents
based on feature flags.

## Usage

``` r
create_standard_registry(
  include_data = TRUE,
  include_file = TRUE,
  include_env = TRUE,
  include_coder = TRUE,
  include_visualizer = TRUE,
  include_planner = TRUE,
  file_allowed_dirs = ".",
  env_allow_install = FALSE
)
```

## Arguments

- include_data:

  Include DataAgent. Default TRUE.

- include_file:

  Include FileAgent. Default TRUE.

- include_env:

  Include EnvAgent. Default TRUE.

- include_coder:

  Include CoderAgent. Default TRUE.

- include_visualizer:

  Include VisualizerAgent. Default TRUE.

- include_planner:

  Include PlannerAgent. Default TRUE.

- file_allowed_dirs:

  Allowed directories for FileAgent.

- env_allow_install:

  Allow package installation for EnvAgent.

## Value

An AgentRegistry object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create registry with all standard agents
registry <- create_standard_registry()

# Create registry with only data and visualization agents
registry <- create_standard_registry(
  include_file = FALSE,
  include_env = FALSE,
  include_planner = FALSE
)
} # }
```
