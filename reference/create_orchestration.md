# Create Orchestration Flow (Compatibility Wrapper)

Creates an orchestration flow using either FlowStack or legacy Flow
based on feature flags.

## Usage

``` r
create_orchestration(
  session,
  model,
  registry = NULL,
  max_depth = 5,
  max_steps_per_agent = 10,
  ...
)
```

## Arguments

- session:

  A session object.

- model:

  The default model ID.

- registry:

  Optional AgentRegistry.

- max_depth:

  Maximum delegation depth. Default 5.

- max_steps_per_agent:

  Maximum ReAct steps per agent. Default 10.

- ...:

  Additional arguments.

## Value

A FlowStack or Flow object.
