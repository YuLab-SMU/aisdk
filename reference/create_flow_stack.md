# Create a FlowStack

Factory function to create a new FlowStack object for enhanced
multi-agent orchestration.

## Usage

``` r
create_flow_stack(
  session,
  model,
  registry = NULL,
  max_depth = 5,
  max_steps_per_agent = 10,
  enable_guardrails = TRUE
)
```

## Arguments

- session:

  A SharedSession or ChatSession object.

- model:

  The default model ID to use.

- registry:

  Optional AgentRegistry for agent lookup.

- max_depth:

  Maximum delegation depth. Default 5.

- max_steps_per_agent:

  Maximum ReAct steps per agent. Default 10.

- enable_guardrails:

  Enable safety guardrails. Default TRUE.

## Value

A FlowStack object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create enhanced multi-agent flow
session <- create_shared_session(model = "openai:gpt-4o")
registry <- create_agent_registry(list(
  create_data_agent(),
  create_file_agent(),
  create_coder_agent()
))

flow <- create_flow_stack(
  session = session,
  model = "openai:gpt-4o",
  registry = registry,
  enable_guardrails = TRUE
)

# Run with unified delegate_task tool
manager <- create_agent("Manager", "Coordinates data analysis tasks")
result <- flow$run(manager, "Load sales.csv and create a summary report")

# Check delegation stats
print(flow$delegation_stats())
} # }
```
