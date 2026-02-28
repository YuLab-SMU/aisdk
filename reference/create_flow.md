# Create a Flow

Factory function to create a new Flow object for enhanced multi-agent
orchestration.

## Usage

``` r
create_flow(
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

  A ChatSession object.

- model:

  The default model ID to use (e.g., "openai:gpt-4o").

- registry:

  Optional AgentRegistry for agent lookup and delegation.

- max_depth:

  Maximum delegation depth. Default 5.

- max_steps_per_agent:

  Maximum ReAct steps per agent. Default 10.

- enable_guardrails:

  Enable safety guardrails. Default TRUE.

## Value

A Flow object.

## Examples

``` r
# \donttest{
if (interactive()) {
# Create an enhanced multi-agent flow
session <- create_chat_session()
cleaner <- create_agent("Cleaner", "Cleans data")
plotter <- create_agent("Plotter", "Creates plots")
registry <- create_agent_registry(list(cleaner, plotter))

manager <- create_agent("Manager", "Coordinates data analysis")

flow <- create_flow(
  session = session,
  model = "openai:gpt-4o",
  registry = registry,
  enable_guardrails = TRUE
)

# Run the manager with auto-delegation (unified delegate_task tool)
result <- flow$run(manager, "Load data and create a visualization")
}
# }
```
