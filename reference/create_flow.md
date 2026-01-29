# Create a Flow

Factory function to create a new Flow object for multi-agent
orchestration.

## Usage

``` r
create_flow(
  session,
  model,
  registry = NULL,
  max_depth = 5,
  max_steps_per_agent = 10
)
```

## Arguments

- session:

  A ChatSession object for shared state.

- model:

  The default model ID to use (e.g., "openai:gpt-4o").

- registry:

  Optional AgentRegistry for agent lookup and delegation.

- max_depth:

  Maximum delegation depth. Default 5.

- max_steps_per_agent:

  Maximum ReAct steps per agent. Default 10.

## Value

A Flow object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a multi-agent flow
session <- create_chat_session()
cleaner <- create_agent("Cleaner", "Cleans data")
plotter <- create_agent("Plotter", "Creates plots")
registry <- create_agent_registry(list(cleaner, plotter))

manager <- create_agent("Manager", "Coordinates data analysis")

flow <- create_flow(
  session = session,
  model = "openai:gpt-4o",
  registry = registry
)

# Run the manager with auto-delegation
result <- flow$run(manager, "Load data and create a visualization")
} # }
```
