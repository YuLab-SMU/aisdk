# Create an Agent Registry

Factory function to create a new AgentRegistry.

## Usage

``` r
create_agent_registry(agents = NULL)
```

## Arguments

- agents:

  Optional list of Agent objects to register immediately.

## Value

An AgentRegistry object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create registry with agents
cleaner <- create_agent("Cleaner", "Cleans data")
plotter <- create_agent("Plotter", "Creates visualizations")

registry <- create_agent_registry(list(cleaner, plotter))
print(registry$list_agents())  # "Cleaner", "Plotter"
} # }
```
