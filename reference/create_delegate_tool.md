# Create a Delegate Tool for an Agent

Internal function to create a Tool that delegates to an Agent.

## Usage

``` r
create_delegate_tool(agent, flow = NULL, session = NULL, model = NULL)
```

## Arguments

- agent:

  The Agent to wrap.

- flow:

  Optional Flow object for context tracking.

- session:

  Optional ChatSession for shared state.

- model:

  Optional model ID for execution.

## Value

A Tool object.
