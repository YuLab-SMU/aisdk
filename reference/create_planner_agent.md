# Create a PlannerAgent

Creates an agent specialized in breaking down complex tasks into steps
using chain-of-thought reasoning. The planner helps decompose problems
and create action plans.

## Usage

``` r
create_planner_agent(name = "PlannerAgent")
```

## Arguments

- name:

  Agent name. Default "PlannerAgent".

## Value

An Agent object configured for planning and reasoning.

## Examples

``` r
if (FALSE) { # \dontrun{
planner <- create_planner_agent()
result <- planner$run(
  "How should I approach building a machine learning model for customer churn?",
  model = "openai:gpt-4o"
)
} # }
```
