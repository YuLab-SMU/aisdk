# Create an Agent

Factory function to create a new Agent object.

## Usage

``` r
create_agent(
  name,
  description,
  system_prompt = NULL,
  tools = NULL,
  skills = NULL
)
```

## Arguments

- name:

  Unique name for this agent.

- description:

  A clear description of what this agent does.

- system_prompt:

  Optional system prompt defining the agent's persona.

- tools:

  Optional list of Tool objects the agent can use.

- skills:

  Optional character vector of skill paths or "auto".

## Value

An Agent object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a simple math agent
math_agent <- create_agent(
  name = "MathAgent",
  description = "Performs arithmetic calculations",
  system_prompt = "You are a math assistant. Return only numerical results."
)

# Run the agent
result <- math_agent$run("Calculate 2 + 2", model = "openai:gpt-4o")

# Create an agent with skills
stock_agent <- create_agent(
  name = "StockAnalyst",
  description = "Stock analysis agent",
  skills = "auto"
)
} # }
```
