# Create a DataAgent

Creates an agent specialized in data manipulation using dplyr and tidyr.
The agent can filter, transform, summarize, and reshape data frames in
the session environment.

## Usage

``` r
create_data_agent(name = "DataAgent", safe_mode = TRUE)
```

## Arguments

- name:

  Agent name. Default "DataAgent".

- safe_mode:

  If TRUE (default), restricts operations to data manipulation only.

## Value

An Agent object configured for data manipulation.

## Examples

``` r
if (FALSE) { # \dontrun{
data_agent <- create_data_agent()
session <- create_shared_session(model = "openai:gpt-4o")
session$set_var("sales", data.frame(
  product = c("A", "B", "C"),
  revenue = c(100, 200, 150)
))
result <- data_agent$run(
  "Calculate total revenue and find the top product",
  session = session,
  model = "openai:gpt-4o"
)
} # }
```
