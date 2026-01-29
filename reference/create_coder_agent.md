# Create a CoderAgent

Creates an agent specialized in writing and executing R code. The agent
can execute R code in the session environment, making results available
to other agents. Enhanced version with better safety controls and
debugging support.

## Usage

``` r
create_coder_agent(
  name = "CoderAgent",
  safe_mode = TRUE,
  timeout_ms = 30000,
  max_output_lines = 50
)
```

## Arguments

- name:

  Agent name. Default "CoderAgent".

- safe_mode:

  If TRUE (default), restricts file system and network access.

- timeout_ms:

  Code execution timeout in milliseconds. Default 30000.

- max_output_lines:

  Maximum output lines to return. Default 50.

## Value

An Agent object configured for R code execution.

## Examples

``` r
if (FALSE) { # \dontrun{
coder <- create_coder_agent()
session <- create_shared_session(model = "openai:gpt-4o")
result <- coder$run(
  "Create a data frame with 3 rows and calculate the mean",
  session = session,
  model = "openai:gpt-4o"
)
} # }
```
