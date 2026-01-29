# Create an EnvAgent

Creates an agent specialized in R environment and package management.
The agent can check, install, and manage R packages with safety
controls.

## Usage

``` r
create_env_agent(
  name = "EnvAgent",
  allow_install = FALSE,
  allowed_repos = "https://cloud.r-project.org"
)
```

## Arguments

- name:

  Agent name. Default "EnvAgent".

- allow_install:

  Allow package installation. Default FALSE.

- allowed_repos:

  CRAN mirror URLs for installation.

## Value

An Agent object configured for environment management.

## Examples

``` r
if (FALSE) { # \dontrun{
env_agent <- create_env_agent(allow_install = TRUE)
result <- env_agent$run(
  "Check if tidyverse is installed and load it",
  session = session,
  model = "openai:gpt-4o"
)
} # }
```
