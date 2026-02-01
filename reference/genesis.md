# Execute a task with automatic agent discovery and team assembly

Execute a task with automatic agent discovery and team assembly

## Usage

``` r
genesis(
  task,
  skill_paths = "auto",
  model = "anthropic:claude-3-5-sonnet-20241022",
  cache = TRUE,
  verbose = FALSE,
  architect_model = NULL,
  max_steps = 10,
  mode = c("plan", "direct"),
  use_computer_tools = FALSE
)
```

## Arguments

- task:

  Character string describing the task to accomplish

- skill_paths:

  Character vector of paths to scan for skills, or "auto" for default
  locations

- model:

  Model to use for agents (default: claude-3-5-sonnet-20241022)

- cache:

  Logical, whether to cache team composition for similar tasks (default:
  TRUE)

- verbose:

  Logical, whether to print orchestration details (default: FALSE)

- architect_model:

  Model to use for Architect agent (default: same as model)

- max_steps:

  Maximum tool execution steps (default: 10)

- mode:

  Execution mode: "plan" (structured plan-script-execute) or "direct"
  (single agent)

- use_computer_tools:

  Logical, whether to use computer abstraction layer (default: FALSE).
  When TRUE, uses atomic tools (bash, read_file, write_file,
  execute_r_code) instead of loading all skill tools into context. This
  reduces context window usage by 30-50%.

## Value

The result from the team execution

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple one-line execution
result <- genesis("Analyze the iris dataset and create a scatter plot")

# With computer tools (reduced context usage)
result <- genesis(
  "Analyze the iris dataset",
  use_computer_tools = TRUE
)

# With custom skill paths
result <- genesis(
  "Perform differential expression analysis",
  skill_paths = "~/bioinformatics/skills"
)

# With verbose output to see orchestration
result <- genesis(
  "Check if my package passes CRAN checks",
  verbose = TRUE
)
} # }
```
