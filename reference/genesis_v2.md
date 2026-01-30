# Genesis V2: Plan-Execute-Refine Architecture

Genesis V2 implements a complete Plan-Execute-Refine cycle with
automatic quality assessment and iterative improvement. It extends
Genesis V1 with the ability to evaluate results and automatically refine
failed executions.

## Usage

``` r
genesis_v2(
  task,
  skill_paths = "auto",
  model = "claude-3-5-sonnet-20241022",
  max_iterations = 3,
  auto_refine = TRUE,
  quality_threshold = 70,
  cache = TRUE,
  verbose = FALSE,
  architect_model = NULL,
  evaluator_model = NULL,
  refiner_model = NULL
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

- max_iterations:

  Maximum number of PER iterations (default: 3)

- auto_refine:

  Logical, whether to enable automatic refinement (default: TRUE)

- quality_threshold:

  Minimum quality score to accept (0-100, default: 70)

- cache:

  Logical, whether to cache team composition (default: TRUE)

- verbose:

  Logical, whether to print orchestration details (default: FALSE)

- architect_model:

  Model to use for Architect agent (default: same as model)

- evaluator_model:

  Model to use for Evaluator agent (default: same as model)

- refiner_model:

  Model to use for Refiner agent (default: same as model)

## Value

List with result, iterations, evaluation, and history

## Details

Execute a task with Plan-Execute-Refine cycle

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with auto-refine
result <- genesis_v2(
  "Analyze the iris dataset and create a comprehensive report with visualizations"
)

# With custom settings
result <- genesis_v2(
  "Analyze iris and create plots",
  max_iterations = 5,
  quality_threshold = 85,
  verbose = TRUE
)

# Disable auto-refine (behaves like V1)
result <- genesis_v2(
  "Simple task",
  auto_refine = FALSE
)
} # }
```
