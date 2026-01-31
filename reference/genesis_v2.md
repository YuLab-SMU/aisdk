# Genesis V2: Direct Execute-Refine Architecture

Genesis V2 implements a direct Execute-Refine loop with automatic
quality assessment and iterative improvement. It runs a single direct
agent with skills and refines based on evaluator feedback.

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
  refiner_model = NULL,
  max_steps = 10
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

  Maximum number of iterations (default: 3)

- auto_refine:

  Logical, whether to enable automatic refinement (default: TRUE)

- quality_threshold:

  Minimum quality score to accept (0-100, default: 70)

- cache:

  Logical, whether to cache team composition for similar tasks (unused
  in direct mode)

- verbose:

  Logical, whether to print orchestration details (default: FALSE)

- architect_model:

  Model to use for criteria generation (default: same as model)

- evaluator_model:

  Model to use for Evaluator agent (default: same as model)

- refiner_model:

  Model to use for Refiner agent (unused in direct mode)

- max_steps:

  Maximum tool execution steps (default: 10)

## Value

List with result, iterations, evaluation, and history

## Details

Execute a task with Direct Execute-Refine cycle

## Examples

``` r
if (FALSE) { # \dontrun{
result <- genesis_v2(
  "Analyze the iris dataset and create a comprehensive report with visualizations",
  max_iterations = 3,
  quality_threshold = 80,
  auto_refine = TRUE,
  verbose = TRUE
)
} # }
```
