# Create a Genesis V2 benchmark suite

Create a Genesis V2 benchmark suite

## Usage

``` r
benchmark_genesis_v2(
  tasks,
  skill_paths = "auto",
  model = "claude-3-5-sonnet-20241022",
  max_iterations = 3,
  quality_threshold = 70
)
```

## Arguments

- tasks:

  Character vector of tasks to benchmark

- skill_paths:

  Skill paths to use

- model:

  Model to use

- max_iterations:

  Maximum iterations per task

- quality_threshold:

  Quality threshold
