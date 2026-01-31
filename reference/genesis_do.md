# Genesis Plan-Script-Execute Workflow

Implements a structured workflow:

1.  Plan: Create a task list in task.md

2.  Script: Write R code to fulfill the next step

3.  Execute: Run the code and update the plan

## Usage

``` r
genesis_do(task, model = "claude-3-5-sonnet-20241022", verbose = FALSE)
```

## Arguments

- task:

  The task description

- model:

  The model to use

- verbose:

  Logical, whether to print details

## Value

The result of the execution
