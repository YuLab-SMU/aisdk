# Benchmark Agent

Run a benchmark suite against an agent and collect performance metrics.

## Usage

``` r
benchmark_agent(agent, tasks, tools = NULL, verbose = TRUE)
```

## Arguments

- agent:

  An Agent object or model string.

- tasks:

  A list of benchmark tasks (see details).

- tools:

  Optional list of tools for the agent.

- verbose:

  Print progress.

## Value

A benchmark result object with metrics.

## Details

Each task in the tasks list should have:

- prompt: The task prompt

- expected: Expected output or criteria

- category: Optional category for grouping

- ground_truth: Optional ground truth for hallucination checking
