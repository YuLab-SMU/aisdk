# Hypothesis-Fix-Verify Loop

Advanced self-healing execution that generates hypotheses about errors,
attempts fixes, and verifies the results.

## Usage

``` r
hypothesis_fix_verify(
  code,
  model = getOption("aisdk.default_model"),
  test_fn = NULL,
  max_iterations = 5,
  verbose = TRUE
)
```

## Arguments

- code:

  Character string of R code to execute.

- model:

  LLM model for analysis.

- test_fn:

  Optional function to verify the result is correct.

- max_iterations:

  Maximum fix iterations.

- verbose:

  Print progress.

## Value

List with result, fix history, and verification status.
