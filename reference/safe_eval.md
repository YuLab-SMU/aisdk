# Safe Eval with Timeout

Execute R code with a timeout to prevent infinite loops.

## Usage

``` r
safe_eval(expr, timeout_seconds = 30, envir = parent.frame())
```

## Arguments

- expr:

  Expression to evaluate.

- timeout_seconds:

  Maximum execution time in seconds.

- envir:

  Environment for evaluation.

## Value

The result or an error.
