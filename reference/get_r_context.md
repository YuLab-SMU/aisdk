# Get R Context

Generates a text summary of R objects to be used as context for the LLM.

## Usage

``` r
get_r_context(vars, envir = parent.frame())
```

## Arguments

- vars:

  Character vector of variable names to include.

- envir:

  The environment to look for variables in. Default is parent.frame().

## Value

A single string containing the summaries of the requested variables.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(x = 1:10, y = rnorm(10))
context <- get_r_context("df")
cat(context)
} # }
```
