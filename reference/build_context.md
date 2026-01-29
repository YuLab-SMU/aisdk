# Build Context

Builds context string from R objects in the environment.

## Usage

``` r
build_context(prompt, context_spec, envir)
```

## Arguments

- prompt:

  The user's prompt.

- context_spec:

  NULL (auto-detect), FALSE (skip), or character vector of var names.

- envir:

  The environment to look for variables.

## Value

A character string with context information.
