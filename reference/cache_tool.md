# Cache Tool

Wrap a tool with caching capabilities using the `memoise` package.

## Usage

``` r
cache_tool(tool, cache = NULL)
```

## Arguments

- tool:

  The Tool object to cache.

- cache:

  An optional memoise cache configuration (e.g., cache_memory() or
  cache_filesystem()). Defaults to
  [`memoise::cache_memory()`](https://memoise.r-lib.org/reference/cache_memory.html).

## Value

A new Tool object that caches its execution.
