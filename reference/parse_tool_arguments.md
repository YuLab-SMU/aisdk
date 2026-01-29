# Parse Tool Arguments

Robustly parse tool call arguments from various formats that different
LLMs may return. Handles edge cases like incomplete JSON, malformed
strings, and various empty representations.

Implements multi-layer parsing strategy (inspired by Opencode):

1.  Direct pass-through for already-parsed lists

2.  Empty value detection and normalization

3.  JSON repair for common LLM mistakes

4.  Fallback parsing with JavaScript object literal support

5.  Graceful degradation to empty args on failure

## Usage

``` r
parse_tool_arguments(args, tool_name = "unknown")
```

## Arguments

- args:

  The arguments to parse (can be string, list, or NULL).

- tool_name:

  Optional tool name for better error messages.

## Value

A named list of parsed arguments (empty named list if no arguments).
