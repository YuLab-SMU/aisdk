# Expect Tool Selection

Test that an agent selects the correct tool(s) for a given task.

## Usage

``` r
expect_tool_selection(result, expected_tools, exact = FALSE, info = NULL)
```

## Arguments

- result:

  A GenerateResult object from generate_text with tools.

- expected_tools:

  Character vector of expected tool names.

- exact:

  If TRUE, require exactly these tools (no more, no less).

- info:

  Additional information for failure message.
