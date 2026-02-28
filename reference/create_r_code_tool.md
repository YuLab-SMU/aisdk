# Create R Code Interpreter Tool

Creates a meta-tool (`execute_r_code`) backed by a SandboxManager. This
single tool replaces all individual tools for the LLM, enabling batch
execution, data filtering, and local computation.

## Usage

``` r
create_r_code_tool(sandbox)
```

## Arguments

- sandbox:

  A SandboxManager object.

## Value

A Tool object named `execute_r_code`.
