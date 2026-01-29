# Execute Tool Calls

Execute a list of tool calls returned by an LLM. This function safely
executes each tool, handling errors gracefully and returning a
standardized result format.

Implements multi-layer defense strategy:

1.  Tool name repair (case fixing, snake_case conversion, fuzzy
    matching)

2.  Invalid tool routing for graceful degradation

3.  Argument parsing with JSON repair

4.  Error capture and structured error responses

## Usage

``` r
execute_tool_calls(
  tool_calls,
  tools,
  hooks = NULL,
  envir = NULL,
  repair_enabled = TRUE
)
```

## Arguments

- tool_calls:

  A list of tool call objects, each with id, name, and arguments.

- tools:

  A list of Tool objects to search for matching tools.

- hooks:

  Optional HookHandler object.

- envir:

  Optional environment in which to execute tools. When provided, tool
  functions can access and modify variables in this environment,
  enabling cross-agent data sharing through a shared session
  environment.

- repair_enabled:

  Whether to attempt tool call repair (default TRUE).

## Value

A list of execution results, each containing:

- id: The tool call ID

- name: The tool name

- result: The execution result (or error message)

- is_error: TRUE if an error occurred during execution
