# Create Computer Tools

Create atomic tools for computer abstraction layer. These tools provide
a small set of primitives that agents can use to perform complex
actions.

## Usage

``` r
create_computer_tools(
  computer = NULL,
  working_dir = getwd(),
  sandbox_mode = "permissive"
)
```

## Arguments

- computer:

  Computer instance (default: create new)

- working_dir:

  Working directory (default: current directory)

- sandbox_mode:

  Sandbox mode: "strict", "permissive", or "none"

## Value

List of Tool objects
