# Create Console Tools

Create a set of tools optimized for console/terminal interaction.
Includes computer tools (bash, read_file, write_file, execute_r_code)
plus additional console-specific tools.

## Usage

``` r
create_console_tools(working_dir = getwd(), sandbox_mode = "permissive")
```

## Arguments

- working_dir:

  Working directory (default: current directory).

- sandbox_mode:

  Sandbox mode: "strict", "permissive", or "none" (default:
  "permissive").

## Value

A list of Tool objects.

## Examples

``` r
if (FALSE) { # \dontrun{
tools <- create_console_tools()
# Use with an agent or session
session <- create_chat_session(model = "openai:gpt-4o", tools = tools)
} # }
```
