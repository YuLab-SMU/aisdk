# Create Console Agent

Create the default intelligent terminal agent for console_chat(). This
agent can execute commands, manage files, and run R code through natural
language interaction.

## Usage

``` r
create_console_agent(
  working_dir = getwd(),
  sandbox_mode = "permissive",
  additional_tools = NULL,
  language = "auto"
)
```

## Arguments

- working_dir:

  Working directory (default: current directory).

- sandbox_mode:

  Sandbox mode: "strict", "permissive", or "none" (default:
  "permissive").

- additional_tools:

  Optional list of additional Tool objects to include.

- language:

  Language for responses: "auto", "en", or "zh" (default: "auto").

## Value

An Agent object configured for console interaction.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create default console agent
agent <- create_console_agent()

# Create with custom working directory
agent <- create_console_agent(working_dir = "~/projects/myapp")

# Use with console_chat
console_chat("openai:gpt-4o", agent = agent)
} # }
```
