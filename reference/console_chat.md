# Start Console Chat

Launch an interactive chat session in the R console. Supports streaming
output, slash commands, and colorful display using the cli package.

By default, the console operates in agent mode with tools for bash
execution, file operations, R code execution, and more. Set
`agent = NULL` for simple chat without tools.

## Usage

``` r
console_chat(
  session = NULL,
  system_prompt = NULL,
  tools = NULL,
  hooks = NULL,
  stream = TRUE,
  agent = "auto",
  working_dir = getwd(),
  sandbox_mode = "permissive"
)
```

## Arguments

- session:

  A ChatSession object, a LanguageModelV1 object, or a model string ID
  to create a new session.

- system_prompt:

  Optional system prompt (merged with agent prompt if agent is used).

- tools:

  Optional list of additional Tool objects.

- hooks:

  Optional HookHandler object.

- stream:

  Whether to use streaming output. Default TRUE.

- agent:

  Agent configuration. Options:

  - `"auto"` (default): Use the built-in console agent with terminal
    tools

  - `NULL`: Simple chat mode without tools

  - An Agent object: Use the provided custom agent

- working_dir:

  Working directory for the console agent (default: current directory).

- sandbox_mode:

  Sandbox mode for the console agent: "strict", "permissive" (default),
  or "none".

## Value

The ChatSession object (invisibly) when chat ends.

## Examples

``` r
if (FALSE) { # \dontrun{
# Start with default agent (intelligent terminal mode)
console_chat("openai:gpt-4o")

# Simple chat mode without tools
console_chat("openai:gpt-4o", agent = NULL)

# Start with an existing session
chat <- create_chat_session("anthropic:claude-3-5-sonnet-latest")
console_chat(chat)

# Start with a custom agent
agent <- create_agent("MathAgent", "Does math", system_prompt = "You are a math wizard.")
console_chat("openai:gpt-4o", agent = agent)

# Available commands in the chat:
# /quit or /exit - End the chat
# /save [path]   - Save session to file
# /load [path]   - Load session from file
# /model [id]    - Switch to a different model
# /history       - Show conversation history
# /stats         - Show token usage statistics
# /clear         - Clear conversation history
# /help          - Show available commands
# /agent [on|off] - Toggle agent mode
} # }
```
