# Start Console Chat

Launch an interactive chat session in the R console. Supports streaming
output, slash commands, and colorful display using the cli package.

## Usage

``` r
console_chat(
  session = NULL,
  system_prompt = NULL,
  tools = NULL,
  hooks = NULL,
  stream = TRUE
)
```

## Arguments

- session:

  A ChatSession object, a LanguageModelV1 object, or a model string ID
  to create a new session.

- system_prompt:

  Optional system prompt (only used if creating a new session).

- tools:

  Optional list of Tool objects (only used if creating a new session).

- hooks:

  Optional HookHandler object.

- stream:

  Whether to use streaming output. Default TRUE.

## Value

The ChatSession object (invisibly) when chat ends.

## Examples

``` r
if (FALSE) { # \dontrun{
# Start with a model ID
console_chat("openai:gpt-4o")

# Start with an existing session
chat <- create_chat_session("anthropic:claude-3-5-sonnet-latest")
console_chat(chat)

# Available commands in the chat:
# /quit or /exit - End the chat
# /save [path]   - Save session to file
# /load [path]   - Load session from file
# /model [id]    - Switch to a different model
# /history       - Show conversation history
# /stats         - Show token usage statistics
# /clear         - Clear conversation history
# /help          - Show available commands
} # }
```
