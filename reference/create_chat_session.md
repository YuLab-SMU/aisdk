# Create a Chat Session

Factory function to create a new ChatSession object.

## Usage

``` r
create_chat_session(
  model = NULL,
  system_prompt = NULL,
  tools = NULL,
  hooks = NULL,
  max_steps = 10
)
```

## Arguments

- model:

  A LanguageModelV1 object or model string ID.

- system_prompt:

  Optional system prompt.

- tools:

  Optional list of Tool objects.

- hooks:

  Optional HookHandler object.

- max_steps:

  Maximum tool execution steps. Default 10.

## Value

A ChatSession object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a chat session
chat <- create_chat_session(
  model = "openai:gpt-4o",
  system_prompt = "You are a helpful R programming assistant."
)

# Send messages
response <- chat$send("How do I read a CSV file?")
print(response$text)

# Continue the conversation (history is maintained)
response <- chat$send("What about Excel files?")

# Check stats
print(chat$stats())

# Save session
chat$save("my_session.rds")
} # }
```
