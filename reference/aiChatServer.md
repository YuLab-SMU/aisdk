# AI Chat Server

Shiny module server for AI-powered chat, featuring non-blocking
streaming via background processes and tool execution bridge.

## Usage

``` r
aiChatServer(
  id,
  model,
  tools = NULL,
  context = NULL,
  system = NULL,
  debug = FALSE,
  on_message_complete = NULL
)
```

## Arguments

- id:

  The namespace ID for the module.

- model:

  Either a LanguageModelV1 object, or a string ID like "openai:gpt-4o".

- tools:

  Optional list of Tool objects for function calling.

- context:

  Optional reactive expression that returns context data to inject into
  the system prompt. This is read with `isolate()` to avoid reactive
  loops.

- system:

  Optional system prompt.

- debug:

  Reactive expression or logical. If TRUE, shows raw debug output in UI.

- on_message_complete:

  Optional callback function called when a message is complete. Takes
  one argument: the complete assistant message text.

## Value

A reactive value containing the chat history.
