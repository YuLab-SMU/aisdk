# Stream Text

Generate text using a language model with streaming output. This
function provides a real-time stream of tokens through a callback.

## Usage

``` r
stream_text(
  model,
  prompt,
  callback = NULL,
  system = NULL,
  temperature = 0.7,
  max_tokens = NULL,
  tools = NULL,
  max_steps = 1,
  skills = NULL,
  session = NULL,
  hooks = NULL,
  registry = NULL,
  ...
)
```

## Arguments

- model:

  Either a LanguageModelV1 object, or a string ID like "openai:gpt-4o".

- prompt:

  A character string prompt, or a list of messages.

- callback:

  A function called for each text chunk: `callback(text, done)`.

- system:

  Optional system prompt.

- temperature:

  Sampling temperature (0-2). Default 0.7.

- max_tokens:

  Maximum tokens to generate.

- tools:

  Optional list of Tool objects for function calling.

- max_steps:

  Maximum number of generation steps (tool execution loops). Default 1.
  Set to higher values (e.g., 5) to enable automatic tool execution.

- skills:

  Optional path to skills directory, or a SkillRegistry object.

- session:

  Optional ChatSession object for shared state.

- hooks:

  Optional HookHandler object.

- registry:

  Optional ProviderRegistry to use.

- ...:

  Additional arguments passed to the model.

## Value

A GenerateResult object (accumulated from the stream).

## Examples

``` r
if (FALSE) { # \dontrun{
model <- create_openai()$language_model("gpt-4o")
stream_text(model, "Tell me a story", callback = function(text, done) {
  if (!done) cat(text)
})
} # }
```
