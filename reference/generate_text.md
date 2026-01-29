# Generate Text

Generate text using a language model. This is the primary high-level
function for non-streaming text generation.

When tools are provided and max_steps \> 1, the function will
automatically execute tool calls and feed results back to the LLM in a
ReAct-style loop until the LLM produces a final response or max_steps is
reached.

## Usage

``` r
generate_text(
  model,
  prompt,
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

- system:

  Optional system prompt.

- temperature:

  Sampling temperature (0-2). Default 0.7.

- max_tokens:

  Maximum tokens to generate.

- tools:

  Optional list of Tool objects for function calling.

- max_steps:

  Maximum number of generation steps (tool execution loops). Default 1
  (single generation, no automatic tool execution). Set to higher values
  (e.g., 5) to enable automatic tool execution.

- skills:

  Optional path to skills directory, or a SkillRegistry object. When
  provided, skill tools are auto-injected and skill summaries are added
  to the system prompt.

- session:

  Optional ChatSession object. When provided, tool executions run in the
  session's environment, enabling cross-agent data sharing.

- hooks:

  Optional HookHandler object for intercepting events.

- registry:

  Optional ProviderRegistry to use (defaults to global registry).

- ...:

  Additional arguments passed to the model.

## Value

A GenerateResult object with text and optionally tool_calls. When
max_steps \> 1 and tools are used, the result includes:

- steps: Number of steps taken

- all_tool_calls: List of all tool calls made across all steps

## Examples

``` r
if (FALSE) { # \dontrun{
# Using hooks
my_hooks <- create_hooks(
  on_generation_start = function(model, prompt, tools) message("Starting..."),
  on_tool_start = function(tool, args) message("Calling tool ", tool$name)
)
result <- generate_text(model, "...", hooks = my_hooks)
} # }
```
