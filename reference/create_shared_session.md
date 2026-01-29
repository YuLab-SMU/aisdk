# Create a Shared Session

Factory function to create a new SharedSession object.

## Usage

``` r
create_shared_session(
  model = NULL,
  system_prompt = NULL,
  tools = NULL,
  hooks = NULL,
  max_steps = 10,
  sandbox_mode = "strict",
  trace_enabled = TRUE
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

- sandbox_mode:

  Sandbox mode: "strict", "permissive", or "none". Default "strict".

- trace_enabled:

  Enable execution tracing. Default TRUE.

## Value

A SharedSession object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a shared session for multi-agent use
session <- create_shared_session(
  model = "openai:gpt-4o",
  sandbox_mode = "strict",
  trace_enabled = TRUE
)

# Execute code safely
result <- session$execute_code("x <- 1:10; mean(x)")

# Check trace
print(session$trace_summary())
} # }
```
