# Create Session (Compatibility Wrapper)

Creates a session using either the new SharedSession or legacy
ChatSession based on feature flags. This provides a migration path for
existing code.

## Usage

``` r
create_session(
  model = NULL,
  system_prompt = NULL,
  tools = NULL,
  hooks = NULL,
  max_steps = 10,
  ...
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

- ...:

  Additional arguments passed to session constructor.

## Value

A SharedSession or ChatSession object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Automatically uses SharedSession if feature enabled
session <- create_session(model = "openai:gpt-4o")

# Force legacy session
sdk_set_feature("use_shared_session", FALSE)
session <- create_session(model = "openai:gpt-4o")
} # }
```
