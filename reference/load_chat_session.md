# Load a Chat Session

Load a previously saved ChatSession from a file.

## Usage

``` r
load_chat_session(path, tools = NULL, hooks = NULL, registry = NULL)
```

## Arguments

- path:

  File path to load from (.rds or .json).

- tools:

  Optional list of Tool objects (tools are not saved, must be
  re-provided).

- hooks:

  Optional HookHandler object.

- registry:

  Optional ProviderRegistry.

## Value

A ChatSession object with restored state.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load a saved session
chat <- load_chat_session("my_session.rds", tools = my_tools)

# Continue where you left off
response <- chat$send("Let's continue our discussion")
} # }
```
