# SandboxManager Class

R6 class that manages an isolated R environment for executing
LLM-generated R code. Tools are bound as callable functions within this
environment, enabling the LLM to batch-invoke and process data locally.

## Methods

### Public methods

- [`SandboxManager$new()`](#method-SandboxManager-new)

- [`SandboxManager$bind_tools()`](#method-SandboxManager-bind_tools)

- [`SandboxManager$execute()`](#method-SandboxManager-execute)

- [`SandboxManager$get_tool_signatures()`](#method-SandboxManager-get_tool_signatures)

- [`SandboxManager$get_env()`](#method-SandboxManager-get_env)

- [`SandboxManager$list_tools()`](#method-SandboxManager-list_tools)

- [`SandboxManager$reset()`](#method-SandboxManager-reset)

- [`SandboxManager$print()`](#method-SandboxManager-print)

- [`SandboxManager$clone()`](#method-SandboxManager-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new SandboxManager.

#### Usage

    SandboxManager$new(
      tools = list(),
      preload_packages = c("dplyr", "purrr"),
      max_output_chars = 8000,
      parent_env = NULL
    )

#### Arguments

- `tools`:

  Optional list of Tool objects to bind into the sandbox.

- `preload_packages`:

  Character vector of package names to preload into the sandbox (their
  exports become available). Default: c("dplyr", "purrr").

- `max_output_chars`:

  Maximum characters to capture from code output. Prevents runaway
  [`print()`](https://rdrr.io/r/base/print.html) from flooding the
  context. Default: 8000.

- `parent_env`:

  Optional parent environment for the sandbox. When a ChatSession is
  available, pass `session$get_envir()` here to enable cross-step
  variable persistence.

------------------------------------------------------------------------

### Method `bind_tools()`

Bind Tool objects into the sandbox as callable R functions.

#### Usage

    SandboxManager$bind_tools(tools)

#### Arguments

- `tools`:

  A list of Tool objects to bind.

#### Returns

Invisible self (for chaining).

------------------------------------------------------------------------

### Method `execute()`

Execute R code in the sandbox environment.

#### Usage

    SandboxManager$execute(code_str)

#### Arguments

- `code_str`:

  A character string containing R code to execute.

#### Returns

A character string with captured stdout, or an error message.

------------------------------------------------------------------------

### Method `get_tool_signatures()`

Get human-readable signatures for all bound tools.

#### Usage

    SandboxManager$get_tool_signatures()

#### Returns

A character string with Markdown-formatted tool documentation.

------------------------------------------------------------------------

### Method `get_env()`

Get the sandbox environment.

#### Usage

    SandboxManager$get_env()

#### Returns

The R environment used by the sandbox.

------------------------------------------------------------------------

### Method `list_tools()`

Get list of bound tool names.

#### Usage

    SandboxManager$list_tools()

#### Returns

Character vector of tool names available in the sandbox.

------------------------------------------------------------------------

### Method `reset()`

Reset the sandbox environment (clear all user variables). Tool bindings
and preloaded packages are preserved.

#### Usage

    SandboxManager$reset()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for SandboxManager.

#### Usage

    SandboxManager$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SandboxManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
