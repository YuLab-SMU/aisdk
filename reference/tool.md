# Tool Class

R6 class representing a callable tool for LLM function calling. A Tool
connects an LLM's tool call request to an R function.

## Public fields

- `name`:

  The unique name of the tool.

- `description`:

  A description of what the tool does.

- `parameters`:

  A z_object schema defining the tool's parameters.

## Methods

### Public methods

- [`Tool$new()`](#method-Tool-new)

- [`Tool$to_api_format()`](#method-Tool-to_api_format)

- [`Tool$run()`](#method-Tool-run)

- [`Tool$print()`](#method-Tool-print)

- [`Tool$clone()`](#method-Tool-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a Tool.

#### Usage

    Tool$new(name, description, parameters, execute)

#### Arguments

- `name`:

  Unique tool name (used by LLM to call the tool).

- `description`:

  Description of the tool's purpose.

- `parameters`:

  A z_object schema defining expected parameters.

- `execute`:

  An R function that implements the tool logic.

------------------------------------------------------------------------

### Method `to_api_format()`

Convert tool to API format.

#### Usage

    Tool$to_api_format(provider = "openai")

#### Arguments

- `provider`:

  Provider name ("openai" or "anthropic"). Default "openai".

#### Returns

A list in the format expected by the API.

------------------------------------------------------------------------

### Method `run()`

Execute the tool with given arguments.

#### Usage

    Tool$run(args, envir = NULL)

#### Arguments

- `args`:

  A list or named list of arguments.

- `envir`:

  Optional environment in which to evaluate the tool function. When
  provided, the environment is passed as `.envir` in the args list,
  allowing the execute function to access and modify session variables.

#### Returns

The result of executing the tool function.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method for Tool.

#### Usage

    Tool$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Tool$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
