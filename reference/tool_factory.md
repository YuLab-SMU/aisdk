# Create a Tool

Factory function to create a Tool object. This is the recommended way to
define tools for LLM function calling.

## Usage

``` r
tool(name, description, parameters, execute)
```

## Arguments

- name:

  Unique tool name (used by LLM to call the tool).

- description:

  Description of the tool's purpose. Be descriptive to help the LLM
  understand when to use this tool.

- parameters:

  A z_object schema defining expected parameters. Use z_object() with
  z_string(), z_number(), etc. to define the schema.

- execute:

  An R function that implements the tool logic. The function receives a
  single list argument containing the parameters.

## Value

A Tool object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Define a weather tool
get_weather <- tool(
  name = "get_weather",
  description = "Get the current weather for a location",
  parameters = z_object(
    location = z_string(description = "The city name, e.g., 'Beijing'"),
    unit = z_enum(c("celsius", "fahrenheit"), description = "Temperature unit")
  ),
  execute = function(args) {
    # In real usage, call a weather API here
    paste("Weather in", args$location, "is 22 degrees", args$unit)
  }
)

# Use with generate_text
result <- generate_text(
  model = "openai:gpt-4o",
  prompt = "What's the weather in Tokyo?",
  tools = list(get_weather)
)
} # }
```
