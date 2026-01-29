# Repair Tool Call

Attempts to repair a failed tool call. This implements a multi-layer
repair strategy inspired by Opencode's experimental_repairToolCall:

1.  Try to fix tool name case issues (e.g., "GetWeather" -\>
    "get_weather")

2.  If repair fails, route to an "invalid" tool for graceful handling

## Usage

``` r
repair_tool_call(tool_call, tools, error_message = NULL)
```

## Arguments

- tool_call:

  A list with name, arguments, and optionally id.

- tools:

  A list of available Tool objects.

- error_message:

  Optional error message from the failed call.

## Value

A repaired tool call list, or an "invalid" tool call if unrepairable.

## Examples

``` r
if (FALSE) { # \dontrun{
# If LLM calls "GetWeather" but tool is "get_weather"
repaired <- repair_tool_call(
  list(name = "GetWeather", arguments = list(city = "Tokyo")),
  tools = list(get_weather_tool)
)
} # }
```
