# Using Tools

Tools allow AI models to perform actions, like calculating numbers,
fetching data, or interacting with the filesystem. **aisdk** makes it
easy to turn R functions into tools.

## Defining Tools

You define a tool using the
[`tool()`](https://YuLab-SMU.github.io/aisdk/reference/tool_factory.md)
function. You also need to define a schema for the tool’s expected
arguments using helper functions (`z_object`, `z_string`, `z_number`,
etc.).

### Example: Weather Tool

``` r
library(aisdk)

# 1. Define the input schema
weather_schema <- z_object(
  location = z_string(description = "The city name, e.g., 'Tokyo'"),
  unit = z_enum(c("celsius", "fahrenheit"), description = "Temperature unit")
)

# 2. Create the tool
get_weather <- tool(
  name = "get_weather",
  description = "Get the current weather for a city",
  parameters = weather_schema,
  execute = function(args) {
    # In a real app, call an API here
    # For demo, returning mock string
    location <- args$location
    unit <- args$unit %||% "celsius"
    
    paste0("Weather in ", location, ": 22°C (", unit, ")")
  }
)
```

## Using Tools with Models

To use tools, pass them as a list to
[`generate_text()`](https://YuLab-SMU.github.io/aisdk/reference/generate_text.md).

``` r
model <- create_openai()$language_model("gpt-4o")

result <- generate_text(
  model = model,
  prompt = "What's the weather like in Paris?",
  tools = list(get_weather)
)

# If the model decided to call the tool, the result text will contain the final answer
# after the tool was executed and the result fed back to the model.
cat(result$text)
```

## Manual Execution

You can also run tools manually to test them:

``` r
result <- get_weather$run(list(location = "London", unit = "celsius"))
print(result)
```

## Supported Schema Types

| Helper                                                                        | Type    | Description                    |
|-------------------------------------------------------------------------------|---------|--------------------------------|
| [`z_string()`](https://YuLab-SMU.github.io/aisdk/reference/z_string.md)       | String  | Text inputs                    |
| [`z_number()`](https://YuLab-SMU.github.io/aisdk/reference/z_number.md)       | Number  | Integers or floats             |
| [`z_boolean()`](https://YuLab-SMU.github.io/aisdk/reference/z_boolean.md)     | Boolean | TRUE/FALSE flags               |
| [`z_enum()`](https://YuLab-SMU.github.io/aisdk/reference/z_enum.md)           | String  | Restricted set of choices      |
| [`z_array()`](https://YuLab-SMU.github.io/aisdk/reference/z_array.md)         | Array   | List of items                  |
| [`z_object()`](https://YuLab-SMU.github.io/aisdk/reference/z_object.md)       | Object  | Nested structure               |
| [`z_dataframe()`](https://YuLab-SMU.github.io/aisdk/reference/z_dataframe.md) | Array   | List of objects (R data.frame) |
