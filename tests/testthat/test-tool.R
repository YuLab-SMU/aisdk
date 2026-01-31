# Tests for Tool class
library(testthat)
library(aisdk)

# Helper for empty named list comparison
empty_named_list <- function() setNames(list(), character(0))

# === Tests for parse_tool_arguments (robust argument parsing) ===

test_that("parse_tool_arguments handles NULL", {
  result <- parse_tool_arguments(NULL)
  expect_equal(result, empty_named_list())
})

test_that("parse_tool_arguments handles empty string", {
  result <- parse_tool_arguments("")
  expect_equal(result, empty_named_list())
})

test_that("parse_tool_arguments handles empty JSON object", {
  result <- parse_tool_arguments("{}")
  expect_equal(result, empty_named_list())
})

test_that("parse_tool_arguments handles JSON null", {
  result <- parse_tool_arguments("null")
  expect_equal(result, empty_named_list())
})

test_that("parse_tool_arguments handles incomplete JSON '{'", {
 # This is the glm-4 model bug case
  result <- parse_tool_arguments("{", tool_name = "test_tool")
  expect_equal(result, empty_named_list())
})

test_that("parse_tool_arguments handles incomplete JSON '}'", {
  result <- parse_tool_arguments("}")
  expect_equal(result, empty_named_list())
})

test_that("parse_tool_arguments handles valid JSON", {
  result <- parse_tool_arguments('{"name": "test", "value": 123}')
  expect_equal(result$name, "test")
  expect_equal(result$value, 123)
})

test_that("parse_tool_arguments handles list input", {
  input <- list(name = "test", value = 123)
  result <- parse_tool_arguments(input)
  expect_equal(result, input)
})

test_that("parse_tool_arguments handles NA", {
  result <- parse_tool_arguments(NA)
  expect_equal(result, empty_named_list())
})

test_that("parse_tool_arguments returns named list for JSON serialization", {
  result <- parse_tool_arguments("{")
  # Should serialize to {} not []
  expect_equal(as.character(jsonlite::toJSON(result, auto_unbox = TRUE)), "{}")
})

# === Tests for repair_json_string ===

test_that("repair_json_string fixes truncated JSON", {
  # Missing closing brace
  result <- repair_json_string('{"name": "test"')
  expect_equal(result, '{"name": "test"}')
})

test_that("repair_json_string fixes trailing comma", {
  result <- repair_json_string('{"name": "test",}')
  expect_equal(result, '{"name": "test"}')
})

test_that("repair_json_string handles empty input", {
  result <- repair_json_string("")
  expect_equal(result, "{}")
})

test_that("repair_json_string handles single brace", {
  result <- repair_json_string("{")
  expect_equal(result, "{}")
})

test_that("repair_json_string fixes multiple missing braces", {
  result <- repair_json_string('{"a": {"b": "c"')
  expect_equal(result, '{"a": {"b": "c"}}')
})

# === Tests for Tool creation ===

test_that("Tool can be created with valid parameters", {
  t <- tool(
    name = "test_tool",
    description = "A test tool",
    parameters = z_object(
      input = z_string(description = "Test input")
    ),
    execute = function(args) args$input
  )
  
  expect_s3_class(t, "Tool")
  expect_equal(t$name, "test_tool")
  expect_equal(t$description, "A test tool")
})

test_that("Tool can infer parameters from execute signature", {
  t <- tool(
    name = "add_numbers",
    description = "Adds two numbers",
    execute = function(a, b) a + b
  )

  expect_s3_class(t$parameters, "z_object")
  expect_true(all(c("a", "b") %in% names(t$parameters$properties)))
  expect_s3_class(t$parameters$properties$a, "z_any")
  expect_equal(t$run(list(a = 1, b = 2)), 3)
})

test_that("Tool accepts simple parameter descriptors", {
  t1 <- tool(
    name = "echo",
    description = "Echo input",
    parameters = c(message = "Message to echo"),
    execute = function(args) args$message
  )

  expect_s3_class(t1$parameters$properties$message, "z_any")

  t2 <- tool(
    name = "sum_list",
    description = "Sum numbers",
    parameters = list(values = "Numbers to sum"),
    execute = function(args) sum(args$values)
  )

  expect_s3_class(t2$parameters$properties$values, "z_any")
})

test_that("Tool validates name", {
  expect_error(
    tool("", "desc", z_object(x = z_string()), function(a) a),
    "non-empty string"
  )
})

test_that("Tool validates description", {
  expect_error(
    tool("name", 123, z_object(x = z_string()), function(a) a),
    "must be a string"
  )
})

test_that("Tool validates parameters", {
  expect_error(
    tool("name", "desc", list(type = "object"), function(a) a),
    "z_schema object"
  )
})

test_that("Tool validates execute", {
  expect_error(
    tool("name", "desc", z_object(x = z_string()), "not a function"),
    "must be a function"
  )
})

# === Tests for Tool$to_api_format ===

test_that("Tool$to_api_format returns OpenAI format", {
  t <- tool(
    name = "get_weather",
    description = "Get weather",
    parameters = z_object(
      location = z_string(description = "City")
    ),
    execute = function(args) "sunny"
  )
  
  format <- t$to_api_format("openai")
  
  expect_equal(format$type, "function")
  expect_equal(format$`function`$name, "get_weather")
  expect_equal(format$`function`$description, "Get weather")
  expect_equal(format$`function`$parameters$type, "object")
})

test_that("Tool$to_api_format returns Anthropic format", {
  t <- tool(
    name = "get_weather",
    description = "Get weather",
    parameters = z_object(
      location = z_string()
    ),
    execute = function(args) "sunny"
  )
  
  format <- t$to_api_format("anthropic")
  
  expect_equal(format$name, "get_weather")
  expect_equal(format$description, "Get weather")
  expect_true("input_schema" %in% names(format))
  expect_equal(format$input_schema$type, "object")
})

# === Tests for Tool$run ===

test_that("Tool$run executes the function", {
  t <- tool(
    name = "echo",
    description = "Echo input",
    parameters = z_object(
      message = z_string()
    ),
    execute = function(args) paste("Echo:", args$message)
  )
  
  result <- t$run(list(message = "Hello"))
  expect_equal(result, "Echo: Hello")
})

test_that("Tool$run handles JSON string arguments", {
  t <- tool(
    name = "echo",
    description = "Echo input",
    parameters = z_object(
      message = z_string()
    ),
    execute = function(args) paste("Echo:", args$message)
  )
  
  result <- t$run('{"message": "Hello"}')
  expect_equal(result, "Echo: Hello")
})

# === Tests for find_tool ===

test_that("find_tool finds tool by name", {
  t1 <- tool("tool1", "desc1", z_object(x = z_string()), function(a) a)
  t2 <- tool("tool2", "desc2", z_object(x = z_string()), function(a) a)
  
  found <- find_tool(list(t1, t2), "tool2")
  expect_equal(found$name, "tool2")
})

test_that("find_tool returns NULL for missing tool", {
  t1 <- tool("tool1", "desc1", z_object(x = z_string()), function(a) a)

  found <- find_tool(list(t1), "nonexistent")
  expect_null(found)
})

# === Tests for Tool$run with edge cases (LLM compatibility) ===

test_that("Tool$run handles incomplete JSON from LLM", {
  t <- tool(
    name = "no_args_tool",
    description = "Tool with optional args",
    parameters = z_object(
      optional_arg = z_string("Optional argument")
    ),
    execute = function(args) "success"
  )

  # glm-4 model returns "{" instead of "{}"
  result <- t$run("{")
  expect_equal(result, "success")
})

test_that("Tool$run handles various empty representations", {
  t <- tool(
    name = "flexible_tool",
    description = "Flexible tool",
    parameters = z_object(
      optional_arg = z_string("Optional")
    ),
    execute = function(args) length(args)
  )

  # All these should work without error
  expect_equal(t$run("{}"), 0)
  expect_equal(t$run(""), 0)
  expect_equal(t$run("null"), 0)
  expect_equal(t$run("{"), 0)
  expect_equal(t$run(NULL), 0)
  expect_equal(t$run(list()), 0)
})

test_that("Tool$run handles JSON with trailing comma", {
  t <- tool(
    name = "test_tool",
    description = "Test",
    parameters = z_object(name = z_string()),
    execute = function(args) args$name
  )

  # Some models add trailing commas
  result <- t$run('{"name": "test",}')
  expect_equal(result, "test")
})

test_that("Tool$run handles truncated JSON with partial data", {
  t <- tool(
    name = "test_tool",
    description = "Test",
    parameters = z_object(
      name = z_string(),
      value = z_number()
    ),
    execute = function(args) {
      paste(args$name %||% "default", args$value %||% 0)
    }
  )

  # Truncated JSON - should recover what's possible
  result <- t$run('{"name": "test"')
  expect_equal(result, "test 0")
})

# === Tests for Tool Call Repair Mechanism (Opencode-inspired) ===

test_that("repair_tool_call fixes lowercase tool name", {
  t <- tool("get_weather", "Get weather", z_object(city = z_string()), function(a) a)
  tools <- list(t)

  # LLM calls "GetWeather" but tool is "get_weather"
  tc <- list(id = "call_1", name = "GetWeather", arguments = list(city = "Tokyo"))
  repaired <- repair_tool_call(tc, tools)

  expect_equal(repaired$name, "get_weather")
  expect_equal(repaired$arguments$city, "Tokyo")
})

test_that("repair_tool_call converts camelCase to snake_case", {
  t <- tool("get_current_weather", "Get weather", z_object(city = z_string()), function(a) a)
  tools <- list(t)

  # LLM calls "getCurrentWeather" but tool is "get_current_weather"
  tc <- list(id = "call_1", name = "getCurrentWeather", arguments = list(city = "Tokyo"))
  repaired <- repair_tool_call(tc, tools)

  expect_equal(repaired$name, "get_current_weather")
})

test_that("repair_tool_call routes to invalid tool when unrepairable", {
  t <- tool("get_weather", "Get weather", z_object(city = z_string()), function(a) a)
  tools <- list(t)

  # LLM calls completely wrong tool name
  tc <- list(id = "call_1", name = "completely_wrong_name", arguments = list(city = "Tokyo"))
  repaired <- repair_tool_call(tc, tools)

  expect_equal(repaired$name, "__invalid__")
  expect_equal(repaired$arguments$original_tool, "completely_wrong_name")
  expect_true(!is.null(repaired$arguments$error))
})

test_that("repair_tool_call uses fuzzy matching for close names", {
  t <- tool("get_weather", "Get weather", z_object(city = z_string()), function(a) a)
  tools <- list(t)

  # LLM calls "get_wether" (typo) but tool is "get_weather"
  tc <- list(id = "call_1", name = "get_wether", arguments = list(city = "Tokyo"))
  repaired <- repair_tool_call(tc, tools)

  expect_equal(repaired$name, "get_weather")
})

# === Tests for to_snake_case ===

test_that("to_snake_case converts camelCase", {
  expect_equal(to_snake_case("getWeather"), "get_weather")
  expect_equal(to_snake_case("getCurrentWeather"), "get_current_weather")
  # XMLParser -> xmlparser (all uppercase sequences stay together)
  expect_equal(to_snake_case("XMLParser"), "xmlparser")
})

test_that("to_snake_case handles already snake_case", {
  expect_equal(to_snake_case("get_weather"), "get_weather")
  expect_equal(to_snake_case("already_snake"), "already_snake")
})

test_that("to_snake_case handles single word", {
  expect_equal(to_snake_case("weather"), "weather")
  expect_equal(to_snake_case("Weather"), "weather")
})

# === Tests for find_closest_match ===

test_that("find_closest_match finds close matches", {
  candidates <- c("get_weather", "set_temperature", "list_cities")

  expect_equal(find_closest_match("get_wether", candidates), "get_weather")
  expect_equal(find_closest_match("get_weather", candidates), "get_weather")
})

test_that("find_closest_match returns NULL for distant matches", {
  candidates <- c("get_weather", "set_temperature")

  result <- find_closest_match("completely_different", candidates, max_distance = 3)
  expect_null(result)
})

test_that("find_closest_match handles empty candidates", {
  result <- find_closest_match("anything", character(0))
  expect_null(result)
})

# === Tests for create_invalid_tool_handler ===

test_that("create_invalid_tool_handler creates valid tool", {
  invalid_tool <- create_invalid_tool_handler()

  expect_s3_class(invalid_tool, "Tool")
  expect_equal(invalid_tool$name, "__invalid__")
})

test_that("invalid tool handler returns structured error", {
  invalid_tool <- create_invalid_tool_handler()

  result <- invalid_tool$run(list(
    original_tool = "nonexistent_tool",
    original_arguments = list(x = 1),
    error = "Tool not found"
  ))

  # Result should be JSON string
  parsed <- jsonlite::fromJSON(result)
  expect_false(parsed$success)
  expect_equal(parsed$error_type, "invalid_tool_call")
  expect_true(grepl("nonexistent_tool", parsed$message))
})

# === Tests for execute_tool_calls with repair ===

test_that("execute_tool_calls repairs tool name automatically", {
  t <- tool("get_weather", "Get weather", z_object(city = z_string()),
            function(args) paste("Weather in", args$city))
  tools <- list(t)

  # LLM calls "GetWeather" but tool is "get_weather"
  tool_calls <- list(
    list(id = "call_1", name = "GetWeather", arguments = list(city = "Tokyo"))
  )

  results <- execute_tool_calls(tool_calls, tools, repair_enabled = TRUE)

  expect_equal(length(results), 1)
  expect_false(results[[1]]$is_error)
  expect_true(grepl("Tokyo", results[[1]]$result))
})

test_that("execute_tool_calls handles invalid tool gracefully", {
  t <- tool("get_weather", "Get weather", z_object(city = z_string()),
            function(args) paste("Weather in", args$city))
  tools <- list(t)

  # LLM calls completely wrong tool
  tool_calls <- list(
    list(id = "call_1", name = "nonexistent_tool_xyz", arguments = list(x = 1))
  )

  results <- execute_tool_calls(tool_calls, tools, repair_enabled = TRUE)

  expect_equal(length(results), 1)
  # Should not crash, but return error result from invalid tool handler
  expect_false(results[[1]]$is_error)  # Invalid tool handler returns success
  expect_true(grepl("not available", results[[1]]$result))
})

test_that("execute_tool_calls can disable repair", {
  t <- tool("get_weather", "Get weather", z_object(city = z_string()),
            function(args) paste("Weather in", args$city))
  tools <- list(t)

  # LLM calls "GetWeather" but tool is "get_weather"
  tool_calls <- list(
    list(id = "call_1", name = "GetWeather", arguments = list(city = "Tokyo"))
  )

  results <- execute_tool_calls(tool_calls, tools, repair_enabled = FALSE)

  expect_equal(length(results), 1)
  expect_true(results[[1]]$is_error)
  expect_true(grepl("not found", results[[1]]$result))
})

# === Tests for enhanced repair_json_string ===

test_that("repair_json_string handles single quotes", {
  result <- repair_json_string("{'name': 'test'}")
  # Should convert single quotes to double quotes for keys
  expect_true(grepl('"name":', result))
})

test_that("repair_json_string handles empty array patterns", {
  expect_equal(repair_json_string("["), "[]")
  expect_equal(repair_json_string("]"), "[]")
})

test_that("repair_json_string handles nested truncation", {
  # repair_json_string is a lightweight repair, use fix_json for complex cases
  # This test verifies that the multi-layer fallback in parse_tool_arguments works
  result <- parse_tool_arguments('{"a": {"b": [1, 2')
  # Should parse successfully via fix_json fallback
  expect_true(is.list(result))
  expect_true(!is.null(result$a))
})

# === Tests for enhanced parse_tool_arguments ===

test_that("parse_tool_arguments handles empty array patterns", {
  expect_equal(parse_tool_arguments("[]"), empty_named_list())
  expect_equal(parse_tool_arguments("[ ]"), empty_named_list())
})

test_that("parse_tool_arguments uses multi-layer fallback", {
  # Severely malformed JSON that needs multiple repair attempts
  result <- parse_tool_arguments('{name: "test"')  # Unquoted key + missing brace

  # Should still parse successfully
  expect_equal(result$name, "test")
})
