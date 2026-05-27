test_that("ObjectStrategy initializes correctly", {
  schema <- z_object(name = z_string(), value = z_number())
  strategy <- ObjectStrategy$new(schema, "test_schema")
  
  expect_s3_class(strategy, "ObjectStrategy")
  expect_equal(strategy$schema_name, "test_schema")
  expect_equal(strategy$schema, schema)
})

test_that("ObjectStrategy get_instruction includes schema", {
  schema <- z_object(
    title = z_string(description = "The title"),
    count = z_integer()
  )
  strategy <- ObjectStrategy$new(schema, "article_data")
  
  instruction <- strategy$get_instruction()
  
  expect_type(instruction, "character")
  expect_match(instruction, "article_data")
  expect_match(instruction, "JSON")
  expect_match(instruction, "title")
  expect_match(instruction, "count")
})

test_that("ObjectStrategy validate parses clean JSON", {
  schema <- z_object(name = z_string())
  strategy <- ObjectStrategy$new(schema)
  
  result <- strategy$validate('{"name": "test"}', is_final = TRUE)
  
  expect_type(result, "list")
  expect_equal(result$name, "test")
})

test_that("ObjectStrategy validate removes markdown code blocks", {
  schema <- z_object(value = z_number())
  strategy <- ObjectStrategy$new(schema)
  
  # JSON wrapped in markdown code block
  result <- strategy$validate('```json\n{"value": 42}\n```', is_final = TRUE)
  
  expect_type(result, "list")
  expect_equal(result$value, 42)
})

test_that("ObjectStrategy validate removes generic code blocks", {
  schema <- z_object(active = z_boolean())
  strategy <- ObjectStrategy$new(schema)
  
  result <- strategy$validate('```\n{"active": true}\n```', is_final = TRUE)
  
  expect_type(result, "list")
  expect_true(result$active)
})

test_that("ObjectStrategy validate handles truncated JSON", {
  schema <- z_object(items = z_array(z_string()))
  strategy <- ObjectStrategy$new(schema)
  
  # Truncated JSON - should be repaired
  result <- strategy$validate('{"items": ["a", "b"', is_final = TRUE)
  
  expect_type(result, "list")
  expect_equal(result$items, c("a", "b"))
})

test_that("ObjectStrategy validate returns NULL for empty input", {
  schema <- z_object(name = z_string())
  strategy <- ObjectStrategy$new(schema)
  
  expect_null(strategy$validate(NULL))
  expect_null(strategy$validate(""))
  expect_null(strategy$validate("   "))
})
