# Tests for Schema DSL functions
library(testthat)
library(aisdk)

# === Tests for z_string ===

test_that("z_string creates string schema", {
  schema <- z_string()
  expect_s3_class(schema, "z_schema")
  expect_s3_class(schema, "z_string")
  expect_equal(schema$type, "string")
})

test_that("z_string includes description", {
  schema <- z_string(description = "A test string")
  expect_equal(schema$description, "A test string")
})

test_that("z_string handles nullable", {
  schema <- z_string(nullable = TRUE)
  expect_equal(schema$type, c("string", "null"))
})

# === Tests for z_number ===

test_that("z_number creates number schema with constraints", {
  schema <- z_number(minimum = 0, maximum = 100)
  expect_equal(schema$type, "number")
  expect_equal(schema$minimum, 0)
  expect_equal(schema$maximum, 100)
})

# === Tests for z_integer ===

test_that("z_integer creates integer schema", {
  schema <- z_integer()
  expect_equal(schema$type, "integer")
})

# === Tests for z_boolean ===

test_that("z_boolean creates boolean schema", {
  schema <- z_boolean(description = "Enable feature")
  expect_equal(schema$type, "boolean")
  expect_equal(schema$description, "Enable feature")
})

# === Tests for z_enum ===

test_that("z_enum creates enum schema", {
  schema <- z_enum(c("celsius", "fahrenheit"))
  expect_equal(schema$type, "string")
  expect_equal(schema$enum, list("celsius", "fahrenheit"))
})

test_that("z_enum validates input", {
  expect_error(z_enum(c()), "non-empty character vector")
  expect_error(z_enum(123), "non-empty character vector")
})

# === Tests for z_array ===

test_that("z_array creates array schema", {
  items <- z_string()
  schema <- z_array(items, min_items = 1, max_items = 10)
  
  expect_equal(schema$type, "array")
  expect_s3_class(schema$items, "z_string")
  expect_equal(schema$minItems, 1)
  expect_equal(schema$maxItems, 10)
})

test_that("z_array validates items type", {
  expect_error(z_array("not a schema"), "z_schema object")
})

# === Tests for z_object ===

test_that("z_object creates object schema", {
  schema <- z_object(
    name = z_string(description = "User name"),
    age = z_integer(description = "User age")
  )
  
  expect_equal(schema$type, "object")
  expect_true("name" %in% names(schema$properties))
  expect_true("age" %in% names(schema$properties))
  expect_equal(schema$required, list("name", "age"))
})

test_that("z_object with custom required fields", {
  schema <- z_object(
    name = z_string(),
    nickname = z_string(),
    .required = c("name")
  )
  
  expect_equal(schema$required, list("name"))
})

test_that("z_object validates properties", {
  expect_error(z_object(), "at least one property")
  expect_error(z_object(z_string()), "must be named")
})

# === Tests for schema_to_json ===

test_that("schema_to_json produces valid JSON", {
  schema <- z_object(
    location = z_string(description = "City name"),
    unit = z_enum(c("celsius", "fahrenheit"))
  )
  
  json <- schema_to_json(schema)
  expect_true(is.character(json))
  
  # Parse back and verify structure
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(parsed$type, "object")
  expect_true("location" %in% names(parsed$properties))
})

test_that("schema_to_json handles nested structures", {
  schema <- z_object(
    items = z_array(z_object(
      id = z_integer(),
      name = z_string()
    ))
  )
  
  json <- schema_to_json(schema)
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  
  expect_equal(parsed$properties$items$type, "array")
  expect_equal(parsed$properties$items$items$type, "object")
})
