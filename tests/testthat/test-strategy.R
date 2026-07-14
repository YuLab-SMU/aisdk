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
  # Structure-preserving parse (simplify = FALSE): a JSON array is a list, so
  # arrays of objects don't collapse to data.frames. Scalar arrays are lists too.
  expect_equal(result$items, list("a", "b"))
})

test_that("ObjectStrategy validate returns NULL for empty input", {
  schema <- z_object(name = z_string())
  strategy <- ObjectStrategy$new(schema)
  
  expect_null(strategy$validate(NULL))
  expect_null(strategy$validate(""))
  expect_null(strategy$validate("   "))
})

# --- X2: generate_object structure-preserving parse + reask retry -------------

test_that("ObjectStrategy keeps arrays of objects as lists (not data.frames)", {
  strategy <- ObjectStrategy$new(z_object(items = z_array(z_object(a = z_string("a")))), "r")
  obj <- strategy$validate('{"items":[{"a":"first"},{"a":"second"}]}')
  expect_true(is.list(obj$items))
  expect_null(dim(obj$items)) # not simplified to a data.frame
  expect_equal(obj$items[[1]]$a, "first") # indexing yields the item, not a column
})

test_that("generate_object reasks when a required field is missing, then recovers", {
  source(test_path("helper-mock.R"))
  schema <- z_object(name = z_string("name"), age = z_number("age"))
  model <- MockModel$new(list(
    list(text = '{"name":"Bob"}', finish_reason = "stop"),        # missing age
    list(text = '{"name":"Bob","age":42}', finish_reason = "stop") # corrected
  ))
  res <- generate_object(model = model, prompt = "who", schema = schema, max_retries = 1)
  expect_true(res$valid)
  expect_equal(res$attempts, 2)
  expect_equal(res$object$age, 42)
})

test_that("generate_object returns invalid without retries and reports it", {
  source(test_path("helper-mock.R"))
  schema <- z_object(name = z_string("name"), age = z_number("age"))
  model <- MockModel$new(list(list(text = '{"name":"X"}', finish_reason = "stop")))
  res <- generate_object(model = model, prompt = "who", schema = schema, max_retries = 0)
  expect_false(res$valid)
  expect_equal(res$attempts, 1)
})

test_that("object_schema_problem detects the common failure modes", {
  schema <- z_object(name = z_string("n"), age = z_number("a"))
  expect_match(aisdk:::object_schema_problem(NULL, schema), "not parseable")
  expect_match(aisdk:::object_schema_problem(list(name = "x"), schema), "missing required")
  expect_null(aisdk:::object_schema_problem(list(name = "x", age = 1), schema))
})
