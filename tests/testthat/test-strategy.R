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

# --- AI1: generate_object mode = "tool" (function-calling structured output) ---

# A tool-call response shaped like a provider's do_generate output.
mock_tool_call <- function(args, name = "result") {
  list(text = "", finish_reason = "tool_calls", usage = list(total_tokens = 5),
       tool_calls = list(list(id = "c1", name = name, arguments = args)))
}

test_that("mode='tool' forces the schema tool and reads the object from its arguments", {
  source(test_path("helper-mock.R"))
  schema <- z_object(name = z_string("name"), age = z_number("age"))
  model <- MockModel$new(list(mock_tool_call(list(name = "Bob", age = 42))))
  res <- generate_object(model = model, prompt = "who", schema = schema, mode = "tool")

  expect_true(res$valid)
  expect_equal(res$attempts, 1)
  expect_equal(res$object$name, "Bob")
  expect_equal(res$object$age, 42)
  # The forced tool_choice (AB1) was sent, and the schema name is the tool name.
  expect_equal(model$last_params$tool_choice, list(type = "tool", name = "result"))
})

test_that("mode='tool' makes exactly ONE model call per attempt (no agent loop)", {
  source(test_path("helper-mock.R"))
  schema <- z_object(name = z_string("name"))
  calls <- 0L
  responder <- function(params) { calls <<- calls + 1L; mock_tool_call(list(name = "Al")) }
  model <- MockModel$new(rep(list(responder), 5))
  generate_object(model = model, prompt = "who", schema = schema, mode = "tool")
  expect_equal(calls, 1L)   # not the 5 the executing agent loop would make
})

test_that("mode='tool' parses JSON-string arguments and coerces to schema types", {
  source(test_path("helper-mock.R"))
  schema <- z_object(name = z_string("name"), age = z_number("age"))
  # Real providers return arguments as a JSON string; a stringified number coerces.
  s <- generate_object(model = MockModel$new(list(mock_tool_call('{"name":"Jo","age":7}'))),
                       prompt = "who", schema = schema, mode = "tool")
  expect_equal(s$object$age, 7)
  c <- generate_object(model = MockModel$new(list(mock_tool_call(list(name = "Al", age = "30")))),
                       prompt = "who", schema = schema, mode = "tool")
  expect_identical(c$object$age, 30)
  expect_true(is.numeric(c$object$age))
})

test_that("mode='tool' reasks on a schema problem then recovers", {
  source(test_path("helper-mock.R"))
  schema <- z_object(name = z_string("name"), age = z_number("age"))
  model <- MockModel$new(list(
    mock_tool_call(list(name = "Bob")),            # missing age
    mock_tool_call(list(name = "Bob", age = 42))   # corrected
  ))
  res <- generate_object(model = model, prompt = "who", schema = schema,
                         mode = "tool", max_retries = 1)
  expect_true(res$valid)
  expect_equal(res$attempts, 2)
  expect_equal(res$object$age, 42)
})

test_that("mode='tool' reports invalid without retries", {
  source(test_path("helper-mock.R"))
  schema <- z_object(name = z_string("name"), age = z_number("age"))
  res <- generate_object(model = MockModel$new(list(mock_tool_call(list(name = "X")))),
                         prompt = "who", schema = schema, mode = "tool", max_retries = 0)
  expect_false(res$valid)
  expect_equal(res$attempts, 1)
})

# --- Z8: stream_object — incremental partial-object parsing -------------------

ChunkedMock <- R6::R6Class("ChunkedMock",
  inherit = LanguageModelV1,
  public = list(
    provider = "mock", model_id = "mock-model", chunks = NULL,
    initialize = function(chunks) self$chunks <- chunks,
    do_generate = function(params) {
      list(text = paste(self$chunks, collapse = ""), finish_reason = "stop", usage = list(total_tokens = 5))
    },
    do_stream = function(params, callback) {
      for (ch in self$chunks) callback(ch, FALSE)
      callback(NULL, TRUE)
      list(text = paste(self$chunks, collapse = ""), finish_reason = "stop",
           usage = list(total_tokens = 5), messages_added = list())
    },
    get_history_format = function() "openai"
  )
)

test_that("stream_object parses progressively more complete partial objects", {
  schema <- z_object(name = z_string("n"), age = z_number("a"))
  model <- ChunkedMock$new(list('{"name":', '"Bob"', ',"age":', "42}"))
  partials <- list()
  res <- stream_object(model = model, prompt = "who", schema = schema,
                       on_partial = function(o, done) {
                         partials[[length(partials) + 1L]] <<- list(obj = o, done = done)
                       })

  # Final object is complete and correct.
  expect_equal(res$object$name, "Bob")
  expect_equal(res$object$age, 42)
  # Callbacks fired per chunk, the last with done = TRUE.
  expect_gt(length(partials), 1)
  expect_true(partials[[length(partials)]]$done)
  # A partial object carrying `name` was seen before `age` completed.
  expect_true(any(vapply(partials, function(p) !is.null(p$obj$name), logical(1))))
})

test_that("stream_object tolerates a partial that isn't yet valid JSON", {
  schema <- z_object(items = z_array(z_string()))
  # First chunk is an unclosed object; fix_json should still yield something.
  model <- ChunkedMock$new(list('{"items":["a"', ',"b"]}'))
  expect_silent(
    res <- stream_object(model = model, prompt = "list", schema = schema,
                         on_partial = function(o, done) invisible(NULL))
  )
  expect_equal(res$object$items, list("a", "b"))
})
