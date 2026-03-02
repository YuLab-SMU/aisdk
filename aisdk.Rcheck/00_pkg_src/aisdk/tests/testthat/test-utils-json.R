test_that("fix_json returns empty object for null/empty input", {
  expect_equal(fix_json(NULL), "{}")
  expect_equal(fix_json(""), "{}")
})

test_that("fix_json handles already valid JSON", {
  expect_equal(fix_json('{"a": 1}'), '{"a": 1}')
  expect_equal(fix_json('[1, 2, 3]'), '[1, 2, 3]')
  expect_equal(fix_json('{"a": [1, 2]}'), '{"a": [1, 2]}')
})

test_that("fix_json closes unclosed strings", {
  expect_equal(fix_json('{"name": "Gene'), '{"name": "Gene"}')
  expect_equal(fix_json('{"a": "hello'), '{"a": "hello"}')
})

test_that("fix_json closes unclosed braces", {
  expect_equal(fix_json('{"a": 1'), '{"a": 1}')
  expect_equal(fix_json('{"a": {"b": 2'), '{"a": {"b": 2}}')
})

test_that("fix_json closes unclosed brackets", {
  expect_equal(fix_json('[1, 2'), '[1, 2]')
  expect_equal(fix_json('[1, [2, 3'), '[1, [2, 3]]')
})

test_that("fix_json handles mixed unclosed cases", {
  expect_equal(fix_json('{"a": [1, 2'), '{"a": [1, 2]}')
  expect_equal(fix_json('[{"a": 1'), '[{"a": 1}]')
  expect_equal(fix_json('{"items": [{"name": "test'), '{"items": [{"name": "test"}]}')
})

test_that("fix_json handles escaped quotes in strings", {
  # String with escaped quote should not close prematurely
  expect_equal(fix_json('{"a": "hello\\"world'), '{"a": "hello\\"world"}')
})

test_that("safe_parse_json parses valid JSON", {
  result <- safe_parse_json('{"a": 1, "b": "test"}')
  expect_equal(result$a, 1)
  expect_equal(result$b, "test")
  
  result <- safe_parse_json('[1, 2, 3]')
  expect_equal(result, c(1, 2, 3))
})

test_that("safe_parse_json repairs and parses truncated JSON", {
  result <- safe_parse_json('{"a": 1, "b": [1, 2')
  expect_equal(result$a, 1)
  expect_equal(result$b, c(1, 2))
})

test_that("safe_parse_json returns NULL for unparseable input", {
  expect_null(safe_parse_json(NULL))
  expect_null(safe_parse_json(""))
  expect_null(safe_parse_json("   "))
  # Severely malformed input that can't be repaired
  expect_null(safe_parse_json("not json at all"))
})
