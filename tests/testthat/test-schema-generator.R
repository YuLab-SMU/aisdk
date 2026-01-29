test_that("create_schema_from_func infers types correctly", {
  # Define a test function with various default types
  test_func <- function(
    str_arg = "default",
    num_arg = 10,
    bool_arg = TRUE,
    null_arg = NULL,
    enum_arg = c("opt1", "opt2"),
    no_default
  ) {}
  
  schema <- create_schema_from_func(test_func)
  
  expect_s3_class(schema, "z_object")
  expect_s3_class(schema$properties$str_arg, "z_string")
  expect_equal(schema$properties$str_arg$default, "default")
  
  expect_s3_class(schema$properties$num_arg, "z_number")
  expect_equal(schema$properties$num_arg$default, 10)
  
  expect_s3_class(schema$properties$bool_arg, "z_boolean")
  expect_equal(schema$properties$bool_arg$default, TRUE)
  
  expect_s3_class(schema$properties$null_arg, "z_string")
  expect_true("null" %in% schema$properties$null_arg$type) # Should be nullable
  
  # Enum detection
  expect_s3_class(schema$properties$enum_arg, "z_enum")
  expect_equal(schema$properties$enum_arg$enum, list("opt1", "opt2"))
  expect_equal(schema$properties$enum_arg$default, "opt1") # First element matches match.arg
  
  # No default = required
  expect_true("no_default" %in% schema$required)
})

test_that("create_z_ggtree works", {
  # Mock a ggtree-like function
  mock_geom_tiplab <- function(mapping = NULL, data = NULL, align = FALSE, ...) {}
  
  schema <- create_z_ggtree(mock_geom_tiplab)
  
  # mapping and data should be excluded
  expect_false("mapping" %in% names(schema$properties))
  expect_false("data" %in% names(schema$properties))
  
  # align should be present and boolean
  expect_true("align" %in% names(schema$properties))
  expect_s3_class(schema$properties$align, "z_boolean")
})

test_that("create_schema_from_func accepts param overrides", {
  test_func <- function(a = 1, b = "text") {}
  
  # Override a with 99
  schema <- create_schema_from_func(test_func, params = list(a = 99))
  
  expect_equal(schema$properties$a$default, 99)
  expect_equal(schema$properties$b$default, "text")
})

test_that("create_z_ggtree extracts params from layer", {
  mock_geom_tiplab <- function(size=3, color="black", ...) {}
  
  # Mock a ggplot layer
  mock_layer <- list(
      aes_params = list(size = 10),
      geom_params = list(color = "purple")
  )
  
  schema <- create_z_ggtree(mock_geom_tiplab, layer = mock_layer)
  
  expect_equal(schema$properties$size$default, 10)
  expect_equal(schema$properties$color$default, "purple")
})
