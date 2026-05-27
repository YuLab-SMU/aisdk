test_that("semantic adapter public API exports are present", {
  expected_exports <- c(
    "create_semantic_adapter",
    "create_semantic_adapter_registry",
    "get_or_create_semantic_adapter_registry",
    "create_default_semantic_adapter_registry",
    "call_object_accessor",
    "is_semantic_class",
    "as_preview_text"
  )

  exports <- getNamespaceExports("aisdk")
  expect_true(all(expected_exports %in% exports))
})

test_that("create_default_semantic_adapter_registry returns a registry object", {
  registry <- aisdk::create_default_semantic_adapter_registry(
    extension_registrars = list()
  )

  expect_s3_class(registry, "SemanticAdapterRegistry")
  expect_true(is.function(registry$register))
  expect_true(is.function(registry$resolve))
})

test_that("get_or_create_semantic_adapter_registry stores and reuses registry in env", {
  envir <- new.env(parent = emptyenv())

  first <- aisdk::get_or_create_semantic_adapter_registry(envir = envir)
  second <- aisdk::get_or_create_semantic_adapter_registry(envir = envir)

  expect_true(exists(".semantic_adapter_registry", envir = envir, inherits = FALSE))
  expect_identical(second, first)
  expect_identical(get(".semantic_adapter_registry", envir = envir, inherits = FALSE), first)
})

test_that("is_semantic_class works for S3 inheritance", {
  obj <- structure(list(value = 1), class = c("my_child", "my_parent"))

  expect_true(aisdk::is_semantic_class(obj, "my_child"))
  expect_true(aisdk::is_semantic_class(obj, "my_parent"))
  expect_false(aisdk::is_semantic_class(obj, "not_a_class"))
})

test_that("call_object_accessor tries candidates in order and falls back to default", {
  expect_identical(
    aisdk::call_object_accessor(
      obj = "abcdef",
      fun_names = c("definitely_missing_accessor", "substr"),
      args = list(start = 1, stop = 3),
      default = "fallback"
    ),
    "abc"
  )

  expect_identical(
    aisdk::call_object_accessor(
      obj = FALSE,
      fun_names = "stopifnot",
      default = "fallback"
    ),
    "fallback"
  )
})

test_that("as_preview_text returns stable preview output for vectors and data frames", {
  expect_identical(
    aisdk::as_preview_text(1:6, max_items = 3),
    "1, 2, 3"
  )

  expect_identical(
    aisdk::as_preview_text(c("alpha", "beta", "gamma"), max_items = 2),
    "alpha, beta"
  )

  preview_df <- aisdk::as_preview_text(
    data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE),
    max_items = 2
  )
  expect_type(preview_df, "character")
  expect_true(grepl("\\ba\\b", preview_df))
  expect_true(grepl("\\bb\\b", preview_df))
  expect_true(grepl("\\bx\\b", preview_df))
  expect_true(grepl("\\by\\b", preview_df))

  expect_null(aisdk::as_preview_text(NULL))
})
