# Tests for Core API functions
library(testthat)
library(aisdk)

openai_model <- get_openai_model()
openai_model_id <- paste0("openai:", openai_model)

# === Tests for generate_text ===

test_that("generate_text accepts a model object", {
  provider <- suppressWarnings(create_openai())
  model <- provider$language_model(openai_model)
  
  # We can't make real API calls in unit tests, but we can verify parameter handling
  # This will fail due to no API key or network error
  expect_error(
    generate_text(model = model, prompt = "Hello")
  )
})

test_that("generate_text accepts a string model identifier", {
  # Register a mock provider
  registry <- get_default_registry()
  provider <- suppressWarnings(create_openai())
  registry$register("openai", provider)
  
  # This will fail due to no API key or network error
  expect_error(
    generate_text(model = openai_model_id, prompt = "Hello")
  )
})

# === Tests for ProviderRegistry ===

test_that("ProviderRegistry registers and retrieves providers", {
  registry <- ProviderRegistry$new()
  provider <- suppressWarnings(create_openai())
  
  registry$register("test-openai", provider)
  # Verify provider is registered by checking list_providers
  expect_true("test-openai" %in% registry$list_providers())
})

test_that("ProviderRegistry parses provider:model syntax", {
  registry <- ProviderRegistry$new()
  provider <- suppressWarnings(create_openai())
  registry$register("myopenai", provider)
  
  model <- registry$language_model(paste0("myopenai:", openai_model))
  
  expect_s3_class(model, "OpenAILanguageModel")
  expect_equal(model$model_id, openai_model)
})

test_that("ProviderRegistry errors on unregistered provider", {
  registry <- ProviderRegistry$new()
  
  expect_error(
    registry$language_model("nonexistent:model"),
    "Provider not found"
  )
})

# === Tests for get_default_registry ===

test_that("get_default_registry returns singleton", {
  reg1 <- get_default_registry()
  reg2 <- get_default_registry()
  
  expect_identical(reg1, reg2)
})
