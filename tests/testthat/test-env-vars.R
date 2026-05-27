# Tests for environment variable handling

test_that("OpenAIProvider uses OPENAI_MODEL env var", {
  # Save original env var
  original_model <- Sys.getenv("OPENAI_MODEL")
  on.exit({
    if (original_model == "") {
      Sys.unsetenv("OPENAI_MODEL")
    } else {
      Sys.setenv(OPENAI_MODEL = original_model)
    }
  })

  # Set test env var
  Sys.setenv(OPENAI_MODEL = "gpt-4-test")
  
  # Create provider and model
  provider <- create_openai(api_key = "test-key")
  model <- provider$language_model()
  
  expect_equal(model$model_id, "gpt-4-test")
  
  # Unset env var and check default
  Sys.unsetenv("OPENAI_MODEL")
  model_default <- provider$language_model()
  expect_equal(model_default$model_id, "gpt-4o")
})

# NvidiaProvider env-var resolution is tested in the aisdk.providers package
# (create_nvidia moved there); the OpenAI block above covers the core mechanism.
