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

test_that("NvidiaProvider uses NVIDIA_MODEL env var", {
  # Save original env var
  original_model <- Sys.getenv("NVIDIA_MODEL")
  on.exit({
    if (original_model == "") {
      Sys.unsetenv("NVIDIA_MODEL")
    } else {
      Sys.setenv(NVIDIA_MODEL = original_model)
    }
  })

  # Set test env var
  Sys.setenv(NVIDIA_MODEL = "nvidia/test-model")
  
  # Create provider and model
  provider <- create_nvidia(api_key = "test-key")
  model <- provider$language_model()
  
  expect_equal(model$model_id, "nvidia/test-model")
  
  # Explicit argument should override env var
  model_explicit <- provider$language_model("explicit-model")
  expect_equal(model_explicit$model_id, "explicit-model")
  
  # Unset env var and check error
  Sys.unsetenv("NVIDIA_MODEL")
  expect_error(provider$language_model(), "Model ID not provided and NVIDIA_MODEL environment variable not set")
})
