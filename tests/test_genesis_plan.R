tryCatch({
  library(aisdk)
  library(dotenv)
  
  if (file.exists(".env")) {
    load_dot_env()
  } else {
    cat("No .env file found, skipping environment loading\n")
  }
  
  # Setup provider based on available keys
  has_api_key <- FALSE
  if (Sys.getenv("ANTHROPIC_API_KEY") != "") {
    cat("Using Anthropic provider\n")
    provider <- create_anthropic()
    model <- provider$language_model("claude-3-5-sonnet-20241022")
    has_api_key <- TRUE
  } else if (Sys.getenv("OPENAI_API_KEY") != "") {
    cat("Using OpenAI provider\n")
    provider <- create_openai()
    model_name <- Sys.getenv("OPENAI_MODEL", "gpt-4-0613")
    model <- provider$language_model(model_name)
    has_api_key <- TRUE
  }
  
  if (!has_api_key) {
    cat("No API keys found in environment, skipping test\n")
    quit(status = 0)
  }
  
  cat("Testing genesis(mode = 'plan')...\n")
  
  result <- genesis(
    "Analyze the mtcars dataset. Calculate mean mpg by cylinder and plot it.",
    mode = "plan",
    verbose = TRUE,
    model = model,
    max_steps = 5
  )
  
  print(result)
  
  cat("\nTest complete!\n")
  
}, error = function(e) {
  cat("\nERROR:", conditionMessage(e), "\n")
  quit(status = 1)
})
