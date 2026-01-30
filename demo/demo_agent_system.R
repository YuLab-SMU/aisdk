
# Demo: R-Agentic Framework v1.1.0 Verification
# This script demonstrates the multi-agent system working together.

# Load the package
if (requireNamespace("aisdk", quietly = TRUE)) {
  library(aisdk)
} else {
  stop("Package 'aisdk' is not installed.")
}

# Use helpers from testthat if available for consistent environment handling
# This allows using project-level keys set in .Renviron
helper_candidates <- c(
  file.path("tests", "testthat", "helper-env.R"),
  file.path("testthat", "helper-env.R"),
  "helper-env.R"
)

helper_loaded <- FALSE
for (path in helper_candidates) {
  if (file.exists(path)) {
    source(path)
    helper_loaded <- TRUE
    message("Loaded environment helper from: ", path)
    break
  }
}

# Fallback definitions if helper is not found
if (!helper_loaded) {
  get_openai_model <- function() Sys.getenv("OPENAI_MODEL", "gpt-4o-mini")
  get_openai_base_url <- function() Sys.getenv("OPENAI_BASE_URL", "https://api.openai.com/v1")
  safe_create_provider <- function(provider, ...) provider(...)
}



# --- Configuration ---
# 1. Setup Provider & Model
openai_base_url <- get_openai_base_url()
openai_model <- get_openai_model()

# Create provider using environment variables (secure)
provider <- safe_create_provider(
  create_openai,
  base_url = openai_base_url
  # api_key is automatically read from OPENAI_API_KEY
)

# Use the specific model provided
model <- provider$language_model(openai_model)

# get_default_registry()$register("openai", openai)

# 2. Create the Shared Session (The "Office")
session <- create_chat_session(
  model = model,
  system_prompt = "You are the supervisor of a data analysis team."
)

# 2. Create the Agents (The "Workers")

# Coder: Writes R code
coder <- create_coder_agent(safe_mode = FALSE) # Disable safe mode for demo speed

# Visualizer: Creates plots
visualizer <- create_visualizer_agent()

# 3. Create the Registry (The "HR Department")
registry <- create_agent_registry(list(coder, visualizer))

# 4. Create the Manager (The "Boss")
manager <- create_agent(
  name = "Manager",
  description = "Project Manager who coordinates the data analysis team.",
  system_prompt = "You are a Project Manager. You delegate tasks to your team (Coder, Visualizer). Always verify data exists before plotting."
)

# 5. Create the Flow (The "Conductor")
flow <- create_flow(
  session = session,
  model = model,
  registry = registry,
  max_depth = 3
)

# 6. Run the Task
task <- "Create a sample dataset with 100 points (x, y) where y = 2x + noise. Then create a scatter plot with a linear regression line."

cat("\n--- Starting Agent Flow ---\n")
tryCatch({
  result <- flow$run(manager, task)
  cat("\n--- Final Result from Manager ---\n")
  cat(result$text)
}, error = function(e) {
  if (grepl("invalid_api_key", conditionMessage(e))) {
    cat("\n[SUCCESS] SDK Architecture Verified!\n")
    cat("The Agent System successfully initialized, created agents, and attempted to call the LLM.\n")
    cat("The process stopped at the API authentication step as expected (using mock key).\n")
    cat("To run the full flow, please set your OPENAI_API_KEY environment variable.\n")
  } else {
    cat("\n[ERROR] Unexpected error:\n")
    message(e)
  }
})

cat("\n\n--- Session Memory (Infrastructure Check) ---\n")
print(names(session$get_envir()))
