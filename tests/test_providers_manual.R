# Manual testing script for various providers
if (identical(Sys.getenv("NOT_CRAN"), "true") || !interactive()) {
    # Skip during R CMD check or non-interactive sessions
    # unless specifically requested
    if (Sys.getenv("FORCE_MANUAL_TESTS") != "TRUE") {
        message("Skipping manual provider tests (non-interactive or R CMD check)")
        if (!interactive()) q("no") else return(NULL)
    }
}
library(devtools)
load_all(".")

# Load .env variables
if (file.exists(".env")) {
    readRenviron(".env")
}

test_model <- function(provider_name, model_id, provider_factory) {
    cli::cli_h2(paste("Testing Provider:", provider_name))
    cli::cli_alert_info(paste("Model ID:", model_id))

    tryCatch(
        {
            provider <- provider_factory()
            model <- provider$language_model(model_id)

            cli::cli_alert_info("Running generate_text...")
            res <- generate_text(model, "Say 'Hello' and identify yourself briefly.")
            cli::cli_alert_success("Response received:")
            cat(res$text, "\n")

            cli::cli_alert_info("Testing ChatSession...")
            session <- ChatSession$new(model = model)
            res_sess <- session$send("What is 2+2?")
            cli::cli_alert_success(paste("Session Response:", res_sess$text))

            return(TRUE)
        },
        error = function(e) {
            msg <- conditionMessage(e)
            cli::cli_alert_danger("Error testing {provider_name}:")
            cat("  ", msg, "\n")
            return(FALSE)
        }
    )
}

# Define tests
tests <- list(
    list(name = "Anthropic", id = Sys.getenv("ANTHROPIC_MODEL"), factory = create_anthropic),
    list(name = "OpenAI", id = Sys.getenv("OPENAI_MODEL"), factory = create_openai),
    list(name = "DeepSeek", id = Sys.getenv("DEEPSEEK_MODEL"), factory = create_deepseek),
    list(name = "Stepfun", id = Sys.getenv("STEPFUN_MODEL"), factory = create_stepfun),
    list(name = "Volcengine", id = Sys.getenv("ARK_MODEL"), factory = create_volcengine),
    list(name = "Gemini", id = Sys.getenv("GEMINI_MODEL"), factory = create_gemini),
    list(name = "NVIDIA", id = Sys.getenv("NVIDIA_MODEL"), factory = create_nvidia),
    list(name = "XAI", id = Sys.getenv("XAI_MODEL"), factory = create_xai),
    list(name = "OpenRouter", id = Sys.getenv("OPENROUTER_MODEL"), factory = create_openrouter)
)

# Run tests
results <- list()
for (t in tests) {
    if (nchar(t$id) > 0) {
        results[[t$name]] <- test_model(t$name, t$id, t$factory)
    } else {
        cli::cli_alert_warning(paste("Skipping", t$name, "- Model ID not set in .env"))
    }
}

cli::cli_h1("Test Summary")
for (name in names(results)) {
    status <- if (results[[name]]) "PASS" else "FAIL"
    cli::cli_text(paste0(name, ": ", if (results[[name]]) cli::col_green(status) else cli::col_red(status)))
}
