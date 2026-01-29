# Autonomous Data Science Pipelines

Self-healing runtime for R code execution. Implements a
"Hypothesis-Fix-Verify" loop that feeds error messages, stack traces,
and context back to an LLM for automatic error correction.

Execute R code with automatic error recovery using LLM assistance. When
code fails, the error is analyzed and a fix is attempted automatically.

## Usage

``` r
auto_fix(
  expr,
  model = getOption("aisdk.default_model", "openai:gpt-4o"),
  max_attempts = 3,
  context = NULL,
  verbose = TRUE,
  memory = NULL
)
```

## Arguments

- expr:

  The R expression to execute.

- model:

  The LLM model to use for error analysis (default: from options).

- max_attempts:

  Maximum number of fix attempts (default: 3).

- context:

  Optional additional context about the code's purpose.

- verbose:

  Print progress messages (default: TRUE).

- memory:

  Optional ProjectMemory object for learning from past fixes.

## Value

The result of successful execution, or an error if all attempts fail.

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple usage - auto-fix a data transformation
result <- auto_fix({
  df <- read.csv("data.csv")
  df %>% filter(value > 100) %>% summarize(mean = mean(value))
})

# With context for better error understanding
result <- auto_fix(
  expr = { model <- lm(y ~ x, data = df) },
  context = "Fitting a linear regression model to predict sales"
)
} # }
```
