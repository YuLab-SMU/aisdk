# Expect LLM Pass

Custom testthat expectation that evaluates whether an LLM response meets
specified criteria. Uses an LLM judge to assess the response.

## Usage

``` r
expect_llm_pass(response, criteria, model = NULL, threshold = 0.7, info = NULL)
```

## Arguments

- response:

  The LLM response to evaluate (text or GenerateResult object).

- criteria:

  Character string describing what constitutes a passing response.

- model:

  Model to use for judging (default: same as response or gpt-4o).

- threshold:

  Minimum score (0-1) to pass (default: 0.7).

- info:

  Additional information to include in failure message.

## Value

Invisibly returns the evaluation result.

## Examples

``` r
if (FALSE) { # \dontrun{
test_that("agent answers math questions correctly", {
  result <- generate_text(
    model = "openai:gpt-4o",
    prompt = "What is 2 + 2?"
  )
  expect_llm_pass(result, "The response should contain the number 4")
})
} # }
```
