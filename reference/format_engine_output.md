# Format Engine Output

Formats the final output for the knitr engine.

## Usage

``` r
format_engine_output(
  options,
  user_prompt,
  response_text,
  code,
  execution_result
)
```

## Arguments

- options:

  Chunk options.

- user_prompt:

  The original user prompt.

- response_text:

  The full LLM response.

- code:

  The extracted R code.

- execution_result:

  The result of code execution.

## Value

Formatted output string.
