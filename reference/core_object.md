# Core Object API: Structured Output Generation

Functions for generating structured objects from LLMs using schemas.

Generate a structured R object (list) from a language model based on a
schema. The model is instructed to output valid JSON matching the
schema, which is then parsed and returned as an R list.

## Usage

``` r
generate_object(
  model,
  prompt,
  schema,
  schema_name = "result",
  system = NULL,
  temperature = 0.3,
  max_tokens = NULL,
  mode = c("json", "tool"),
  registry = NULL,
  ...
)
```

## Arguments

- model:

  Either a LanguageModelV1 object, or a string ID like "openai:gpt-4o".

- prompt:

  A character string prompt describing what to generate.

- schema:

  A schema object created by
  [`z_object()`](https://YuLab-SMU.github.io/aisdk/reference/z_object.md),
  [`z_array()`](https://YuLab-SMU.github.io/aisdk/reference/z_array.md),
  etc.

- schema_name:

  Optional human-readable name for the schema (default: "result").

- system:

  Optional system prompt.

- temperature:

  Sampling temperature (0-2). Default 0.3 (lower for structured output).

- max_tokens:

  Maximum tokens to generate.

- mode:

  Output mode: "json" (prompt-based) or "tool" (function calling).
  Currently, only "json" mode is implemented.

- registry:

  Optional ProviderRegistry to use (defaults to global registry).

- ...:

  Additional arguments passed to the model.

## Value

A GenerateObjectResult with:

- object: The parsed R object (list)

- usage: Token usage information

- raw_text: The raw text output from the LLM

- finish_reason: The reason the generation stopped

## Examples

``` r
if (FALSE) { # \dontrun{
# Define a schema for the expected output
schema <- z_object(
  title = z_string(description = "Title of the article"),
  keywords = z_array(z_string()),
  sentiment = z_enum(c("positive", "negative", "neutral"))
)

# Generate structured object
result <- generate_object(
  model = "openai:gpt-4o",
  prompt = "Analyze this article: 'R programming is great for data science!'",
  schema = schema
)

print(result$object$title)
print(result$object$sentiment)
} # }
```
