# Structured Outputs

AI models often output unstructured text.
[`generate_object()`](https://YuLab-SMU.github.io/aisdk/reference/core_object.md)
forces the model to return data that matches a specific schema, making
it safe for programmatic use.

## Basic Object Extraction

Use `z_object` to define the shape you want.

``` r
library(aisdk)

# Define schema
article_schema <- z_object(
  title = z_string(description = "The main title"),
  sentiment = z_enum(c("positive", "negative", "neutral")),
  keywords = z_array(z_string(), max_items = 5)
)

# Generate object
result <- generate_object(
  model = "openai:gpt-4o",
  prompt = "R is an amazing language for data science!",
  schema = article_schema
)

# Access typed results
print(result$object$title)
print(result$object$sentiment)
```

## Extracting Data Frames

**aisdk** includes a special
[`z_dataframe()`](https://YuLab-SMU.github.io/aisdk/reference/z_dataframe.md)
helper that requests a list of objects, which is perfect for extracting
tables.

``` r
# Define table schema
gene_schema <- z_dataframe(
  gene_name = z_string(description = "Name of the gene"),
  expression_level = z_number(description = "Expression value"),
  significant = z_boolean(description = "p < 0.05")
)

result <- generate_object(
  model = "openai:gpt-4o",
  prompt = "Extract data: gene BRCA1 (2.5), TP53 (-1.8), MYC (0.3)",
  schema = gene_schema
)

# Convert to R data frame
library(dplyr)
df <- bind_rows(result$object)
print(df)
```

## Nested Structures

Schemas can be arbitrarily deep.

``` r
analysis_schema <- z_object(
  experiment = z_object(
    name = z_string(),
    date = z_string()
  ),
  results = z_array(
    z_object(
      metric = z_string(),
      value = z_number()
    )
  )
)
```
