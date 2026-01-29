# Create Dataframe Schema

Create a schema that represents a dataframe (or list of row objects).
This is an R-specific convenience function that generates a JSON Schema
for an array of objects. The LLM will be instructed to output data in a
format that can be easily converted to an R dataframe using
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
or `do.call(rbind, lapply(..., as.data.frame))`.

Create a schema that represents a dataframe (or list of row objects).
This is an R-specific convenience function that generates a JSON Schema
for an array of objects.

## Usage

``` r
z_dataframe(
  ...,
  .description = NULL,
  .nullable = FALSE,
  .default = NULL,
  .min_rows = NULL,
  .max_rows = NULL
)
```

## Arguments

- ...:

  Named arguments where names are column names and values are z_schema
  objects representing the column types.

- .description:

  Optional description of the dataframe.

- .nullable:

  If TRUE, allows null values.

- .default:

  Optional default value.

- .min_rows:

  Optional minimum number of rows.

- .max_rows:

  Optional maximum number of rows.

## Value

A z_schema object representing an array of objects.

A z_schema object representing an array of objects.

## Examples

``` r
# Define a schema for a dataframe of genes
gene_schema <- z_dataframe(
  gene_name = z_string(description = "Name of the gene"),
  expression = z_number(description = "Expression level"),
  significant = z_boolean(description = "Is statistically significant")
)

# Use with generate_object
# result <- generate_object(model, "Extract gene data...", gene_schema)
# df <- dplyr::bind_rows(result$object)
```
