# Create a FileAgent

Creates an agent specialized in file system operations using fs and
readr. The agent can read, write, and manage files with safety
guardrails.

## Usage

``` r
create_file_agent(
  name = "FileAgent",
  allowed_dirs = ".",
  allowed_extensions = c("csv", "tsv", "txt", "json", "rds", "rda", "xlsx", "xls")
)
```

## Arguments

- name:

  Agent name. Default "FileAgent".

- allowed_dirs:

  Character vector of allowed directories. Default current dir.

- allowed_extensions:

  Character vector of allowed file extensions.

## Value

An Agent object configured for file operations.

## Examples

``` r
if (FALSE) { # \dontrun{
file_agent <- create_file_agent(
  allowed_dirs = c("./data", "./output"),
  allowed_extensions = c("csv", "json", "txt", "rds")
)
result <- file_agent$run(
  "Read the sales.csv file and store it as 'sales_data'",
  session = session,
  model = "openai:gpt-4o"
)
} # }
```
