# Project Memory System

Long-term memory storage for AI agents using SQLite. Stores successful
code snippets, error fixes, and execution history for RAG (Retrieval
Augmented Generation) and learning from past interactions.

Factory function to create or connect to a project memory database.

## Usage

``` r
project_memory(project_root = getwd(), db_name = "memory.sqlite")
```

## Arguments

- project_root:

  Project root directory.

- db_name:

  Database filename.

## Value

A ProjectMemory object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create memory for current project
memory <- project_memory()

# Store a successful code snippet
memory$store_snippet(
  code = "df %>% filter(x > 0) %>% summarize(mean = mean(y))",
  description = "Filter and summarize data",
  tags = c("dplyr", "summarize")
)

# Store an error fix
memory$store_fix(
  original_code = "mean(df$x)",
  error = "argument is not numeric or logical",
  fixed_code = "mean(as.numeric(df$x), na.rm = TRUE)",
  fix_description = "Convert to numeric and handle NAs"
)

# Search for relevant snippets
memory$search_snippets("summarize")
} # }
```
