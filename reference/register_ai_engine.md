# Register AI Engine

Registers the `{ai}` engine with knitr. Call this function once before
knitting a document that uses `{ai}` chunks.

## Usage

``` r
register_ai_engine()
```

## Value

Invisible NULL.

## Examples

``` r
if (FALSE) { # \dontrun{
library(aisdk)
register_ai_engine()
# Now you can use ```{ai} chunks in your RMarkdown
} # }
```
