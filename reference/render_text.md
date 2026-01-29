# Render Markdown Text

Render markdown-formatted text in the console with beautiful styling.
This function uses the same rendering engine as the streaming output,
supporting headers, lists, code blocks, and other markdown elements.

## Usage

``` r
render_text(text)
```

## Arguments

- text:

  A character string containing markdown text, or a GenerateResult
  object.

## Value

NULL (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
# Render simple text
render_text("# Hello\n\nThis is **bold** text.")

# Render with code block
render_text("Here is some R code:\n\n```r\nx <- 1:10\nmean(x)\n```")
} # }
```
