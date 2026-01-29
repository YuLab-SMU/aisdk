# Export ggplot as Frontend-Ready JSON

Exports a ggplot object as JSON optimized for frontend rendering.
Addresses all frontend feedback:

- Strict scalar typing (no for missing values)

- Structured units with pre-calculated pixel values

- Stable IDs for React keys

- Consistent Array of Structures pattern

## Usage

``` r
ggplot_to_frontend_json(
  plot,
  width = 800,
  height = 600,
  include_data = TRUE,
  include_built = FALSE,
  pretty = FALSE
)
```

## Arguments

- plot:

  A ggplot object.

- width:

  Plot width in pixels.

- height:

  Plot height in pixels.

- include_data:

  Whether to include data.

- include_built:

  Whether to include ggplot_build() output.

- pretty:

  Format JSON with indentation.

## Value

JSON string optimized for frontend.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
json <- ggplot_to_frontend_json(p, width = 800, height = 600)
} # }
```
