# Convert ggplot Object to Schema-Compliant Structure

Converts a ggplot object to a JSON-serializable structure with precise
empty value handling and render hints for frontend.

## Usage

``` r
ggplot_to_z_object(plot, include_data = TRUE, include_render_hints = TRUE)
```

## Arguments

- plot:

  A ggplot object.

- include_data:

  Whether to include data in output.

- include_render_hints:

  Whether to include frontend render hints.

## Value

A list structure matching z_ggplot schema.
