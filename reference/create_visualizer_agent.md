# Create a VisualizerAgent

Creates an agent specialized in creating data visualizations using
ggplot2. Enhanced version with plot type recommendations, theme support,
and automatic data inspection.

## Usage

``` r
create_visualizer_agent(
  name = "VisualizerAgent",
  output_dir = NULL,
  default_theme = "theme_minimal",
  default_width = 8,
  default_height = 6
)
```

## Arguments

- name:

  Agent name. Default "VisualizerAgent".

- output_dir:

  Optional directory to save plots. If NULL, plots are stored in the
  session environment.

- default_theme:

  Default ggplot2 theme. Default "theme_minimal".

- default_width:

  Default plot width in inches. Default 8.

- default_height:

  Default plot height in inches. Default 6.

## Value

An Agent object configured for data visualization.

## Examples

``` r
if (FALSE) { # \dontrun{
visualizer <- create_visualizer_agent()
session <- create_shared_session(model = "openai:gpt-4o")
session$set_var("df", data.frame(x = 1:10, y = (1:10)^2))
result <- visualizer$run(
  "Create a scatter plot of df showing the relationship between x and y",
  session = session,
  model = "openai:gpt-4o"
)
} # }
```
