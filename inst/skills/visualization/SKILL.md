---
name: visualization
description: Create beautiful data visualizations using ggplot2
agent:
  role: Visualizer
  persona: |
    You are a data visualization expert specializing in ggplot2.
    You create clear, informative, and aesthetically pleasing visualizations.
    You understand which plot types best communicate different types of data relationships.
  capabilities:
    - Scatter plots
    - Bar charts
    - Line graphs
    - Histograms
    - Box plots
    - Custom themes
---

# Visualization Skill

This skill provides data visualization capabilities using ggplot2.

## Available Scripts

### plot_scatter.R
Create scatter plots to show relationships between two numeric variables.

**Arguments:**
- `data`: Data frame name (as string)
- `x`: Column name for x-axis
- `y`: Column name for y-axis
- `color`: Optional column for color grouping
- `title`: Optional plot title

**Example:**
```r
execute_skill_script(
  skill_name = "visualization",
  script_name = "plot_scatter.R",
  args = list(
    data = "iris",
    x = "Sepal.Length",
    y = "Sepal.Width",
    color = "Species",
    title = "Iris Sepal Dimensions"
  )
)
```

### plot_histogram.R
Create histograms to show distribution of a numeric variable.

**Arguments:**
- `data`: Data frame name (as string)
- `x`: Column name for the variable
- `bins`: Number of bins (default: 30)
- `title`: Optional plot title

## Usage Tips

- Always inspect your data first to understand its structure
- Choose plot types that match your data and question
- Use color strategically to highlight patterns
- Include clear titles and axis labels
