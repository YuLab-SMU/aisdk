# Plot Scatter
# Create a scatter plot using ggplot2

library(ggplot2)

# Get arguments
data_name <- args$data
x_col <- args$x
y_col <- args$y
color_col <- args$color
plot_title <- args$title %||% sprintf("%s vs %s", y_col, x_col)

# Get data from environment
if (!exists(data_name, envir = .GlobalEnv)) {
  # Try to load as built-in dataset
  data(list = data_name, envir = environment())
  df <- get(data_name)
} else {
  df <- get(data_name, envir = .GlobalEnv)
}

# Validate columns
if (!x_col %in% names(df)) {
  stop(sprintf("Column '%s' not found in data", x_col))
}
if (!y_col %in% names(df)) {
  stop(sprintf("Column '%s' not found in data", y_col))
}

# Create plot
p <- ggplot(df, aes_string(x = x_col, y = y_col))

if (!is.null(color_col) && color_col %in% names(df)) {
  p <- p + aes_string(color = color_col)
}

p <- p +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(title = plot_title, x = x_col, y = y_col) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )

# Print plot
print(p)

# Return plot object
p
