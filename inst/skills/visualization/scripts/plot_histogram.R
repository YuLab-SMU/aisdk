# Plot Histogram
# Create a histogram using ggplot2

library(ggplot2)

# Get arguments
data_name <- args$data
x_col <- args$x
bins <- args$bins %||% 30
plot_title <- args$title %||% sprintf("Distribution of %s", x_col)

# Get data from environment
if (!exists(data_name, envir = .GlobalEnv)) {
  # Try to load as built-in dataset
  data(list = data_name, envir = environment())
  df <- get(data_name)
} else {
  df <- get(data_name, envir = .GlobalEnv)
}

# Validate column
if (!x_col %in% names(df)) {
  stop(sprintf("Column '%s' not found in data", x_col))
}

# Create plot
p <- ggplot(df, aes_string(x = x_col)) +
  geom_histogram(bins = bins, fill = "steelblue", color = "white", alpha = 0.7) +
  theme_minimal() +
  labs(title = plot_title, x = x_col, y = "Count") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )

# Print plot
print(p)

# Return plot object
p
