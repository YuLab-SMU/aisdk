# Create stock price chart with technical indicators

library(ggplot2)

data <- args$data
title <- args$title %||% "Stock Price Chart"

# Ensure data is ordered by date
data <- data[order(data$Date)]

# Create the base plot
p <- ggplot(data, aes(x = Date, y = Close)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(title = title, x = "Date", y = "Price ($)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9)
  )

# Add moving averages if present
if ("SMA_Short" %in% colnames(data)) {
  p <- p + geom_line(aes(y = SMA_Short), color = "orange", linewidth = 0.6, linetype = "dashed")
}
if ("SMA_Long" %in% colnames(data)) {
  p <- p + geom_line(aes(y = SMA_Long), color = "red", linewidth = 0.6, linetype = "dotted")
}

# Add volume bars at bottom if present
if ("Volume" %in% colnames(data)) {
  # Create volume subplot
  volume_plot <- ggplot(data, aes(x = Date, y = Volume)) +
    geom_col(fill = "gray70", alpha = 0.5) +
    labs(x = "", y = "Volume") +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
  # Combine plots using patchwork or return both
  result <- list(
    price_chart = p,
    volume_chart = volume_plot,
    combined = patchwork::wrap_plots(p, volume_plot, ncol = 1, heights = c(3, 1))
  )
} else {
  result <- list(price_chart = p)
}
