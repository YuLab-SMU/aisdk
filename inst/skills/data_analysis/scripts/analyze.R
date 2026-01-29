# Analyze Dataset
# Uses args$dataset (string) and args$top_n (integer, optional)

dataset_name <- args$dataset
if (is.null(dataset_name) || !nzchar(dataset_name)) {
  dataset_name <- "iris"
}

top_n <- args$top_n
if (is.null(top_n)) {
  top_n <- 5
}

# Load the dataset
data <- get(dataset_name)

# Create summary
result <- list(
  dataset = dataset_name,
  dimensions = paste0(nrow(data), " rows x ", ncol(data), " cols"),
  columns = paste(names(data), collapse = ", ")
)

# Add species summary for iris
if (dataset_name == "iris" && "Species" %in% names(data)) {
  species_tbl <- table(data$Species)
  result$species_counts <- paste(names(species_tbl), species_tbl, sep = ": ", collapse = ", ")
}

# Add summary statistics for numeric columns
numeric_cols <- sapply(data, is.numeric)
if (any(numeric_cols)) {
  stats <- sapply(names(data)[numeric_cols], function(col_name) {
    col <- data[[col_name]]
    sprintf("%s: min=%.2f, max=%.2f, mean=%.2f", 
            col_name, min(col), max(col), mean(col))
  })
  result$numeric_summary <- paste(stats, collapse = "\n")
}

# Show head
result$head <- paste(capture.output(print(head(data, top_n))), collapse = "\n")

# Return as formatted string
paste(
  sprintf("Dataset: %s", result$dataset),
  sprintf("Dimensions: %s", result$dimensions),
  sprintf("Columns: %s", result$columns),
  if (!is.null(result$species_counts)) sprintf("Species: %s", result$species_counts) else NULL,
  sprintf("\nNumeric Summary:\n%s", result$numeric_summary),
  sprintf("\nFirst %d rows:\n%s", top_n, result$head),
  sep = "\n"
)
