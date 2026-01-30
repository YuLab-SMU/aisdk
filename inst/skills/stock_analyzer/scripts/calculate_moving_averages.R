# Calculate Simple and Exponential Moving Averages

data <- args$data
short_window <- args$short_window %||% 20
long_window <- args$long_window %||% 50

# Ensure data is ordered by date
data <- data[order(data$Date)]

# Calculate Simple Moving Averages
data$SMA_Short <- zoo::rollmean(data$Close, k = short_window, fill = NA, align = "right")
data$SMA_Long <- zoo::rollmean(data$Close, k = long_window, fill = NA, align = "right")

# Calculate Exponential Moving Averages
data$EMA_Short <- TTR::EMA(data$Close, n = short_window)
data$EMA_Long <- TTR::EMA(data$Close, n = long_window)

# Return updated data frame
result <- data
