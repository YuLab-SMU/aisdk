# Analyze stock price volatility

data <- args$data
window <- args$window %||% 20

# Ensure data is ordered by date
data <- data[order(data$Date)]

# Calculate daily returns
data$Daily_Return <- c(NA, diff(log(data$Close)))

# Calculate rolling volatility (standard deviation of returns)
data$Rolling_Volatility <- zoo::rollapply(data$Daily_Return, width = window, FUN = sd, fill = NA, align = "right")

# Annualized volatility (assuming 252 trading days)
annualized_vol <- sd(data$Daily_Return, na.rm = TRUE) * sqrt(252)

# Calculate Value at Risk (95% confidence)
var_95 <- quantile(data$Daily_Return, 0.05, na.rm = TRUE)

# Calculate maximum drawdown
wealth_index <- cumprod(1 + data$Daily_Return[-1])
previous_peaks <- cummax(wealth_index)
drawdowns <- (wealth_index - previous_peaks) / previous_peaks
max_drawdown <- min(drawdowns)

# Return volatility analysis results
result <- list(
  data = data,
  annualized_volatility = annualized_vol,
  var_95 = var_95,
  max_drawdown = max_drawdown,
  avg_daily_return = mean(data$Daily_Return, na.rm = TRUE),
  return_summary = summary(data$Daily_Return)
)
