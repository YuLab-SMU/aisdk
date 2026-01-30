# Generate trend summary for stock data
generate_trend_summary <- function(stock_data_list) {
  summaries <- list()
  
  for (ticker in names(stock_data_list)) {
    data <- stock_data_list[[ticker]]
    if (is.null(data) || nrow(data) == 0) {
      summaries[[ticker]] <- paste("No data available for", ticker)
      next
    }
    
    # Calculate key metrics
    latest_date <- max(data$Date)
    oldest_date <- min(data$Date)
    period_days <- as.numeric(latest_date - oldest_date)
    
    # Price changes
    first_close <- data$Close[data$Date == oldest_date]
    latest_close <- data$Close[data$Date == latest_date]
    price_change <- latest_close - first_close
    price_change_pct <- (price_change / first_close) * 100
    
    # Recent trend (last 30 days)
    recent_dates <- data$Date >= (latest_date - 30)
    recent_data <- data[recent_dates]
    recent_first_close <- recent_data$Close[1]
    recent_latest_close <- recent_data$Close[nrow(recent_data)]
    recent_change <- recent_latest_close - recent_first_close
    recent_change_pct <- (recent_change / recent_first_close) * 100
    
    # Volatility (standard deviation of daily returns)
    daily_returns <- diff(data$Close) / data$Close[-nrow(data)]
    volatility <- sd(daily_returns, na.rm = TRUE) * sqrt(252) * 100 # Annualized volatility
    
    # Moving averages (50-day and 200-day)
    data$MA50 <- zoo::rollmean(data$Close, k = 50, fill = NA)
    data$MA200 <- zoo::rollmean(data$Close, k = 200, fill = NA)
    latest_ma50 <- tail(data$MA50, 1)
    latest_ma200 <- tail(data$MA200, 1)
    
    # Trend direction
    trend_direction <- if (price_change_pct > 5) "Strong uptrend" else 
                       if (price_change_pct > 0) "Uptrend" else
                       if (price_change_pct < -5) "Strong downtrend" else
                       if (price_change_pct < 0) "Downtrend" else "Sideways"
    
    # Recent trend direction
    recent_trend <- if (recent_change_pct > 2) "Recent strong uptrend" else 
                    if (recent_change_pct > 0) "Recent uptrend" else
                    if (recent_change_pct < -2) "Recent strong downtrend" else
                    if (recent_change_pct < 0) "Recent downtrend" else "Recent sideways"
    
    # Generate summary text
    summary_text <- paste0(
      "**", ticker, " Trend Summary**\n",
      "- Period: ", format(oldest_date, "%Y-%m-%d"), " to ", format(latest_date, "%Y-%m-%d"), " (", period_days, " days)\n",
      "- Price Change: ", round(price_change, 2), " (", round(price_change_pct, 2), "%)\n",
      "- Trend: ", trend_direction, "\n",
      "- Recent (30-day) Change: ", round(recent_change, 2), " (", round(recent_change_pct, 2), "%)\n",
      "- Recent Trend: ", recent_trend, "\n",
      "- Annualized Volatility: ", round(volatility, 2), "%\n",
      "- Latest Price: ", round(latest_close, 2), "\n",
      if (!is.na(latest_ma50)) paste0("- 50-day Moving Average: ", round(latest_ma50, 2), "\n"),
      if (!is.na(latest_ma200)) paste0("- 200-day Moving Average: ", round(latest_ma200, 2), "\n"),
      if (!is.na(latest_ma50) && !is.na(latest_ma200)) {
        if (latest_ma50 > latest_ma200) "- Technical Signal: Golden Cross (bullish)\n" else
        if (latest_ma50 < latest_ma200) "- Technical Signal: Death Cross (bearish)\n" else
        "- Technical Signal: Moving averages aligned\n"
      }
    )
    
    summaries[[ticker]] <- summary_text
  }
  
  # Combine all summaries into a single text
  full_summary <- paste(unlist(summaries), collapse = "\n\n")
  return(full_summary)
}

# Execute the function with the input data
result <- generate_trend_summary(args$stock_data)
print(result)
result
