---
name: stock_analyzer
description: Analyze stock market data including price trends, volatility, moving averages, and technical indicators
---

## Stock Analyzer Skill

This skill provides stock market analysis capabilities. Use the scripts in the `scripts/` folder to perform various analyses.

### Available Scripts

1. **fetch_stock_data.R** - Retrieve historical stock price data
   - Args: `symbol` (stock ticker, e.g., "AAPL"), `period` (e.g., "1y", "6mo")
   - Returns: Data frame with Date, Open, High, Low, Close, Volume

2. **calculate_moving_averages.R** - Calculate SMA and EMA
   - Args: `data` (stock data frame), `short_window` (default: 20), `long_window` (default: 50)
   - Returns: Data frame with added MA columns

3. **analyze_volatility.R** - Calculate volatility metrics
   - Args: `data` (stock data frame), `window` (default: 20)
   - Returns: List with daily returns, rolling volatility, and statistics

4. **plot_stock_chart.R** - Generate price charts with indicators
   - Args: `data` (stock data frame), `title` (chart title)
   - Returns: Plot object

### Usage Example
```
# 1. Fetch data
data <- fetch_stock_data(symbol = "AAPL", period = "1y")

# 2. Add moving averages
data <- calculate_moving_averages(data, short_window = 20, long_window = 50)

# 3. Analyze volatility
vol_analysis <- analyze_volatility(data)

# 4. Create chart
chart <- plot_stock_chart(data, title = "AAPL Stock Analysis")
```
