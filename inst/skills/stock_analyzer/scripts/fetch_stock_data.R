# Install and load required package
if (!require('quantmod')) {
  install.packages('quantmod', dependencies = TRUE)
  library(quantmod)
}

# Fetch stock data for given symbols
symbols <- args$tickers
period <- if (!is.null(args$period)) args$period else '1y'

stock_data <- list()
for (sym in symbols) {
  tryCatch({
    # Calculate date range based on period
    if (grepl('y$', period)) {
      years <- as.numeric(sub('y', '', period))
      start_date <- Sys.Date() - years * 365
    } else if (grepl('mo$', period)) {
      months <- as.numeric(sub('mo', '', period))
      start_date <- Sys.Date() - months * 30
    } else {
      start_date <- Sys.Date() - 365 # Default to 1 year
    }
    
    data <- getSymbols(sym, src = 'yahoo', from = start_date, to = Sys.Date(), auto.assign = FALSE)
    data_df <- as.data.frame(data)
    # Rename columns to remove ticker prefix
    colnames(data_df) <- gsub(paste0(sym, "\\."), '', colnames(data_df))
    data_df$Date <- index(data)
    data_df$Ticker <- sym
    stock_data[[sym]] <- data_df
  }, error = function(e) {
    message(paste('Error fetching data for', sym, ':', e$message))
  })
}

# Return the list of data frames
stock_data
