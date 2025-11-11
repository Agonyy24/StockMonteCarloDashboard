# ==========================================
# get_stock_data.R
# Function to download stock price data from Yahoo Finance
# ==========================================

library(quantmod)

get_stock_data <- function(symbol, start_date = Sys.Date() - 365*3) {
  
  df_xts <- quantmod::getSymbols(
    Symbols = symbol,
    src = "yahoo",
    from = start_date,
    to = Sys.Date(),
    auto.assign = FALSE # If FALSE returns data directly instead of assigning to environment
  )
  
  df <- data.frame(Date = index(df_xts), coredata(df_xts))

  df <- na.omit(df)
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  
  return(df)
}