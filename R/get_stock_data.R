# ==========================================
# get_stock_data.R
# Function to download stock price data from Yahoo Finance
# ==========================================

# Dependencies
if (!requireNamespace("quantmod", quietly = TRUE)) {
  install.packages("quantmod")
}
library(quantmod)

# Function to download historical data
get_stock_data <- function(symbol, from = "2018-01-01", to = Sys.Date()) {
  tryCatch({
    data_env <- new.env()
    quantmod::getSymbols(Symbols = symbol, src = "yahoo",
                         from = from, to = to, env = data_env, auto.assign = TRUE)
    df_raw <- data_env[[symbol]]
    df <- data.frame(Date = index(df_raw),
                     coredata(df_raw))
    df <- na.omit(df)
    # Rename columns to generic form
    colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    return(df)
  },
  error = function(e) {
    message(paste("Error downloading", symbol, ":", e$message))
    return(NULL)
  })
}

