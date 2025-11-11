# ==========================================
# get_polish_data.R
# Function to download stock price data from stooq.pl
# ==========================================

get_polish_data <- function(symbol, start_date = Sys.Date() - 365*3) {
  
  # --- Building required URL ---
  d1 <- format(as.Date(start_date), "%Y%m%d") # Changing format from 2000-01-01 to 20000101 
  d2 <- format(as.Date(Sys.Date()), "%Y%m%d")
  url <- paste0("https://stooq.pl/q/d/l/?s=", symbol,
                "&d1=", d1,
                "&d2=", d2,
                "&i=d")
  
  dane <- read.csv(url, header = TRUE, sep = ",", dec = ".")
  dane <- na.omit(dane)
  
  return(dane)
}
