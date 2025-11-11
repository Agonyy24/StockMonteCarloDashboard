# ==========================================
# simulate_montecarlo_t.R
# Function to conduct montecarlo simulation with use of t distribution
# The goal is to simulate more volatile market
# ==========================================

simulate_montecarlo_t <- function(prices, days = 252, paths = 1000, df = 5) {
  if (is.null(prices) || length(prices) < 2) {
    stop("Not enough data to simulate.")
  }
  
  returns <- diff(log(prices))
  mu <- mean(returns, na.rm = TRUE)
  sigma <- sd(returns, na.rm = TRUE)
  S0 <- tail(prices, 1)
  
  # rt
  sim <- matrix(rt(days * paths, df = df), ncol = paths)
  
  # Normalization
  sim <- mu + sigma * sim / sqrt(df / (df - 2))
  
  sim_prices <- S0 * exp(apply(sim, 2, cumsum))
  return(sim_prices)
}