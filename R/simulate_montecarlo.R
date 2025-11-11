# ==========================================
# simulate_montecarlo.R
# Function to conduct montecarlo simulation on selected data
# ==========================================

simulate_montecarlo <- function(prices, days = 252, paths = 1000) {
  if (is.null(prices) || length(prices) < 2) {
    stop("Not enough data to simulate.")
  }
  returns <- diff(log(prices))
  mu <- mean(returns, na.rm = TRUE)
  sigma <- sd(returns, na.rm = TRUE)
  S0 <- tail(prices, 1) # last price from the given vector -> first price in conducted sim
  
  sim <- matrix(rnorm(days * paths, mean = mu, sd = sigma), ncol = paths)
  sim_prices <- S0 * exp(apply(sim, 2, cumsum))
  return(sim_prices)
}
