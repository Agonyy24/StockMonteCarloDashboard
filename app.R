library(shiny)
library(dplyr)
library(ggplot2)

# --- Load data helper ---
getData <- function(symbol) {
  file_path <- paste0("data/", symbol, "_d0101201819122019.csv")
  df <- read.csv(file_path, sep = ",", dec = ".")
  df$Date <- as.Date(df$Date)
  return(df)
}

# --- Monte Carlo Simulation ---
simulateMonteCarlo <- function(prices, days = 252, paths = 1000) {
  returns <- diff(log(prices))
  mu <- mean(returns, na.rm = TRUE)
  sigma <- sd(returns, na.rm = TRUE)
  S0 <- tail(prices, 1)
  
  sim <- matrix(rnorm(days * paths, mean = mu, sd = sigma), ncol = paths)
  sim_prices <- S0 * exp(apply(sim, 2, cumsum))
  return(sim_prices)
}

# ---------------- UI ----------------
ui <- fluidPage(
  includeCSS("www/style.css"),
  titlePanel("📊 Stock Risk Dashboard"),
  
  navbarPage("",
             tabPanel("Monte Carlo Simulation",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("stock", "Choose Stock:", 
                                      choices = c("Google" = "goog", "Apple" = "aapl")),
                          numericInput("days", "Days to Simulate:", 252, min = 50, max = 1000),
                          numericInput("paths", "Number of Simulations:", 1000, min = 100, max = 5000),
                          actionButton("run", "Run Simulation")
                        ),
                        mainPanel(
                          h3(textOutput("title")),
                          plotOutput("mcPlot", height = "450px"),
                          plotOutput("histPlot", height = "300px"),
                          verbatimTextOutput("riskMetrics")
                        )
                      )
             )
  )
)

# ---------------- SERVER ----------------
server <- function(input, output) {
  
  observeEvent(input$run, {
    data <- getData(input$stock)
    sim <- simulateMonteCarlo(data$Close, input$days, input$paths)
    final_prices <- sim[nrow(sim), ]
    returns <- final_prices / tail(data$Close, 1) - 1
    
    # Compute risk metrics
    VaR_95 <- quantile(returns, 0.05)
    ES_95 <- mean(returns[returns <= VaR_95])
    
    output$title <- renderText({
      paste("Monte Carlo Simulation for", toupper(input$stock))
    })
    
    output$mcPlot <- renderPlot({
      matplot(sim, type = "l", lty = 1, col = rgb(0, 0, 1, 0.1),
              main = "Simulated Price Paths", xlab = "Days", ylab = "Price")
      lines(apply(sim, 1, mean), col = "red", lwd = 2)
    })

    output$histPlot <- renderPlot({
      hist(final_prices, breaks = 40, col = "lightblue", border = "white",
           main = "Distribution of Final Prices", xlab = "Simulated Final Price")
      abline(v = mean(final_prices), col = "red", lwd = 2)
    })
    
    output$riskMetrics <- renderPrint({
      cat("Value at Risk (95%):", round(VaR_95 * 100, 2), "%\n")
      cat("Expected Shortfall (95%):", round(ES_95 * 100, 2), "%\n")
      cat("Mean Expected Return:", round(mean(returns) * 100, 2), "%")
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
