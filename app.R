library(shiny)
library(dplyr)
library(ggplot2)

# --- Loading Functions  ---

source("R/simulate_montecarlo.R")
source("R/get_stock_data.R")

getData <- function(symbol) {
  df <- get_stock_data(symbol)
  if (is.null(df)) {
    stop("Data download failed for: ", symbol)
  }
  return(df)
}

# ---------------- UI ----------------
ui <- fluidPage(
  includeCSS("www/style.css"),
  titlePanel("Stock MonteCarlo Sim Dashboard"),
  
  navbarPage("",
             tabPanel("Monte Carlo Simulation",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("stock", "Choose Stock:",
                                      choices = c("Google" = "GOOG",
                                                  "Apple" = "AAPL",
                                                  "Microsoft" = "MSFT",
                                                  "Amazon" = "AMZN",
                                                  "Tesla" = "TSLA",
                                                  "NVIDIA" = "NVDA",
                                                  "Meta" = "META",
                                                  "Intel" = "INTC",
                                                  "Netflix" = "NFLX",
                                                  "Coca-Cola" = "KO"),
                                      selected = "GOOG"),
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
