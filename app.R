library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)

# --- Loading Functions  ---

source("R/simulate_montecarlo.R")
source("R/simulate_montecarlo_t.R")
source("R/get_stock_data.R")
source("R/get_polish_data.R")

# ---------------- UI ----------------
ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "darkly",
    base_font = "Roboto"
  ),
  
  navbarPage(
    "📊 Monte Carlo Dashboard",
    
    # --- Tab 1: US Monte Carlo Simulation ---
    tabPanel("US Market Simulation",
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
                                         "Palantir" = "PLTR",
                                         "Nike" = "NKE"),
                             selected = "GOOG"),
                 
                 # Choosing starting point
                 dateInput(
                   "start_date",
                   label = "Select Start Date:",
                   value = Sys.Date() - 365 * 3,   # default: 3 years
                   min = as.Date("2010-01-01"),
                   max = Sys.Date() - 1
                 ),
                 
                 # Days to simulate slider
                 sliderInput(
                   "days", "Days to Simulate:",
                   min = 50, max = 725, value = 252, step = 15
                 ),
                 
                 # Number of sims slider
                 sliderInput(
                   "paths", "Number of Simulations:",
                   min = 100, max = 4600, value = 1000, step = 100
                 ),
                 
                 actionButton("run", "Run Simulation", class = "btn-primary"),
                 br(), br(),
                 
                 # --- Results section ---
                 h4("📉 Calculated Risk Metrics:"),
                 tags$div(
                   style = "background-color:#222; padding:10px; border-radius:8px; 
             border:1px solid #444; color:#f8f9fa; font-family: monospace;",
                   verbatimTextOutput("riskMetrics")
                 )
               ),
               
               mainPanel(
                 h3(textOutput("title")),
                 plotOutput("mcPlot", height = "450px"),
                 plotOutput("histPlot", height = "300px")
               )
             )
    ),
    
    # --- Tab 2: Polish Market Simulation ---
    tabPanel("Polish Market Simulation",
             sidebarLayout(
               sidebarPanel(
                 selectInput("pl_symbol", "Choose Stock from WIG20:",
                             choices = c("PKN Orlen" = "pkn",
                                         "PKO BP" = "pko",
                                         "PZU" = "pzu",
                                         "KGHM" = "kgh",
                                         "Allegro" = "ale",
                                         "CD Projekt" = "cdr",
                                         "LPP" = "lpp",
                                         "JSW" = "jsw",
                                         "mBank" = "mbk",
                                         "Santander Bank Polska" = "spl",
                                         "PGE" = "pge",
                                         "Tauron" = "tau",
                                         "Cyfrowy Polsat" = "cps",
                                         "Orange Polska" = "opl",
                                         "Dino Polska" = "dnp",
                                         "Kruk" = "krk",
                                         "Pepco Group" = "ppg",
                                         "Asseco Poland" = "acp",
                                         "Alior Bank" = "alr",
                                         "WIG20 Index" = "wig20"),
                             selected = "wig20"),
                 
                 # Choosing data starting point
                 dateInput(
                   "pl_start",
                   label = "Select Start Date:",
                   value = Sys.Date() - 365 * 3, # default: 3 years
                   min = as.Date("2010-01-01"),
                   max = Sys.Date() - 1
                 ),
                 
                 # Days to sim
                 sliderInput(
                   "pl_days", "Days to Simulate:",
                   min = 50, max = 725, value = 252, step = 15
                 ),
                 
                 sliderInput(
                   "pl_paths", "Number of Simulations:",
                   min = 100, max = 4600, value = 1000, step = 100
                 ),
                 
                 actionButton("pl_run", "Run Simulation", class = "btn-primary"),
                 br(), br(),
                 
                 h4("📉 Calculated Risk Metrics:"),
                 tags$div(
                   style = "background-color:#222; padding:10px; border-radius:8px; 
                        border:1px solid #444; color:#f8f9fa; font-family: monospace;",
                   verbatimTextOutput("pl_riskMetrics")
                 )
               ),
               
               mainPanel(
                 h3(textOutput("pl_title")),
                 plotOutput("pl_mcPlot", height = "450px"),
                 plotOutput("pl_histPlot", height = "300px")
               )
             )
    ),
    
    # --- Tab 3: VaR & ES Explanation ---
    tabPanel("Understanding VaR & ES",
             
             h3("Understanding Value at Risk (VaR) and Expected Shortfall (ES)"),
             br(),
             
             h4("🔹 What is Value at Risk (VaR)?"),
             p("Value at Risk (VaR) is a statistical measure that estimates how much a portfolio 
            could lose over a specific period of time with a given confidence level. 
            For example, a 95% VaR of -10% means there is only a 5% chance that 
            losses will exceed 10% during the next period."),
             
             tags$blockquote(
               "In other words: VaR answers the question — 
            'How bad can things get, with 95% confidence?'"
             ),
             
             h4("🔹 What is Expected Shortfall (ES)?"),
             p("Expected Shortfall (also known as Conditional VaR) goes one step further. 
            It measures the average loss that would occur if the loss does exceed the VaR threshold. 
            This gives a better picture of tail risk — 
            the severity of the worst-case scenarios."),
             
             tags$blockquote(
               "In short: ES tells us how bad the 'really bad days' can be."
             ),
             
             h4("🔹 How This Simulation Works"),
             p("In our Monte Carlo simulation, we generate thousands of random price paths 
            based on the historical volatility and average return of a stock. 
            From these simulated outcomes, we calculate potential future prices 
            and use them to estimate VaR and ES values. 
            The histogram in the main tab shows the range of possible future prices — 
            with VaR and ES marked on the left tail of the distribution."),
             
             tags$div(
               style = "margin-top:20px; padding:15px; border-left:4px solid #3498db; background-color:#1a1a1a; border-radius:8px;",
               HTML("
                <b>Example:</b><br>
                • 95% VaR = -18% → There is only a 5% chance that losses exceed 18%.<br>
                • 95% ES = -24% → On average, the worst 5% of cases lose about 24%.<br><br>
                VaR gives the threshold, while ES gives the expected loss beyond that threshold.
                ")
             ),
             
             br(),
             p(em("Both VaR and ES are commonly used in quantitative finance and risk management 
               to understand the potential downside of investments."))
    ),
    
    # --- Tab 4: Student-t Monte Carlo ---
    tabPanel("US Volatile Market Simulation",
             sidebarLayout(
               sidebarPanel(
                 selectInput("stock_t", "Choose Stock:",
                             choices = c("Google" = "GOOG",
                                         "Apple" = "AAPL",
                                         "Microsoft" = "MSFT",
                                         "Amazon" = "AMZN",
                                         "Tesla" = "TSLA",
                                         "NVIDIA" = "NVDA",
                                         "Meta" = "META",
                                         "Intel" = "INTC",
                                         "Palantir" = "PLTR",
                                         "Nike" = "NKE"),
                             selected = "GOOG"),
                 
                 # Choosing starting point
                 dateInput(
                   "start_date_t",
                   label = "Select Start Date:",
                   value = Sys.Date() - 365 * 3,   # default: 3 years
                   min = as.Date("2010-01-01"),
                   max = Sys.Date() - 1
                 ),
                 
                 # Days to simulate slider
                 sliderInput(
                   "days_t", "Days to Simulate:",
                   min = 50, max = 725, value = 252, step = 15
                 ),
                 
                 # Degrees of freedom slider
                 sliderInput(
                   "df", "Degrees of Freedom:",
                   min = 2, max = 100, value = 5, step = 1
                 ),
                 
                 # Number of sims slider
                 sliderInput(
                   "paths_t", "Number of Simulations:",
                   min = 100, max = 4600, value = 1000, step = 100
                 ),
                 
                 actionButton("run_t", "Run Simulation", class = "btn-primary"),
                 br(), br(),
                 
                 # --- Results section ---
                 h4("📉 Calculated Risk Metrics:"),
                 tags$div(
                   style = "background-color:#222; padding:10px; border-radius:8px; 
             border:1px solid #444; color:#f8f9fa; font-family: monospace;",
                   verbatimTextOutput("riskMetrics_t")
                 )
               ),
               
               mainPanel(
                 h3(textOutput("title_t")),
                 plotOutput("mcPlot_t", height = "450px"),
                 plotOutput("histPlot_t", height = "300px")
               )
             )
    )
  )
)


# ---------------- SERVER ----------------

server <- function(input, output) {

  # --- Tab 1: US Monte Carlo Simulation ---
  
  observeEvent(input$run, {
    
    # Downloading data
    data <- get_stock_data(input$stock, start_date = input$start_date)
    req(!is.null(data), nrow(data) > 0)
    sim <- simulate_montecarlo(data$Close, input$days, input$paths)
    final_prices <- sim[nrow(sim), ]
    returns <- final_prices / tail(data$Close, 1) - 1
    
    # Compute risk metrics
    VaR_95 <- quantile(returns, 0.05)
    ES_95 <- mean(returns[returns <= VaR_95]) # Take returns which are worse than VaR_95
    mean_return <- mean(returns)
    
    output$title <- renderText({
      paste("Monte Carlo Simulation for", toupper(input$stock))
    })
    
    # --- Price path plot ---
    output$mcPlot <- renderPlot({
      par(bg = "#222222", col.axis = "white", col.lab = "white", col.main = "white", fg = "white")
      matplot(sim, type = "l", lty = 1, col = "lightyellow",
              main = paste("Simulated Price Paths -", toupper(input$stock)),
              xlab = "Days", ylab = "Price")
      
      # Mean path
      lines(apply(sim, 1, mean), col = "red", lwd = 2)
      
      legend("topleft",
             legend = c("Simulated Paths", "Mean Path"),
             col = c(rgb(0, 0, 1, 0.3), "red"),
             lwd = c(1, 2),
             bty = "n")
    })
    
    # --- Hist with VaR & ES ---
    output$histPlot <- renderPlot({
      par(bg = "#222222", col.axis = "white", col.lab = "white", col.main = "white", fg = "white")
      hist(returns, breaks = 40, border = "black",
           main = "Distribution of Simulated Returns",
           xlab = "Simulated Return",
           xlim = c(min(returns), max(returns)))
      
      abline(v = VaR_95, col = "red", lwd = 2, lty = 2)
      abline(v = ES_95, col = "darkorange", lwd = 2, lty = 3)
      abline(v = mean_return, col = "green3", lwd = 2, lty = 1)
      
      legend("topright",
             legend = c(
               paste0("Mean Return = ", round(mean_return * 100, 2), "%"),
               paste0("VaR (95%) = ", round(VaR_95 * 100, 2), "%"),
               paste0("ES (95%) = ", round(ES_95 * 100, 2), "%")
             ),
             col = c("green3", "red", "darkorange"),
             lty = c(1, 2, 3),
             lwd = 2,
             bty = "n")
    })
    
    # --- Risk measurement output ---
    output$riskMetrics <- renderPrint({
      cat("Value at Risk (95%):", round(VaR_95 * 100, 2), "%\n")
      cat("Expected Shortfall (95%):", round(ES_95 * 100, 2), "%\n")
      cat("Mean Expected Return:", round(mean_return * 100, 2), "%")
    })
  })
  
  # --- Tab 2: Polish market Monte Carlo simulation ---
  
  observeEvent(input$pl_run, {
    
    # Downloading data
    data_pl <- get_polish_data(symbol = input$pl_symbol, start_date = input$pl_start)
    req(!is.null(data_pl), nrow(data_pl) > 0)
    ceny_zamkniecia <- data_pl$Zamkniecie
    
    sim_pl <- simulate_montecarlo(
      ceny_zamkniecia,
      input$pl_days,
      input$pl_paths
    )
    
    final_prices_pl <- sim_pl[nrow(sim_pl), ]
    returns_pl <- final_prices_pl / tail(ceny_zamkniecia, 1) - 1
    
    # Caluclating risk measurements
    VaR_95_pl <- quantile(returns_pl, 0.05)
    ES_95_pl  <- mean(returns_pl[returns_pl <= VaR_95_pl])
    mean_ret_pl <- mean(returns_pl)
    
    output$pl_title <- renderText({
      paste("Monte Carlo simulation for", toupper(input$pl_symbol))
    })
    
    # Simulated Price plot
    output$pl_mcPlot <- renderPlot({
      par(bg = "#222222", col.axis = "white", col.lab = "white", col.main = "white", fg = "white")
      matplot(sim_pl, type = "l", lty = 1, col = "lightyellow",
              main = paste("Simulated Price Paths -", toupper(input$pl_symbol)),
              xlab = "Days", ylab = "Price")
      
      # Mean path
      lines(apply(sim_pl, 1, mean), col = "red", lwd = 2)
      
      legend("topleft",
             legend = c("Simulated Paths", "Mean Path"),
             col = c(rgb(0, 0, 1, 0.3), "red"),
             lwd = c(1, 2),
             bty = "n")
    })
    
    # --- Hist with VaR & ES ---
    output$pl_histPlot <- renderPlot({
      par(bg = "#222222", col.axis = "white", col.lab = "white", col.main = "white", fg = "white")
      hist(returns_pl, breaks = 40, border = "black",
           main = "Distribution of Simulated Returns",
           xlab = "Simulated Return",
           xlim = c(min(returns_pl), max(returns_pl)))
      
      abline(v = VaR_95_pl, col = "red", lwd = 2, lty = 2)
      abline(v = ES_95_pl, col = "darkorange", lwd = 2, lty = 3)
      abline(v = mean_ret_pl, col = "green3", lwd = 2, lty = 1)
      
      legend("topright",
             legend = c(
               paste0("Mean Return = ", round(mean_ret_pl * 100, 2), "%"),
               paste0("VaR (95%) = ", round(VaR_95_pl * 100, 2), "%"),
               paste0("ES (95%) = ", round(ES_95_pl * 100, 2), "%")
             ),
             col = c("green3", "red", "darkorange"),
             lty = c(1, 2, 3),
             lwd = 2,
             bty = "n")
    })
    
    # --- Risk measurement output ---
    output$pl_riskMetrics <- renderPrint({
      cat("Value at Risk (95%):", round(VaR_95_pl * 100, 2), "%\n")
      cat("Expected Shortfall (95%):", round(ES_95_pl * 100, 2), "%\n")
      cat("Mean Expected return:", round(mean_ret_pl * 100, 2), "%")
    })
  })
  
  # --- Tab 4: US Volatile Market Monte Carlo Simulation ---
  
  observeEvent(input$run_t, {
    
    # Downloading data
    data_t <- get_stock_data(input$stock_t, start_date = input$start_date_t)
    req(!is.null(data_t), nrow(data_t) > 0)
    sim_t <- simulate_montecarlo_t(data_t$Close, input$days_t, input$paths_t, input$df)
    final_prices_t <- sim_t[nrow(sim_t), ]
    returns_t <- final_prices_t / tail(data_t$Close, 1) - 1
    
    # Compute risk metrics
    VaR_95_t <- quantile(returns_t, 0.05)
    ES_95_t <- mean(returns_t[returns_t <= VaR_95_t])
    mean_return_t <- mean(returns_t)
    
    output$title_t <- renderText({
      paste("Monte Carlo Simulation for", toupper(input$stock_t))
    })
    
    # --- Price path plot ---
    output$mcPlot_t <- renderPlot({
      par(bg = "#222222", col.axis = "white", col.lab = "white", col.main = "white", fg = "white")
      matplot(sim_t, type = "l", lty = 1, col = "lightyellow",
              main = paste("Simulated Price Paths -", toupper(input$stock_t)),
              xlab = "Days", ylab = "Price")
      
      # Mean path
      lines(apply(sim_t, 1, mean), col = "red", lwd = 2)
      
      legend("topleft",
             legend = c("Simulated Paths", "Mean Path"),
             col = c(rgb(0, 0, 1, 0.3), "red"),
             lwd = c(1, 2),
             bty = "n")
    })
    
    # --- Hist with VaR & ES ---
    output$histPlot_t <- renderPlot({
      par(bg = "#222222", col.axis = "white", col.lab = "white", col.main = "white", fg = "white")
      hist(returns_t, breaks = 40, border = "black",
           main = "Distribution of Simulated Returns",
           xlab = "Simulated Return",
           xlim = c(min(returns_t), max(returns_t)))
      
      abline(v = VaR_95_t, col = "red", lwd = 2, lty = 2)
      abline(v = ES_95_t, col = "darkorange", lwd = 2, lty = 3)
      abline(v = mean_return_t, col = "green3", lwd = 2, lty = 1)
      
      legend("topright",
             legend = c(
               paste0("Mean Return = ", round(mean_return_t * 100, 2), "%"),
               paste0("VaR (95%) = ", round(VaR_95_t * 100, 2), "%"),
               paste0("ES (95%) = ", round(ES_95_t * 100, 2), "%")
             ),
             col = c("green3", "red", "darkorange"),
             lty = c(1, 2, 3),
             lwd = 2,
             bty = "n")
    })
    
    # --- Risk measurement output ---
    output$riskMetrics_t <- renderPrint({
      cat("Value at Risk (95%):", round(VaR_95_t * 100, 2), "%\n")
      cat("Expected Shortfall (95%):", round(ES_95_t * 100, 2), "%\n")
      cat("Mean Expected Return:", round(mean_return_t * 100, 2), "%")
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
