install.packages('quantmod')
library(quantmod)


# Simulate stock prices, then plots and returns said simulates prices against the real prices
# symbol - symbol of stock
# days - trading days of simulation
# paths - number of simulated realizations of stock
# interest rate - Allows user to enter custom interest rate, 
#                 defaults to true interest rate at start of simulation
Stock.Simulator <- function(symbol, days = 30, paths = 10, interest_rate = NULL) {
  
  # downloads historical prices for double the amount of trading days we're 
  # simulating + 10(min amount of data for training), takes double since
  # it'll be split for training data
  getSymbols(symbol, from = Sys.Date() - 2.91*(days) - 10)
  
  # Gets closing values for stock
  closings <- Cl(get(symbol))
  
  # separates test and training data based on # of simulation days
  closing_train <- closings[1:(length(closings) - days)]
  closing_test <- closings[(length(closings) - days - 1):length(closings)]
  
  # Calculates daily returns
  returns <- dailyReturn(closing_train)

  # Calculates volatility based on last 30 days(tested for a while and this way gave best results)
  vol <- mean(c((as.numeric(tail(volatility(closing_train, n = days), 1))), sd(returns)))
  
  # Calculates drift
  drift <- mean(returns)
  
  # downloads historical treasury interest rate data, 
  interest_rates <- getSymbols("DGS10", src = "FRED", 
                               from = Sys.Date() - 6*(days) - 10, auto.assign = FALSE)
  
  # sections off the interest rate data so we are only using data from before start of simulation
  interest_rates <- interest_rates[1:(length(interest_rates) - days)]
  
  # checks if user inputted custom interest rate
  if (is.null(interest_rate)) {
    # Calculates last interest rate
    latest_interest_rate <- as.numeric(tail(interest_rates, 1))
  } else {
    latest_interest_rate <- interest_rate
  }
    
  # Calculates mean of past interest rates
  mean_interest_rate <- mean(interest_rates, na.rm = TRUE)
    
  # finds difference for avg interest rate vs current rate
  cur_rate_diff <- mean_interest_rate - latest_interest_rate
    
  # adjusts drift to account for current interest rate
  drift <- drift + (cur_rate_diff/1000)
    
  # gets last price of stock
  init_price <- as.numeric(closings[(length(closings) - days - 1)])
  
  # gets simulated prices
  sim_prices <- Simulated.Prices(drift, vol, init_price, days, paths)
  
  # adds real prices to matrix
  sim_prices_and_real <- cbind(sim_prices, closing_test)
  
  # plots simulated stock prices(dotted blue) vs real prices(bold red)
  matplot(c(0:(days + 1)), sim_prices_and_real, type = "l",
          xlab = "Trading Day Since Start of Simulation",
          ylab = "Price of Stock ($)",
          xlim = c(1, days),
          main = paste(symbol, "Stock Price Simulation"),
          col = c(rep("blue", paths), "red"),
          lty = c(rep(2, paths), 1),
          lwd = c(rep(1, paths), 2))
  
  return (sim_prices_and_real)
  
}

#test
apple_table <- Stock.Simulator("AAPL")


# Simulates and plots/returns future stock prices
# symbol - symbol of stock
# days - trading days of simulation
# paths - number of simulated realizations of stock
# interest rate - Allows user to enter custom interest rate, 
#                 defaults to true interest rate at start of simulation
Future.Stock.Simulator<- function(symbol, days = 30, paths = 10, interest_rate = NULL) {
  
  # downloads historical prices based on the amount of trading days we're 
  # simulating + 10(min amount of data for training)
  getSymbols(symbol, from = Sys.Date() - 1.45*(days) - 10)
  
  # Gets closing values for stock
  closings <- Cl(get(symbol))
  
  # Calculates daily returns
  returns <- dailyReturn(closings)
  
  # Calculates volatility based on last 30 days(tested for a while and this way gave best results)
  vol <- mean(c((as.numeric(tail(volatility(closings, n = days), 1))), sd(returns)))
  
  # Calculates mean return (drift)
  drift <- mean(returns)
  
  # downloads historical treasury interest rate data
  interest_rates <- getSymbols("DGS10", src = "FRED", 
                               from = Sys.Date() - 3*(days) - 10, auto.assign = FALSE)
  
  # checks if user inputted custom interest rate
  if (is.null(interest_rate)) {
    # Calculates last interest rate
    latest_interest_rate <- as.numeric(tail(interest_rates, 1))
  } else {
    latest_interest_rate <- interest_rate
  }
  
  # Calculates mean of past interest rates
  mean_interest_rate <- mean(interest_rates, na.rm = TRUE)
  
  # finds difference for avg interest rate vs current rate
  cur_rate_diff <- mean_interest_rate - latest_interest_rate
  
  # adjusts drift to account for current interest rate
  drift <- drift + (cur_rate_diff/1000)
  
  # gets last price of stock
  init_price <- as.numeric(closings[(length(closings) - days - 1)])
  
  # gets simulated prices
  sim_prices <- Simulated.Prices(drift, vol, init_price, days, paths)
  
  # plots simulated stock priceshttp://127.0.0.1:30373/graphics/4792ea19-48cc-44b9-bce5-438226030b51.png
  matplot(c(0:(days + 1)), sim_prices, type = "l",
          xlab = "Trading Day Since Start of Simulation",
          ylab = "Price of Stock ($)",
          xlim = c(1, days),
          main = paste(symbol, "Stock Price Simulation"))
  
  return (sim_prices)
  
}

#test
future_apple_table <- Future.Stock.Simulator("AAPL")


# Simulates multiple price paths and store them in a matrix and returns said matrix
# drift - drift of the stock
# vol - volatility of the stock
# init_price - initial price of the stock
# days - trading days of simulation
# paths - number of simulated realizations of stock
Simulated.Prices <- function(drift, vol, init_price, days = 30, paths = 10) {
  
  # creates matrix to hold all possible realizations
  sim_price_matrix <- matrix(0, nrow = days + 2, ncol = paths)
  
  for (i in 0:paths) {
    
    # Unexpected event(1 in a 1000) making volatility double
    if (runif(1) < .001) {
      vol <- vol*2
    }
    
    # Uses GBM function to generate new stock prices
    sim_price <- c(init_price ,gbm.f(n = days, s0 = init_price, mu = drift, sigma = vol))
    
    # appends matrix to hold new path stock could take
    sim_price_matrix[,i] <- sim_price
  }
  return (sim_price_matrix)
}


# Geometric Brownian Motion Stock Price Simulator
# n - end time
# s0 - init price
# mu - drift
# sigma - volatility
gbm.f <- function(n, s0, mu, sigma) {
  #Get a time horizon, 1 day
  t <- 1
  
  # time step, makes sequence from 0 to end time by time horizon
  t.s <- seq(0,t,length=n+1) 
  
  # change in time
  dt <- t/n 
  
  # generating Brownian random variable 
  Bt <- sqrt(dt)*cumsum(rnorm((n+1),0,1))
  
  # Uses GBM formula to generate future stock prices
  St <- s0*exp((mu-sigma^2/2)*t.s+sigma*Bt)
  
  return (St)
  
}

