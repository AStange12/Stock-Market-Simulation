# Stock-Market-Simulation
Author: Aaron Stange

Description: Stock market simulator project I built for my modeling and simulation in r course, 
uses market data to both predict future stock trends and try and predict recent stock trends which
are then compared to the actual stocks movements for accuracy testing
How to use: 

            1. Download R file, open in R-studios, and load imports/functions

            2. To predict a stock you will need it's symbol (Ex: "AAPL" = Apple.co)
            
            3. Once you have the stock symbol you will pass it to either the 
               Future.Stock.Simulator("AAPL") or Stock.Simulator("AAPL") depending
               on whether you want to predict future prices or test accuracy.
               
            4. These functions will build plots making 10 predicted paths for either 
               the next or past 30 days. These parameters can be changes along with
               setting a hypothetical market interest rate by specifying days, paths,
               and interest_rate when calling either function.
