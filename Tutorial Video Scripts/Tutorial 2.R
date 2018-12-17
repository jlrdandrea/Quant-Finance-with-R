library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(imputeTS)
library(PortfolioAnalytics)

tickers <- c("FB", "AAPL", "AMZN", "NFLX")
weights <- c(.25, .25, .25, .25)

#Get Prices (can be monthly or weekly)
portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from="2016-01-01", periodicity = "daily", auto.assign=FALSE)[,4])

benchmarkPrices <- getSymbols.yahoo("SPY", from="2016-01-01", periodicity = "daily", auto.assign=FALSE)[,4]
colSums(is.na(benchmarkPrices))
benchmarkReturns <- na.omit(ROC(benchmarkPrices, type="discrete"))


#Rename Columns
colnames(portfolioPrices) <- tickers

#Get sum of NA per column
colSums(is.na(portfolioPrices))

#Plo
plot(portfolioPrices, legend = tickers)


#Calculate Returns For DF
dailyReturns <- na.omit(ROC(portfolioPrices, type="discrete"))

#Calculate Portfolio Returns
portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)

#Plot Performance
chart.CumReturns(portfolioReturn)
charts.PerformanceSummary(portfolioReturn)

#Calculate Metrics 
CAPM.beta(portfolioReturn, benchmarkReturns, .035/252)
CAPM.beta.bull(portfolioReturn, benchmarkReturns, .035/252)
CAPM.beta.bear(portfolioReturn, benchmarkReturns, .035/252)

#CAPM.alpha(portfolioReturn, benchmarkReturns, .035/252)
CAPM.jensenAlpha(portfolioReturn, benchmarkReturns, .035/252)

SharpeRatio(portfolioReturn, Rf = .035/252, p = 0.95, FUN = "StdDev",
            weights = NULL, annualize = FALSE)

table.AnnualizedReturns(portfolioReturn, Rf=.035/252, geometric=TRUE)


