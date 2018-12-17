library(quantmod)
library(Quandl)
library(PerformanceAnalytics)
library(dplyr)

#-----------Benchmark-----------#
ticker = '^GSPC'
#Get Data
stock <- getSymbols.yahoo(ticker, from="2016-01-01", periodicity = "daily", auto.assign=FALSE)[,4]
#Remove Dates With No Prices
stock <- stock[apply(stock,1,function(x) all(!is.na(x))),]
#Calculate Returns for Assets: Daily RoC
bench_ret <- na.omit(ROC(stock, type="discrete"))
#Rename Columns
colnames(bench_ret) <- "SP500"

#-----------Asset-----------#
asset = 'MMM'
#Get Data
stock <- getSymbols.yahoo(asset, from="2016-01-01", periodicity = "daily", auto.assign=FALSE)[,4]
#Remove Dates With No Prices
stock <- stock[apply(stock,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(stock) <- asset
#Calculate Returns for Assets: Daily RoC
asset_ret <- na.omit(ROC(stock, type="discrete"))

#-----------Portfolio-----------#
#Create Vector of Tickers
tickers <- c("AAPL", "INTC",  "ADSK", "NVDA", "ADBE","ATVI", "BIDU", "CELG", "CMCSA", "QCOM")

#Get Prices (can be monthly or weekly)
portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from="2016-01-01", periodicity = "daily", auto.assign=FALSE)[,4])

#Delete all dates with no prices
portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(portfolioPrices) <- tickers

#Calculate Returns for Assets: Daily RoC
portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))

#Calculate Aggregate Returns for Portfolio
weights <-c(".1",".1", ".1", ".1", ".1", ".1", ".1", ".1", ".1", ".1")
port_rets <- Return.portfolio(portfolioReturns, weights = NULL, geometric = TRUE)
names(port_rets) <- "Portfolio"

#-----------Risk Free Data-----------#
rf_data = Quandl("USTREASURY/YIELD",start_date="2016-11-13")
rf <- data.frame(rf_data[4])
#calculate mean
rf <- colMeans(rf[sapply(rf, is.numeric)])

#----------Beta-------------#
CAPM.beta(port_rets, bench_ret, Rf = rf)
CAPM.beta(asset_ret, bench_ret, Rf = rf)

#----------Beta in Up Market-------------#
#Get > 0 returns for benchmark
pos_rets <- bench_ret[bench_ret$SP500 > 0, ]
#Get Date Values
dates <- index(pos_rets)
#Filter By Date List
asset_pos <- asset_ret[index(asset_ret) %in% as.Date(dates),]
beta_pos <- CAPM.beta(asset_pos, pos_rets, Rf = rf)

#----------Beta in Down Market-------------#
neg_rets <- bench_ret[bench_ret$SP500 < 0, ]
neg_dates <- index(neg_rets)
asset_neg <- asset_ret[index(asset_ret) %in% as.Date(neg_dates),]
beta_neg <- CAPM.beta(asset_neg, neg_rets, Rf = rf)

#----------Timing Ratio-------------#
#Timing Raio (>1 in rising market, <1 in falling market)
time_ratio = beta_pos / beta_neg

#----------Regression Chart-------------#
chart.Regression(asset_ret, bench_ret, Rf = rf,
           excess.returns = FALSE, fit = c("loess", "linear"),
           legend.loc = "topleft", element.color = "blue",  main = paste(asset, "to", "S&P 500"))