library(quantmod)
library(PerformanceAnalytics)

#----------------------Getting the Data----------------------#
#Create Vector of Tickers
tickers <- c("XLE", "XLF",  "XLU", "MUNI", "QQQ","GLD", "^DJI", "^IXIC", "^GSPC")

#Get Prices (can be monthly or weekly)
portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from="2012-01-01", periodicity = "monthly", auto.assign=FALSE)[,4])

#Delete all dates with no prices
portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(portfolioPrices) <- tickers

#Calculate Returns for Assets: Daily RoC
portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))

#Calculate Aggregate Returns for Portfolio
weights <-c(".15",".1", ".1", ".1", ".1", ".1", ".1", ".1", ".15")
combined_rets <- Return.portfolio(portfolioReturns, weights = NULL, geometric = TRUE)
names(combined_rets) <- "Portfolio"

#----------------------Capture Ratios for Assets----------------------#
#Only Up and Down Ratios
table.UpDownRatios(portfolioReturns[,1:8], portfolioReturns[,2], digits = 4)
#More Detailed Table
table.CaptureRatios(portfolioReturns[,1:8], portfolioReturns[,2], digits = 4)
#Chart
chart.CaptureRatios(portfolioReturns[,1:8], portfolioReturns[,9], main = "Capture Ratio", 
                    add.names = TRUE, element.color = "red", benchmark.color = "blue",
                    xlab = "Downside Capture", ylab = "Upside Capture")

#More Stats
UpsideFrequency(portfolioReturns[,1])
UpsidePotentialRatio(portfolioReturns[,1])
UpsideRisk(portfolioReturns[,1])

table.DownsideRisk(portfolioReturns[,1])
DownsideFrequency(portfolioReturns[,1])
DownsidePotential(portfolioReturns[,1])
DownsideDeviation(portfolioReturns[,1])

#----------------------Capture Ratios for Portfolio----------------------#
#Only Up and Down Ratios
table.CaptureRatios(combined_rets, portfolioReturns[,9], digits = 4)
#More Detailed Table
table.UpDownRatios(combined_rets, portfolioReturns[,9], digits = 4)
#Chart
chart.CaptureRatios(combined_rets, portfolioReturns[,9], main = "Capture Ratio", 
                    add.names = TRUE, element.color = "red", benchmark.color = "blue",
                    xlab = "Downside Capture", ylab = "Upside Capture")

#More Stats
UpsideFrequency(combined_rets)
UpsidePotentialRatio(combined_rets)
UpsideRisk(combined_rets)

table.DownsideRisk(combined_rets)
DownsideFrequency(combined_rets)
DownsidePotential(combined_rets)
DownsideDeviation(combined_rets)
