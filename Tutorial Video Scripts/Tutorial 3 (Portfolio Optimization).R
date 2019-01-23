library(quantmod)
library(PerformanceAnalytics)

tickers <- c("FB", "AAPL", "AMZN", "NFLX", "GOOGL", "SQ", "NVDA")

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(ticker, from='2016-01-03', periodicity = 'daily', auto.assign=FALSE)[,4])
}

portfolioReturns <- na.omit(ROC(portfolioPrices))

portf <- portfolio.spec(colnames(portfolioReturns))

portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1)
portf <- add.constraint(portf, type="box", min=.10, max=.40)
portf <- add.objective(portf, type="return", name="mean")
portf <- add.objective(portf, type="risk", name="StdDev")

optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI", trace=TRUE)

chart.Weights(optPort)

ef <- extractEfficientFrontier(optPort, match.col = "StdDev", n.portfolios = 25,
                         risk_aversion = NULL)

chart.EfficientFrontier(ef,
                        match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)
