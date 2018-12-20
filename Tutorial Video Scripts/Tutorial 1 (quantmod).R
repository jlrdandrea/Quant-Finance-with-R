library(quantmod)
library(PerformanceAnalytics)

dt <- "2017-2-1"

aapl <- getSymbols.yahoo("AAPL", from=dt, auto.assign = F)
aaplClose <- getSymbols.yahoo("AAPL", from=dt, auto.assign = F)[,6]

aaplRets <- na.omit(dailyReturn(aaplClose, type="log"))

chartSeries(aapl)
