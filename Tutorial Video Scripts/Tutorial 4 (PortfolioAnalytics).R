library(PortfolioAnalytics)
library(quantmod)
library(dplyr)
library(PerformanceAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(DEoptim)
library(imputeTS)
library(xts)

#-----------------------IMPORT DATA-----------------------#
#Create vector of tickers
tickers <- c("PM", "BTI", "KHC", "DE", "C", "GM", "XOM", "VZ", "V", "GILD", "WFC", "KO", "FB", "NKE", "AMZN", "GOOGL", "MSFT", "NVDA", "PEP", "MCD")

#Get stock data
port <- NULL
for (Ticker in tickers) {
  tryCatch({
    port <- cbind(port, getSymbols.yahoo(Ticker, from='2016-1-1', periodicity = "daily", auto.assign=FALSE)[,4])
  },
  error = function(e) {
    errSym <- Ticker
  })
}


#Get sum of NA per column (prices)
colSums(is.na(port))

#Calculate periodic returns
portRets <- na.omit(ROC(port))

#Rename Columns
colnames(portRets) <- tickers

#---------------------------------------------------------#

#-----------------------BENCHMARK PORTFOLIO-----------------------#
# Create a vector of equal weights
equal_weights <- rep(1 / ncol(portRets), ncol(portRets))

# Compute the benchmark returns
benchmark <- Return.portfolio(R = portRets, 
                                  weights = equal_weights 
)
colnames(benchmark) <- 'benchmark'

# Plot the benchmark returns
plot(benchmark)
#-----------------------------------------------------------------#

#-----------------------CREATE PORTFOLIO WITH OBJECTIVES-----------------------#
fund.names <- colnames(portRets)
pspec <- portfolio.spec(assets=fund.names)

# Weight sum constraint such that the weights sum to ~0
pspec <- add.constraint(portfolio = pspec, type = 'weight_sum',
                             min_sum = 0.99, max_sum = 1.01)

# Box constraint such that no asset can have a weight that is 
# greater than 20% or less than 0%
pspec <- add.constraint(portfolio = pspec, type = 'box', 
                             min = 0, max = 0.20)

# Objective to maximize portfolio return
pspec <- add.objective(portfolio = pspec, type = 'return', 
                            name = 'mean')

# Objective to minimize portfolio standard deviation
pspec <- add.objective(portfolio = pspec, 
                            type = 'risk', 
                            name = 'StdDev')

#-------------------------------------------------------------------------------#

#-----------------------------RUN OPTIMIZATION----------------------------------#
# Random portfolios
rp <- random_portfolios(pspec, 5000, 'sample')

# Run optimization - method = random
opt_base_rand <- optimize.portfolio(R = portRets, 
                                        portfolio = pspec, 
                                        optimize_method = 'random',
                                        rp = rp,
                                        trace = TRUE)

# Plot portfolio risk return
plot(opt_base_rand, main = 'Random Optimized Base Portfolio',
     risk.col = 'StdDev', neighbors = 10)

#Next run optimization with backtest
opt_base_rebal <- optimize.portfolio.rebalancing(R = portRets, portfolio = pspec,
    optimize_method = "random", rp = rp, rebalance_on = "quarters",
    training_period = 12, rolling_window = 8)

# Chart the weights
chart.Weights(opt_base_rebal, main = 'Weights - Random Rebal (Quarters)')

#Calculate Returns
returns_base_rebal <- Return.portfolio(R = portRets,
                                         weights = extractWeights(opt_base_rebal))
#---------------------------------------------------------------------------------#

#----------------------------PLOT PERFORMANCE--------------------------------------#
opt_rets <- cbind(benchmark, returns_base_rebal)
charts.PerformanceSummary(R = opt_rets, main = 'Returns Performance')
#----------------------------------------------------------------------------------#
