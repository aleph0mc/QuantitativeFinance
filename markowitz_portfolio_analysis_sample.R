setwd("C:\\...\\financial_market_analysis")
getwd()

# portfolio analysis using Markowitz model

library(ggplot2)
library(quantmod)
library(lubridate)
library(corrplot)
library(PerformanceAnalytics)
library(TTR)
library(tidyr)
library(PortfolioAnalytics)

theme_set(theme_minimal())

# portfolio with 4 stocks
stocks <- c("IBM", "GOOG", "TSLA", "AMZN", "AAPL")

# download prices and create returns from Adjusted Prices - save in a list data1
# ROC <- rate of change or momentum
stockData_ret <- lapply(stocks, FUN = function(stock) {
  ROC(Ad(getSymbols(stock, from = "2020-01-01", auto.assign = F)),
      type = "discrete") * 100
})  #%returns

# convert to data frame
stocks_ret <- as.data.frame(do.call(merge, stockData_ret))
#removes na
stocks_ret <- na.omit(stocks_ret)
# change column names
colnames(stocks_ret) = c('Stock1', 'Stock2', 'Stock3', 'Stock4', 'Stock5')
# add dates column
stocks_ret = data.frame(Date = as.Date(row.names(stocks_ret)), stocks_ret)
row.names(stocks_ret) = NULL

stockData <- lapply(stocks, FUN = function(stock) {
  Ad(getSymbols(stock, from = "2020-01-01", auto.assign = F))
})
# convert to data frame
dfstockData <- as.data.frame(do.call(merge, stockData))
#removes na
dfstockData <- na.omit(dfstockData)
# change column names
colnames(dfstockData) = c('Stock1', 'Stock2', 'Stock3', 'Stock4', 'Stock5')
# add dates column
dfstockData = data.frame(Date = as.Date(row.names(dfstockData)), dfstockData)
row.names(dfstockData) = NULL

# convert to long - transpose the data frame
tr_stocks_ret <- pivot_longer(stocks_ret, cols = -c(Date), values_to = "Return", names_to = "Stock")
# plot stock returns
ggplot(tr_stocks_ret, aes(Date, Return, color = Stock)) +
  geom_path(stat = "identity") +
  facet_grid(Stock ~ .) +
  labs(x = "Date", y = "Returns") +
  ggtitle("Stock Returns")

#plot price fluctuations
ggplot(data = dfstockData, mapping = aes(x = as.Date(Date))) +
  geom_line(mapping = aes(y = Stock1, color = "Stock1"),
            linetype = 'solid', linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = Stock2, color = "Stock2"),
            linetype = 'solid', linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = Stock3, color = "Stock3"),
            linetype = 'solid', linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = Stock4, color = "Stock4"),
            linetype = 'solid', linewidth = 1, alpha = .5) +
  geom_line(mapping = aes(y = Stock5, color = "Stock5"),
            linetype = 'solid', linewidth = 1, alpha = .5) +
  scale_x_date(breaks = seq(from = as.Date("2020-01-01"), to = as.Date("2023-12-31"), by="3 months"), date_labels="%b %Y") +
  scale_colour_manual("Stock", values=c("Stock1"="red", "Stock2"="blue", "Stock3"="darkgreen",
                                        "Stock4" = "darkorange", "Stock5" = 'darkgrey')) +
  xlab("Date") +
  ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  ggtitle("Price series")

# compute mean of returns
col_names <- colnames(stocks_ret)[-1]
mean_stocks_ret <- lapply(col_names, FUN = function(cn) {
  mean(stocks_ret[[cn]])
})
# convert to data frame
dfStockRetMean <- as.data.frame(do.call(cbind, mean_stocks_ret))
# assign column names
colnames(dfStockRetMean) <- c("Stock1_m", "Stock2_m", "Stock3_m", "Stock4_m", "Stock5_m")

#compute log returns
col_names <- colnames(dfstockData)[-1]
lstStockLogRets <- lapply(col_names, FUN = function(stock) {
  diff(log(dfstockData[[stock]]))
})
# list to data frame
stockDataLogRets <- as.data.frame(do.call(cbind, lstStockLogRets))
colnames(stockDataLogRets) <- c("Stock1_log", "Stock2_log", "Stock3_log", "Stock4_log", "Stock5_log")
#correlation
cor_mat <- cor(stockDataLogRets)
print(cor_mat)
corrplot(cor_mat)

# compute mean of LOG returns
col_names <- colnames(stockDataLogRets)
mean_log_stocks_ret <- lapply(col_names, FUN = function(cn) {
  mean(stockDataLogRets[[cn]])
})
# convert to data frame
dfStockRetLogMean <- as.data.frame(do.call(cbind, mean_log_stocks_ret))
# assign column names
colnames(dfStockRetLogMean) <- c("Stock1_lm", "Stock2_lm", "Stock3_lm", "Stock4_lm", "Stock5_lm")

############################ PORTFOLIO ANALYSIS ############################
numTradingDays <- 252
tot_inv_amt <- 2500 #dollars
inv_shares <- c(200, 500, 500, 600, 700)
weights <- inv_shares / tot_inv_amt

#portfolio return
portf_ret <- dfStockRetLogMean * weights
print(portf_ret)
#portfolio return
sum(portf_ret)*numTradingDays

#covariance
cov_mat <- cov(stockDataLogRets)*numTradingDays
print(cov_mat)
#standard dev - portfolio volatility
sqrt(t(weights) %*% (cov_mat %*% weights))

#PORTFOLIOS WITH RANDOM WEIGHTS
numPortf <- 1000
# removes date column
stocks_retValues <- stocks_ret[, -1]
# returns mean
vect_mu <- colMeans(stocks_retValues)
print(vect_mu)
# number of stocks
numStocksBought <- ncol(stocks_retValues)
print(numStocksBought)
# covariance of bought stocks
cov_stocks_mat <- cov(stocks_retValues)
print(cov_stocks_mat)

# risk vector init
vect_risk <- NULL
#returns vector init
vect_rets <- NULL
# using loop
for (i in 1:numPortf) {
  # random weights
  rand_w = diff(c(0, sort(runif(numStocksBought - 1)), 1))
  #matrix multiplication
  # stock return
  stockRet = t(rand_w) %*% vect_mu
  # stock standard deviation
  stockSd = t(rand_w) %*% cov_stocks_mat %*% rand_w
  # concatenate data
  vect_risk = rbind(vect_risk, stockSd)
  vect_rets = rbind(vect_rets, stockRet)
}
# risk and return data frame
dfRiskRet <- data.frame(Return = vect_rets, Risk = vect_risk)
# plot x = risk, y = return
ggplot(data = dfRiskRet, mapping = aes(x = Risk, y = Return)) +
         geom_point(size = 3, shape = 1, color = 'blue') +
         xlab("Risk") +
         ylab("Return") +
         theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
         ggtitle("Portfolio Frontiers")

# efficient portfolio
# asset names uses time series data init
tsData <- zoo(dfstockData[, -1], order.by = as.Date(dfstockData$Date))
# create specification
portf_specs <- portfolio.spec(assets = c(colnames(tsData)))
# add long only constraint - positive weights only
portf_specs <- add.constraint(portfolio = portf_specs, type = "long_only")
# add full investment constraint - allocate in all the assets (min 0 and max 1 weight)
portf_specs <- add.constraint(portfolio = portf_specs, type = "full_investment")
# objective: minimize risk => standard dev
portf_rnd <- add.objective(portfolio = portf_specs, type = "risk", name = "StdDev")
# objective: maximize return => mean
portf_rnd <- add.objective(portfolio = portf_rnd, type = "return", name = "mean")
# optimize random portfolios
rand_portf <- optimize.portfolio(R = tsData,
                                portfolio = portf_rnd,
                                optimize_method = "random",
                                trace = T,
                                search_size = 1000
                                )
# plot and also plots the equally weighted portfolio
chart.RiskReward(rand_portf, risk.col = "StdDev", return.col = "mean", chart.assets = T)

# minimum risk portfolio - optimize standard dev
# NOTE: ROI method requires ROI packages installed
portf_msd <- add.objective(portfolio = portf_specs, type = "risk", name = "StdDev")
minVals <- optimize.portfolio(R = tsData,
                              portfolio = portf_msd,
                              optimize_method = "ROI",
                              trace = T
                             )
print(minVals)
# plot
plot(minVals, risk.col = "StdDev", main = "Mean Variance Portfolio", chart.assets = T)

# efficient frontier
minVals_ef <- create.EfficientFrontier(R = tsData,
                                      portfolio = portf_msd,
                                      type = "mean-StdDev"
                                     )
chart.EfficientFrontier(minVals_ef,
                        match.col = "StdDev",
                        type = "l",
                        tangent.line = T,
                        chart.assets = T
                        )

# convert data frame to xts object
xts_stock_ret <- xts(stocks_ret[,-1], order.by=as_date(stocks_ret$Date))
sharpe_ratio <- round(SharpeRatio(xts_stock_ret, Rf = 0), 4)
print(sharpe_ratio)
