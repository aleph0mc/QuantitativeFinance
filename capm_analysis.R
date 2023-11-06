setwd("C:\\Users\\milko\\OneDrive\\Documenti\\R_statistics_language\\financial_market_analysis")
getwd()

library(ggplot2)
library(quantmod)
library(lubridate)
library(corrplot)
library(PerformanceAnalytics)
library(TTR)
library(tidyr)
library(PortfolioAnalytics)
library(latex2exp)

theme_set(theme_minimal())

# portfolio with the stock to check compare with the market, represented by the S&P 500 index
CONST_STOCK <- "GIS" # General Mills
stocks <- c(CONST_STOCK, "^GSPC")
# Adjusted closing price is used 
stockData <- lapply(stocks, FUN = function(stock) {
  Ad(getSymbols(stock, from = "2020-01-01", auto.assign = F))
})

# convert to data frame
dfstockData <- as.data.frame(do.call(merge, stockData))
#removes na
dfstockData <- na.omit(dfstockData)
# change column names
colnames(dfstockData) = c('Stock1', 'SP500')
# add dates column
dfstockData = data.frame(Date = as.Date(row.names(dfstockData)), dfstockData)
row.names(dfstockData) = NULL

# daily rets
dfLogDailyRets <- data.frame(Stock1_lr = diff(log(dfstockData$Stock1)), SP500_lr = diff(log(dfstockData$SP500)))
# histogram of log returns - it approximates a normal distribution
ggplot(dfLogDailyRets, mapping = aes(x = Stock1_lr)) +
  geom_histogram(bins = 20, fill = 'yellow', color = 'red')
# pause 1 sec to give time plot to be rendered
Sys.sleep(1)

# we need the monthly adjusted closing price
# convert data frame to xts object
xts_stock_ret <- xts(dfstockData[,-1], order.by=as_date(dfstockData$Date))
xtsMonthly_Adj_Data <- xts_stock_ret[endpoints(xts_stock_ret,'months')]
# convert xts to a data frame
dfMonthlyAd <- data.frame(date=index(xtsMonthly_Adj_Data), coredata(xtsMonthly_Adj_Data))
# mean of adjusted close
# data.frame[row, col], neg sign to remove cols, the current date
colMeans(dfMonthlyAd[,-1])
# compute the log returns
dfLogMonthlyRets <- data.frame(Stock1_lr = diff(log(dfMonthlyAd$Stock1)), SP500_lr = diff(log(dfMonthlyAd$SP500)))

# covariance value
cov_val <- cov(dfLogMonthlyRets$Stock1_lr, dfLogMonthlyRets$SP500_lr)
print(cov_val)
# compute covariance matrix
cov_mat <- cov(dfLogMonthlyRets)
print(cov_mat)
# variance of index
varSP500 <- var(dfLogMonthlyRets$SP500_lr)
print(varSP500)
# correlation matrix
cor_mat <- cor(dfLogMonthlyRets)
print(cor_mat)
corrplot(cor_mat)
# compute beta value
beta <- cov_val / varSP500
print(beta)
 
# computing beta using a linear model (linear regression)
model <- lm(dfLogMonthlyRets$Stock1_lr ~ dfLogMonthlyRets$SP500_lr)
model_sum <- summary(model)
intercept <- model_sum$coefficients[1]
slope <- model_sum$coefficients[2]
# plot the model
par(mfrow = c(2,2))
plot(model)

# compute expected annual return
risk_free_rate <- .05
beta <- slope
expect_return_yr <- risk_free_rate + beta * (mean(dfLogMonthlyRets$SP500)*12 - risk_free_rate)
expect_return_yr_pc <- expect_return_yr * 100
print(paste0('Expected asset return: ', expect_return_yr_pc, "%"))
# scatter plot
ggplot(dfLogMonthlyRets, aes(x = Stock1_lr, y = SP500_lr)) + 
  geom_point() + 
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE) +
  geom_smooth(method = 'gam', formula = y ~ poly(x, 4), color = 'midnightblue', linetype = 'solid', se = T) +
  labs(x = CONST_STOCK, y = 'S&P 500', caption = TeX(paste0("\\alpha=", intercept, " - ", "$\\beta=$", slope))) +
  ggtitle(TeX(paste0(CONST_STOCK, " vs. S&P 500 - ", "$R_{a}=\\alpha+\\beta \\cdot R_{m}=$", expect_return_yr_pc, "%")))
