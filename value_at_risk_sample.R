setwd("C:\\Users\\milko\\OneDrive\\Documenti\\R_statistics_language\\financial_market_analysis")
getwd()

# Wiener process simulation

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

# stock historical data
stock <- 'C'
xtsHistData <- getSymbols(stock, src = "yahoo", from = "2020-01-01", auto.assign = FALSE)
# convert xts to data frame
dfStockHistData <- data.frame(date=index(xtsHistData), coredata(xtsHistData))
# change column names
colnames(dfStockHistData)[-1] = c('open', 'high', 'low', 'close', 'volume', 'adjusted')
# print some data
head(dfStockHistData, n = 5)

#plot price fluctuations
ggplot(data = dfStockHistData, mapping = aes(x = as.Date(date))) +
  geom_line(mapping = aes(y = adjusted, color = "adj"),
            linetype = 'solid', linewidth = 1, alpha = .5) +
  scale_x_date(breaks = seq(from = as.Date("2020-01-01"),
                            to = as.Date("2023-12-31"),
                            by="6 months"),
               date_labels="%b %Y") +
  scale_colour_manual("Stock", values=c("adj"="red")) +
  xlab("Date") +
  ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  ggtitle(paste0("Price series for ", stock))

# pause the script 1 sec to give time plot to be rendered
Sys.sleep(1)

# daily rets
dfLogDailyRets <- data.frame(Stock1_lr = diff(log(dfStockHistData$adjusted)))
# print some data
head(dfLogDailyRets, n = 5)

############### Compute Value at Risk ###############
position <- 1e6
conf_lev <- .01 # 99%
mu <- mean(dfLogDailyRets$Stock1_lr)
sigma <- sd(dfLogDailyRets$Stock1_lr)
# inverse of the cumulative distribution function (percent point fuction, ppf)
ppf <- qnorm(conf_lev)
# value at risk tomorrow => num. days = 1 => dt term can be omitted
Var <- position * (mu - sigma * ppf)
print(Var)

# value at risk for num. days = n in the future
n <- 10
Var <- position * (mu * n - sigma * sqrt(n) * ppf)
print(Var)

############### MONTE CARLO METHOD to Compute Value at Risk ###############
# number of iterations
N_iter <- 100000
# vector of random num. from normal distrib. with mean 0 and standard dev. 1
rand <- rnorm(N_iter)
stock_price <- position * exp((mu-.5*sigma^2)*n + sigma * sqrt(n) * rand)
# sort data by price ascending
stock_price_sorted <- sort(stock_price)
# compute percentale according to confidence level
stock_perc <- quantile(stock_price_sorted, probs = c(conf_lev))
Var <- position - stock_perc
print(Var)
