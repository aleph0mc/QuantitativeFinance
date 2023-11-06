setwd("C:\\...\\financial_market_analysis")
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
library(gridExtra)

theme_set(theme_minimal())

# stock historical data
stock <- 'IBM'
xtsHistData <- getSymbols(stock, src = "yahoo", from = "2020-01-01", auto.assign = FALSE)
# rename columns
names(xtsHistData) <- c('open', 'high', 'low', 'close', 'volume', 'adjusted')
# convert xts to data frame
dfStockHistData <- data.frame(date=index(xtsHistData), coredata(xtsHistData))
# print some data
head(dfStockHistData, n = 5)

# long MA
lgma <- 50
# short MA
stma <- 30
# SMA long (slow SMA)
dfStockHistData$lgma <- SMA(dfStockHistData$adjusted, lgma)
# SMA short (fast SMA)
dfStockHistData$stma <- SMA(dfStockHistData$adjusted, stma)

# compute standard RSI with n = 14 day period
rsi <- RSI(dfStockHistData$adjusted)
dfStockHistData$RSI <- rsi

# remove na
dfStockHistData <- na.omit(dfStockHistData)

smapl <- ggplot(data = dfStockHistData, mapping = aes(x = as.Date(date))) +
  geom_line(mapping = aes(y = adjusted, color = "Main"),
            linetype = 'solid', linewidth = 1, alpha = 1) +
  geom_line(mapping = aes(y = lgma, color = "Long MA"),
            linetype = 'solid', linewidth = 1, alpha = 1) +
  geom_line(mapping = aes(y = stma, color = "Short MA"),
            linetype = 'solid', linewidth = 1, alpha = 1) +  
  scale_x_date(breaks = seq(from = as.Date("2020-01-01"),
                            to = as.Date("2023-12-31"),
                            by="6 months"),
               date_labels="%b %Y") +
  scale_colour_manual("Stock", values=c("Main" = "black", "Long MA" = "blue",
                                        "Short MA" = "orange")) +
  xlab("Date") +
  ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  ggtitle(paste0("Price series for ", stock, " with SMA"))

#plot RSI fluctuations
rsipl <- ggplot(data = dfStockHistData, mapping = aes(x = as.Date(date))) +
  geom_line(mapping = aes(y = RSI, color = "RSI"),
            linetype = 'solid', linewidth = 1, alpha = 1) +
  scale_x_date(breaks = seq(from = as.Date("2020-01-01"),
                            to = as.Date("2023-12-31"),
                            by="6 months"),
               date_labels="%b %Y") +
  scale_colour_manual("RSI Indicator", values=c("RSI" = "green")) +
  xlab("Date") +
  ylab("RSI") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  ggtitle(paste0("RSI for ", stock))

# plot sma and ema
grid.arrange(smapl, rsipl, nrow = 2)

################# COMPUTE SHARPE RATIO #################
# compute log returns on the adjusted column only
xtsRets <- Return.calculate(xts(xtsHistData[,'adjusted']), method="log")
# compute Sharpe Ratio
sr <- SharpeRatio.annualized(xtsRets, Rf=0, scale = 252, geometric = F)
print(sr[1])
