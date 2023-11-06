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
library(gridExtra)

theme_set(theme_minimal())

# stock historical data
stock <- 'PLUG'
xtsHistData <- getSymbols(stock, src = "yahoo", from = "2020-01-01", auto.assign = FALSE)
# convert xts to data frame
dfStockHistData <- data.frame(date=index(xtsHistData), coredata(xtsHistData))
# change column names
colnames(dfStockHistData)[-1] = c('open', 'high', 'low', 'close', 'volume', 'adjusted')
# print some data
head(dfStockHistData, n = 5)

# long MA
lgma <- 50
# short MA
stma <- 30

# SMA long (slow SMA)
# USING rollmean
#dfStockHistData$lma <- rollmean(dfStockHistData$adjusted, 50, fill = NA, align = "right")
# OR 
dfStockHistData$lgma <- SMA(dfStockHistData$adjusted, lgma)
# SMA short (fast SMA)
dfStockHistData$stma <- SMA(dfStockHistData$adjusted, stma)
# remove na
dfStockHistData <- na.omit(dfStockHistData)

#plot price fluctuations
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
# 1 sec pause for rendering
Sys.sleep(1)

# Exponential Moving Average (EMA) for long period (slow EMA)
dfStockHistData$lgexma <- EMA(dfStockHistData$adjusted, lgma)
# Exponential Moving Average (EMA) for short period (fast EMA)
dfStockHistData$stexma <- EMA(dfStockHistData$adjusted, stma)
# remove na
dfStockHistData <- na.omit(dfStockHistData)

#plot price fluctuations
emepl <- ggplot(data = dfStockHistData, mapping = aes(x = as.Date(date))) +
  geom_line(mapping = aes(y = adjusted, color = "Main"),
            linetype = 'solid', linewidth = 1, alpha = 1) +
   geom_line(mapping = aes(y = lgexma, color = "Exp MA Long"),
            linetype = 'solid', linewidth = 1, alpha = 1) +
  geom_line(mapping = aes(y = stexma, color = "Exp MA Short"),
            linetype = 'solid', linewidth = 1, alpha = 1) +
  scale_x_date(breaks = seq(from = as.Date("2020-01-01"),
                            to = as.Date("2023-12-31"),
                            by="6 months"),
               date_labels="%b %Y") +
  scale_colour_manual("Stock", values=c("Main" = "black", "Exp MA Long" = "blue", "Exp MA Short" = "orange")) +
  xlab("Date") +
  ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  ggtitle(paste0("Price series for ", stock, " with EMA"))

# plot sma and ema
grid.arrange(smapl, emepl, nrow = 2)
