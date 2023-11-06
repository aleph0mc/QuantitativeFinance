setwd("C:\\...\\financial_market_analysis")
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

# stock price at t=t0
S0 <- 100
# strike price
Ep <- 100
# start time
t0 <- 0
# expiry time exT in year = 365 days
exT <- 1
# risk-free rate 5%
rf <- .05
# volatility - standard deviation 2%
sigma <- .2

# compute d1 and d2
d1 <- (log(S0/Ep) + (rf+.5*sigma^2)*(exT-t0))/(sigma * sqrt(exT-t0))
print(d1)
d2 <- d1 - sigma * sqrt(exT-t0)
print(d2)

# use standard normal distribution to compute the price option
callOpnPrice <- S0*pnorm(d1)-Ep*exp(-rf*(exT-t0))*pnorm(d2)
print(callOpnPrice)

putOpnPrice <- -S0*pnorm(-d1)+Ep*exp(-rf*(exT-t0))*pnorm(-d2)
print(putOpnPrice)

############### Monte Carlo Simulation of BS equation ###############
# number of simulations
Nsim <- 100
# stock historical data
stock <- 'IBM'
xtsHistData <- getSymbols(stock, src = "yahoo", from = "2020-01-01", auto.assign = FALSE)
# convert xts to data frame
dfStockHistData <- data.frame(date=index(xtsHistData), coredata(xtsHistData))

# daily rets
dfLogDailyRets <- data.frame(Stock1_lr = diff(log(dfStockHistData$IBM.Adjusted)))
# histogram of log returns - it approximates a normal distribution
ggplot(dfLogDailyRets, mapping = aes(x = Stock1_lr)) +
  geom_histogram(bins = 20, fill = 'yellow', color = 'red')
# pause 1 sec to give time plot to be rendered
Sys.sleep(1)

# get the init price
S0 <- dfStockHistData$IBM.Adjusted[1]
# compute the mean of log rets
adjMean <- mean(dfLogDailyRets$Stock1_lr)
# compute the standard deviation of log rets
adjSigma <- sd(dfLogDailyRets$Stock1_lr)
# number of data points used
Ndp <- 252 # trading days in a year
# result vector
result <- NULL
dfResult <- 
# number of simulations - possible S(t) realizations (of the stochastic process)
vect_n <- seq(from = 1, to = 253, by = 1)
for (i in 1:Nsim) {
  prices <- c(S0)
  for (j in 1:Ndp) {
    # random num. from normal distrib. with mean 0 and standard dev. 1
    randNd <- runif(1)
    # simulation day by day => t=1 (t can be omitted in the formula below)
    # tail function returns the last element of prices vector
    stockPrice <- 
      tail(prices,n=1) * exp((adjMean-.5*adjSigma^2)+adjSigma* randNd)
    prices <- c(prices, stockPrice)
  }
  result <- cbind(result, prices)
}
dfResults <- as.data.frame(result)
# change columna names
colnames(dfResults) = paste0("price", c(1:Nsim))
# add period and row mean
dfResults$period <- vect_n
dfResults$rowMean <- rowMeans(dfResults)
# convert to long - transpose the data frame
trResults <- pivot_longer(dfResults, cols = -c(period), values_to = "value", names_to = "price")
# plot
p1 <- ggplot(data = trResults, mapping = aes(x = period, y = value, fill = price, colour = price)) +
  geom_line() +
  geom_smooth() +
  theme(legend.position="none")
p1

############### Monte Carlo method for option pricing ###############
# stock price at t=0
S0 <- 100
# strike price - price at t=T
Ep <- 100
# expiry time exT in year = 365 days
exT <- 1
# risk-free rate 5%
rf <- .05
# volatility - standard deviation 2%
sigma <- .2
# num. of iterations
N_iter <- 10000

# call option
# initialization - two columns: first with 0s, second with payoff
# payoff function is max(0, S-E)
dfOpnData <- data.frame(zeros = rep(0, N_iter), payoff = rep(0, N_iter))
# Wiener proicess with as many random numbers as iterations
rand <- rnorm(N_iter)
# equation for the stock price
stock_price <- S0 * exp(T*(rf-.5*sigma^2)+sigma * sqrt(T) * rand)
head(stock_price, n = 3)
dfOpnData$payoff <- stock_price - Ep
# compute the max for each column
dfOpnData$maxVal <- apply(dfOpnData, 1, max, na.rm = T)
# compute average value of option price
opn_price_fv <- sum(dfOpnData$maxVal) / N_iter
# compute the discount factor as it is the future value but we need the present value
disc_factor <- exp(-rf * exT)
# compute the PV of option price
opn_price_pv <- opn_price_fv * disc_factor
print(opn_price_pv)

# put option
# initialization - two columns: first with 0s, second with payoff
# payoff function is max(0, E-S)
dfOpnData <- data.frame(zeros = rep(0, N_iter), payoff = rep(0, N_iter))
# Wiener proicess with as many random numbers as iterations
rand <- rnorm(N_iter)
# equation for the stock price
stock_price <- S0 * exp(T*(rf-.5*sigma^2)+sigma * sqrt(T) * rand)
head(stock_price, n = 3)
dfOpnData$payoff <- Ep - stock_price
# compute the max for each column
dfOpnData$maxVal <- apply(dfOpnData, 1, max, na.rm = T)
# compute average value of option price
opn_price_fv <- sum(dfOpnData$maxVal) / N_iter
# compute the discount factor as it is the future value but we need the present value
disc_factor <- exp(-rf * exT)
# compute the PV of option price
opn_price_pv <- opn_price_fv * disc_factor
print(opn_price_pv)
