setwd("C:\\...\\financial_market_analysis")
getwd()

# Wiener process simulation

library(ggplot2)
library(tidyr)

theme_set(theme_minimal())

#################### Ornstein-Uhlenbeck process simulation ####################
# time step
dt <- .1
# Theta - rate by which the process reverts towards the mean
th <- .3
# mean
mu <- .05
# variance around the mean
sigma <- .03
# num. iterations
N_iter <- 10000
vect_x <- rep(0, N_iter)
for (t in 1:N_iter) {
  vect_x[t+1] <- vect_x[t] + th * (mu-vect_x[t]) * dt + sigma * rnorm(n = 1,mean = 0, sd = sqrt(dt))
}

# building data frame to plot data
dfOU <- data.frame(time = seq(from = 0, by = dt, len = N_iter+1), values = vect_x)

#plot data
ggplot(data = dfOU, mapping = aes(x = time, y = values)) +
  geom_line(linetype = 'solid', linewidth = .3, color = 'blue', alpha = .5) +
  xlab("t") +
  ylab("x(t)") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  ggtitle("Ornstein-Uhlembeck Process")

#################### Vasicek Model implementation ####################
# init val interest rate
r0 <- .13
# speed of mean reversion
kappa <- .3
# expiry time in years
expT <- 1
dt <- expT / N_iter
timeSpan = seq(from = 0, by = dt, len = N_iter+1)
rates <- c(r0)
for (i in 1:N_iter) {
  last_rate <- tail(rates,n=1)
  dr <- kappa*(th-last_rate)*dt+sigma*sqrt(dt)*rnorm(1)
  new_rate <- last_rate + dr
  rates <- c(rates ,new_rate)
}

dfVM <- data.frame(time = timeSpan, values = rates)
#plot data
ggplot(data = dfVM, mapping = aes(x = time, y = values)) +
  geom_line(linetype = 'solid', linewidth = .3, color = 'blue', alpha = .5) +
  xlab("t") +
  ylab("r(t)") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  ggtitle("Vasicek Model for Rates")

#################### Estimating bond price using Monte Carlo method ####################
# num- of simulation
N_sim <- 1000
# num. of points in a single r(t) process
N_points <- 200
# principal amount of the bond, future value
fv <- 1000
# other parameters such as kappa, Theta, sigma, expiry time as above
dt <- expT / N_points

# result vector
result <- NULL
# number of simulations - possible r(t) (of the stochastic process)
for (i in 1:N_sim) {
  rates <- c(r0)
  for (j in 1:N_points) {
    last_rate <- tail(rates,n=1)
    dr <- kappa*(th-last_rate)*dt+sigma*sqrt(dt)*rnorm(1)
    new_rate <- last_rate + dr
    rates <- c(rates ,new_rate)
  }
  result <- cbind(result, rates)
}
dfResults <- as.data.frame(result)
# change column names
colnames(dfResults) = paste0("rates", c(1:N_sim))
# add period and row mean
period = seq(from = 0, by = dt, len = N_points+1)
rowMean <- rowMeans(dfResults)
dfResultsAdj <- cbind(period, rowMean, dfResults)
# convert to long - transpose the data frame
trResults <- pivot_longer(dfResultsAdj, cols = -c(period), values_to = "value", names_to = "rate")
# plot
p1 <- ggplot(data = trResults, mapping = aes(x = period, y = value, fill = rate, colour = rate)) +
  geom_line() +
  geom_smooth() +
  theme(legend.position="none")
p1

# calculating the integral
int_sum <- colSums(dfResults) * dt
# present value of integral sum
pv_int_sum <- exp(-int_sum)
# mean because the integral is the average
bond_price = fv * mean(pv_int_sum)
print(bond_price)
