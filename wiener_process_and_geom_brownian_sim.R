setwd("C:\\...\\financial_market_analysis")
getwd()

# Wiener process simulation

library(ggplot2)

theme_set(theme_minimal())

# number of samples
n <- 1000
# increment (variance)
dt <- .1
# starting value
x0 <- 0
# init vector with zeros
# n+1 timesteps
t <- seq(from = x0, by = dt, len = n+1)
# normal distribution with mean 0 and standard deviation = sqrt(variance)
norm_dist <- rnorm(n+1, mean = 0, sd = sqrt(dt))
# compute cumulative sum
W <- cumsum(norm_dist)
dfWP <- data.frame(time = t, price = W)
# plot
ggplot(dfWP, mapping = aes(x = time, y = price)) +
  geom_line(linewidth = 1, color = 'green') +
  ggtitle("Wiener Process")
# 1 sec pause to render the plot
Sys.sleep(1)

# geometric brownian motion simulation
# initial value of the stock
S0 <- .9453
# time interval - time horizon
T <- 2
# number of items to generate
n <- 1000
# mean
mu <- .1
# sigma value - basically 95% confidence level - random fluctiation around the mean
sigma <- .05
# time steps
dt <- T/n
print(dt)
# time steps
t <- seq(from = 0, to = T, len = n)
print(t)
# Wiener process walk - normal distribution
norm_dist <- rnorm(n, mean = 0, sd = sqrt(dt))
W <- cumsum(norm_dist)
# geometric random walk equation
S <- S0 * exp((mu - .5 * sigma^2) * dt + sigma * W)
print(S)
# create a date frame with results
dfGeomWalk <- data.frame(time = t, price = S)
# plot
ggplot(dfGeomWalk, mapping = aes(x = time, y = price)) +
  geom_line(linewidth = 1, color = 'green') +
  ggtitle("Geometric Random Walk")
