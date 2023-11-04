setwd("C:\\...\\financial_market_analysis")
getwd()

# Wiener process simulation

library(ggplot2)

theme_set(theme_minimal())

#################### Ornstein-Uhlenbeck process ####################
# time step
dt <- .1
# Theta
th <- 1.2
# mean
mu <- .5
# variance around the mean
sigma <- .3
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
