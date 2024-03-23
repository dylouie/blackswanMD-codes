library(boot)
library(moments)
library(ggplot2)

# generate 10000 observations from a population of size N(10000, 500)
n_obs <- 10000
population <- rnorm(n_obs, 100000, 500)

# defining the statistical model, events of IF and no_IF, and relative risk (RR)
boot.RR <- function(x, i) {
  x_IF <- rbinom(x, 20078, 31/20078)
  x_noIF <- rbinom(x, 20078, 31/20078/1.91)
  RR <- x_IF/x_noIF
  return(RR[i])
}

# setting 1000 trials
n_trials <- 1000

# bootstrapping and calculating 95% confidence intervals
mono <- boot(sample(population, n_obs), boot.RR, R = n_trials)
mono.ci <- boot.ci(mono, conf = c(0.90, 0.95, 0.99), type = "basic")
mono.data <- data.frame(mono$t)
mono.ci

# plotting
mono.plot <- ggplot(mono.data) +
  geom_density(aes(x = X1), stat = "density") +
  geom_vline(xintercept = 1) +
  scale_x_log10() + 
  labs(x = "Relative Risk (RR)", y = "Density", title = "Density of Resampled Relative Risk")
mono.plot