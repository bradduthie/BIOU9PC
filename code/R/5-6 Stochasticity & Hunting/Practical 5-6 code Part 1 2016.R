# Practical 5-6 Part 1
# BIOU9PC Population and Community Ecology
# Stochastic population models. 

#### Illustrate the poisson distribution ####
# The Poisson distribution expresses the probability of a given number of events
# That occur in a fixed interval of time or space, assuming that these events occur independently and with a known mean rate.
# So, it's good for birth rates
lambda<-1.9  # The mean rate. 

random_poisson <- rpois(n = 1000, lambda = lambda)
xpos <- seq(min(random_poisson), max(random_poisson))
density<-dpois(xpos,lambda) 

hist(random_poisson, breaks = c(-1, xpos)+0.5, probability = T)
lines(xpos, density, type = "h", lwd = 6) 
abline(v= lambda, col = 'red', lwd = 3)

#### Illustrate the binomial distribution. ####
# The binomial distribution gives the number of successes in a sequence 
# of independent yes/no experiments, each of which yields success with probability p.
# It's therefore useful for modeling mortality, since death can happen only once
# to each organism. For these purposes, we consider death as a success.
# The mean 
size <- 1000  # Number of trials. 
p   <- 0.01    # Probability of success (ie, probability of death)
random_binomial <- rbinom(n = 1000000, size = size, prob = p)
xpos    <- seq(min(random_binomial), max(random_binomial))
density <- dbinom(xpos,size,p)

hist(random_binomial, breaks = c(-1, xpos)+0.5, probability = T)
lines(xpos, density, type = "h", lwd = 6) 
abline(v= p*size, col = 'red', lwd = 3)

#### Illustrate the normal distribution ####
# The normal distribution is one of the most useful probability distribitions, because the  averages of random variables independently drawn from independent distributions converge to the normal. In other words, they become normally distributed when the number of random variables is sufficiently large. 
# For our purposes, it's useful because most measurement errors are small, with only a few large ones. So it's useful for expressing random variation in continuous variables, such as population size or individual size.
mean <- 1472
sd   <- 76
random_normal <- rnorm(n = 1000, mean = mean, sd = sd)
xpos <- seq(min(random_normal), max(random_normal))
density       <- dnorm(xpos, mean, sd)

hist(random_normal, breaks = 100, probability = T)
lines(xpos, density)
abline(v= mean, col = 'red', lwd = 3)
segments(mean-sd, max(density)/2, mean+sd, max(density)/2, col = 'red', lwd = 3)

#### Demonstrate demographic stochsticity ####
# Using the fundamental equation of population dynamics.
# We assume a closed system, so there is no migration. 
# We create <trials> replicate populations, each that start with <N0> individuals
# <b> and <d> are the baseline per-capita birth rates of birth and death
max.time <-  50        # How many timesteps to run the simulation
b        <-   0.07     # The mean number of offspring produced by each member of the population
d        <-   0.09     # The mean probability of death for every member of the population
N0       <- 200        # Initial population sizes
trials   <-  10        # How many replicate populations to generate

N <- matrix(NA, nrow = trials, ncol = max.time) # a matrix to hold the projected population sizes
Ndet <- rep(NA, max.time) # Same, but for a deterministic population. IE, one WITHOUT stochasticity
N[,1] <- Ndet[1] <- N0
for(t in 1:(max.time-1)){
  Births <- rpois(trials, N[, t] * b)
  Deaths <- rbinom(trials, N[, t] , d)
  N[, t+1] <- N[, t] + Births - Deaths
  Ndet[t+1] <- Ndet[t] + Ndet[t]*b - Ndet[t]*d
}
CI <- apply(N, 2, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975))

# Plot all the predicted population sizes from all of the trials
matplot(x = 1:max.time, y = t(N), type = "l", lty = 1, ylab = "Population size (N)", xlab = "Timesteps")
lines(x = 1:max.time, Ndet, lwd = 3, lty = 2)
legend("topleft", lty = 2, legend = c("Deterministic"), bty = 'n', inset = 0.05, cex = 1.5, lwd = 2,  border = NA)

# Plot the mean population size, plus the 50 and 95% confidence intervals around the mean
plot(x = 1:max.time, y = colMeans(N), lwd = 2, type = "l", ylab = "Population size (N)", xlab = "Timesteps", ylim = range(N))
lines(x = 1:max.time, Ndet, lwd = 3, lty = 2)
polygon(c(1:max.time, max.time:1), c(CI[1,], rev(CI[5,])), col = "#00000010", border = NA)
polygon(c(1:max.time, max.time:1), c(CI[2,], rev(CI[4,])), col = "#00000030", border = NA)
legend("topleft", lty = 1:2, legend = c("Stochastic", "Deterministic"), fill = c("#00000030", NA), bty = 'n', inset = 0.05, cex = 1.5, lwd = 2, merge = T, border = NA)

# Show how the variation in population size among replicate trials increases through time. 
plot(1:max.time, apply(N, 2, sd), xlab = "Timestep", ylab = "standard deviation of population size", type = "l")















