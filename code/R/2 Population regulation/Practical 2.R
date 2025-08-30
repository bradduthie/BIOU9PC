# Practical 1
# BIOU9PC Population and Community Ecology
# Week 2
# Logistic population growth
# BIOU9PC, Population and Community Ecology
########################
# Discrete-time model  #
########################

#### Logistic Population Growth ####

N <- numeric()    # A vector to hold the population size predictions.
N[1] <- 15      # The initial population size (N0)
r <- 0.1          # The intrinsic growth rate
K <- 500
max.time <- 100  # How many time-steps into the future do you want to project the population size?
for(t in 1:(max.time-1)){
  N[t+1] <- N[t]+ r*N[t]*(1-N[t]/K)
}

# One of R's strengths is that it is easy to make graphs. A basic figure of predicted population size can be made with:
plot(1:max.time, N, type = 'b')


#### Maximum Sustainable Yield (MSY) ####
# Define population_growth_rate_calc(), a function that calculates population growth rates for populations growing according to the logistic model.
# It takes three parameters:
#   N:  current population size
#   r:  intrinsic population growth rate
#   K:  Carrying capacity
# This function returns one or more population population growth rates, calculated depending upon the values of the parameters provided. The units are "individuals per timestep"
population_growth_rate_calc <- function(r, N, K){
  H <- N*r*(1-N/K)
  return(H)
  }

K <- 100
r <- 0.1
N <- 1:100
PGR <- population_growth_rate_calc(N = N, K = K, r = r)
plot(N, PGR, ylab = expression("Change in population size"~~(dN/dt)), xlab = "Population size (N)")
max(PGR) # What is the maximal population growth rate?
N[which.max(PGR)] # at what population size is population growth rate maximal?


