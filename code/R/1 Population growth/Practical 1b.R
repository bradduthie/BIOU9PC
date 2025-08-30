# Practical 1
# BIOU9PC Population and Community Ecology
# Exponential population growth

########################
# Discrete-time model  #
########################

N <- numeric()   # A vector to hold the population size predictions. 
N[1] <- 100      # The initial population size (N0)
r <- 1.1         # The intrinsic growth rate

# predictions can be made one time-step at a time. But this is boring and slow.
N[2] <- r*N[1]
N[3] <- r*N[2]

# Use loops instead!
max.time <- 20  # How many time-steps into the future do you want to project the population size?
for(t in 1:(max.time-1)){
  N[t+1] <- r*N[t]
}

# One of R's strengths is that it is easy to make graphs. A basic figure of predicted population size can be made with:
plot(1:max.time, N)
plot(1:max.time, N, xlab = 'Time step', col = 'red') # add an x-axis label & use red points
# see ?plot to see some of the many ways in which figures can be modified. 


##########################
# Continuous-time model  #
##########################
# Define pred.size(), the function that encapsulates the essence of the continuous-time exponential growth model. 
# It takes three parameters:
#   N0: initial population size
#   r:  intrinsic population growth rate
#   t:  the time into the future that we want a prediction for
# This function returns one or more population sizes, calculated depending upon the values of the parameters provided.
pred.size <- function(N0, r, t){
  Nt <- N0*exp(r*t)
  return(Nt)
}

# For example: 
initial.pop.size <- 100
intrinsic.growth.rate <- 0.1
time <- 100
pred.size(N0 = initial.pop.size, r = intrinsic.growth.rate, t = time)

# Note that we do not need to name the arguments AS LONG AS they are provided in the order expected by R. 
# This provides the exact sam eoutput:
pred.size(initial.pop.size, intrinsic.growth.rate, time)
# Naming the arguments is useful because it reminds the user what is going on.







