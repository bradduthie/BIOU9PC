# Practical 5-6 Part 2
# BIOU9PC Population and Community Ecology
# Stochastic population models.
# Demonstration of looping to obtain estimates from stochastic models.
# Note that you will still have to vary the relvant parameters by hand. THis code just gives you replication of a model, given a set of parameters.
# I illustrate this example using the constant hunting model. You could apply this to any of the models, however.

# Parameters that goven population dynamics. DO NOT change these without first consulting with Dr. Paine or one of the demonstrators.
b       <- 0.11  # Mean per-capita probability fecundity
d       <- 0.10  # Mean per-capita probabiliy of mortality
sd      <- 0.3	 # Standard deviation for estimating the population size. Smaller values mean that human observers more accurately estimate the population size.
K      <-10000	 # Carrying capacity
N0     <-  800	 # Starting population size
max.time<- 50 	 # Number of years the model is run

# Parameters that govern the hunting regimes
h      <-10		  # Constant harvest number
prop   <-0.02		# Proportional harvest rate
thresh <-800		# Harvest threshold
flex   <-0.01	  # Flexibility parameter for adaptive harvest

# define this code so that it can be used later
AAV_calc <- function(x){ mean(abs(diff(x)))}



# How many replicates to run?
n.trials <- 1000
# Set up a place to store the results

const_out <- data.frame(
    N_final = numeric(n.trials),
    H_mean  = numeric(n.trials),
    AAV     = numeric(n.trials))

for(i in 1:n.trials){
    Nconst    <- Hconst <- rep(NA, max.time)
    Nconst[1] <- N0
    Hconst[1] <- h
    for (t in 1:(max.time-1)){
        Births.t <- rpois(1, Nconst[t] * b)
        Deaths.t <- rbinom(1, Nconst[t], d)
        Nconst[t+1] <- round(Nconst[t] + (Births.t-Deaths.t) * (1-Nconst[t]/K))
        Hconst[t+1] <- round(h)
        Nconst[t+1] <- Nconst[t+1] - Hconst[t+1]
        if(Nconst[t+1]<0){Nconst[t+1] <- 0}
    }
    const_out$N_final[i] <- Nconst[max.time]
    const_out$H_mean     <- mean(Hconst)
    const_out$AAV[i]     <- AAV_calc(Hconst)
}

# to obtain mean values of each response value
apply(const_out, 2, mean)
#or, equivalently
colMeans(const_out)

# To obtain standard deviations of each response value
apply(const_out, 2, sd)


