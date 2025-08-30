source("Practical 8 functions.R")

### SET PARAMETERS for ALL MODELS ###
max.time <- 200
times <- 0:max.time

#######################################
###          Model 1                ###
### Simplest possible predator-prey ###
### model. Prey grow exponentially, ###
### No density dependence           ###
#######################################
# Set PARAMETERS FOR MODEL 1
a1     <-  0.0125  # /day  m, rate of ingestion
b1     <-  0.25   # -      , assimilation efficiency
d1     <-  0.1    # /day   , mortality rate of predator
r1     <-  0.2    # /day   , potential population growth rate of prey
N_init <- 30
P_init <- 20

# Run Model 1:
out1  <- calc1(a=a1, b=b1, d=d1, r=r1, yini=c(N=N_init, P=P_init), times=times)


# print out equilibrium population sizes
out1$equilibria

# Plot up results of Model 1
plot1(out1)

# This version of calc1 will run the model using the initial population sizes you indicate through N_init and P_init. Then it will run four additional versions of the model, beginning with many prey, few prey, many predators and few predators. THis shows the global stability of the model.
out1 <- calc1a(a=a1, b=b1, d=d1, r=r1, yini=c(N=N_init, P=P_init), times=times)
plot1a(out1, add = F)





#######################################
###          Model 2                ###
### Now introduce a carrying        ###
### capacity for the prey.          ###
### Rather than exponential growth, ###
### they now have logistic growth   ###
### in the absence of the predator. ###
#######################################

# Set PARAMETERS FOR MODEL 2
a2     <-  0.0125  # /day  m, rate of ingestion
b2     <-  0.25   # -      , assimilation efficiency
d2     <-  0.1    # /day   , mortality rate of predator
r2     <-  0.2    # /day   , potential population growth rate of prey
N_init <- 30
P_init <- 20
K2     <- 200    # carrying capacity for Prey

# Run Model 2N_init
out2 <- calc2(a=a2, r=r2, d=d2, b=b2, K=K2, times=times, yini = c(N = N_init, P = P_init))
# Obtain 'equilibrium' population sizes from Model 2
out2a$equilibria
# Plot up results of Model 2
plot2(out2)


# To determine the global stability of the model, we initialize the population sizes to have many prey, few prey, many predators and few predators
out2a <- calc2(a=a2, r=r2, d=d2, b=b2, K=K2, times=times, yini = c(N = 40, P = 30))
plot2(out2a, add = F)
out2b <- calc2(a=a2, r=r2, d=d2, b=b2, K=K2, times=times, yini = c(N = 40, P = 10))
plot2(out2b, add = T)
out2c <- calc2(a=a2, r=r2, d=d2, b=b2, K=K2, times=times, yini = c(N = 10, P = 30))
plot2(out2c, add = T)
out2d <- calc2(a=a2, r=r2, d=d2, b=b2, K=K2, times=times, yini = c(N = 10, P = 10))
plot2(out2d, add = T)


#######################################
###          Model 3                ###
### Now introduce a allee effects   ###
### together with carrying capacity ###
### for the prey population.        ###
#######################################

# Set PARAMETERS FOR MODEL 3
a3     <-  0.0125  # /day  m, rate of ingestion
b3     <-  0.25   # -      , assimilation efficiency
d3     <-  0.1    # /day   , mortality rate of predator
r3     <-  0.2    # /day   , potential population growth rate of prey
N_init <- 30
P_init <- 20
K3     <- 200
f3     <-   7      # Minimum sustainable population size for Prey

# Run Model 3:
out3  <- calc3(a=a3, b=b3, d=d3, r=r3, K=K3, f=f3, times=times)

# Obtain equilibrium population sizes from Model 3
out3$equilibria

# Plot up results of Model 3
plot3(out3)

# To determine the global stability of the model, we initialize the population sizes to have many prey, few prey, many predators and few predators
out3a <- calc3(a=a3, b=b3, d=d3, r=r3, K=K3, f=f3, times=times, yini = c(N = 15, P = 7))
plot3(out3a, add = F)
out3b <- calc3(a=a3, b=b3, d=d3, r=r3, K=K3, f=f3, times=times, yini = c(N = 40, P = 20))
plot3(out3b, add = T)
out3c <- calc3(a=a3, b=b3, d=d3, r=r3, K=K3, f=f3, times=times, yini = c(N = 40, P = 7))
plot3(out3c, add = T)
out3d <- calc3(a=a3, b=b3, d=d3, r=r3, K=K3, f=f3, times=times, yini = c(N = 15, P = 20))
plot3(out3d, add = T)
