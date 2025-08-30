# Practical 9
# Population and Community Ecology BIOU9PC
# University of Stirling
# C. E. Timothy Paine
# 07/11/2016

### Source in the code for all models ###
source("http://guianaplants.stir.ac.uk/BIOU9PC/Practical_9_functions.R")

### SET PARAMETERS for ALL MODELS ###
max.time <- 200
times <- 0:max.time


#######################################
###          Model 1                ###
### Lotka-Volterra Competition      ###
### 2 consumers,                    ###
### resources are implicit          ###
### a11 & a22 are assumed to be 1   ###
#######################################

# MODEL PARAMETERS
K1  <- 200   # Carrying Capacity Species 1
K2  <- 190   # Carrying Capacity Species 2
r1  <- 0.15   # Maximum potential population growth rate Species 1
r2  <- 0.1   # Maximum potential population growth rate Species 1
a12 <- 0.5  # Effect of Species 1 on Species 2
a21 <- 0.6  # Effect of Species 2 on Species 1

# Run the model:
LVcomp   <- calcLVcomp(a12, a21, K1, K2) #
# NOTE: By default, this function will chooses starting population sizes at random. To provide population sizes, use the argument yini = c(N1 = <INITIAL SIZE 1>, N2 = <INITIAL SIZE 2>)

plotLVcomp(LVcomp) # Plot the outcome.
# NOTE: to add a line to the phase-plane diagram, use the argument 'add=T'.
LVcomp$outcome     # Print the outcome to the screen
LVcomp$eq          # Print the equilibrium population sizes to the screen




#######################################
###          Model 2                ###
### One consumer, one resource      ###
### Monod model                     ###
### Resources are explicit          ###
#######################################
# MODEL PARAMETERS
D    <- 0.1   # Flow rate through the system
R0   <- 7.6   # Concentration of resources entering into the system

r    <- 0.52  # Maximal per-capita increase of the consumer population
Ku   <- 6.21  # The concentration of Resource that allows consumer population growth to be r/2
b    <- 1     # Assimilation efficiency: How much resource is needed to make 1 new consumer?

# Run the model:
MONODcomp  <- calcMONODcomp(r, Ku, D, R0, b)
# NOTE: By default, this function will chooses starting population sizes at random. To provide population sizes, use the argument yini = c(Consumer = <INITIAL SIZE Consumer>, Resource = <INITIAL SIZE Resource>)
plotMONODcomp(MONODcomp)
# NOTE: to add a line to the phase-plane diagram, use the argument 'add=T'.
MONODcomp$eq  # Print the equilibrium population sizes to the screen



#######################################
###          Model 3                ###
### Two consumers, one resource     ###
### Monod model                     ###
### Resources are explicit          ###
#######################################
# MODEL PARAMETERS
D     <- 0.1   # Flow rate through the system
R0    <- 7.6   # Concentration of resources entering into the system

r1    <- 0.6  # Maximal per-capita increase of consumer1 population size
r2    <- 0.3  # Maximal per-capita increase of consumer2 population size
Ku1   <- 6.24  # The concentration of Resource that allows consumer1 population growth to be r1/2
Ku2   <- 6.18  # The concentration of Resource that allows consumer1 population growth to be r1/2
b1    <- 1.1  # Assimilation efficiency: How much resource is needed to make 1 new consumer1?
b2    <- 0.9  # Assimilation efficiency: How much resource is needed to make 1 new consumer2?

# Run the model:
MONODcomp2  <- calcMONODcomp2(r1, r2, Ku1, Ku2, D, R0, b1, b)
# NOTE: By default, this function will chooses starting population sizes at random. To provide population sizes, use the argument yini = c(Consumer1 = <INITIAL SIZE Consumer1>, Consumer2 = <INITIAL SIZE Consumer2>, Resource = <INITIAL SIZE Resource>)
plotMONODcomp2(MONODcomp2)
# NOTE that there is NO phase-plane diagram for this model. Making one would require 3 dimensions (one each for the two consumers, and one for the resource.)
MONODcomp2$eq  # Print the equilibrium population sizes to the screen

