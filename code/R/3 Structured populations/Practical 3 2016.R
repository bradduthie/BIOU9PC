# Practical 3
# Dynamics of structured populations
# BIOU9PC, Population and Community Ecology


# Read in the popbio library, which is useful for modeling structured populations.
if (!"popbio"  %in% installed.packages()[, 1]) {install.packages('popbio')} #  If it's not yet installed, this will install it.
require(popbio)

##### MATRIX MULTIPLICATION EXAMPLE ####
# Example of matrix multiplication to project population sizes one time-step into the future
A<-matrix(c(
    0.0, 1.1, 4.5,
    0.3, 0.0, 0.0,
    0.0, 0.5, 0.1), byrow=TRUE, nrow=3) # set up A, the transition matrix
N0 <-c(20,20,20)    # Set up N0, the vector of initial population sizes

A   # Print A out to the screen
N0  # Print N0 out to the screen

N1<-A %*% N0  # the operator %*% performs matrix multiplication
N1 #there will be 112 individulals in Age-class 1, 6 in age-class 2 and 12 in age-class 3.
## END of MATRIX MULTIPLICATION EXAMPLE ##



##### POPULATION PROJECTION EXAMPLE ####
# Example of matrix multiplication to project population sizes an ARBITRAY number of time-steps into the future
max.time <- 20     # How many time steps to project forward?
A<-matrix(c(
    0.0, 1.1, 4.5,
    0.3, 0.0, 0.0,
    0.0, 0.5, 0.1),byrow=TRUE, nrow=3) # set up A, the transition matrix
number.classes <- 3 # How many age/stage classes to use?
N  <-matrix(rep(NA,number.classes*max.time),nrow=number.classes)
N0 <-c(20,20,20)    # Set up N0, the vector of initial population sizes
if(length(N0) != number.classes){cat ("*** ERROR *** Length of N0 vector must match number.classes")}
N[,1] <- N0

for(t in 1:(max.time-1)){ # set up a loop to cycle up to the maximum time (-1)
    N[,t+1] <- A %*% N[,t] # Perform the matrix multipication
}
N # This prints the number of individuals in each class (rows) at every point in time (columns)

#### USEFUL FUNCTIONS TO ANALYSE STRUCTURED POPULATIONS ####
# Calculate total population size at each time
Ntot <- colSums(N) # Take the sum of each column, giving total population size at each time-step

# Calculate the observed population growth rate
dN_dt <- Ntot[-1]/Ntot[-max.time] # Note that negative subscripts drop that element out of a vector

# Calculate the fraction of the population in each size-/age-class at each time-step
Nperc <- sweep(N, 2, Ntot, "/")

# Calculate the asymptotic (ie, long-run expectation) population growth rate
lamb <- lambda(A)

# Calculate the asymptotic (ie, long-run expectation) age- or stage-class distribution
stable.dist <- stable.stage(A)


#### SOME USEFUL GRAPHS ####

# plot the projected population dynamics against time
barplot(N, xlab = 'Time step', ylab = 'Individuals', space = 0.1, border = NA, names.arg = 1:max.time, col = 1:number.classes)
legend('topleft', fill = number.classes:1, legend = number.classes:1, title = 'Class', inset = 0.1, border = NA, bg = 'white', bty = 'o') # Add a legend to the figure

# plot the proportion of individuals in each age- or size-class against time
barplot(Nperc, xlab = 'Time step', ylab = 'Proportion of individuals', space = 0.1, border = NA, names.arg = 1:max.time, col = 1:number.classes)
legend('bottomright', fill = number.classes:1, legend = number.classes:1, title = 'Class', inset = 0.1, border = NA, bg = 'white', bty = 'o') # Add a legend to the figure

# Plot the total population size against time
plot(x = 1:max.time, y = Ntot, type = 'l')

