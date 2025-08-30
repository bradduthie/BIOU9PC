# Practical 4
# C. E. Timothy Paine
# 06/10/2016


#### Levins' 1969 Metapopulation model
c <- 0.4
e <- 0.5
Prop <- 1:100/100
Pstar<-1-e/c

par(mar = c(5, 5, 1, 5), las = 1, bty = 'n', mfrow = c(1, 1), lwd = 2)
plot(Prop, c*Prop*(1-Prop)  , col = "forestgreen", type = "l", ylab = "", xlab = "Proportion of sites occupied", xlim = range(Prop, Pstar))
lines(Prop, e*Prop, col = "red")
abline(v = Pstar, col = "blue", lty = 2)
axis(4)
mtext("Colonization rate (c)", 2, col = "forestgreen", line = 3, las = 3)
mtext("Extinction rate (e)", 4, col = "red", line = 3, las = 3)
mtext("Pstar (1-e/c)", 3, col = "blue", at = Pstar)


#Code for Question 2

# Set up some variables
N.patch <- 5     # Number of patches
max.time <- 70   # How many years to project?
K       <- rep(300, N.patch) # Carrying capacity. Explore scenarios in which sub-populations have different carrying capacities.
r       <-  rep(0.05, N.patch) # intrinsic population growth rate.  Explore scenarios in which sub-populations have different intrinsic population growth rates.
N0      <- round(runif(N.patch, K/2, K)) # initial population sizes for each patch. Random numbers, uniformly distributed between K/2 and K*2
m       <- 0.1 # frequency of migration. Adjust to < 0.00001 to shut off migration

# Set up migration.
# Assume that patches are arranged linearly.
# The frequency of migration between two patches is inversely dependent on the distance between them.
M <- m/(1+abs(outer(1:N.patch, 1:N.patch, "-")))

# Visualize migration rates. Hotter colors mean that migration is more likely.
# More reddish colors indicate greater values
par(mfrow = c(1, 1))
image(x = 1:N.patch, y = 1:N.patch, 1-(M), xlab = "from patch", ylab = "to patch", axes = F, main = "Probability of migration", ylim = c(2+N.patch,0), asp = 1)
axis(1, at = 1:N.patch)
axis(2, at = 1:N.patch)
text(x = rep(1:N.patch, each = N.patch), y = rep(1:N.patch, times = N.patch), sprintf("%1.2f", M))
text(N.patch+1, 1:N.patch, sprintf("%1.2f", rowSums(M)-diag(M)), font = 2)
text(1:N.patch, N.patch+1, sprintf("%1.2f", colSums(M)-diag(M)), font = 2)
# Bold numbers around the edges indicate the marginal sums.

# Set up a place to hold population projections, as well as the number of migrants
N     <-  immigrants <- emigrants <- matrix(0, nrow = N.patch, ncol = max.time)
N[,1] <- N0     # Initialize population sizes

# Run the model
stopifnot(all.equal(N.patch, length(r), length(K), length(N0), nrow(M), ncol(M))) # error-checking to make sure that the input data is sett up correctly. This give NO output unless an error is detected.
for(t in 1:(max.time-1)){
  N[,t+1] <- (N[,t]+N[,t]*r*(1-(N[,t]/K)))     # each sub-population grows according to a logistic growth model
  if(t>=max.time/2){                         # Start migration 1/2 way through model run.
    emigrants[,t+1] <- (colSums(M)*N[,t+1])    # how many individuals leave each population?
    immigrants[,t+1] <- ((M/rowSums(M)) %*% emigrants[,t+1])   # Where do they go?
    N[,t+1] <-  N[,t+1] - emigrants[,t+1] + immigrants[,t+1] # Transfer the migrants
  }
}

#Plot (sub) population trajectories
par(mfrow = c(1, N.patch), mar = c(5, 5, 0, 0)) # Set up panels for each patch
for(i in 1:N.patch){
  plot(1:max.time, N[i,], col = i, xlab = "", ylab = "", yaxt = "n", ylim = c(0, max(N, K)*1.1), xlim = c(0, max.time), type = 'l') # Set up the base graphic
  ifelse(i == 1, axis(2), axis(2, labels = NA))
  mtext(paste(i, "\nr = ", round(r[i], 2), "\nK = ", K[i], sep = ""), font = 1, cex = 1.2, line = -6) # Annotate the figure
  segments(1, K[i], max.time, K[i], lty = 2, col = i)
  if(any(N[i,]== 0)){points(mean(which(N[i,]== 0)), 0, col = 'red', pch = 4, cex = 5)} # if sub-population extinction occurs, this marks it on the figure
}
title(xlab = "Timesteps", ylab = "(sub)-population size", outer = T, cex.lab= 2, line = -2)

# plot sub-population dynamics all on one xet of axes
par(mfrow = c(1, 1))
matplot(x = 1:max.time, y = t(N), type = "l", lty = 1, ylab = "Sub population size", xlab = "Timestep")
abline(h = K, lty = 2, col = 1:4, lwd = 1)
if(any(N== 0)){points(mean(which(apply(N, 2, min)== 0)), 0, col = 'red', pch = 4, cex = 5)} # if sub-population extinction occurs, this marks it on the figure
legend("topright", col = 1:N.patch, lwd = 2, legend = 1:N.patch, title = "Sub-population", bty = 'n', inset = 0.1)
abline(h = K, lty= 2)

# Plot number of emigrants
matplot(x = (max.time/2+1):max.time, y = t(emigrants[,(max.time/2+1):max.time]), type = "l", lty = 1, ylab = "N. emigrants", xlim = c(0, max.time), xlab = "Timestep")
legend("left", col = 1:N.patch, lwd = 2, legend = 1:N.patch, title = "Sub-population", bty = 'n', inset = 0.1)

# Plot number of immigrants
matplot(x = (max.time/2+1):max.time, y = t(immigrants[,(max.time/2+1):max.time]), type = "l", lty = 1, ylab = "N. immigrants", xlim = c(0, max.time),xlab = "Timestep")
legend("left", col = 1:N.patch, lwd = 2, legend = 1:N.patch, title = "Sub-population", bty = 'n', inset = 0.1)

# Plot net migration
matplot(x = (max.time/2+1):max.time, y = t(emigrants[,(max.time/2+1):max.time]) - t(immigrants[,(max.time/2+1):max.time]), type = "l", lty = 1, ylab = "Net migration gain (or loss)", xlim = c(0, max.time),xlab = "Timestep")
abline(h = 0, col = 'gray', lty = 3)
legend("left", col = 1:N.patch, lwd = 2, legend = 1:N.patch, title = "Sub-population", bty = 'n', inset = 0.1)
