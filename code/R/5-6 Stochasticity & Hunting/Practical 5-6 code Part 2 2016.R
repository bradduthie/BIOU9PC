# Practical 5-6 Part 2
# BIOU9PC Population and Community Ecology
# Stochastic population models. 

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


#True population size (unobserved by the humans) 
Ntrue     <-rep(NA, max.time)	
Ntrue[1] <- N0
for (t in 1:(max.time-1)){
  Births.t <- rpois(1, Ntrue[t] * b)
  Deaths.t <- rbinom(1, Ntrue[t], d)
  Ntrue[t+1] <- round(Ntrue[t] + (Births.t-Deaths.t) * (1-Ntrue[t]/K))
}


#Constant hunting
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

#Proportional hunting
Nprop    <- Hprop <- rep(NA, max.time)	
Nprop[1] <- N0
Hprop[1] <- h
for (t in 1:(max.time-1)){
  Births.t <- rpois(1, Nprop[t] * b)
  Deaths.t <- rbinom(1, Nprop[t], d)
  Nprop[t+1]  <- round(Nprop[t] + (Births.t-Deaths.t) * (1-Nprop[t]/K))
  Nprop_obs   <- rlnorm(n=1, meanlog = log(Nprop[t]),sdlog=sd)
  Hprop[t+1]  <- round(Nprop_obs * prop)
  Nprop[t+1]  <- Nprop[t+1] - Hprop[t+1]
}

#Threshold proportional hunting
Nthres    <- Hthres <- rep(NA, max.time)	
Nthres[1] <- N0
Hthres[1] <- h
for (t in 1:(max.time-1)){
  Births.t <- rpois(1, Nthres[t] * b)
  Deaths.t <- rbinom(1, Nthres[t], d)
  Nthres[t+1] <- round(Nthres[t] + (Births.t-Deaths.t) * (1-Nthres[t]/K))
  Nthres_obs   <-rlnorm(n=1, meanlog=log(Nthres[t]), sdlog=sd)
  Hprop[t+1]  <- round(Nthres_obs * prop)
  Hthres[t+1] <- ifelse(Nthres_obs > thresh, Hprop[t+1], 0)
  Nthres[t+1] <- Nthres[t+1] - Hthres[t+1]
}

#Adaptive hunting
Nadapt_obs<- Nadapt    <- Hadapt <- rep(NA, max.time)	
Nadapt_obs[1]<- Nadapt[1] <- N0
Hadapt[1] <- h
for (t in 1:(max.time-1)){
  Births.t <- rpois(1, Nadapt[t] * b)
  Deaths.t <- rbinom(1, Nadapt[t], d)
  Nadapt[t+1]     <- round(Nadapt[t] + (Births.t-Deaths.t) * (1-Nadapt[t]/K))
  if(Nadapt[t+1]<0){Nadapt[t+1] <- 0}
  Nadapt_obs[t+1] <- rlnorm(n=1, meanlog=log(Nadapt[t+1]), sdlog=sd)	
  
  if(t < 5){
    Hadapt[t+1] <- round(h)
  }else{
    H_test<- round(mean(Hadapt[t:(t-3)]) * (1+flex*(Nadapt_obs[t+1]-Nadapt_obs[t-2])/4)) 
    Hadapt[t+1] <- ifelse(H_test > 0, H_test, 0)
  }
  Nadapt[t+1] <- Nadapt[t+1] - Hadapt[t+1]
}



# Assemble all the projected population sizes into one data.frame.
N <- data.frame(True = Ntrue, Constant = Nconst, Proportional = Nprop, Threshold = Nthres, Adaptive = Nadapt)

# Assemble all the projected hunting offtake into one data.frame.
H <- data.frame(Constant = Hconst, Proportional = Hprop, Threshold = Hthres, Adaptive = Hadapt)

#Plot up population sizes
par(mar = c(5, 5, 0, 0), las = 1, bty = "n")
matplot(N, type = "l", lty = 1, xlab = "Timestep", ylab = "population size", lwd = c(2, 1, 1, 1, 1, 1))
legend("topleft", legend = colnames(N), lwd =2, col = 1:6, cex= 1.0, inset = 0.05, bty = "n")

matplot(H, type = "l", xlab = "Timestep", ylab = "hunting yield", col = 2:6, lwd = 1, lty = 1)
legend("topleft", legend = colnames(H), lwd =2, col = 2:6, cex= 1.0, inset = 0.05, bty = "n")

# this defines a function to calculate the average annual variation
AAV_calc <- function(x){ mean(abs(diff(x)))}

AAV<-apply(H, MARGIN=2, FUN=aav_f)
mean_yield<-colMeans(H, na.rm = T)

AAV
mean_yield


