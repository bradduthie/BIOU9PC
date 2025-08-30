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


# this defines a function to calculate the average annual variation
AAV_calc <- function(x){ mean(abs(diff(x)))}


#Constant hunting
hs <- seq(0, 25, by = 1)
out_const <- data.frame(h = hs, N_mean = NA, N_sd = NA, H_mean = NA, H_sd = NA, AAV_mean = NA, AAV_sd = NA)
for(i in 1:length(hs)){
    Nconst    <- Hconst <- matrix(NA, nrow = max.time, ncol = 1000)
    Nconst[1,] <- N0
    Hconst[1,] <- hs[i]
    for (t in 1:(max.time-1)){
        Births.t <- rpois(1000, Nconst[t,] * b)
        Deaths.t <- rbinom(1000, Nconst[t,], d)
        Nconst[t+1,] <- round(Nconst[t,] + (Births.t-Deaths.t) * (1-Nconst[t,]/K))
        Hconst[t+1,] <- round(hs[i])
        Nconst[t+1,] <- Nconst[t+1,] - Hconst[t+1,]
        Nconst[t+1,] <- ifelse(Nconst[t+1,]<0, 0, Nconst[t+1,])
    }
    out_const[i,] <- c(h = hs[i], N_mean = mean(Nconst[max.time,]), N_sd = sd(Nconst[max.time,]), H_mean = mean(Hconst), H_sd = sd(Hconst), AAV_mean = mean(apply(Hconst, 2, AAV_calc)), AAV_sd =mean(apply(Hconst, 2, AAV_calc)))
}
out_const$N_mean <- out_const$N_mean/10
out_const$N_sd <- out_const$N_sd/10



#Proportional hunting
props <- seq(0, 0.1, length = 100)
out_prop <- data.frame(prop = props, N_mean = NA, N_sd = NA, H_mean = NA, H_sd = NA, AAV_mean = NA, AAV_sd = NA)
for(i in 1:length(props)){

    Nprop    <- Hprop <- matrix(NA, nrow = max.time, ncol = 1000)
    Nprop[1,] <- N0
    Hprop[1,] <- props[i]*Nprop[1,]
    for (t in 1:(max.time-1)){
        Births.t <- rpois(1000, Nprop[t,] * b)
        Deaths.t <- rbinom(1000, Nprop[t,], d)
        Nprop[t+1,]  <- round(Nprop[t,] + (Births.t-Deaths.t) * (1-Nprop[t,]/K))
        Nprop_obs    <- rlnorm(n=1000, meanlog = log(Nprop[t,]),sdlog=sd)
        Hprop[t+1,]  <- round(Nprop_obs * props[i])
        Nprop[t+1,]  <- Nprop[t+1,] - Hprop[t+1,]
        Nprop[t+1,] <- ifelse(Nprop[t+1,] < 0, 0, Nprop[t+1,])

    }
    out_prop[i,] <- c(prop = props[i], N_mean = mean(Nprop[max.time,]), N_sd = sd(Nprop[max.time,]), H_mean = mean(Hprop), H_sd = sd(Hprop), AAV_mean = mean(apply(Hprop, 2, AAV_calc)), AAV_sd =mean(apply(Hprop, 2, AAV_calc)))
}
out_prop$N_mean <- out_prop$N_mean/10
out_prop$N_sd <- out_prop$N_sd/10



#Threshold proportional hunting
prop   <-0.02		# Proportional harvest rate
threshs <- seq(0, 1000, length = 100)
out_thresh <- data.frame(thresh = threshs, N_mean = NA, N_sd = NA, H_mean = NA, H_sd = NA, AAV_mean = NA, AAV_sd = NA)
for(i in 1:length(threshs)){
    Nthresh    <- Hthresh <- matrix(NA, nrow = max.time, ncol = 1000)
    Nthresh[1,] <- N0
    Hthresh[1,] <- Nthresh[1,] * prop
    for (t in 1:(max.time-1)){
        Births.t <- rpois(1000, Nthresh[t,] * b)
        Deaths.t <- rbinom(1000, Nthresh[t,], d)
        Nthresh[t+1,]  <- round(Nthresh[t,] + (Births.t-Deaths.t) * (1-Nthresh[t,]/K))
        Nthresh_obs    <- rlnorm(n=1000, meanlog = log(Nthresh[t,]), sdlog=sd)
        Hthresh[t+1,]  <- round(Nthresh_obs * prop)
        Hthresh[t+1,]   <- ifelse(Nthresh_obs > threshs[i], Hthresh[t+1,], 0)
        Nthresh[t+1,]  <- Nthresh[t+1,] - Hthresh[t+1,]
        Nthresh[t+1,]  <- ifelse(Nthresh[t+1,] < 0, 0, Nthresh[t+1,])
    }
    out_thresh[i,] <- c(thresh = threshs[i], N_mean = mean(Nthresh[max.time,]), N_sd = sd(Nthresh[max.time,]), H_mean = mean(Hthresh), H_sd = sd(Hthresh), AAV_mean = mean(apply(Hthresh, 2, AAV_calc)), AAV_sd =mean(apply(Hthresh, 2, AAV_calc)))
}
out_thresh$N_mean <- out_thresh$N_mean/10
out_thresh$N_sd   <- out_thresh$N_sd/10




#Adaptive hunting
h      <-10		  # Constant harvest number
flexs <- seq(0, 0.1, length = 100)
out_adapt <- data.frame(flex = flexs, N_mean = NA, N_sd = NA, H_mean = NA, H_sd = NA, AAV_mean = NA, AAV_sd = NA)
for(i in 1:length(threshs)){
    Nadapt_obs <- Nadapt    <- Hadapt <- matrix(NA, nrow = max.time, ncol = 1000)
    Nadapt_obs[1,] <- Nadapt[1,] <- N0
    Hadapt[1,] <- h
    for (t in 1:(max.time-1)){
        Births.t <- rpois(1000, Nadapt[t,] * b)
        Deaths.t <- rbinom(1000, Nadapt[t,], d)
        Nadapt[t+1,] <- round(Nadapt[t,] + (Births.t-Deaths.t) * (1-Nadapt[t,]/K))
        Nadapt[t+1,] <- ifelse(Nadapt[t+1,] < 0, 0, Nadapt[t+1,])
        Nadapt_obs[t+1,] <- rlnorm(n=1000, meanlog=log(Nadapt[t+1,]), sdlog=sd)
        if(t < 5){
            Hadapt[t+1,] <- round(h)
        }else{
            H_test<- round(colMeans(Hadapt[t:(t-3),]) * (1+flexs[i]*(Nadapt_obs[t+1,]-Nadapt_obs[t-2,])/4))
            Hadapt[t+1,] <- ifelse(H_test > 0, H_test, 0)
        }
        Nadapt[t+1,] <- Nadapt[t+1,] - Hadapt[t+1,]
        Nadapt[t+1,]  <- ifelse(Nadapt[t+1,] < 0, 0, Nadapt[t+1,])

    }
    out_adapt[i,] <- c(flex = flexs[i], N_mean = mean(Nadapt[max.time,]), N_sd = sd(Nadapt[max.time,]), H_mean = mean(Hadapt), H_sd = sd(Hadapt), AAV_mean = mean(apply(Hadapt, 2, AAV_calc)), AAV_sd =mean(apply(Hadapt, 2, AAV_calc)))
}
out_adapt$N_mean <- out_adapt$N_mean/10
out_adapt$N_sd   <- out_adapt$N_sd/10



### plot up results
par(mfrow = c(2, 2), las = 1, bty = 'n', tcl = 0.2, mar =c(5, 3, 0, 1), oma = c(1, 1, 1, 1), xpd = NA)
matplot(x = out_const$h, y = out_const[,c(2, 4, 6)], type = 'l', lty = 1, ylab = '', main = "Constant", xlab = "Animals hunted", lwd = 2, ylim = c(0, 80))
abline(h = N0/10, col = 'gray', xpd = F)
segments(out_const$h, out_const$N_mean+ out_const$N_sd, out_const$h, out_const$N_mean- out_const$N_sd)
segments(out_const$h, out_const$H_mean + out_const$H_sd, out_const$h, out_const$H_mean- out_const$H_sd)

matplot(x = out_prop$prop, y = out_prop[,c(2, 4, 6)], type = 'l', lty = 1, ylab = 'N', main = "Proportional", xlab = "Proportion", lwd = 2, ylim = c(0, 80))
abline(h = N0/10, col = 'gray', xpd = F)
segments(out_prop$prop, out_prop$N_mean+ out_prop$N_sd, out_prop$prop, out_prop$N_mean- out_prop$N_sd, )
segments(out_prop$prop, out_prop$H_mean + out_prop$H_sd, out_prop$prop, out_prop$H_mean- out_prop$H_sd, col = 'red')

matplot(x = out_thresh$thresh, y = out_thresh[,c(2, 4, 6)], type = 'l', lty = 1, pch = '.', ylab = 'N', lwd = 2, main = "Threshold proportional", xlab = "Threshold", ylim = c(0, 80))
abline(h = N0/10, col = 'gray', xpd = F)
segments(out_thresh$thresh, out_thresh$N_mean+ out_thresh$N_sd, out_thresh$thresh, out_thresh$N_mean - out_thresh$N_sd)
segments(out_thresh$thresh, out_thresh$H_mean + out_thresh$H_sd, out_thresh$thresh, out_thresh$H_mean- out_thresh$H_sd, col = 'red')

matplot(x = out_adapt$flex, y = out_adapt[,c(2, 4, 6)], type = 'l', lty = 1, pch = '.', ylab = 'N', lwd = 2, main = "Adaptive", xlab = "Flexibility", ylim = c(0, 80))
abline(h = N0/10, col = 'gray', xpd = F)
segments(out_adapt$flex, out_adapt$N_mean+ out_adapt$N_sd, out_adapt$flex, out_adapt$N_mean - out_adapt$N_sd)
segments(out_adapt$flex, out_adapt$H_mean + out_adapt$H_sd, out_adapt$flex, out_adapt$H_mean- out_adapt$H_sd, col = 'red')
title(ylab = "N (black), Offtake (red), AAV (green)", outer = T, line = -1)




# Another way to do it:
# plot offtake versus popultion size
par(mfrow = c(2, 2), las = 1, bty = 'n', tcl = 0.2, mar =c(5, 3, 0, 1), oma = c(1, 1, 1, 1), xpd = NA)
plot(N_mean ~ H_mean, data = out_const, type = 'l', lwd = 2)
segments(out_const$H_mean, out_const$N_mean- out_const$N_sd, out_const$H_mean, out_const$N_mean+ out_const$N_sd)
abline(h = N0/10, col = 'gray', xpd = F)

plot(N_mean ~ H_mean, data = out_prop, type = 'l', lwd = 2)
segments(out_prop$H_mean, out_prop$N_mean- out_prop$N_sd, out_prop$H_mean, out_prop$N_mean+ out_prop$N_sd)
abline(h = N0/10, col = 'gray', xpd = F)

plot(N_mean ~ H_mean, data = out_thresh, type = 'l', lwd = 2)
segments(out_thresh$H_mean, out_thresh$N_mean- out_thresh$N_sd, out_thresh$H_mean, out_thresh$N_mean+ out_thresh$N_sd)
abline(h = N0/10, col = 'gray', xpd = F)

plot(N_mean ~ H_mean, data = out_adapt, type = 'l', lwd = 2)
segments(out_adapt$H_mean, out_adapt$N_mean- out_adapt$N_sd, out_adapt$H_mean, out_adapt$N_mean+ out_adapt$N_sd)
abline(h = N0/10, col = 'gray', xpd = F)

