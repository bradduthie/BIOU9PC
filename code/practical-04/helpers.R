# practical specific function/s
poisson_distribution <-  function(lambda=1.9){
  random_poisson <- rpois(n = 100000, lambda = lambda)
  xpos <- seq(min(random_poisson), max(random_poisson))
  return(list(
    "lambda" = lambda,
    "randP" = random_poisson,
    "xpos" = xpos,
    "density" = dpois(xpos,lambda)
  ))
}
binomial_distribution <- function(size, p){
  random_binomial <- rbinom(n = 10000, size = size, prob = p)
  xpos <- seq(min(random_binomial), max(random_binomial))
  return(list(
    "size" = size,
    "p" = p,
    "randB" = random_binomial,
    "xpos" = xpos,
    "density" = dbinom(xpos,size,p)
  ))
}
standard_distribution <- function(mean, sd){
  random_normal <- rnorm(n = 10000, mean = mean, sd = sd)
  xpos <- seq(min(random_normal), max(random_normal), length = 100)
  return(list(
    "mean" = mean,
    "sd" = sd,
    "randN" = random_normal,
    "xpos" = xpos,
    "density" = dnorm(xpos, mean, sd)
  ))
}
demographic_stochasticity <- function(max.time, b, d, N0, trials){
  N <- matrix(NA, nrow = trials, ncol = max.time) # a matrix to hold the projected population sizes
  Ndet <- rep(NA, max.time) # Same, but for a deterministic population. IE, one WITHOUT stochasticity
  N[,1] <- Ndet[1] <- N0
  for(t in 1:(max.time-1)){
    Deaths <- rbinom(trials, N[, t], d)
    N[, t+1] <- N[, t]  - Deaths
    Births <- rpois(trials, N[, t+1] * b)
    N[, t+1] <- N[, t+1] + Births
    N[, t+1] <- ifelse(N[, t+1] > 1, N[, t+1], 0) # if the population is less than 1, replace it with 0
    
    Ndet[t+1] <- Ndet[t]  - Ndet[t]*d
    Ndet[t+1] <- Ndet[t+1] + Ndet[t+1]*b
    Ndet[t+1] <- ifelse(Ndet[t+1] > 1, Ndet[t+1], 0) # if the population is less than 1, replace it with 0
    #   Ndet[t+1] <- Ndet[t] + Ndet[t]*b - Ndet[t]*d
  }
  CI <- apply(N, 2, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975))
  steps.Ndet.N <- data.frame(Timestep = 1:max.time,
                             Deterministic = Ndet,
                             Trial = t(N),
                             Mean  = colMeans(N))
  return(list(
    max.time = max.time,
    Ndet = Ndet,
    N = N,
    CI = CI,
    matrix = steps.Ndet.N
  ))
}

find_Ncontrol <- function(max.time, N0, b, d, K, sd, trials){
  Ncontrol_obs <- Ncontrol <-  matrix(NA, nrow = max.time, ncol = trials)	
  Ncontrol_obs[1,] <- Ncontrol[1,] <- N0
  Hcontrol <- matrix(0, nrow = max.time, ncol = trials)	
  for (t in 1:(max.time-1)){
    Deaths.t   <- fix_numbers(rbinom(trials, Ncontrol[t,], d))
    Ncontrol[t+1,] <- Ncontrol[t,] - Deaths.t * (1-Ncontrol[t,]/K)
    Births.t   <- fix_numbers(rpois(trials, Ncontrol[t+1,] * b))
    Ncontrol[t+1,]      <- round(Ncontrol[t+1,] + Births.t * (1-Ncontrol[t+1,]/K))
    Ncontrol_obs[t+1,]  <- fix_numbers(rlnorm(trials, meanlog = log(Ncontrol[t+1,]), sdlog=sd))
    }
  CI <- apply(Ncontrol_obs, 1, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975))
  return(list(N = Ncontrol_obs, CI = CI, H = Hcontrol))
}
find_Nconst <- function(max.time, N0, h, b, d, K, sd, trials){
  Nconst_obs <- Nconst <- Hconst <- matrix(NA, nrow = max.time, ncol = trials)	
  Nconst_obs[1,] <- Nconst[1,] <- N0
  Hconst <- matrix(round(h), nrow = max.time, ncol = trials)	
  for (t in 1:(max.time-1)){
    Deaths.t   <- fix_numbers(rbinom(trials, Nconst[t,], d))
    Nconst[t+1,] <- Nconst[t,] - Deaths.t * (1-Nconst[t,]/K)
    Births.t   <- fix_numbers(rpois(trials, Nconst[t+1,] * b))
    Nconst[t+1,]       <- round(Nconst[t+1,] + Births.t * (1-Nconst[t+1,]/K))
    Nconst_obs[t+1,] <- fix_numbers(rlnorm(trials, meanlog = log(Nconst[t+1,]), sdlog=sd))
    Hconst[t+1,] <- ifelse(Nconst[t+1,]>=h, h, Nconst[t+1,])
    Nconst[t+1,] <- Nconst[t+1,] - Hconst[t+1,]
  }
  CI <- apply(Nconst_obs, 1, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975))
  return(list(N = Nconst_obs, CI = CI, H = Hconst))
}
find_Nprop <- function(max.time, N0, b, d, K, sd, prop, trials){
  Nprop_obs <- Nprop <- Hprop <- matrix(NA, nrow = max.time, ncol = trials)	
  Nprop_obs[1,] <- Nprop[1,] <- N0
  for (t in 1:(max.time-1)){
    Deaths.t    <- fix_numbers(rbinom(trials, Nprop[t,], d))
    Nprop[t+1,] <- Nprop[t,] - Deaths.t * (1-Nprop[t,]/K)
    Births.t    <- fix_numbers(rpois(trials, Nprop[t+1,] * b))
    Nprop[t+1,] <- round(Nprop[t+1,] + Births.t * (1-Nprop[t+1,]/K))
    Nprop_obs[t+1,] <- fix_numbers(rlnorm(trials, meanlog = log(Nprop[t+1,]), sdlog=sd))
    Hprop[t+1,] <- round(Nprop_obs[t+1,] * prop)
    Nprop[t+1,] <- Nprop[t+1,] - Hprop[t+1,]
  }
  CI <- apply(Nprop_obs, 1, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975))
  return(list(N = Nprop_obs, CI = CI, H = Hprop))
}

find_Nthres <- function(max.time, N0, b, d, K, sd, prop_thresh, thresh, trials){
  Nthres_obs <- Nthres <- Hthres <- matrix(NA, nrow = max.time, ncol = trials)	
  Nthres_obs[1,] <- Nthres[1,] <- N0
  for (t in 1:(max.time-1)){
    Deaths.t     <- fix_numbers(rbinom(trials, Nthres[t,], d))
    Nthres[t+1,] <- Nthres[t,] - Deaths.t * (1-Nthres[t,]/K)
    Births.t     <- fix_numbers(rpois(trials, Nthres[t+1,] * b))
    Nthres[t+1,] <- round(Nthres[t+1,] + Births.t * (1-Nthres[t+1,]/K))
    Nthres_obs[t+1,] <- fix_numbers(rlnorm(trials, meanlog = log(Nthres[t+1,]), sdlog=sd))
    Hthres[t+1,] <- round(Nthres_obs[t+1,] * prop_thresh)
    Hthres[t+1,] <- ifelse(Nthres_obs[t+1,] > thresh, Hthres[t+1,], 0)
    Nthres[t+1,] <- Nthres[t+1,] - Hthres[t+1,]
  }
  CI <- apply(Nthres_obs, 1, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975))
  return(list(N = Nthres_obs, CI = CI, H = Hthres))
}

find_Nadapt <- function(max.time, N0, b, d, K, sd, flex, h_initial, span, trials){
  Nadapt_obs <- Nadapt <- Hadapt <- matrix(NA, nrow = max.time, ncol = trials)	
  Nadapt_obs[1,] <- Nadapt[1,] <- N0
  Hadapt[1,] <- round(h_initial)
  for (t in 1:(max.time-1)){
    Deaths.t     <- fix_numbers(rbinom(trials, Nadapt[t,], d))
    Nadapt[t+1,] <- Nadapt[t,] - Deaths.t * (1-Nadapt[t,]/K)
    Births.t     <- fix_numbers(rpois(trials, Nadapt[t+1,] * b))
    Nadapt[t+1,] <- round(Nadapt[t+1,] + Births.t * (1-Nadapt[t+1,]/K))
    Nadapt_obs[t+1,] <- fix_numbers(rlnorm(trials, meanlog = log(Nadapt[t+1,]), sdlog=sd))
    if(t < span){
      Hadapt[t+1,] <- round(h_initial)
    } else {
      H_test<- round(colMeans(Hadapt[t:(t-span),]) * (1+flex*(Nadapt_obs[t+1,]-Nadapt_obs[t-span+1,])/span)) 
      Hadapt[t+1,] <- ifelse(H_test > 0, H_test, 0)
      Hadapt[t+1,] <- ifelse(Nadapt_obs[t+1,] >= Hadapt[t+1,], Hadapt[t+1,], 0)
    }
    Nadapt[t+1,] <- Nadapt[t+1,] - Hadapt[t+1,]
  }
  CI <- apply(Nadapt_obs, 1, quantile, c(0.025, 0.25, 0.5, 0.75, 0.975))
  return(list(N = Nadapt_obs, CI = CI, H = Hadapt))
}

# set negative values, NaNs and NAs to zero
fix_numbers <- function(v){
  v[is.na(v)]  <- 0
  v[is.nan(v)] <- 0
  v[v < 0]     <- 0
  return(v)
}
# colour
brand.primary = "#006635"
brand.secondary = "#6E2C6B"
brand.tertiary = "#ed8231"
# plot style
plotstyle <- function(){
  par(
    pch=18, 
    bty="n", 
    pty="m", 
    cex=1.2, 
    lwd=1, 
    las=1, 
    family="sans", 
    font.axis=1, 
    mar=c(5,8,2,2), 
    mgp=c(2, 0.5, 0),
    tck=0.015
  )
}
# legend position
position_legend <- function(pos = "topright"){
  return(list(
    "pos" = pos
  ))
}