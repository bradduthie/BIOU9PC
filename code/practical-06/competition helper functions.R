# Code placed in this file fill be executed every time the
# lesson is started. Any variables created here will show up in
# the user's working directory and thus be accessible to them
# throughout the lesson.
my_ode <- function(y, times, func, parms) {
  # y: named vector of initial conditions (e.g., c(N=1, P=2))
  # times: vector of time points
  # func: the model function
  # parms: list of parameters
  
  # Initialize output matrix
  n <- length(times)
  n_vars <- length(y)
  out <- matrix(NA, nrow = n, ncol = n_vars + 1)
  colnames(out) <- c("time", names(y))
  out[1, ] <- c(times[1], y)
  
  # RK4 algorithm
  for (i in 1:(n-1)) {
    t <- times[i]
    h <- times[i+1] - times[i]
    current_y <- out[i, -1]  # Get current state (exclude time column)
    
    k1 <- func(t, current_y, parms)[[1]]
    k2 <- func(t + h/2, current_y + (h/2)*k1, parms)[[1]]
    k3 <- func(t + h/2, current_y + (h/2)*k2, parms)[[1]]
    k4 <- func(t + h, current_y + h*k3, parms)[[1]]
    
    # Update state
    new_y <- current_y + (h/6) * (k1 + 2*k2 + 2*k3 + k4)
    out[i+1, ] <- c(times[i+1], new_y)
  }
  
  return(as.data.frame(out))
}


#### DEFINE THE MODELS ###
LVcomp1 <- function(time, state, params) {
  N1  <- state[1]
  N2  <- state[2]
  K1  <- params$K1
  K2  <- params$K2
  r1  <- params$r1
  r2  <- params$r2
  a12 <- params$a12
  a21 <- params$a21
  with(as.list(c(state, params)), {
    dN1 <- r1*N1*(K1 - N1 - a21*N2)/K1
    dN2 <- r2*N2*(K2 - N2 - a12*N1)/K2
    return(list(c(dN1, dN2)))
  })
}

# One-species monod competition with explicit resources. Stable
monodMod <- function(time, state, params) {
  Consumer  <- state[1]
  Resource  <- state[2]
  r  <- params$r
  D  <- params$D
  Ku <- params$Ku
  R0 <- params$R0
  with(as.list(c(state, params)), {
    Ingestion <- r*Resource/(Ku+Resource)
    dConsumer <- Ingestion-D
    dResource <- D*(R0-Resource) - Consumer*Ingestion
    return(list(c(dConsumer, dResource)))
  })
}

# Two-species monod competition with explicit resources. Unstable
monodMod2 <- function(time, state, params) {
  Consumer1  <- state[1]
  Consumer2  <- state[2]
  Resource   <- state[3]
  r1  <- params$r1
  r2  <- params$r2
  D   <- params$D
  Ku1 <- params$Ku1
  Ku2 <- params$Ku2
  R0  <- params$R0
  
    with(as.list(c(state, params)), {
    Ingestion1 <- r1*(Resource/(Ku1+Resource))
    Ingestion2 <- r2*(Resource/(Ku2+Resource))
    dConsumer1 <- Consumer1*Ingestion1-Consumer1*D
    dConsumer2 <- Consumer2*Ingestion2-Consumer2*D
    dResource  <- D*(R0-Resource) -(Consumer1)*Ingestion1-(Consumer2)*Ingestion2
#    browser()
    return(list(c(dConsumer1, dConsumer2, dResource)))
  })
}

# Predict outcome of LV competition, given parameters
outcomePredictor1 <- function(a12, a21, K1, K2) {
  if (K2 > (K1/a12)) {
    if (K1 > (K2/a21)) {
      outcome <- "Outcome depends on initial conditions: either 1 or 2 wins"
    } else {
      outcome <- "Species 2 always wins"
    }
  } else {
    if (K1 > (K2/a21)) {
      outcome <- "Species 1 always wins"
    } else {
      outcome <- "Stable coexistence"
    }
  }
  return(outcome)
}

# Calculate ZNGIs for each species
#N1star <- function(N1, K1, a21){(K1-N1)/a21}
#N2star <- function(N1, k2, a12){K2-a12*N1}

# Run the calculations for teh LV competition model
calcLVcomp <- function(a12, a21, K1, K2, r1, r2, yini, times){
  outcome <- outcomePredictor1(a12, a21, K1, K2)
  N1star  <- (K1-a21*K2)/(1-a12*a21)
  N2star  <- (K2-a12*K1)/(1-a12*a21)
  
  outLVa <- data.frame(my_ode(yini, times, LVcomp1, parms = list(K1 = K1, K2 = K2, r1 = r1, r2 = r2, a12 = a12, a21 = a21)))
  outLVb <- expand.grid(N1 = seq(1, max(K1, outLVa$N1), length = 6), N2 = seq(1, max(K2, outLVa$N2), length = 6), N1_end = NA, N2_end = NA)
  for(i in 1:nrow(outLVb)){
    outLVb[i,3:4] <- my_ode(c(N1 = outLVb$N1[i], N2 = outLVb$N2[i]), times = 0:1, LVcomp1, parms = list(K1 = K1, K2 = K2, r1 = r1, r2 = r2, a12 = a12, a21 = a21))[2,2:3]
  }
  return(list(
    predictions = outLVa, 
    phase_plane = outLVb, 
    outcome = outcome, 
    equilibria = c(N1star = N1star, N2star = N2star), 
    initial_pop_size = yini))
}


# Plot population trajectories though timeup predictions from the LV competition model
plotLVtraj <- function(input, max.time){
  plotstyle()
   options(warn = -1)
  # Set up graphs to examime the model projections
  # Plot population trajectories over time
  
  matplot(input$predictions[,-1], type = "n", xlab = "Time", ylab = "Population sizes")
  if(any(input$predictions$N1 < 1)){
    points(which(input$predictions$N1 < 1)[1], 0, col = "blue", lwd = 2, pch = 1, cex = 3)
    input$predictions$N1[which(input$predictions$N1 < 1)[-1]] <- NA}
  if(any(input$predictions$N2 < 1)){
    points(which(input$predictions$N2 < 1)[1], 0, col = "blue", lwd = 2, pch = 1, cex = 3)
    input$predictions$N2[which(input$predictions$N2 < 1)[-1]] <- NA}
  matlines(input$predictions[,-1], type = "l",  col = "blue", lty = 1:2, lwd = 3)
  legend("topright", c("Species 1", "Species 2"), text.col = "blue", box.lwd = 0, lty = 1:2, col = "blue", inset = 0.1)
}

# Plot up predictions from the LV competition model
plotLVpp <- function(input1, max.time, K1, K2, a12, a21, ...){
  plotstyle()
   pred <- input1$predictions
  p_p  <- input1$phase_plane
  #  eq   <- input1$equilibria
  options(warn = -1)
  # Plot Phase-plane: Resource vs Consumer population dynamics, including the force-field.
  plot(p_p$N1, p_p$N2, type = "n", ylab = expression(Species~2~population~size~(N[2])), xlab = expression(Species~1~population~size~(N[1])))
  if(a21 == 0){abline(v = K1, lwd = 2)} else{curve((K1-x)/a21, 1, 10000, add = T, lwd = 2)}
  curve(K2-a12*x, 1, 10000, add = T, lty = 2, lwd = 2)
  arrows(p_p$N1, p_p$N2, p_p$N1_end, p_p$N2_end, col = "gray", length = 0.05, xpd = NA)
  
  legend('top', ncol = 2, lty = 1:2, legend = expression(N[1]^'*', N[2]^'*'), bty ='0', inset = -0.04, xpd = NA, seg.len = 4, bg = 'white')
  segments(pred$N1[-max.time], pred$N2[-max.time], pred $N1[-1], pred$N2[-1], col = rainbow(max.time, end = 0.7), lwd = 3, xpd = NA)
  points(pred$N1[1], pred$N2[1], cex = 2, xpd = NA)
  text(pred$N1[1], pred$N2[1], "Start", pos = 4, cex = 0.7, xpd = NA)
  points(pred$N1[max.time], pred$N2[max.time], cex = 2, pch = 16, xpd = NA)
  text(pred$N1[max.time], pred$N2[max.time], "End", pos = 4, cex = 0.7, xpd = NA)
}



# Run the calculations for the Monod one species, one resource model
calcMONODcomp <- function(r, Ku, D, R0, yini, times){
  Rstar <- D*Ku/(r-D)   # The equilibrium concentration of Resource (dR/dt = 0)
  Nstar <- (R0-Rstar)   # The equilibrium concentration of Consumer (dN/dt = 0)
  outMONODa  <- data.frame(my_ode(yini, times, monodMod, parms = list(r = r, Ku = Ku, D=D, R0=R0)))
  outMONODa[outMONODa<0] <- 0
  outMONODb <- expand.grid(Consumer = seq(0, max(Nstar, outMONODa$Consumer)*1.2, length =6), Resource = seq(0, max(Rstar, outMONODa$Resource)*1.2, length = 6), Consumer_end = NA, Resource_end = NA)
  for(i in 1:nrow(outMONODb)){
    outMONODb[i,3:4] <- my_ode(c(Consumer = outMONODb$Consumer[i], Resource = outMONODb$Resource[i]), times = 0:1, monodMod, parms = list(r=r, Ku=Ku, D=D, R0=R0))[2,2:3]
  }
  
  return(list(
    predictions = outMONODa, 
    phase_plane = outMONODb, 
    equilibria = c(Nstar = Nstar, Rstar = Rstar), 
    initial_pop_size = yini))
}

# Plot up predictions from the monod one-species competition model
plotMONODtraj <- function(input, max.time, log){
    # Set up graphs to examime the model projections
    # Plot population trajectories over time
  plotstyle()
  pred <- input$predictions
   matplot(pred[,-1], type = "n", xlab = "time", ylab = "Population sizes", ylim = range(pred[,-1], input$equilibria), log = ifelse(log, 'y', ''))
  
  if(any(pred[,2] == 0)){
    points(which(pred[,2] == 0)[1], 0, col = "blue", lwd = 2, pch = 1, cex = 3)
    pred[which(pred[,2] == 0)[1]:nrow(pred),2] <- NA
    }
  if(any(pred[,3] == 0)){
    points(which(pred[,3] == 0)[1], 0, col = "green", lwd = 2, pch = 1, cex = 3)
    pred[which(pred[,3] == 0)[1]:nrow(pred),2] <- NA
  }
  matlines(pred[,-1], type = "l", col = c("blue", "green"), lty = 1, lwd = 2)
  
    abline(h = input$equilibria, col = c("blue", "green"), lty = 3)
    mtext(expression(R^'*', N^'*'), at = c(input$equilibria[2], input$equilibria[1]), side = 4, col = c("green", "blue"), cex = 1.2)
    
    legend("topright", c("Consumer", "Resource"), text.col = c("blue", "green"), box.lwd = 0, lty = 1, col = c("blue", "green"), inset = 0.1)
}

# Plot Phase-plane: Resource vs Consumer population dynamics, including the force-field.
plotMONODpp <- function(input1, max.time){
  plotstyle()
    pred <- input1$predictions
  p_p  <- input1$phase_plane
  plot(p_p$Resource, p_p$Consumer, type = "n", ylab = "Consumer concentration (N)", xlab = "Resource concentration (R)")
  arrows(p_p$Resource, p_p$Consumer, p_p$Resource_end, p_p$Consumer_end, col = "gray", length = 0.05, xpd = NA)
  abline(v = input1$equilibria[2], h = input1$equilibria[1], col = c("blue", "green"), lty = 3)
  legend('top', ncol = 2, lty = 3, legend = expression(ZNGI[R], ZNGI[N]), bty ='n', inset = -0.04, xpd = NA, seg.len = 4, col = c("green", "blue"), text.col = c("green", "blue"))
  
  segments(pred$Resource[-max.time], pred$Consumer[-max.time], pred $Resource[-1], pred$Consumer[-1], col = rainbow(max.time, end = 0.7), lwd = 2, xpd = NA)
  points(pred$Resource[1], pred$Consumer[1], cex = 2, xpd = NA)
  text(pred$Resource[1], pred$Consumer[1], "Start", pos = 4, cex = 0.7, xpd = NA)
  points(pred$Resource[max.time], pred$Consumer[max.time], cex = 2, pch = 16, xpd = NA)
  text(pred$Resource[max.time], pred$Consumer[max.time], "End", pos = 4, cex = 0.7, xpd = NA)
}


# Run the calculations for the Monod two species, one resource model
calcMONODcomp2 <- function(r1, r2, Ku1, Ku2, D, R0, yini, times){
  Rstar1 <- D*Ku1/(r1-D)  # The equilibrium concentration of Resource with Consumer1
  Rstar2 <- D*Ku2/(r2-D)  # The equilibrium concentration of Resource with Consumer2
  Nstar1 <- (R0-Rstar1)  # The equilibrium concentration of Consumer1 (dN1/dt = 0)
  Nstar2 <- (R0-Rstar2)  # The equilibrium concentration of Consumer2 (dN2/dt = 0)
  Rstar <- min(Rstar1, Rstar2) # take the minimum of those 2 Rstars.
  outMONODa  <- data.frame(my_ode(yini, times, monodMod2, parms = list(r1 = r1, r2 = r2, Ku1 = Ku1, Ku2 = Ku2, D = D, R0 = R0)))
  outMONODa[outMONODa < 0] <- 0
  
  return(list(predictions = outMONODa, equilibria = c(Nstar1 = Nstar1, Nstar2 = Nstar2, Rstar = Rstar), initial_pop_size = yini))
}

# Plot up predictions from the monod two-species competition model
plotMONODcomp2 <- function(input1, log){
  pred <- input1$predictions
  eq   <- input1$equilibria
  #browser()
  # Set up graphs to examime the model projections
  # Plot population trajectories over time
  plotstyle()
  if(log ==T & (any(pred[,-1]<=0) | any(eq <= 0))){
    plot.new()
    mtext("One of the consumers went extinct.\nUn-tick the 'Log Y axis?' box to see which.", col = 'red', font = 2)
  } else {
    matplot(pred[,-1], type = "l", xlab = "time", ylab = "Concentrations of Resource and Consumer", col = c("blue", "blue", "green"), lty = c(1,2,1), lwd = 2, ylim = range(pred[,-1], eq), log = ifelse(log, 'y', ''))
  abline(h = eq, col = c("blue", "blue", "green"), lty = 3)
  mtext(expression(N[1]^'*', N[2]^'*', R^'*'), at = eq, side = 4, col = c("blue", "blue", "green"), cex = 1.2)
  if(eq[1]<0){mtext("N1*<0: Consumer 1 goes extinct", col = 'blue', line = -4)}
  if(eq[2]<0){mtext("N2*<0: Consumer 2 goes extinct", col = 'blue', line = -4)}
  legend("topright", c("Consumer1", "Consumer2", "Resource"), text.col = c("blue", "blue", "green"), col = c("blue", "blue", "green"), box.lwd = 0, inset = 0.1, lty = c(1, 2, 1))
  }
}
