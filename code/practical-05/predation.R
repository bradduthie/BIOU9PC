# LotkaVolterra Model
# passed to ode() with time, state and params
# (state must be numeric so list type is automatic)
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

lotkavol.model <- function(time, state, params){
  N <- state[1]
  P <- state[2]
  a <- params$a
  b <- params$b
  d <- params$d
  r <- params$r
  f <- params$f
  Ref  <- params$Ref
  K    <- params$K
  beta <- params$beta
  
  model <- params$model
  with(as.list(c(state, params)), {
    P[P < 1] <- 0
    N[N < 1] <- 0
    Ingestion <- a * N * P
    if(model == 1){
      dN <- r * N - Ingestion
    } else if(model == 2){
      dN <- r * N * (1 - N/K) - Ingestion
    } else if(model == 3){
      dN <- r * (N - f) * (1 - N/K) - Ingestion
    } else if(model == 4){
      dN <- r * (N + Ref) - Ingestion
    }
#    if(model < 5){
      dP <- Ingestion * b - d * P
#    } else {
    #   Ingestion <- N*P*a/(1-beta*P)
    #   dN <- r * N - Ingestion
    #   dP <- Ingestion * b - d * P
    # }
    return(list(c(dN, dP)))
  })
}


calc1 <- function(a, r, d, b, yini, times){
  Nstar <-  r/a
  Pstar <-  d/(a * b)
  params <- list(model=1, a=a, r=r, d=d, b=b, f=NULL, Ref = NULL, K=NULL, beta=NULL)
  out1a  <- data.frame(my_ode(yini, times, lotkavol.model, params))
  out1a[out1a < 0 ] <- 0
  out1b <- expand.grid(
    N = seq(min(out1a$N, 0.9*Pstar), max(out1a$N, 1.1*Pstar), length = 5), 
    P = seq(min(out1a$P, 0.9*Nstar), max(out1a$P, 1.1*Nstar), length = 5), 
    N_end = NA, 
    P_end = NA
  )
  for(i in 1:nrow(out1b)){
    out1b[i,3:4] <- my_ode(c(N = out1b$N[i], P = out1b$P[i]), 0:1, 
                                 lotkavol.model, params)[2,2:3]
  }
  return(list(
    predictions = out1a, 
    phase_plane = out1b, 
    params = list(a=a, r=r, d=d, b=b), 
    equilibria = c(Nstar = Nstar, Pstar = Pstar), 
    initial_pop_size = yini
  ))
}

calc2 <- function(a, r, d, b, K, yini, times){
  options(warn = -1); on.exit(options(warn = 0))
  # Determine the equilibrium population sizes
  Pstar <- d/(a*b)
  Nstar <- Nstar2(a, r, K, Pstar)
  params <- list(model=2, a=a, r=r, d=d, b=b, f=NULL, Ref = NULL, K=K, beta=NULL)
  out2a <- data.frame(my_ode(yini, times, lotkavol.model, params))
  out2a[out2a < 0] <- 0
  out2b <- expand.grid(
    N = seq(min(1, out2a$N, 0.9*Pstar), max(out2a$N, 1.1*Pstar), length = 5), 
    P = seq(min(1, out2a$P, 0.9*Nstar), max(Nstar2(a, r, K, 0), out2a$P, 1.1*Nstar), length = 5), 
    N_end = NA, 
    P_end = NA
  )
  for(i in 1:nrow(out2b)){
    out2b[i,3:4] <- my_ode(c(N = out2b$N[i], P = out2b$P[i]), 0:1, lotkavol.model, params)[2,2:3]
  }
  return(list(
    predictions = out2a, 
    phase_plane = out2b, 
    params = list(a=a, r=r, d=d, b=b, K=K), 
    equilibria = c(Nstar = Nstar, Pstar = Pstar), 
    initial_pop_size = yini
  ))
}

calc3 <- function(a, r, d, b, K, f, yini, times){
  options(warn = -1); on.exit(options(warn = 0))
  # Determine the equilibrium population sizes
  Pstar <- d/(a*b)
  Nstar <- Nstar3(a=a, r=r, K=K, f=f, N=Pstar)
  params <- list(model=3, a=a, r=r, d=d, b=b, f=f, Ref = NULL, K=K, beta=NULL)
  out3a   <- data.frame(my_ode(yini, times, lotkavol.model, params))
  out3a <- out3a[apply(out3a, 1, min)>0,]
  out3b <- expand.grid(
    N = seq(min(1, out3a$N, 0.9*Pstar), max(out3a$N, 1.1*Pstar), length = 5), 
    P = seq(min(1, out3a$P, 0.9*Nstar), max(out3a$P, 1.1*Nstar), length = 5), 
    N_end = NA, 
    P_end = NA
  )
  for(i in 1:nrow(out3b)){
    out3b[i,3:4] <- my_ode(c(N = out3b$N[i], P = out3b$P[i]), 0:1, 
                           lotkavol.model, params)[2,2:3]
  }
  return(list(
    predictions = out3a, 
    phase_plane = out3b, 
    params = list(a=a, b=b, d=d, r=r, K=K, f=f), 
    equilibria = c(Nstar = Nstar, Pstar = Pstar), 
    initial_pop_size = yini
  ))
}

calc4 <- function(a, r, d, b, Ref, yini, times){
  Pstar <-  d/(a * b)
  Nstar <- Nstar4(a=a, r=r, Ref=Ref, N=Pstar)
  params <- list(model=4, a=a, r=r, d=d, b=b, f = NULL, Ref=Ref,
                 K=NULL, beta=NULL)
  out1a  <- data.frame(my_ode(yini, times, lotkavol.model, params))
  out1a[out1a < 0 ] <- 0
  out1b <- expand.grid(
    N = seq(min(out1a$N, 0.9*Pstar), max(out1a$N, 1.1*Pstar), length = 5), 
    P = seq(min(out1a$P, 0.9*Nstar), max(out1a$P, 1.1*Nstar), length = 5), 
    N_end = NA, 
    P_end = NA
  )
  for(i in 1:nrow(out1b)){
    out1b[i,3:4] <- my_ode(c(N = out1b$N[i], P = out1b$P[i]), 0:1, 
                           lotkavol.model, params)[2,2:3]
  }
  return(list(
    predictions = out1a, 
    phase_plane = out1b, 
    params = list(a=a, b=b, d=d, r=r, Ref=Ref), 
    equilibria = c(Nstar = Nstar, Pstar = Pstar), 
    initial_pop_size = yini
  ))
}

# competition between predators. interesting, but not implememnted
# calc5 <- function(a, r, d, b, yini, times){
#   Nstar <-  r/a
#   Pstar <-  d/(a * b) ## NEED TO FIX
#   params <- list(model=5, a=a, r=r, d=d, b=b, f=NULL, Ref = NULL, K=NULL, beta=beta)
#   out1a  <- data.frame(ode(yini, times, lotkavol.model, params))
#   out1a[out1a < 0 ] <- 0
#   out1b <- expand.grid(
#     N = seq(min(out1a$N, 0.9*Pstar), max(out1a$N, 1.1*Pstar), length = 5), 
#     P = seq(min(out1a$P, 0.9*Nstar), max(out1a$P, 1.1*Nstar), length = 5), 
#     N_end = NA, 
#     P_end = NA
#   )
#   for(i in 1:nrow(out1b)){
#     out1b[i,3:4] <- ode(c(N = out1b$N[i], P = out1b$P[i]), 0:1, lotkavol.model, params)[2,2:3]
#   }
#   return(list(
#     predictions = out1a, 
#     phase_plane = out1b, 
#     params = list(a=a, r=r, d=d, b=b), 
#     equilibria = c(Nstar = Nstar, Pstar = Pstar), 
#     initial_pop_size = yini
#   ))
# }

# In Model 2, Predator ZNGI is a bit more complicated - it varys, depending on how many Prey there are!
Nstar2 <- function(a, r, K, N){r/a*(1-(N/K))}

# In Model 3, Predator ZNGI is more complicated - it varys, depending on how many Prey there are!
Nstar3 <- function(a, r, K, f, N){r/(a*N)*(N-f)*(1-N/K)}

# In Model 4, Predator ZNGI is more complicated - it varys, depending on how many Prey there are!
Nstar4 <- function(a, r, Ref, N){r/(a*N)*(N+Ref)}

# In Model 5, Predator ZNGI is more complicated - it varys, depending on how many Prey there are!
# Nstar5 <- function(a, r, beta){r/(a - r*beta)}
# Pstar5 <- function(a, r, beta){r/(a - r*beta)}


# Set up graphs to examime the model projections
plot1 <- function(type = NULL, input, max.time, add = F, ...) {
  stopifnot(!is.null(type))
  pred <- input$predictions
  p_p  <- input$phase_plane
  eq   <- input$equilibria
  if(add == F){
    # layout(matrix(c(1, 2, 2), nrow = 1))
    # op <- par(las = 1, bty = "n", mar = c(4, 4, 4, 4), xaxs = "i", yaxs = "i")
    if (type == "trajectories"){
      # Plot population trajectories over time
      matplot(pred[, -1], type = "l", xlab = "Time", ylab = "Population size", col = c("blue", "red"), lty = 1, ylim = range(1, pred$N*1.1, pred$P*1.1), ...)
      abline(v = which(pred$N <= 1)[1], col = "blue", lty = 3) # THese indicate when (if) the populatoins go extinct
      abline(v = which(pred$P <= 1)[1], col = "red", lty = 3)
      legend("bottomleft", c("Prey (N)", "Predator (P)"), text.col = c("blue", "red"), box.lwd = 0)
    } else if (type == "phase-plane"){
      # Make the phase-plane diagram
      p_p_plotter(input)
      abline(v = eq[2], h = eq[1], col = "gray")# add the ZNGI's
      text(eq[2], eq[1], expression(paste("N*:P= ", frac(r, a))), cex = 1, adj = c(0, 1.5))
      text(eq[2], eq[1], expression(paste("P*:N= ", frac(d, a * b))), cex = 1, adj = c(0, -0.5))
      mtext(side = 3, expression(ZNGI[P]), at = eq[2], cex = 0.6)
      mtext(side = 4, expression(ZNGI[N]), at = eq[1], cex = 0.6)
    }
  }
  if (type == "phase-plane"){
    segments(pred$N[-nrow(pred)], pred$P[-nrow(pred)], pred$N[-1], pred$P[-1], col = rainbow(max.time, end = 0.7))
    points(pred$N[1], pred$P[1], cex = 2)
    points(pred$N[max.time], pred$P[max.time], cex = 2, pch = 16)
    text(pred$N[1], pred$P[1], "Start", pos = 4, cex = 1, xpd = NA)
    text(pred$N[max.time], pred$P[max.time], "End", pos = 4, cex = 1, xpd = NA)
  }
  # if(add == F){par(op)}
}

plot2 <- function(type = NULL, input, max.time, add = F, ...){
  stopifnot(!is.null(type))
  pred <- input$predictions
  p_p <- input$phase_plane
  params <- input$params
  eq <- input$equilibria
  if(add == F){
    # Set up graphs to examime the model projections
    # layout(matrix(c(1, 2, 2), nrow = 1))
    # op <- par(las = 1, bty = "n", mar = c(4, 4, 4, 4))
    if (type == "trajectories"){
      # Plot population trajectories over time
      matplot(pred[,-1], type = "l", xlab = "time", ylab = "Population size", col = c("blue", "red"), lty = 1, ylim = c(0, max(pred$N, max(pred$P))), ...)
      abline(v = which(pred$Prey<=1)[1], col = "blue", lty = 3) # THese indicate when (if) the populatoins go extinct
      abline(v = which(pred$Predator<=1)[1], col = "red", lty = 3)
      legend("bottomleft", c("Prey (N)", "Predator (P)"), text.col = c("blue", "red"), box.lwd = 0)
    } else if (type == "phase-plane"){
      # Make the phase-plane diagram
      p_p_plotter(input)
      # indicate prey carrying capacity
      abline(v = params$K, lwd = 2)
      # add the ZNGI's
      abline(v = eq[2], col = "gray")
      temp <- seq(0.8*min(p_p$N, p_p$N_end), 1.2*max(p_p$N, p_p$N_end), length = 101)
      lines(temp, Nstar2(params$a, params$r, params$K, temp), col = 'gray')
      text(eq[2], eq[1], expression(paste("N*:P= ", frac(r,a)*bgroup("(",1 - frac(N,K),")"))), cex = 1, adj = c(0, 1.5))
      text(eq[2], eq[1], expression(paste("P*:N= ", frac(d, a * b))), cex = 1, adj = c(0, -0.5))
      mtext(side = 3, expression(ZNGI[P]), at = eq[2], cex = 0.6)
      mtext(side = 4, expression(ZNGI[N]), at = Nstar2(params$a, params$r, params$K, temp[100]), cex = 0.6)
    }
  }
  if (type == "phase-plane"){
    segments(pred$N[-nrow(pred)], pred$P[-nrow(pred)], pred$N[-1], pred$P[-1], col = rainbow(max.time, end = 0.7))
    points(pred$N[1], pred$P[1], cex = 2)
    points(pred$N[max.time], pred$P[max.time], cex = 2, pch = 16)
    text(pred$N[1], pred$P[1], "Start", pos = 4, cex = 1, xpd = NA)
    text(pred$N[max.time], pred$P[max.time], "End", pos = 4, cex = 1, xpd = NA)
  }
  # if(add == F){par(op)}
}

plot3 <- function(type = NULL, input, max.time, add = F, ...){
  stopifnot(!is.null(type))
  pred <- input$predictions
  p_p <- input$phase_plane
  params <- input$params
  eq <- input$equilibria
  # Set up graphs to examime the model projections
  if(add == F){
    # layout(matrix(c(1, 2, 2), nrow = 1))
    # op <- par(las = 1, bty = "n", mar = c(4, 4, 4, 4))
    if (type == "trajectories"){
      # Plot population trajectories over time
      matplot(pred[,-1], type = "l", xlab = "time", ylab = "Population sizes", col = c("blue", "red"), lty = 1, ylim = c(0, max(pred$N, max(pred$P))), ...)
      abline(v = which(pred$Prey<=1)[1], col = "blue", lty = 3) # THese indicate when (if) the populatoins go extinct
      abline(v = which(pred$Predator<=1)[1], col = "red", lty = 3)
      legend("topright", c("Prey (N)", "Predator (P)"), text.col = c("blue", "red"), box.lwd = 0)
    } else if (type == "phase-plane"){
      # Plot Phase-plane: Predator vs prey population dynamics
      p_p_plotter(input)
      # indicate prey carrying capacity
      abline(v = params$K, lwd = 2)
      # add the ZNGI's
      abline(v = eq[2], col = "gray")
      temp <- seq(0.8*min(p_p$N), 1.2*max(p_p$N), length = 101)
      lines(temp, Nstar3(params$a, params$r, params$K, params$f, temp), col = 'gray')
      text(eq[2], eq[1], expression(paste("N*:P= ", frac(r, a*N)*( f-N)*(1-N/K))), cex = 1, adj = c(0, 1.5))
      text(eq[2], eq[1], expression(paste("P*:N= ", frac(d,a*b))), cex = 1, adj = c(0, -0.5))
      mtext(side = 3, expression(ZNGI[P]), at = eq[2], cex = 0.6)
      mtext(side = 4, expression(ZNGI[N]), at = Nstar3(params$a, params$r, params$K, params$f, temp[100]), cex = 0.6)
      #	indicate minimum population size
      abline(v = params$f, lwd = 2)
    }
  }
  if (type == "phase-plane"){
    segments(pred$N[-nrow(pred)], pred$P[-nrow(pred)], pred$N[-1], pred$P[-1], col = rainbow(max.time, end = 0.7))
    points(pred$N[1], pred$P[1], cex = 2)
    points(pred$N[max.time], pred$P[max.time], cex = 2, pch = 16)
    text(pred$N[1], pred$P[1], "Start", pos = 4, cex = 1, xpd = NA)
    text(pred$N[max.time], pred$P[max.time], "End", pos = 4, cex = 1, xpd = NA)
  }
  # if(add == F){par(op)}
}


plot4 <- function(type = NULL, input, max.time, add = F, ...){
  stopifnot(!is.null(type))
  pred   <- input$predictions
  p_p    <- input$phase_plane
  params <- input$params
  eq     <- input$equilibria
  # Set up graphs to examime the model projections
  if(add == F){
    # layout(matrix(c(1, 2, 2), nrow = 1))
    # op <- par(las = 1, bty = "n", mar = c(4, 4, 4, 4))
    if (type == "trajectories"){
      # Plot population trajectories over time
      matplot(pred[,-1], type = "l", xlab = "time", ylab = "Population sizes", col = c("blue", "red"), lty = 1, ylim = c(0, max(pred$N, max(pred$P))), ...)
      abline(v = which(pred$Prey<=1)[1], col = "blue", lty = 3) # THese indicate when (if) the populatoins go extinct
      abline(v = which(pred$Predator<=1)[1], col = "red", lty = 3)
      legend("topright", c("Prey (N)", "Predator (P)"), text.col = c("blue", "red"), box.lwd = 0)
    } else if (type == "phase-plane"){
      # Plot Phase-plane: Predator vs prey population dynamics
      p_p_plotter(input)

      # add the ZNGI's
      abline(v = eq[2], col = "gray")
      temp <- seq(0.8*min(p_p$N), 1.2*max(p_p$N), length = 101)
      lines(temp, Nstar4(params$a, params$r, params$Ref, temp), col = 'gray')
#browser()
#            lines(temp, params$r/(params$a*temp)*(temp+params$f), col = 'gray')
      text(eq[2], eq[1], expression(paste("N*:P= ", frac(r, a*N)*(Ref+N))), cex = 1, adj = c(0, 1.5))
      text(eq[2], eq[1], expression(paste("P*:N= ", frac(d,a*b))), cex = 1, adj = c(0, -0.5))
      mtext(side = 3, expression(ZNGI[P]), at = eq[2], cex = 0.6)
      mtext(side = 4, expression(ZNGI[N]), at = Nstar4(params$a, params$r, params$Ref, temp[100]), cex = 0.6)
      #	indicate size of refuges
#      abline(v = params$Ref, lwd = 2)
    }
  }
  if (type == "phase-plane"){
    segments(pred$N[-nrow(pred)], pred$P[-nrow(pred)], pred$N[-1], pred$P[-1], col = rainbow(max.time, end = 0.7))
    points(pred$N[1], pred$P[1], cex = 2)
    points(pred$N[max.time], pred$P[max.time], cex = 2, pch = 16)
    text(pred$N[1], pred$P[1], "Start", pos = 4, cex = 1, xpd = NA)
    text(pred$N[max.time], pred$P[max.time], "End", pos = 4, cex = 1, xpd = NA)
  }
  # if(add == F){par(op)}
}


# Plot Phase-plane: Predator vs prey population dynamics
p_p_plotter <- function(input){
  pred <- input$predictions
  p_p  <- input$phase_plane
  eq   <- input$equilibria
  options(warn = -1)
  plot(pred$N, pred$P, type = "n", ylab = "", xlab = "", xlim = range(pred$N, p_p$N, p_p$N_end), ylim = range(pred$P, p_p$P, p_p$P_end))
  mtext("Prey population size (N)", 1, col = 'blue', las = 0, line = 3)
  mtext("Predator population size (P)", 2, col = 'red', las = 0, line = 3)
  arrows(p_p$N, p_p$P, p_p$N_end, p_p$P_end, col = "gray", length = 0.04)
  points(eq[2], eq[1], pch = 4, cex = 2)
  options(warn = 0)
}