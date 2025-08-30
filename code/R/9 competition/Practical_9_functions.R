# Code placed in this file fill be executed every time the
# lesson is started. Any variables created here will show up in
# the user's working directory and thus be accessible to them
# throughout the lesson.

if (!"deSolve" %in% installed.packages()[, 1]) {
    install.packages('deSolve')
}
library(deSolve)

#### DEFINE THE MODELS ###
LVcomp1 <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
        dN1 <- r1*N1*(K1 - N1 - a21*N2)/K1
        dN2 <- r2*N2*(K2 - N2 - a12*N1)/K2
        return(list(c(dN1, dN2)))
    })
}

# One-species monod competition with explicit resources. Stable
monodMod <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
        Ingestion <- r*Resource/(Ku+Resource)
        dConsumer <- Ingestion-D
        dResource <- D*(R0-Resource) - Consumer*Ingestion/b
        return(list(c(dConsumer, dResource)))
    })
}

# Two-species monod competition with explicit resources. Unstable
monodMod2 <- function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
        Ingestion1 <- r1*(Resource/(Ku1+Resource))
        Ingestion2<- r2*(Resource/(Ku2+Resource))
        dConsumer1 <- Consumer1*Ingestion1-Consumer1*D
        dConsumer2<- Consumer2*Ingestion2-Consumer2*D
        dResource <- D*(R0-Resource) - (Consumer1/b1)*(Ingestion1)-(Consumer2/b2)*(Ingestion2)
        return(list(c(dConsumer1,dConsumer2, dResource)))
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
N1star <- function(N1){(K1-N1)/a21}
N2star <- function(N1){K2-a12*N1}

# Run the calculations for teh LV competition model
calcLVcomp <- function(a12, a21, K1, K2, yini = NULL){
    outcome <- outcomePredictor1(a12, a21, K1, K2)
    N1star  <- (K1-a21*K2)/(1-a12*a21)
    N2star  <- (K2-a12*K1)/(1-a12*a21)

    if(is.null(yini)){yini <-  c(N1 = runif(1, 0, 1.5*abs(N1star)), N2 = runif(1, 0, 1.5*abs(N2star)))}

    outLVa  <- data.frame(ode(yini, times, LVcomp1, parms = c(K1, K2, r1, r2, a12, a21)))
    outLVb <- expand.grid(N1 = seq(1, max(K1, outLVa$N1), length = 20), N2 = seq(1, max(K2, outLVa$N2), length = 20), N1_end = NA, N2_end = NA)
    for(i in 1:nrow(outLVb)){
        outLVb[i,3:4] <- ode(c(N1 = outLVb$N1[i], N2 = outLVb$N2[i]), times = 0:1, LVcomp1, parms = c(K1, K2, r1, r2, a12, a21))[2,2:3]
    }
    return(list(predictions = outLVa, phase_plane = outLVb, outcome = outcome, equilibria = c(N1star = N1star, N2star = N2star), initial_pop_size = yini))
}

# Plot up predictions from the LV competition model
plotLVcomp <- function(input, add = F, ...){
    op <- par()
    pred <- input$predictions
    p_p  <- input$phase_plane
    eq   <- input$equilibria
    options(warn = -1)
    # Set up graphs to examime the model projections
    if(add == F){
        layout(matrix(c(1, 1, 2, 2, 2, 2), nrow = 2))
        op <- par(las = 1, bty = "n", mar = c(4, 4, 3, 1), pty = "m")

        # Plot population trajectories over time
        matplot(pred[,-1], type = "l", xlab = "time", ylab = "Population sizes", col = "blue", lty = 1:2, lwd = 2)
        legend("topright", c("Consumer1", "Consumer2"), text.col = "blue", box.lwd = 0, lty = 1:2, col = "blue", inset = 0.1)

        # Plot Phase-plane: Resource vs Consumer population dynamics, including the force-field.
        plot(p_p$N1, p_p$N2, type = "n", ylab = "Consumer 2 population size (N2)", xlab = "Consumer 1 population size (N1)")
        curve(N1star, 1, 10000, add = T)
        curve(N2star, 1, 10000, add = T, lty = 2)
        arrows(p_p$N1, p_p$N2, p_p$N1_end, p_p$N2_end, col = "gray", length = 0.01, xpd = NA)

        legend('top', ncol = 2, lty = 1:2, legend = expression(N[1]^'*', N[2]^'*'), bty ='0', inset = -0.04, xpd = NA, seg.len = 4, bg = 'white')
    }
    segments(pred$N1[-max.time], pred$N2[-max.time], pred $N1[-1], pred$N2[-1], col = rainbow(max.time, end = 0.7), lwd = 2, xpd = NA)
    points(pred$N1[1], pred$N2[1], cex = 2, xpd = NA)
    text(pred$N1[1], pred$N2[1], "Start", pos = 4, cex = 0.7, xpd = NA)
    points(pred$N1[max.time], pred$N2[max.time], cex = 2, pch = 16, xpd = NA)
    text(pred$N1[max.time], pred$N2[max.time], "End", pos = 4, cex = 0.7, xpd = NA)
    if(add == F){par(op)}
}


# Run the calculations for the Monod one species, one resource model
calcMONODcomp <- function(r, Ku, D, R0, b, yini = NULL){
    Rstar <- D*Ku/(r-D)  # The equilibrium concentration of Resource (dR/dt = 0)
    Nstar <- (R0-Rstar)/b   # The equilibrium concentration of Consumer (dN/dt = 0)
    if(is.null(yini)){yini <- c(Consumer = runif(1, 0.0001, 1.5*abs(Nstar)), Resource = runif(1, 0.0001, 1.5*abs(Rstar)))}
    outMONODa  <- data.frame(ode(yini, times, monodMod, parms = c(r, Ku, D, R0, b))); outMONODa[outMONODa<0] <- 0
    outMONODb <- expand.grid(Consumer = seq(0, max(Nstar, outMONODa$Consumer)*1.2, length =11), Resource = seq(0, max(Rstar, outMONODa$Resource)*1.2, length =11), Consumer_end = NA, Resource_end = NA)
    for(i in 1:nrow(outMONODb)){
        outMONODb[i,3:4] <- ode(c(Consumer = outMONODb$Consumer[i], Resource = outMONODb$Resource[i]), times = 0:1, monodMod, parms = c(r, Ku, D, R0, b))[2,2:3]
    }

    return(list(predictions = outMONODa, phase_plane = outMONODb, equilibria = c(Nstar = Nstar, Rstar = Rstar), initial_pop_size = yini))
}

# Plot up predictions from the monod one-species competition model
plotMONODcomp <- function(input, add = F){
    pred <- input$predictions
    p_p  <- input$phase_plane
    eq   <- input$equilibria
    options(warn = -1)

    if(add == F){
        # Set up graphs to examime the model projections
        layout(matrix(c(1, 1, 2, 2, 2, 2), nrow = 2))
        op <- par(las = 1, bty = "n", mar = c(4, 4, 3, 1), pty = "m")

        # Plot population trajectories over time
        matplot(pred[,-1], type = "l", xlab = "time", ylab = "Population sizes", col = c("blue", "green"), lty = 1, lwd = 2, ylim = range(pred[,-1], input$equilibria), log = 'y')
        abline(h = input$equilibria, col = c("blue", "green"), lty = 3)
        mtext(c("Rstar", "Nstar"), at = c(input$equilibria[2], input$equilibria[1]), side = 4, col = c("green", "blue"), cex = 0.8)
        legend("topright", c("Consumer", "Resource"), text.col = c("blue", "green"), box.lwd = 0, lty = 1, col = c("blue", "green"), inset = 0.1)

        # Plot Phase-plane: Resource vs Consumer population dynamics, including the force-field.
        plot(p_p$Resource, p_p$Consumer, type = "n", ylab = "Consumer population size (N)", xlab = "Resource concentration (R)")
        arrows(p_p$Resource, p_p$Consumer, p_p$Resource_end, p_p$Consumer_end, col = "gray", length = 0.01, xpd = NA)
        abline(v = input$equilibria[2], h = input$equilibria[1], col = c("blue", "green"), lty = 3)
        legend('top', ncol = 2, lty = 3, legend = expression(ZNGI[R], ZNGI[C]), bty ='n', inset = -0.04, xpd = NA, seg.len = 4, col = c("green", "blue"), text.col = c("green", "blue"))
    }

    segments(pred$Resource[-max.time], pred$Consumer[-max.time], pred $Resource[-1], pred$Consumer[-1], col = rainbow(max.time, end = 0.7), lwd = 2, xpd = NA)
    points(pred$Resource[1], pred$Consumer[1], cex = 2, xpd = NA)
    text(pred$Resource[1], pred$Consumer[1], "Start", pos = 4, cex = 0.7, xpd = NA)
    points(pred$Resource[max.time], pred$Consumer[max.time], cex = 2, pch = 16, xpd = NA)
    text(pred$Resource[max.time], pred$Consumer[max.time], "End", pos = 4, cex = 0.7, xpd = NA)
    if(add == F){par(op)}
}

# Run the calculations for the Monod two species, one resource model
calcMONODcomp2 <- function(r1, r2, Ku1, Ku2, D, R0, b1, b, yini = NULL){
    Rstar1 <- D*Ku1/(r1-D)  # The equilibrium concentration of Resource with Consumer1
    Rstar2 <- D*Ku2/(r2-D)  # The equilibrium concentration of Resource with Consumer2
    Rstar <- min(Rstar1, Rstar2) # take the minimum of those 2 Rstars.
    Nstar1 <- b1*(R0-Rstar1)  # The equilibrium concentration of Consumer1 (dN1/dt = 0)
    Nstar2 <- b2*(R0-Rstar2)  # The equilibrium concentration of Consumer2 (dN2/dt = 0)

    if(is.null(yini)){
        yini <- c(Consumer1 = runif(1, 0.0001, 1.5*abs(Nstar1)), Consumer2 = runif(1, 0.0001, 1.5*abs(Nstar2)), Resource = runif(1, 0.0001, 1.5*abs(Rstar)))}
    outMONODa  <- data.frame(ode(yini, times, monodMod2, parms = c(r1, r2, Ku1, Ku2, D, R0, b1, b2))); outMONODa[outMONODa<0] <- 0

    return(list(predictions = outMONODa, equilibria = c(Nstar1 = Nstar1, Nstar2 = Nstar2, Rstar1 = Rstar1, Rstar2 = Rstar2), initial_pop_size = yini))
}

# Plot up predictions from the monod two-species competition model
plotMONODcomp2 <- function(input){
    pred <- input$predictions
    eq   <- input$equilibria

    # Set up graphs to examime the model projections
    op <- par(las = 1, bty = "n", mfrow = c(1, 1), mar = c(5, 5, 1, 5), pty = "m")

    # Plot population trajectories over time
    matplot(pred[,-1], type = "l", xlab = "time", ylab = "Concentrations of Resource and Consumer", col = c("blue", "blue", "green"), lty = c(1,2,1), lwd = 2, ylim = range(pred[,-1], max(0.0001, eq)), log = "y")
    abline(h = input$equilibria, col = c("blue", "blue", "green", "green"), lty = 3)
    mtext(c("Nstar1", "Nstar2", "Rstar1", "Rstar2"), at = eq, side = 4, col = c("blue", "blue", "green", "green"), cex = 0.8)
    legend("topright", c("Consumer1", "Consumer2", "Resource"), text.col = c("blue", "blue", "green"), col = c("blue", "blue", "green"), box.lwd = 0, inset = 0.1, lty = c(1, 2, 1))
    par(op)
}
