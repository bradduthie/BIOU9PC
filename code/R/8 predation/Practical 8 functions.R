# Code placed in this file fill be executed every time the
# lesson is started. Any variables created here will show up in
# the user's working directory and thus be accessible to them
# throughout the lesson.

if (!"deSolve" %in% installed.packages()[, 1]) {
	install.packages('deSolve')
}
library(deSolve)


#### DEFINE THE MODELS ###
#######################################
###          Model 1                ###
### Simplest possible predator-prey ###
### model. Prey grow exponentially, ###
### No density dependence           ###
#######################################
LotVmod1 <- function(Time, State, Pars) {
	with(as.list(c(State, Pars)), {
		if (P < 1) P <- 0
		if (N < 1) N <- 0
		Ingestion <- a1 * N * P
		dN <- r1 * N - Ingestion
		dP <- Ingestion * b1 - d1 * P
		return(list(c(dN, dP)))
	})
}

#######################################
###          Model 2                ###
### Now introduce a carrying        ###
### capacity for the prey.          ###
### Rather than exponential growth, ###
### they now have logistic growth   ###
### in the absence of the predator. ###
#######################################

LotVmod2 <- function(Time, State, Pars) {
	with(as.list(c(State, Pars)), {
		if (P < 1)
			P <- 0
		if (N < 1)
			N <- 0
		Ingestion <- a2 * N * P
		dN <- r2 * N * (1 - N/K2) - Ingestion
		dP <- Ingestion * b2 - d2 * P
		return(list(c(dN, dP)))
	})
}

#######################################
###          Model 3                ###
### Now introduce a allee effects   ###
### together with carrying capacity ###
### for the prey population.        ###
#######################################
LotVmod3 <- function(Time, State, Pars) {
	with(as.list(c(State, Pars)), {
		if (P < 1)
			P <- 0
		if (N < 1)
			N <- 0
		Ingestion <- a3 * N * P
		dN <- r3 * (N - f3) * (1 - N/K3) - Ingestion
		dP <- Ingestion * b3 - d3 * P
		return(list(c(dN, dP)))
	})
}


calc1 <- function(a, r, d, b, times, yini = NULL){
	stopifnot(a>0, r>0, b>0, d>0, yini>0)
	if(is.null(yini)){yini <-  c(N = runif(1, 0.5, 1.5)*abs(Pstar), P = runif(1, 0.5, 1.5)*abs(Nstar))}
	out1a  <- data.frame(ode(yini, times, LotVmod1, parms = c(a, r, d, b)))
	out1a[out1a<0] <- 0
	Nstar = r/a
	Pstar = d/(a * b)
	out1b <- expand.grid(N = seq(min(out1a$N, 0.9*Pstar), max(out1a$N, 1.1*Pstar), length = 11), P = seq(min(out1a$P, 0.9*Nstar), max(out1a$P, 1.1*Nstar), length = 11), N_end = NA, P_end = NA)
	for(i in 1:nrow(out1b)){
	out1b[i,3:4] <- ode(c(N = out1b$N[i], P = out1b$P[i]), times = 0:1, LotVmod1, parms = c(a, r, d, b))[2,2:3]
	}
	return(list(predictions = out1a, phase_plane = out1b, equilibria = c(Nstar = Nstar, Pstar = Pstar), initial_pop_size = yini))
}


calc1a <- function(a, r, d, b, times, yini = NULL){
	stopifnot(a>0, r>0, b>0, d>0, yini>0)
	Nstar = r/a
	Pstar = d/(a * b)
	y <- expand.grid(N = c(0.5, 1.5)*abs(Pstar), P = c(0.5, 1.5)*abs(Nstar))
	y <- rbind(y, yini)
	out1a <- list()
	for(i in 1:nrow(y)){
		out1a.i  <- data.frame(ode(unlist(y[i,]), times, LotVmod1, parms = c(a, r, d, b)))
		out1a.i[out1a.i<0] <- 0
		out1a[[i]] <- out1a.i
		}
	ranges <- sapply(out1a, function(x){apply(x, 2, range)})

	out1b <- expand.grid(N = seq(min(ranges[3,], 0.9*Pstar), max(ranges[4,], 1.1*Pstar), length = 11), P = seq(min(ranges[5,], 0.9*Nstar), max(ranges[6,], 1.1*Nstar), length = 11), N_end = NA, P_end = NA)
	for(i in 1:nrow(out1b)){
	out1b[i,3:4] <- ode(c(N = out1b$N[i], P = out1b$P[i]), times = 0:1, LotVmod1, parms = c(a, r, d, b))[2,2:3]
	}
	return(list(predictions = out1a, phase_plane = out1b, equilibria = c(Nstar = Nstar, Pstar = Pstar), initial_pop_sizes = y))
}


calc2 <- function(a, r, d, b, K, times, yini = NULL){
	stopifnot(a>0, r>0, b>0, d>0, K>0)
	options(warn = -1)
	# Determine the equilibrium population sizes
	Pstar <- d/(a*b)
	Nstar <- Nstar2(a = a, r=r, K=K, N=Pstar)
	if(is.null(yini)){yini <-  c(N = runif(1, 0.5, 1.5)*abs(Pstar), P = runif(1, 0.5, 1.5)*abs(Nstar))}
	out2a   <- data.frame(ode(yini, times, LotVmod2, parms = c(a, r, d, b, K)))
	out2a[out2a<0] <- 0
	out2b <- expand.grid(N = seq(min(1, out2a$N, 0.9*Pstar), max(out2a$N, 1.1*Pstar), length = 11), P = seq(min(1, out2a$P, 0.9*Nstar), max(Nstar2(a, r, K, 0), out2a$P, 1.1*Nstar), length = 11), N_end = NA, P_end = NA)
	for(i in 1:nrow(out2b)){
		out2b[i,3:4] <- ode(c(N = out2b$N[i], P = out2b$P[i]), times = 0:1, LotVmod2, parms = c(a, r, d, b, K))[2,2:3]
	}
	options(warn = 0)
	return(list(predictions = out2a, phase_plane = out2b, params = c(a=a, r=r, d=d, b=b, K=K), equilibria = c(Nstar = Nstar, Pstar = Pstar), initial_pop_size = yini))
}



calc3 <- function(a, b, d, r, K, f, times, yini = NULL){
	stopifnot(a>0, r>0, b>0, d>0, K>0, f>0)
	options(warn = -1)
	# Determine the equilibrium population sizes
	Pstar <- d/(a*b)
	Nstar <- Nstar3(a=a, r=r, K=K, f=f, N=Pstar)
	if(is.null(yini)){yini <-  c(N = runif(1, 0.5, 1.5)*abs(Pstar), P = runif(1, 0.5, 1.5)*abs(Nstar))}
	out3a   <- data.frame(ode(yini, times, LotVmod3, parms = c(a, r, d, b, K, f)))
	out3a <- out3a[apply(out3a, 1, min)>0,]
	out3b <- expand.grid(N = seq(min(1, out3a$N, 0.9*Pstar), max(out3a$N, 1.1*Pstar), length = 11), P = seq(min(1, out3a$P, 0.9*Nstar), max(out3a$P, 1.1*Nstar), length = 11), N_end = NA, P_end = NA)
	for(i in 1:nrow(out3b)){
		out3b[i,3:4] <- ode(c(N = out3b$N[i], P = out3b$P[i]), times = 0:1, LotVmod3, parms = c(a, r, d, b, K, f))[2,2:3]
	}
	options(warn = 0)
	return(list(predictions = out3a, phase_plane = out3b, params = c(a=a, b=b, d=d, r=r, K=K, f=f), equilibria = c(Nstar = Nstar, Pstar = Pstar), initial_pop_size = yini))
}

# In Model 2, Predator ZNGI is a bit more complicated - it varys, depending on how many Prey there are!
Nstar2 <- function(a, r, K, N){r/a*(1-(N/K))}

# In Model 3, Predator ZNGI is more complicated - it varys, depending on how many Prey there are!
Nstar3 <- function(a, r, K, f, N){r/(a*N)*(N-f)*(1-N/K)}



# Set up graphs to examime the model projections
plot1 <- function(input, add = F, ...) {
	pred <- input$predictions
	p_p  <- input$phase_plane
	eq   <- input$equilibria
	if(add == F){
		layout(matrix(c(1, 2, 2), nrow = 1))
		op <- par(las = 1, bty = "n", mar = c(4, 4, 4, 4), xaxs = "i", yaxs = "i")

	# Plot population trajectories over time
		matplot(pred[, -1], type = "l", xlab = "time", ylab = "Population size", col = c("blue", "red"), lty = 1, ylim = range(1, pred$N*1.1, pred$P*1.1), ...)
		abline(v = which(pred$N <= 1)[1], col = "blue", lty = 3) # THese indicate when (if) the populatoins go extinct
		abline(v = which(pred$P <= 1)[1], col = "red", lty = 3)
		legend("topright", c("Prey (N)", "Predator (P)"), text.col = c("blue", "red"), box.lwd = 0)

	# Make the phase-plane diagram
		p_p_plotter(input)
		abline(v = eq[2], h = eq[1], col = "gray")# add the ZNGI's
		text(eq[2], eq[1], expression(paste("N*:P= ", frac(r, a))), cex = 1, adj = c(0, 1.5))
		text(eq[2], eq[1], expression(paste("P*:N= ", frac(d, a * b))), cex = 1, adj = c(0, -0.5))
		mtext(side = 3, expression(ZNGI[P]), at = eq[2], cex = 0.6)
		mtext(side = 4, expression(ZNGI[N]), at = eq[1], cex = 0.6)
	}
	segments(pred$N[-nrow(pred)], pred$P[-nrow(pred)], pred$N[-1], pred$P[-1], col = rainbow(max.time, end = 0.7))
	points(pred$N[1], pred$P[1], cex = 2)
	points(pred$N[max.time], pred$P[max.time], cex = 2, pch = 16)
	text(pred$N[1], pred$P[1], "Start", pos = 4, cex = 1, xpd = NA)
	text(pred$N[max.time], pred$P[max.time], "End", pos = 4, cex = 1, xpd = NA)
	if(add == F){par(op)}
}

plot1a <- function(input, add = F, ...) {
	pred <- input$predictions
	p_p  <- input$phase_plane
	eq   <- input$equilibria
	layout(matrix(c(1, 2, 2), nrow = 1))
	op <- par(las = 1, bty = "n", mar = c(4, 4, 4, 4), xaxs = "i", yaxs = "i")

	# Plot population trajectories over time
	ranges <- sapply(pred, function(x){apply(x, 2, range)})
	YLIM <- range(ranges[3:6,])
	matplot(pred[[1]][, -1], type = "l", xlab = "time", ylab = "Population size", col = c("blue", "red"), lty = 1, ylim = range(ranges[3:6,]))
		abline(v = which(pred$N <= 1)[1], col = "blue", lty = 3) # THese indicate when (if) the populatoins go extinct
		abline(v = which(pred$P <= 1)[1], col = "red", lty = 3)
		legend("topright", c("Prey (N)", "Predator (P)"), text.col = c("blue", "red"), box.lwd = 0)

	# Make the phase-plane diagram
		p_p_plottera(input)
		abline(v = eq[2], h = eq[1], col = "gray")# add the ZNGI's
		text(eq[2], eq[1], expression(paste("N*:P= ", frac(r, a))), cex = 1, adj = c(0, 1.5))
		text(eq[2], eq[1], expression(paste("P*:N= ", frac(d, a * b))), cex = 1, adj = c(0, -0.5))
		mtext(side = 3, expression(ZNGI[P]), at = eq[2], cex = 0.6)
		mtext(side = 4, expression(ZNGI[N]), at = eq[1], cex = 0.6)
	for(i in 1:length(pred)){
		pred.i <- pred[[i]]
		segments(pred.i$N[-nrow(pred.i)], pred.i$P[-nrow(pred.i)], pred.i$N[-1], pred.i$P[-1], col = rainbow(max.time, end = 0.7), lwd = if(i==5) 3 else 1)
		points(pred.i$N[1], pred.i$P[1], cex = 2)
		points(pred.i$N[max.time], pred.i$P[max.time], cex = 2, pch = 16)
		text(pred.i$N[1], pred.i$P[1], paste("S", i), pos = 4, cex = 1, xpd = NA)
		text(pred.i$N[max.time], pred.i$P[max.time], paste("E", i), pos = 4, cex = 1, xpd = NA)
		}
	par(op)
}

plot2 <- function(input, add = F, ...){
	pred <- input$predictions
	p_p  <- input$phase_plane
	eq   <- input$equilibria

	if(add == F){
	# Set up graphs to examime the model projections
	layout(matrix(c(1, 2, 2), nrow = 1))
	op <- par(las = 1, bty = "n", mar = c(4, 4, 4, 4))

# Plot population trajectories over time
	matplot(pred[,-1], type = "l", xlab = "time", ylab = "Population size", col = c("blue", "red"), lty = 1, ylim = c(0, max(pred$N, max(pred$P))), ...)
	abline(v = which(pred$Prey<=1)[1], col = "blue", lty = 3) # THese indicate when (if) the populatoins go extinct
	abline(v = which(pred$Predator<=1)[1], col = "red", lty = 3)
	legend("topright", c("Prey (N)", "Predator (P)"), text.col = c("blue", "red"), box.lwd = 0)

	p_p_plotter(input)
# indicate prey carrying capacity
	abline(v = K2, lwd = 2)
# add the ZNGI's
	abline(v = eq[2], col = "gray")
	temp <- seq(min(p_p$N, p_p$N_end), max(p_p$N, p_p$N_end), length = 100)
	lines(temp, Nstar2(a2, r2, K2, temp), col = 'gray')
	text(eq[2], eq[1], expression(paste("N*:P= ", frac(r,a)*bgroup("(",1 - frac(N,K),")"))), cex = 1, adj = c(0, 1.5))
	text(eq[2], eq[1], expression(paste("P*:N= ", frac(d, a * b))), cex = 1, adj = c(0, -0.5))
	mtext(side = 3, expression(ZNGI[P]), at = eq[2], cex = 0.6)
	mtext(side = 4, expression(ZNGI[N]), at = Nstar2(a2, r2, K2, temp[100]), cex = 0.6)
	}
	segments(pred$N[-nrow(pred)], pred$P[-nrow(pred)], pred$N[-1], pred$P[-1], col = rainbow(max.time, end = 0.7))
	points(pred$N[1], pred$P[1], cex = 2)
	points(pred$N[max.time], pred$P[max.time], cex = 2, pch = 16)
	text(pred$N[1], pred$P[1], "Start", pos = 4, cex = 1, xpd = NA)
	text(pred$N[max.time], pred$P[max.time], "End", pos = 4, cex = 1, xpd = NA)
	if(add == F){par(op)}
}

plot3 <- function(input, add = F, ...){
	pred <- input$predictions
	p_p  <- input$phase_plane
	eq   <- input$equilibria

	# Set up graphs to examime the model projections
	if(add == F){
	layout(matrix(c(1, 2, 2), nrow = 1))
	op <- par(las = 1, bty = "n", mar = c(4, 4, 4, 4))

# Plot population trajectories over time
	matplot(pred[,-1], type = "l", xlab = "time", ylab = "Population size", col = c("blue", "red"), lty = 1, ylim = c(0, max(pred$N, max(pred$P))), ...)
	abline(v = which(pred$Prey<=1)[1], col = "blue", lty = 3) # THese indicate when (if) the populatoins go extinct
	abline(v = which(pred$Predator<=1)[1], col = "red", lty = 3)
	legend("topright", c("Prey (N)", "Predator (P)"), text.col = c("blue", "red"), box.lwd = 0)

# Plot Phase-plane: Predator vs prey population dynamics
	p_p_plotter(input)
# indicate prey carrying capacity
	abline(v = K3, lwd = 2)

# add the ZNGI's
	abline(v = eq[2], col = "gray")
	temp <- seq(min(p_p$N), max(p_p$N), length = 100)
	lines(temp, Nstar3(a3, r3, K3, f3, temp), col = 'gray')
	text(eq[2], eq[1], expression(paste("N*:P= ", frac(r,a*N)*(f-N)*(1-N/K))), cex = 1, adj = c(0, 1.5))
	text(eq[2], eq[1], expression(paste("P*:N= ", frac(d,a*b))), cex = 1, adj = c(0, -0.5))
	mtext(side = 3, expression(ZNGI[P]), at = eq[2], cex = 0.6)
	mtext(side = 4, expression(ZNGI[N]), at = Nstar3(a3, r3, K3, f3, temp[100]), cex = 0.6)

#	indicate minimum population size
	abline(v = f3, lwd = 2)
	}
	segments(pred$N[-nrow(pred)], pred$P[-nrow(pred)], pred$N[-1], pred$P[-1], col = rainbow(max.time, end = 0.7))
	points(pred$N[1], pred$P[1], cex = 2)
	points(pred$N[max.time], pred$P[max.time], cex = 2, pch = 16)
	text(pred$N[1], pred$P[1], "Start", pos = 4, cex = 1, xpd = NA)
	text(pred$N[max.time], pred$P[max.time], "End", pos = 4, cex = 1, xpd = NA)
	if(add == F){par(op)}

}


# Plot Phase-plane: Predator vs prey population dynamics
p_p_plotter <- function(input){
	pred <- input$predictions
	p_p  <- input$phase_plane
	eq   <- input$equilibria
	options(warn = -1)
	plot(pred$N, pred$P, type = "n", ylab = "", xlab = "", xlim = range(pred$N, p_p$N, p_p$N_end), ylim = range(pred$P, p_p$P, p_p$P_end))
	mtext("Prey population size (N)", 1, col = 'blue', las = 0, line = 2)
	mtext("Predator population size (P)", 2, col = 'red', las = 0, line = 2)
	arrows(p_p$N, p_p$P, p_p$N_end, p_p$P_end, col = "gray", length = 0.04)
	points(eq[2], eq[1], pch = 4, cex = 2)
	options(warn = 0)
}


p_p_plottera <- function(input){
	pred <- input$predictions
	p_p  <- input$phase_plane
	eq   <- input$equilibria
	options(warn = -1)
	ranges <- sapply(pred, function(x){apply(x, 2, range)})
	plot(pred$N, pred$P, type = "n", ylab = "", xlab = "", xlim = range(ranges[3:4,], p_p$N, p_p$N_end), ylim = range(ranges[5:6,], p_p$P, p_p$P_end))
	mtext("Prey population size (N)", 1, col = 'blue', las = 0, line = 2)
	mtext("Predator population size (P)", 2, col = 'red', las = 0, line = 2)
	arrows(p_p$N, p_p$P, p_p$N_end, p_p$P_end, col = "gray", length = 0.04)
	points(eq[2], eq[1], pch = 4, cex = 2)
	options(warn = 0)
}


