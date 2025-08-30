###############################################
### BIOU9PC                                 ###
### Lab 12: spatially explicit interactions ###
### 2016-12-01                              ###
###############################################
if (!"spatstat" %in% installed.packages()[, 1]) {
  install.packages('spatstat')
}
if (!"compiler" %in% installed.packages()[, 1]) {
    install.packages('compiler')
}
library(compiler)



#### Helper functions
rwrapnorm  <- function(n, mu) {rnorm(n, mu, exp(-0.5))%%(2 * pi)}
dist_calc  <- function(x, y)  {Mod(x - y)}
which_near <- function(x)     {length(x[x < 5 * catch_dist])}

dist_tor <- function(X, Y, period = NULL) {
	nX <- length(X)
	nY <- length(Y)
	if(!is.null(period)){
	z <- .C("CcrossPdist", nfrom = as.integer(nX), xfrom = as.double(Re(X)), yfrom = as.double(Im(X)), nto = as.integer(nY), xto = as.double(Re(Y)), yto = as.double(Im(Y)), xwidth = as.double(period[1]), yheight = as.double(period[2]), squared = 0, d = as.double(matrix(0, nrow = nX, ncol = nY)))
	} else {
    z <- .C("Ccrossdist", nfrom = as.integer(nX), xfrom = as.double(Re(X)),                yfrom = as.double(Im(X)), nto = as.integer(nY), xto = as.double(Re(Y)), yto = as.double(Im(Y)), squared = 0, d = as.double(matrix(0, nrow = nX, ncol = nY)))
	}
	return(matrix(z$d, nrow = nX, ncol = nY))
}
plotNPpos

# initial distribution of prey and predators
initial_setup <- function() {
	if (arena == "rect") {
		Npos <<- runif(Nn_init, 0, Xlim) + 1i * runif(Nn_init, 0, Ylim)
		Ppos <<- runif(Pn_init, 0, Xlim) + 1i * runif(Pn_init, 0, Ylim)
	} else {
		Ninit.angles <- runif(Nn_init, 0, pi * 2)
		Ninit.dists  <- runif(Nn_init, 0, plate_radius)
		Pinit.angles <- runif(Pn_init, 0, pi * 2)
		Pinit.dists  <- runif(Pn_init, 0, plate_radius)
		Npos <<- Ninit.dists * cos(Ninit.angles) + Ninit.dists * sin(Ninit.angles)*1i # prey starts at random locations
		Ppos <<- Pinit.dists * cos(Pinit.angles) + Pinit.dists * sin(Pinit.angles) *1i # predator starts at random locations
	}

	Nenergy <<- runif(Nn_init, 1, Nb) # Prey start with 1 unit of resource each
	Penergy <<- runif(Pn_init, 1, Pb) # Predators start with energy of 1 each

	# Set up a data.frame to write out results for each time-step
	out <<- data.frame(time = 1:max.time, N = NA, P = NA)
	out$N[1] <<- Nn_init
	out$P[1] <<- Pn_init
	edge <<- plate_radius * exp((0+1i) * seq(0, 2 * pi, length = 100)) # just defines where the edge of the petri plate is, for graphing
}
initial_setup <- cmpfun(initial_setup)


# Movement of prey
# Now individuals are on a torus. The dist calcwill have to change to account for the fact that 'nareast' individual may be on other side of arena!
Nmover <- function(t) {
	if(torus){
		NPdist <- dist_tor(Npos, Ppos, period = c(Xlim, Ylim))
	} else {
		NPdist <- dist_tor(Npos, Ppos)
	}
	dist_to_nearest_pred <- apply(NPdist, 1, min)
	which_dodge  <- dist_to_nearest_pred < Ndodge_radius
	closest_predator <- apply(NPdist, 1, which.min)[which_dodge]

	Ntheta.t        <- runif(out$N[t - 1], 0, 2 * pi)
	x <- 	rbind(Npos[which_dodge], Npos[which_dodge]+Xlim,Npos[which_dodge]-Xlim, Npos[which_dodge]+Ylim*1i,Npos[which_dodge]-Ylim*1i) - Ppos[closest_predator]
	Nmean_direction <- Arg(apply(x, 2, FUN = function(x){x[which.min(Mod(x))]})) # yields the angle from the predators to their closest prey, even if it's on the other side of the torus!
	Ntheta.t[which_dodge] <- rwrapnorm(sum(which_dodge), Nmean_direction) # this introduces some variation into the angle of prey response movements
	Npos.t <- Npos + Nspeed * cos(Ntheta.t) + Nspeed * sin(Ntheta.t) * (0+1i)

	# this wraps the movements around the rectangle
	if (arena == "rect") {
		Npos <<- Re(Npos.t)%%Xlim + 0+1i*Im(Npos.t)%%Ylim
	} else { # this keeps them from leaving the arena...
		Npos <<- ifelse(Mod(Npos.t) > plate_radius, Npos - Nspeed * cos(Ntheta.t) - Nspeed * sin(Ntheta.t) * (0+1i), Npos.t)
	}
}
Nmover <- cmpfun(Nmover)

Pmover <- function(t) {
	if(torus){
		NPdist <- dist_tor(Npos, Ppos, period = c(Xlim, Ylim))
	} else {
		NPdist <- dist_tor(Npos, Ppos)
	}
	dist_to_nearest_prey <- apply(NPdist, 2, min)
	which_attack <- dist_to_nearest_prey < Psearch_radius
	closest_prey     <- apply(NPdist, 2, which.min)[which_attack]

	# Movement of predator
	Ptheta.t        <- runif(out$P[t - 1], 0, 2 * pi)

	x <- 	rbind(Npos[closest_prey], Npos[closest_prey]+Xlim, Npos[closest_prey]-Xlim, Npos[closest_prey]+Ylim*1i,Npos[closest_prey]-Ylim*1i) - Ppos[which_attack]
	Pmean_direction <- Arg(apply(x, 2, FUN = function(x){x[which.min(Mod(x))]})) # yields the angle from the predators to their closest prey. This means LAZER focus on prey, no varation. Even if it's on the other side of the torus!
	Ptheta.t[which_attack] <- rwrapnorm(sum(which_attack), Pmean_direction) # this introduces some variation into the angle of prey response movements
	Ppos.t <- Ppos + Pspeed * cos(Ptheta.t) + Pspeed * sin(Ptheta.t) * (0+1i)

	if (arena == "rect") { # this wraps motion around the rect
		Ppos <<- Re(Ppos.t)%%Xlim + 0+1i*Im(Ppos.t)%%Ylim
	} else {# this keeps them from going out of the plate...
		Ppos <<- ifelse(Mod(Ppos.t) > plate_radius, Ppos - Pspeed * cos(Ptheta.t) - Pspeed * sin(Ptheta.t) * (0+1i), Ppos.t)
	}
}
Pmover <- cmpfun(Pmover)

# predators mortality
Pkiller <- function(t) {
	Pwhich_die <- rbinom(length(Ppos), 1, d) == 1 # Predator mortality. Any that are F live, and T die
	Ppos    <<- Ppos[!Pwhich_die] # drop the ones that die
	Penergy <<- Penergy[!Pwhich_die] # drop the ones that die
	out$P[t]<<- length(Ppos)
}
Pkiller <- cmpfun(Pkiller)

# Prey mortality. Remove any N's that were caught by predators
Nkiller <- function(t) {
	if(torus){
		NPdist <- dist_tor(Npos, Ppos, period = c(Xlim, Ylim))
	} else {
		NPdist <- dist_tor(Npos, Ppos)
	}
	catch.tab <- NPdist < catch_dist                # Which prey get caught by which predators?
	temp <- rowSums(catch.tab)
	temp[temp == 0] <- 1
	catch.tab <- catch.tab/temp

	Npos     <<- Npos[temp == 1]
	Nenergy  <<- Nenergy[temp == 1]
	out$N[t] <<- length(Npos)
}
Nkiller <- cmpfun(Nkiller)


# Prey reproduction
Nreproducer <- function(t) {
	Ncrowding <- apply(dist_tor(Npos, Npos, period = c(Xlim, Ylim)), 2, which_near) # caount up how many other prey are within 5*catch_dist of each individual. this is an index of crowding around each prey
	Nenergy <- Nenergy + (1/(Ncrowding^dd)) # the more crowded you are, the less resource you gather

	Nwhich_reproduce <- Nenergy > Nb #only those prey with enough energy reproduce
	Npos     <<- c(Npos, Npos[Nwhich_reproduce])
	out$N[t] <<- length(Npos) # Record population size
	Nenergy[Nwhich_reproduce] <- Nenergy[Nwhich_reproduce] - Nb # prey energy depletion
	Nenergy  <<- c(Nenergy, rep(1, length = sum(Nwhich_reproduce)))
}
Nreproducer <- cmpfun(Nreproducer)

# predator reproduction
Preproducer <- function(t) {
	if(torus){
		NPdist <- dist_tor(Npos, Ppos, period = c(Xlim, Ylim))
	} else {
		NPdist <- dist_tor(Npos, Ppos)
	}
	catch.tab <- NPdist < catch_dist                # Which prey get caught by which predators?
	Penergy <- Penergy + colSums(catch.tab)
	Pwhich_reproduce <- Penergy > Pb
	Ppos <<- c(Ppos, Ppos[Pwhich_reproduce])
	out$P[t] <<- length(Ppos) # Record population size
	Penergy[Pwhich_reproduce] <- Penergy[Pwhich_reproduce] - Pb #  predator energy depletion
	Penergy <<- c(Penergy, rep(1, length = sum(Pwhich_reproduce))) # predator reproduction
}
Preproducer <- cmpfun(Preproducer)

# this runs the overall model
run_model <- function(plot_frequency = 100){
  initial_setup() # ESTABLISH initial distribution of prey and predators
  for (t in 2:max.time) {
    if (plot_frequency != 0 && t%%plot_frequency == 0) {
      plotNPpos(t)
    }
    # Plot current positions. If model is running slowly, increase interval, or do not specify an interval. Or, to make the model run much faster, comment out this line. THis will disable plotting altogether, but greatly accelerate the speed at which the model runs
  #	text(Re(Npos), Im(Npos), 1:length(Npos), pos = 4)
  #	points(Npos[which_dodge], col = 'green')
  #	points(Npos[closest_prey], col = 'blue', cex = 2)
  Nmover(t)                                        # This Function causes the prey to move
  Pmover(t)                                        # This Function causes the predators to move
  Pkiller(t)                                       # Mortality to predators
  if (out$P[t] <= 0) {cat("\n\n*** Predator extinct ***\n\n"); return(out)} # Stop if predator goes extinct
  Nkiller(t)                                       # Mortality to prey
  if (out$N[t] <= 0) {cat("\n\n*** Prey extinct ***\n\n");	 return(out)} # Stop if prey goes extinct
  Nreproducer(t)                                   # Prey reproduction - density dependent
  Preproducer(t)                                   # Predator reproduction
  if(t %% 100 == 0)cat(paste0(t, " "))
  }
  if(t %% 100 == 0)cat("\n")

  out <- out[!is.na(out$N) & !is.na(out$P), ]  # cut down the output file, if one population went extinct.
  return(out)
}
run_model <- cmpfun(run_model)

plotNPpos <- function(t) {
		if (.Platform$OS.type == "windows") {
			if ("windows" %in% names(dev.list())) { #Open and activate an windows  plotting device, if not already active.
				dev.set(which(names(dev.list()) == "windows") + 1)
			} else {
				windows()
			}
		}
		op <- par(mar = c(0, 0, 0, 0), mfrow = c(1, 1))
		if (arena == "circular") {
			plot(edge, type = "l", asp = 1, axes = F, xlab = "", ylab = "", xpd = NA)
			points(c(plate_radius, plate_radius), c(-plate_radius, plate_radius), pch = "_")
			segments(plate_radius, -plate_radius, plate_radius, 2 * plate_radius * (t/max.time) - plate_radius) # Progress bar
			segments(1.2 * plate_radius, 0, 1.2 * plate_radius, catch_dist, lwd = 3, lend = 2)
		} else {
			plot(c(0, 1.1*Xlim), c(0, Ylim), type = "n", asp = 1, axes = T, xlab = "", ylab = "", xpd = NA)
			rect(0, 0, Xlim, Ylim, lty = ifelse(torus, 3, 1))
			points(1.1* c(Xlim, Xlim), c(0, Ylim), pch = "_")
			segments(1.1 * Xlim, 0, 1.1 * Xlim, Ylim * (t/max.time)) # Progress bar
			segments(1.12 * Xlim, 0, 1.12 * Xlim, catch_dist, lwd = 2) # Catch dist
			segments(1.14 * Xlim, 0, 1.14 * Xlim, Psearch_radius, col = 'red') # Catch dist
			segments(1.16 * Xlim, 0, 1.16 * Xlim, Ndodge_radius, lwd = 2, col = 'red') # Catch dist
		}
		points(Npos, cex = 2 * pmax(0.5, Nenergy/Nb), pch = 1, col = "black")
		points(Ppos, cex = 2 * pmax(0.5, Penergy/Pb), pch = 8, col = "red")
		mtext(sprintf("nN: %d\nnP: %d", length(Npos), length(Ppos)), line = -2, adj = 1)
		par(op)
		Sys.sleep(0.01)
}
plotNPpos <- cmpfun(plotNPpos)



plot_output <- function(input, smooth = 0.03) {
	op <- par(mar = c(4, 4, 1, 1), las = 1, mfrow = c(1, 3), bty = "n")
    input <- input[(0.1*nrow(input)):nrow(input),]
	matplot(input$time, input[, c("N", "P")], type = "l", lty = 1, ylab = "Population sizes", xlab = "Time", ylim = c(0, max(input[, c("N", "P")])), lwd = 2)
	legend("topleft", legend = c("N", "P"), col = 1:2, lwd = 1, cex = 0.8, ncol = 2)

	Nsmooth <- lowess(input$N, f = smooth)
	Psmooth <- lowess(input$P, f = smooth)
	plot(Nsmooth, type = "l", ylab = "smoothed population sizes", xlab = "Time", ylim = range(0, Nsmooth$y, Psmooth$y), lwd = 2)
	lines(Psmooth, type = "l", col = "red", lwd = 2)
	legend("topleft", legend = c("N", "P"), col = 1:2, lwd = 1, cex = 0.8, ncol = 2)


	plot(Nsmooth$y, Psmooth$y, type = "n", xlab = "Prey population", ylab = "Predator population")
	points(Nsmooth$y, Psmooth$y, cex = 0.5, col = rep(rainbow(100), each = nrow(input)/100), pch = 16)
	text(Nsmooth$y[1], Psmooth$y[1], "Start", pos = 4, xpd = NA)
	text(Nsmooth$y[length(Nsmooth$y)], Psmooth$y[length(Psmooth$y)], "End", pos = 4, xpd = NA)
	par(op)
}

summary_output <- function(out) {
    run_time <- nrow(out)
    out <- out[(0.1*nrow(out)):nrow(out),]

    cat(sprintf("Max time:\t%d\nRun time:\t%d\nArena size:\t%1.0g\nMean Prey population:\t%1.0f Range: %1.0f-%1.0f\nMean Pred. population:\t%1.0f Range: %1.0f-%1.0f",
                max.time,
                run_time,
                ifelse(arena == "circular", pi * plate_radius^2, Xlim * Ylim),
                mean(out$N),
                min(out$N),
                max(out$N),
                mean(out$P),
                mean(out$P),
                min(out$P)))
}
