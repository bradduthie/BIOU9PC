#########################################
### This augemnted piece of code includes a facility to ease varying one (or more) parameters systematically, and recording the outcomes.  I set it up to vary Nspeed in 100-unit steps from 100 to 1200. I run 10 replicate simulations of each Nspeed, for 120 runs in total. This takes approximately 10 seconds to run on my lap top. 

#	Note that the parameter definitions MUST be modified to accomodate the systematically varying parameters. See below how I do this (Line 28)

# 	Note that you want to turn off plotting entirely if you use this. 

# 	Note that at the bottom of this modified script are some ways to visualise the output. You'll want to make any output into an attractive figure(s) or table. 
#########################################


### Read in a library
require(spatstat)
source("http://guianaplants.stir.ac.uk/BIOU9PC/Practical_12_functions.R")

#Define the Arena
max.time     <- 1000
torus        <- F # only can be true if arena = 'rect'
arena        <- "circular" # Change to 'circular' for a circular arena
plate_radius <- 30000 # from luckinbill, 60 mm radius is 30 mm radius, is 30000 µm radius # Only used for arena = 'circular'
Xlim         <- c(-50000, 15000) # Only used for arena = 'rect'
Ylim         <- c(-5000,  15000) # Only used for arena = 'rect'


# establish a place to record the outcomes of the repeated simulations
summary.table <- expand.grid(
  parameter1 = 1:10 * 100, 
  run = 1:1, 
  outcome = NA, 
  time = max.time) 
summary.table$outcome <- as.character(summary.table$outcome)

for (i in 1:nrow(summary.table)) {
	cat("Run:", i, "Parameter1 value:", summary.table$parameter1[i], "\n")

	#### START Define parameters of the model ###
	Nspeed <- summary.table$parameter1[i] ### NOTE THAT the Nspeed is defined differently, depending upon the value of i, ie, which row of the summary table we're using. 
	# What is the maximum distance that prey move in one timestep? 
	Pspeed <- 1000 # What is the maximum distance that predator move in one timestep?
	Nb <- 2.01 # how much resource does a prey have to consume to make a new prey. Must exceed 1, as each new prey gets an initial value of 1. 
	Pb <- 2.1 # how many prey does a predator have to consume to make a new predator? Must exceed 1, as each new prey gets an initial value of 1. 
	d <- 0.01 # What's the per-capita likelihood that a predator will die in any time-step?
	dd <- 3.5 # Density dependence for prey. Vary it between 1 & 3 (or so). If it's large, then competiton among prey is strong when density is high. If close to 1, then energy gain is insensitive to density

	catch_dist <- 2 * Pspeed # What is the 'event horizon' for a predator-prey interaction? Ie, at what distance is detection and ingestion inevitable? If predator and prey get this close together, then the prey's a goner!
	Psearch_radius <- 4 * catch_dist # within this distance of a prey, the predator apprches that prey. Otherwise, it moves randomly. Set to 0 for predators to always hunt randomly
	Ndodge_radius <- 4 * catch_dist # Within this distance of a predator, the prey tries to move away from that predator. Set to 0 for prey to always move randomly. 
	### END Definition of parameters  ###
	

	### START INITIAL CONDITIONS
	Nn_init <- 40 # initial number of prey
	Pn_init <- 20 # initial number of predators
	### END INITIAL CONDITIONS
	

	### Run the model ###
	initial_setup() # ESTABLISH initial distribution of prey and predators
	for (t in 2:max.time) {
		cat(t, " ")
		plotNPpos()                        # Plot current positions. If model is running slowly, increase interval, or do not specify an interval. Or, to make the model run much faster, comment out this line. THis will disable plotting altogether, but greatly accelerate the speed at which the model runs
		Nmover() # This Function causes the prey to move
		Pmover() # This Function causes the predators to move
		Pkiller() # Mortality to predators 
		if (out$P[t] <= 0) {
			cat("\n\n*** Predator extinct ***\n\n")
			summary.table$outcome[i] <- "Predator extinct" # record that the predator died
			summary.table$time[i] <- t # Record when the extinction happened
			break
		} # Stop if predator goes extinct
		Nkiller() # Mortality to prey
		if (out$N[t] <= 0) {
			cat("\n\n*** Prey extinct ***\n\n")
			summary.table$outcome[i] <- "Prey extinct" # record that the prey died
			summary.table$time[i] <- t # Record when the extinction happened
			break
		} # Stop if prey goes extinct 
		Nreproducer() # Prey reproduction - density dependent
		Preproducer() # Predator reproduction
	}
	### End running the model ###
	out <- out[!is.na(out$N) & !is.na(out$P), ] # cut down the output file, if one population went extinct. 
	print(summary_output(out))
	
}

# View the summary table
summary.table

# This summarizes the outcomes for each level of parameter1
by(summary.table$outcome, list(Parameter1 = summary.table$parameter1), summary)

# This gives the mean survival time of the community at each level of parameter1
tapply(summary.table$time, INDEX = list(Parameter1 = summary.table$parameter1), mean)


plot_output(out, smooth = 0.1) # increase smoothing to see broader patterns in the model output
