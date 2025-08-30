###############################################
### BIOU9PC                                 ###
### Lab 12: spatially explicit interactions ###
### 2016-12-01                              ###
###############################################
#source("http://guianaplants.stir.ac.uk/BIOU9PC/Practical_12_functions.R")
source("Practical_12_functions.R")
library(aprof)
?aprof
#Define global settings
max.time     <- 1000
arena        <- 'rect'  # Change to 'circular' for a circular arena
torus        <- T       # Only relevant if arena == 'rect'
plate_radius <- 30000   # Only used for arena = 'circular'
Xlim         <- 150000  # Only used for arena = 'rect'
Ylim         <- 100000  # Only used for arena = 'rect'


#### START Define parameters of the model ###
# Reproduction
Nb     <-    3.01 # how much resource does a prey have to consume to make a new prey. Must exceed 1, as each new prey gets an initial value of 1.
Pb     <-    20.1  # how many prey does a predator have to consume to make a new predator? Must exceed 1, as each new prey gets an initial value of 1.
d      <-    0.01 # What's the per-capita likelihood that a predator will die in any time-step?
dd     <-    1.0  # Density dependence for prey. Vary it between 1 & 3 (or so). If it's large, then competiton among prey is strong when density is high. If close to 1, then energy gain is insensitive to density

# Mobility
Nspeed <-   2000    # What is the maximum distance that prey move in one timestep?
Pspeed <-   2500    # What is the maximum distance that predator move in one timestep?
catch_dist     <- 2*Pspeed      # What is the 'event horizon' for a predator-prey interaction? Ie, at what distance is detection and ingestion inevitable? If predator and prey get this close together, then the prey's a goner!
Psearch_radius <- 7*catch_dist  # within this distance of a prey, the predator apprches that prey. Otherwise, it moves randomly. Set to 0 for predators to always hunt randomly
Ndodge_radius  <- 5*catch_dist  # Within this distance of a predator, the prey tries to move away from that predator. Set to 0 for prey to always move randomly.
### END Definition of parameters  ###


### START INITIAL CONDITIONS
Nn_init <- 100  # initial number of prey
Pn_init <- 10  # initial number of predators
### END INITIAL CONDITIONS



### Run the model ###
out <- run_model(plot_frequency = 20)
# plot_frequency controls how often the positions of predator and prey are plotted. If model is running slowly, increase this interval, or do not specify an interval. set to 0 to suppress plotting, and make the model run much faster!

dump("run_model",file="run_model.R")
source("run_model.R")

## create file to save profiler output
tmp<-tempfile()

## Profile the function
Rprof(tmp,line.profiling=TRUE)
run_model(plot_frequency = 0)
Rprof(append=FALSE)

## Create a aprof object
fooaprof<-aprof("run_model.R",tmp)
## display basic information, summarize and plot the object
fooaprof
summary(fooaprof)
plot(fooaprof)
profileplot(fooaprof)



summary_output(out)
plot_output(out, smooth = 0.1) # increase smoothing to see broader patterns in the model output. These plots OMIT the first 10% of the run to allow the model to stabilize.
par(mfrow = c(1, 2), mar = c(4, 4, 4, 4))
hist(Nenergy, breaks = 20)
hist(Penergy, breaks = 20)
