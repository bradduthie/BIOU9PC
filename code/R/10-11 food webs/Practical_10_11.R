#####################################
### BIOU9PC                       ###
### Lab 10&11: Food Web Stability ###
### 2016-11-17 & 2016-11-24       ###
#####################################

### NOTE: add test of reciprocity! 
### makes sure that all links among species are recprocated. if == 1, then completely reciprocal, as it should be. 
### library(igraph)
?reciprocity
x <- matrix(sample(0:1, 16, replace = T),ncol = 4, nrow = 4)
x
x[4,3] <- 1

diag(x) <- 1
x1 <- graph_from_adjacency_matrix(x)
reciprocity(x1)


source("http://guianaplants.stir.ac.uk/BIOU9PC/Practical_10_11_functions.R")
# Global variable
n.runs <- 1000

# model A: 3 spp food chain: Producer, consumer, predator
Ma <- expression({
	n.sp <- 3
	M <- matrix(0, nrow = n.sp, ncol = n.sp)
	M[1, 1] <- -1
	M[1, 2] <- runif(1, -10, 0)
	M[2, 1] <- runif(1, 0, 0.1)
	M[2, 3] <- runif(1, -10, 0)
	M[3, 2] <- runif(1, 0, 0.1)
})

# plot the community diagram
community_diagram(Ma)
# NOTE: using 'labels=T' adds nuMeric interaction strengths to each arrow
community_diagram(Ma, labels = T)
# NOTE: using naMes = c(...) adds descriptive naMes for each species
community_diagram(Ma, names = c("Prod.", 'Cons.', 'Pred.'))

# Print the community matrix.
# Green values are positive, red are negative, and black are zero.
# Bold values are > 1 or < -1
community_matrix(Ma)

# Assess the stability of the community
Model_A <- M_tester(Ma)
# This function returns a data.fraMe indicating the stability and the return tiMe of each of the n.runs repititions. It also returns a data.fraMe indicating which of the species went extinct
str(Model_A)

# Plot the stability of the community
M_plotter(Model_A, model = Ma)

# A description of the stability of the community
M_summary(Model_A)

# Indicate the fraction of simulations in which each species went extinct.
M_extinctions(Model_A)

# model B: 4 spp food chain: Producer, consumer, predator, top predator
Mb <- expression({
	n.sp <- 4
	M <- matrix(0, nrow = n.sp, ncol = n.sp)
	M[1, 1] <- -1
	M[1, 2] <- runif(1, -10, 0)
	M[2, 1] <- runif(1, 0, 0.1)
	M[2, 3] <- runif(1, -10, 0)
	M[3, 2] <- runif(1, 0, 0.1)
	M[3, 4] <- runif(1, -10, 0)
	M[4, 3] <- runif(1, 0, 0.1)
})
community_diagram(Mb, labels = F)

# model C: 4 spp: 2 producers, one consumer, one predator
Mc <- expression({
	n.sp <- 4
	M <- matrix(0, nrow = n.sp, ncol = n.sp)
	M[1, 1] <- -1
	M[2, 2] <- -1
	M[1, 3] <- runif(1, -10, 0.0)
	M[3, 1] <- runif(1,   0, 0.1)
	M[2, 3] <- runif(1, -10, 0.0)
	M[3, 2] <- runif(1,   0, 0.1)
	M[3, 4] <- runif(1, -10, 0.0)
	M[4, 3] <- runif(1,   0, 0.1)
})
community_diagram(Mc, labels = F, trophic.levels = c(1, 1, 2, 3))
# NOTE: using 'trophic.levels = c(1, 2 ...)' will set the vertical position of each species in the diagram

### model D: 4 spp: 2 producers, one consumer, one predator, competition between producers
Md <- expression({
	n.sp <- 5
	M <- matrix(0, nrow = n.sp, ncol = n.sp)
	M[1, 1] <- -1
	M[2, 2] <- -1
	M[1, 2] <- runif(1,  -1, 0.0)
	M[2, 1] <- runif(1,  -1, 0.0)
	M[1, 3] <- runif(1, -10, 0.0)
	M[3, 1] <- runif(1,   0, 0.1)
	M[2, 3] <- runif(1, -10, 0.0)
	M[3, 2] <- runif(1,   0, 0.1)
	M[3, 4] <- runif(1, -10, 0.0)
	M[4, 3] <- runif(1,   0, 0.1)
	M[4, 5] <- runif(1, -10, 0.0)
	M[5, 4] <- runif(1,   0, 0.1)
})
community_diagram(Md, labels = F, trophic.levels = c(1, 1, 2, 3, 4))

# Assess the stability of the community
Model_D <- M_tester(Md)
# A description of the stability of the community
M_summary(Model_D)

# Indicate the fraction of simulations in which each species went extinct.
M_extinctions(Model_D)


### model E: 5 spp: 2 producers, 2 consumers, one predator
Me <- expression({
	n.sp <- 5
	M <- matrix(0, nrow = n.sp, ncol = n.sp)
	M[1,1] <- -1
	M[2,2] <- -1
	Aij <- runif(6, -10, 0.0)
	Aji <- runif(6,   0, 0.1)
	for(i in 1:2){
		M[i, 3] <- Aij[i]
		M[3, i] <- Aji[i]
	}
	for(i in 1:2){
		M[i, 4] <- Aij[i+2]
		M[4, i] <- Aji[i+2]
	}
	for(i in 3:4){
		M[i, 5] <- Aij[i+2]
		M[5, i] <- Aji[i+2]
	}
})
community_diagram(Me, labels = F, trophic.levels = c(1, 1, 2, 2, 3))

### model F: 3 spp food chain
### three-species cycle
Mf <- expression({
	n.sp <- 3
	M <- matrix(0, nrow = n.sp, ncol = n.sp)
	Aij <- runif(2, -10, 0.0)
	Aji <- runif(2,   0, 0.1)
	M[1,1] <- -1
	for(i in 1:2){
		M[i, i+1] <- Aij[i]
		M[i+1, i] <- Aji[i]
	}
	M[1,3] <- runif(1,   0, 0.1)
	M[3,1] <- runif(1, -10, 0.0)
})
community_diagram(Mf, labels = F, trophic.levels = c(1, 1, 2))

### model G: Many species pred-prey food web. Vary n.sp and connectance to evaluate effects
n.sp <- 8          # Should be an even number.
connectance <- 0.4  # Vary between 0 & 1
Mg <- expression({
	n.links <- round(connectance * (n.sp/2)^2)
	M <- matrix(0, nrow = n.sp, ncol = n.sp)
	diag(M)[1:n.sp/2] <- -1

	which.interact <- sample((n.sp/2)^2, n.links)
	temporary <- M[1:(n.sp/2), (1+n.sp/2):n.sp]

	for(j in which.interact){
		M[(1+n.sp/2):n.sp, 1:(n.sp/2)][j] <- runif(1, 0,   1)
		temporary[1:(n.sp/2)  , 1:(n.sp/2)][j] <- runif(1, -10, 0)
		}
	M[1:(n.sp/2), (1+n.sp/2):n.sp] <- t(temporary)
	})
community_diagram(Mg, labels = F, trophic.levels = c(rep(1, n.sp/2), rep(2, n.sp/2)))


### ALTERNATIVE WAY TO DEFINE a community matrix
### M_generator takes a dataframe and generates an community matrix from it.
### It provides an alternate method to specifying community matrix.
### The data.frame can have the following entries in it:
### 0     - indicates no interaction between these species
### WSC   - indicates within-species competition
### ASC   - indicates among-species competition
### eater - indicates that this species consumes another
### eaten - indicates that this species is consumed by another
###
### The dataframe MUST have row and column names.
### The row and column names MUST be identical.
### The dataframe must have the same number of rows and columns.

# first read in the csv file containing the community matrix
dat <- read.csv("example_web.csv", row.names = 1) ### Modify the file name to suit!
M0 <- M_generator(dat) # now, generate the community matrix.
# Now, M0 can be used just like any of the community matrices defined above.
