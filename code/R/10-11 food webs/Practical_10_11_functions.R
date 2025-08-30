#####################################
### BIOU9PC                       ###
### Lab 10&11: Food Web Stability ###
### 2016-11-17 & 2016-11-24       ###
#####################################

if (!"igraph" %in% installed.packages()[, 1]) {
	install.packages('igraph')
}
library(igraph)



# Plot a diagram representing the interactions among in the organisms in the ecological community
community_diagram <- function(model, labels = F, trophic.levels = c(1:n.sp), names = 1:n.sp){
	eval(model)
	stopifnot(length(trophic.levels) == n.sp)
	stopifnot(length(names) == n.sp)
	op <- par(las = 1, bty = 'n', mar = c(2, 1, 1, 1), mfrow = c(1, 1))
	g <- graph.adjacency(M, weighted = T)
	E(g)$color <- ifelse(M[M!= 0]<0, 'red', 'forestgreen')
	W <- abs(M[M!= 0])
	E(g)$width <- 4*((W-min(W))/(max(W)-min(W)))+0.5
	sp.per.trophic.level <- table(trophic.levels)
	Xpos <-	unlist(sapply(sp.per.trophic.level, FUN = function(x){if(x == 1){mean(range(sp.per.trophic.level))} else{seq(1, x, by = 1)}}))
	L <- matrix(c(Xpos, trophic.levels), nrow = n.sp)
	if(labels == T){
		plot(g, edge.loop.angle = pi/2, vertex.size = 30, vertex.color = 'white', layout = L, edge.curved = 0.3, edge.arrow.size = E(g)$width, edge.label = sprintf('%1.2f', M[M!= 0]), vertex.label = names)
	} else {
		plot(g, edge.loop.angle = pi/2, vertex.size = 30, vertex.color = 'white', layout = L, edge.curved = 0.3, edge.arrow.size = E(g)$width, vertex.label = names)
	}
	par(op)
}


community_matrix <- function(model){
    eval(model)
    out <- array(NA, dim = c(n.sp, n.sp, 100))
    for(i in 1:100){eval(model);out[,,i] <- M}
    M <- apply(out, 1:2, mean)
    plot(1, type = 'n', ann = F, axes = F)
    mtext(sprintf('%+1.2f', M), family = 'mono', cex = 1, adj = rep(seq(0,  min(0.15*(n.sp), 1), length = n.sp), each = n.sp), line = 1:-(n.sp-2), col = ifelse(M<0, 'red', ifelse(M>0, 'forestgreen', 'black')), font = ifelse(abs(M)>1, 2, 1))
}

# Test the stability of the ecological community expressed in the matrix 'model'
M_tester <- function(model) {
    eval(model)
    out <- data.frame(runs = 1:n.runs, return_time = NA, stable = NA)
    which_dies <- matrix(0, ncol = n.sp, nrow = n.runs)
    for (run in 1:n.runs) {
        eval(model)
        eigens <- Re(eigen(M)$values)
        neg.eigens <- eigens[eigens < 0]
        out$stable[run] <- length(neg.eigens) == n.sp
        out$return_time[run] <- -1/max(neg.eigens)
        temp <- eigens == max(eigens) & eigens>=0
        if(sum(temp)>0){which_dies[run,] <-temp/sum(temp)}
    }
    return(c(list(out = out, which_dies = which_dies)))
}


M_plotter <- function(input, model) {
	op <- par(las = 1, bty = 'n', xpd = F, tcl = 0.2)
	eval(model)
	input <- input$out
	unstable <- input[input$stable == F, ]
	stable <- input[input$stable == T, ]
	hist(log(stable$return_time, 10), breaks = min(100, n.runs/10), main = '', xlab = "Return time", border = NA, col = "gray", xaxt = 'n')
	abline(v = quantile(log(stable$return_time, 10), c(0.25, 0.50, 0.75)), lty = c(2, 1, 2))
	axis(1, at = 1:max(round(log(stable$return_time, 10))), labels = 10^(1:max(round(log(stable$return_time, 10)))))
	par(op)
}

M_summary <- function(input){
    input <- input$out
    unstable <- input[input$stable == F, ]
    stable <- input[input$stable == T, ]
    summ <- data.frame(value = c(median(stable$return_time), quantile(stable$return_time, 0.25), quantile(stable$return_time, 0.75),length(stable$return_time[stable$return_time > 100])/nrow(stable) * 100,length(unstable$return_time)/nrow(input)))
    rownames(summ) <- c("Median return time", "25 quantile of RT", "75 quantile of RT", "Percent of RT > 100", "Percent of runs with extinction(s)")
    return(summ)
}

M_extinctions <- function(input, names = 1:n.sp){
    wd  <- colSums(input$which_dies)/nrow(input$which_dies)
    n.sp <- length(wd)
    names(wd) <- names
    return(wd)

}


### THis function takes a dataframe and generates an interaction matrix from it.
### It provides an alternate method to specifying interaction matrices.
### The data.frame can have the following entries in it:
###
### 0     - indicates no interaction
### WSC   - indicates within-species competition
### ASC   - indicates among-species competition
### eater - indicates that this species eats another
### eaten - indicates that this species is eaten by another
###
### The dataframe MUST have row and column names, and they MUST be identical
### The datafrae must have the same number of rows and columns.
M_generator <- function(dat){
    stopifnot(identical(ncol(dat), nrow(dat)))
    stopifnot(identical(names(dat), rownames(dat)))
    stopifnot(unique(unlist(dat)) %in% c("WSC","ASC","eaten", "eater", 0))
    expr <- expression({
        n.sp <- ncol(dat)
        M <- matrix(0, ncol = ncol(dat), nrow = nrow(dat))
        M[dat == "WSC"]   <- -1
        M[dat == "ASC"]   <- runif(sum(dat == "ASC"),  -1, 0.0)
        M[dat == "eater"] <- runif(sum(dat == "eater"), 0, 0.1)
        M[dat == "eaten"] <- runif(sum(dat == "eaten"), -10, 0)
    })
    return(expr)
}

